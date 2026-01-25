#!/usr/bin/env node
/**
 * tcelm2-bundle.js - Multi-module bundler for tcelm2
 *
 * This script resolves imports, collects all module sources,
 * and generates a single bundled C file using tcelm2.
 *
 * Usage: node bin/tcelm2-bundle.js <source.elm>
 */

const fs = require('fs');
const path = require('path');

// Builtin modules that don't need compilation
const BUILTINS = new Set([
    'Basics', 'String', 'List', 'Maybe', 'Result',
    'Tuple', 'Debug', 'Char', 'Platform', 'Bitwise',
    'Array', 'Dict', 'Set', 'Json.Decode', 'Json.Encode',
    'Platform.Cmd', 'Platform.Sub', 'Types'
]);

// RTEMS modules handled by runtime headers
const RTEMS_MODULES = new Set([
    'Rtems.Task', 'Rtems.Interrupt', 'Rtems.Uart', 'Rtems.Gpio',
    'Rtems.Sync', 'Rtems.Semaphore', 'Rtems.Mutex', 'Rtems.Events',
    'Rtems.Clock', 'Rtems.Barrier', 'Rtems.SMP', 'Rtems.SPI',
    'Rtems.I2C', 'Rtems.ADC', 'Rtems.DAC', 'Rtems.Socket',
    'Rtems.Network', 'Rtems.Timer', 'Rtems.Channel', 'Rtems.MVar',
    'Rtems.Budget', 'Rtems.Protected', 'RMS', 'Rtems.RMS'
]);

function isBuiltin(name) {
    return BUILTINS.has(name) || RTEMS_MODULES.has(name) || name.startsWith('Rtems.');
}

/**
 * Resolve a module name to its source file path
 */
function resolveModulePath(baseDir, moduleName) {
    const relativePath = moduleName.replace(/\./g, '/') + '.elm';
    const candidates = [
        path.join(baseDir, 'lib', relativePath),
        path.join(baseDir, 'src', relativePath),
        path.join(baseDir, 'src2', relativePath),
        path.join(baseDir, relativePath)
    ];

    for (const candidate of candidates) {
        if (fs.existsSync(candidate)) {
            return candidate;
        }
    }
    return null;
}

/**
 * Extract import names from Elm source
 */
function extractImports(source) {
    const imports = [];
    const importRegex = /^import\s+([\w.]+)/gm;
    let match;
    while ((match = importRegex.exec(source)) !== null) {
        imports.push(match[1]);
    }
    return imports;
}

/**
 * Extract module name from source
 */
function extractModuleName(source) {
    const match = source.match(/^(?:port\s+)?module\s+([\w.]+)/m);
    return match ? match[1] : 'Main';
}

/**
 * Collect all modules recursively
 */
function collectModules(baseDir, mainSource) {
    const modules = new Map(); // name -> { source, imports, order }
    const queue = [{ name: extractModuleName(mainSource), source: mainSource, isMain: true }];
    let order = 0;

    while (queue.length > 0) {
        const { name, source, isMain } = queue.shift();

        if (modules.has(name)) continue;

        const imports = extractImports(source).filter(i => !isBuiltin(i));
        modules.set(name, { source, imports, order: order++, isMain });

        // Queue imports
        for (const importName of imports) {
            if (!modules.has(importName)) {
                const importPath = resolveModulePath(baseDir, importName);
                if (importPath) {
                    try {
                        const importSource = fs.readFileSync(importPath, 'utf8');
                        queue.push({ name: importName, source: importSource, isMain: false });
                    } catch (e) {
                        console.error(`Warning: Could not read module ${importName} from ${importPath}`);
                    }
                } else {
                    console.error(`Warning: Could not find module ${importName}`);
                }
            }
        }
    }

    return modules;
}

/**
 * Topological sort modules by dependencies
 */
function sortModules(modules) {
    const sorted = [];
    const visited = new Set();
    const visiting = new Set();

    function visit(name) {
        if (visited.has(name)) return;
        if (visiting.has(name)) {
            console.error(`Warning: Circular dependency involving ${name}`);
            return;
        }

        const mod = modules.get(name);
        if (!mod) return;

        visiting.add(name);
        for (const dep of mod.imports) {
            visit(dep);
        }
        visiting.delete(name);
        visited.add(name);
        sorted.push(name);
    }

    // Visit all modules
    for (const name of modules.keys()) {
        visit(name);
    }

    return sorted;
}

/**
 * Load the tcelm2 compiler
 */
function loadCompiler() {
    const elmModule = require('./tcelm2-compiler.js');
    const Elm = elmModule.Elm || global.Elm || this.Elm;

    if (!Elm || !Elm.Compiler) {
        console.error('Failed to load Elm.Compiler module');
        process.exit(1);
    }
    return Elm;
}

/**
 * Compile a single module to C code using tcelm2
 */
function compileModule(Elm, source, target) {
    return new Promise((resolve, reject) => {
        const app = Elm.Compiler.init({
            flags: {
                source: source,
                target: target
            }
        });

        let resolved = false;

        app.ports.printOutput.subscribe(function(output) {
            if (!resolved) {
                resolved = true;
                resolve(output);
            }
        });

        app.ports.printErrors.subscribe(function(errors) {
            if (!resolved) {
                resolved = true;
                reject(new Error(errors.join('\n')));
            }
        });
    });
}

/**
 * Extract type tags from compiled C code
 */
function extractTypeTags(cCode) {
    const tags = [];
    const tagRegex = /#define\s+(TAG_\w+)\s+(\d+)/g;
    let match;
    while ((match = tagRegex.exec(cCode)) !== null) {
        tags.push({ name: match[1], value: parseInt(match[2]), line: match[0] });
    }
    return tags;
}

/**
 * Extract just the module-specific code (skip headers/runtime and type tags)
 */
function extractModuleCode(cCode, moduleName) {
    // Find where module-specific content starts
    const forwardDeclStart = cCode.indexOf('/* Forward declarations */');
    const userFuncsStart = cCode.indexOf('/* User-defined functions */');
    const mainStart = cCode.indexOf('/* Main */');
    const elmMainStart = cCode.indexOf('static elm_value_t elm_main');
    const intMainStart = cCode.indexOf('int main(void)');

    // Find earliest module content
    const candidates = [forwardDeclStart, userFuncsStart]
        .filter(x => x !== -1);
    let start = candidates.length > 0 ? Math.min(...candidates) : -1;

    // Find end marker
    let end = -1;
    if (mainStart !== -1) {
        end = mainStart;
    } else if (elmMainStart !== -1) {
        end = elmMainStart;
    } else if (intMainStart !== -1) {
        end = intMainStart;
    } else {
        end = cCode.length;
    }

    if (start === -1 || start >= end) {
        return `/* Module ${moduleName} - no extractable code */\n`;
    }

    return `/* === Module: ${moduleName} === */\n` + cCode.substring(start, end);
}

/**
 * Extract headers and runtime from compiled C
 */
function extractHeadersAndRuntime(cCode) {
    const forwardDeclStart = cCode.indexOf('/* Forward declarations */');
    const userFuncsStart = cCode.indexOf('/* User-defined functions */');
    const mainStart = cCode.indexOf('/* Main */');
    const elmMainStart = cCode.indexOf('static elm_value_t elm_main');
    const intMainStart = cCode.indexOf('int main(void)');

    const end = Math.min(
        forwardDeclStart !== -1 ? forwardDeclStart : cCode.length,
        userFuncsStart !== -1 ? userFuncsStart : cCode.length,
        mainStart !== -1 ? mainStart : cCode.length,
        elmMainStart !== -1 ? elmMainStart : cCode.length,
        intMainStart !== -1 ? intMainStart : cCode.length
    );

    return cCode.substring(0, end);
}

/**
 * Extract main function from compiled C
 */
function extractMainSection(cCode) {
    const mainStart = cCode.indexOf('/* Main */');
    if (mainStart !== -1) {
        return cCode.substring(mainStart);
    }
    const elmMainStart = cCode.indexOf('static elm_value_t elm_main');
    if (elmMainStart !== -1) {
        return cCode.substring(elmMainStart);
    }
    const intMainStart = cCode.indexOf('int main(void)');
    if (intMainStart !== -1) {
        return cCode.substring(intMainStart);
    }
    return '';
}

/**
 * Main bundling function
 */
async function bundle(sourceFile) {
    const baseDir = path.dirname(path.resolve(sourceFile));
    const projectDir = findProjectRoot(baseDir);

    // Read main source
    const mainSource = fs.readFileSync(sourceFile, 'utf8');

    // Collect all modules
    const modules = collectModules(projectDir, mainSource);

    if (modules.size === 1) {
        // Single module - just compile normally
        const Elm = loadCompiler();
        const code = await compileModule(Elm, mainSource, 'main');
        return code;
    }

    // Sort by dependencies
    const sortedNames = sortModules(modules);

    console.error(`Bundling ${modules.size} modules: ${sortedNames.join(', ')}`);

    // Load compiler once
    const Elm = loadCompiler();

    // Compile each module
    const compiledModules = new Map();
    for (const name of sortedNames) {
        const mod = modules.get(name);
        try {
            const code = await compileModule(Elm, mod.source, 'main');
            compiledModules.set(name, { code, isMain: mod.isMain });
        } catch (e) {
            console.error(`Error compiling module ${name}:`, e.message);
        }
    }

    // Find the main module
    let mainModuleName = null;
    for (const [name, mod] of modules) {
        if (mod.isMain) {
            mainModuleName = name;
            break;
        }
    }

    // Build bundled output
    const mainCode = compiledModules.get(mainModuleName);
    if (!mainCode) {
        console.error('Main module not compiled');
        process.exit(1);
    }

    // Extract headers/runtime from main module
    const headers = extractHeadersAndRuntime(mainCode.code);

    // Collect all type tags from all modules (avoid duplicates)
    const allTypeTags = new Map();
    for (const name of sortedNames) {
        const compiled = compiledModules.get(name);
        if (compiled) {
            const tags = extractTypeTags(compiled.code);
            for (const tag of tags) {
                if (!allTypeTags.has(tag.name)) {
                    allTypeTags.set(tag.name, { value: tag.value, line: tag.line });
                }
            }
        }
    }

    // Generate merged type tags section
    const typeTagsSection = allTypeTags.size > 0
        ? '/* ========== MERGED TYPE TAGS ========== */\n' +
          Array.from(allTypeTags.values()).map(t => t.line).join('\n') + '\n\n'
        : '';

    // Extract module code for dependencies (in order)
    const depCode = [];
    for (const name of sortedNames) {
        if (name === mainModuleName) continue;

        const compiled = compiledModules.get(name);
        if (compiled) {
            depCode.push(extractModuleCode(compiled.code, name));
        }
    }

    // Extract main module's code and main function
    const mainModuleCode = extractModuleCode(mainCode.code, mainModuleName);
    const mainSection = extractMainSection(mainCode.code);

    // Combine everything
    return [
        headers,
        typeTagsSection,
        '\n/* ========== BUNDLED MODULES ========== */\n',
        depCode.join('\n'),
        '\n/* ========== MAIN MODULE ========== */\n',
        mainModuleCode,
        mainSection
    ].join('\n');
}

/**
 * Find project root (directory containing elm.json)
 */
function findProjectRoot(startDir) {
    let dir = startDir;
    while (dir !== path.dirname(dir)) {
        if (fs.existsSync(path.join(dir, 'elm.json'))) {
            return dir;
        }
        dir = path.dirname(dir);
    }
    return startDir;
}

// Main
const args = process.argv.slice(2);
if (args.length < 1) {
    console.error('Usage: tcelm2-bundle <source.elm>');
    process.exit(1);
}

const sourceFile = args[0];

bundle(sourceFile)
    .then(code => console.log(code))
    .catch(err => {
        console.error('Bundle error:', err);
        process.exit(1);
    });
