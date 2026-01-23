#!/usr/bin/env node
/**
 * tcelm-bundle.js - Multi-module bundler for tcelm
 *
 * This script resolves imports, collects all module sources,
 * and generates a single bundled C file.
 *
 * Usage: node bin/tcelm-bundle.js <source.elm> [--target <target>]
 */

const fs = require('fs');
const path = require('path');

// Builtin modules that don't need compilation
const BUILTINS = new Set([
    'Basics', 'String', 'List', 'Maybe', 'Result',
    'Tuple', 'Debug', 'Char', 'Platform', 'Bitwise',
    'Array', 'Dict', 'Set', 'Json.Decode', 'Json.Encode',
    'Platform.Cmd', 'Platform.Sub'
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
    const match = source.match(/^module\s+([\w.]+)/m);
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
 * Load the Elm compiler
 */
function loadCompiler() {
    const elmModule = require('./tcelm-compiler.js');
    const Elm = elmModule.Elm || global.Elm || this.Elm;

    if (!Elm || !Elm.Cli) {
        console.error('Failed to load Elm.Cli module');
        process.exit(1);
    }
    return Elm;
}

/**
 * Compile a single module to C code
 */
function compileModule(Elm, source, target) {
    return new Promise((resolve, reject) => {
        const app = Elm.Cli.init({ flags: { target } });

        app.ports.sendOutput.subscribe(function(result) {
            if (result.success) {
                resolve(result.code);
            } else {
                reject(new Error(result.error));
            }
        });

        app.ports.receiveSource.send(source);
    });
}

/**
 * Extract just the module-specific code (skip headers/runtime)
 */
function extractModuleCode(cCode, moduleName) {
    // Find where user-defined types/functions start
    const typeTagsStart = cCode.indexOf('/* Type tags for');
    const userFuncsStart = cCode.indexOf('/* User-defined functions */');
    const mainStart = cCode.indexOf('/* Elm main value */');
    const cMainStart = cCode.indexOf('int main(void)');

    let start = typeTagsStart !== -1 ? typeTagsStart : userFuncsStart;
    let end = mainStart !== -1 ? mainStart : cMainStart;

    if (start === -1) {
        // Try to find forward declarations
        start = cCode.indexOf('/* Forward declarations */');
    }

    if (start === -1 || end === -1 || start >= end) {
        return `/* Module ${moduleName} - no extractable code */\n`;
    }

    return `/* === Module: ${moduleName} === */\n` + cCode.substring(start, end);
}

/**
 * Extract headers and runtime from compiled C
 */
function extractHeadersAndRuntime(cCode) {
    const userFuncsStart = cCode.indexOf('/* Forward declarations */');
    const typeTagsStart = cCode.indexOf('/* Type tags for');

    const end = Math.min(
        userFuncsStart !== -1 ? userFuncsStart : cCode.length,
        typeTagsStart !== -1 ? typeTagsStart : cCode.length
    );

    return cCode.substring(0, end);
}

/**
 * Extract main function from compiled C
 */
function extractMainSection(cCode) {
    const mainStart = cCode.indexOf('/* Elm main value */');
    if (mainStart === -1) {
        const cMainStart = cCode.indexOf('int main(void)');
        return cMainStart !== -1 ? cCode.substring(cMainStart) : '';
    }
    return cCode.substring(mainStart);
}

/**
 * Main bundling function
 */
async function bundle(sourceFile, target) {
    const baseDir = path.dirname(path.resolve(sourceFile));
    const projectDir = findProjectRoot(baseDir);

    // Read main source
    const mainSource = fs.readFileSync(sourceFile, 'utf8');

    // Collect all modules
    const modules = collectModules(projectDir, mainSource);

    if (modules.size === 1) {
        // Single module - just compile normally
        const Elm = loadCompiler();
        const code = await compileModule(Elm, mainSource, target);
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
            const code = await compileModule(Elm, mod.source, target);
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
    const allTypeTags = new Set();
    const typeTagLines = [];

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
    console.error('Usage: tcelm-bundle <source.elm> [--target <target>]');
    process.exit(1);
}

const sourceFile = args[0];
let target = 'tcc';

const targetIdx = args.indexOf('--target');
if (targetIdx !== -1 && args[targetIdx + 1]) {
    target = args[targetIdx + 1];
}

bundle(sourceFile, target)
    .then(code => console.log(code))
    .catch(err => {
        console.error('Bundle error:', err);
        process.exit(1);
    });
