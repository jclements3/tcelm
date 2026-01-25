#!/usr/bin/env node
/**
 * tcelm2-merge.js - Multi-module merger for tcelm2
 *
 * This script merges all modules into a single Elm source file,
 * then compiles it with tcelm2.
 *
 * Approach:
 * 1. Collect all module sources in dependency order
 * 2. Extract declarations from each (remove module/import headers)
 * 3. Merge into a single Main module with all declarations
 * 4. Compile the merged source
 *
 * Usage: node bin/tcelm2-merge.js <source.elm>
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
    const modules = new Map(); // name -> { source, imports }
    const queue = [{ name: extractModuleName(mainSource), source: mainSource, isMain: true }];

    while (queue.length > 0) {
        const { name, source, isMain } = queue.shift();

        if (modules.has(name)) continue;

        const imports = extractImports(source).filter(i => !isBuiltin(i));
        modules.set(name, { source, imports, isMain });

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
 * Extract all import statements from source (for hoisting to top)
 */
function extractAllImports(source) {
    const imports = [];
    const importRegex = /^import\s+.+$/gm;
    let match;
    while ((match = importRegex.exec(source)) !== null) {
        imports.push(match[0]);
    }
    return imports;
}

/**
 * Extract the body of an Elm module (everything after module/import declarations)
 * Also removes all import statements from the body
 */
function extractModuleBody(source) {
    const lines = source.split('\n');
    let bodyStartLine = 0;
    let inBlockComment = false;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();

        // Track block comments (including doc comments)
        if (line.includes('{-')) {
            inBlockComment = true;
        }
        if (line.includes('-}')) {
            inBlockComment = false;
            bodyStartLine = i + 1;
            continue;
        }

        // Skip module declaration, exposing, and imports
        if (line.startsWith('module ') || line.startsWith('port module ') ||
            line.startsWith('import ') || line.startsWith('exposing') ||
            line.startsWith('(') || line.startsWith(',') ||
            line.startsWith(')') || line === '' ||
            line.startsWith('--') || inBlockComment) {
            bodyStartLine = i + 1;
        } else if (line && !inBlockComment) {
            // Found first real content (not in block comment)
            break;
        }
    }

    // Get body lines and filter out any import statements
    const bodyLines = lines.slice(bodyStartLine).filter(line => {
        const trimmed = line.trim();
        return !trimmed.startsWith('import ');
    });

    return bodyLines.join('\n');
}

/**
 * Prefix all top-level definitions with module name
 */
function prefixDeclarations(source, moduleName) {
    const prefix = moduleName.replace(/\./g, '_');

    // This is a simplified version - a proper implementation would parse the AST
    // For now, we'll handle common patterns

    let result = source;

    // Find all type declarations: "type Foo = ..." or "type alias Foo = ..."
    const typePattern = /^(type\s+(?:alias\s+)?)([A-Z][a-zA-Z0-9_]*)/gm;
    result = result.replace(typePattern, `$1${prefix}_$2`);

    // Find all top-level function definitions: "foo x y = ..."
    // This is tricky because we need to not replace local bindings
    // For simplicity, only replace lines that start at column 0 with lowercase
    const lines = result.split('\n');
    const processedLines = lines.map(line => {
        // Check for top-level function definition (lowercase name at start)
        const fnMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*([:=])/);
        if (fnMatch && fnMatch[1] !== 'if' && fnMatch[1] !== 'then' && fnMatch[1] !== 'else' &&
            fnMatch[1] !== 'case' && fnMatch[1] !== 'of' && fnMatch[1] !== 'let' && fnMatch[1] !== 'in') {
            // Replace the function name with prefixed version
            return `${prefix}_${line}`;
        }
        return line;
    });

    // Also replace references to the module's own functions
    // This is very simplified and won't handle all cases

    return processedLines.join('\n');
}

/**
 * Rewrite qualified names (Module.func) to unqualified or prefixed names
 */
function rewriteQualifiedNames(source, moduleNames) {
    let result = source;

    // For each non-builtin module, replace qualified names
    for (const modName of moduleNames) {
        // Use lowercase prefix for functions (Elm functions must start lowercase)
        const lowerPrefix = modName.replace('.', '_').toLowerCase();
        // Use original case for types
        const upperPrefix = modName.replace('.', '_');

        // Replace Module.function with module_function (functions start lowercase)
        const funcPattern = new RegExp(`\\b${modName.replace('.', '\\.')}\\.([a-z][a-zA-Z0-9_]*)`, 'g');
        result = result.replace(funcPattern, `${lowerPrefix}_$1`);

        // Replace Module.Type with Module_Type (types start uppercase)
        const typePattern = new RegExp(`\\b${modName.replace('.', '\\.')}\\.([A-Z][a-zA-Z0-9_]*)`, 'g');
        result = result.replace(typePattern, `${upperPrefix}_$1`);
    }

    return result;
}

/**
 * Collect all type names defined in a module (both aliases and union types)
 */
function collectTypeNames(source) {
    const typeNames = [];
    const typePattern = /^type\s+(?:alias\s+)?([A-Z][a-zA-Z0-9_]*)/gm;
    let match;
    while ((match = typePattern.exec(source)) !== null) {
        typeNames.push(match[1]);
    }
    return typeNames;
}

/**
 * Collect constructor names from union types
 * e.g., "type Foo = Bar | Baz Int" -> ["Bar", "Baz"]
 */
function collectConstructorNames(source) {
    const ctorNames = [];
    const lines = source.split('\n');
    let inTypeDef = false;

    for (const line of lines) {
        const trimmed = line.trim();

        // Start of union type definition (not alias)
        if (trimmed.match(/^type\s+[A-Z]/) && !trimmed.match(/^type\s+alias\s+/)) {
            inTypeDef = true;
            // Check for constructors on same line as type keyword
            const sameLineMatch = trimmed.match(/^type\s+[A-Z][a-zA-Z0-9_]*[^=]*=\s*(.+)/);
            if (sameLineMatch) {
                const rest = sameLineMatch[1];
                extractCtorsFromLine(rest, ctorNames);
            }
            continue;
        }

        // Inside type definition - look for constructors
        if (inTypeDef) {
            // Check if we've left the type definition
            if (trimmed && !trimmed.startsWith('|') && !trimmed.startsWith('=')) {
                if (trimmed.match(/^[a-z]/) || trimmed.match(/^type\s+/) ||
                    trimmed.match(/^--/) || trimmed.match(/^{-/)) {
                    inTypeDef = false;
                    continue;
                }
            }

            extractCtorsFromLine(trimmed, ctorNames);
        }
    }

    return ctorNames;
}

/**
 * Extract constructor names from a line of a type definition
 */
function extractCtorsFromLine(line, ctorNames) {
    // Remove leading = or |
    let rest = line.replace(/^[|=]\s*/, '');
    if (!rest) return;

    // Match constructor name at start of each variant
    // Constructor is uppercase identifier at start, possibly followed by type args
    const ctorMatch = rest.match(/^([A-Z][a-zA-Z0-9_]*)/);
    if (ctorMatch) {
        const ctorName = ctorMatch[1];
        if (!ctorNames.includes(ctorName)) {
            ctorNames.push(ctorName);
        }
    }
}

/**
 * Collect all function names defined in a module
 */
function collectFunctionNames(source) {
    const funcNames = [];
    const lines = source.split('\n');
    for (const line of lines) {
        // Function with type annotation
        const typeAnnotMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*:/);
        if (typeAnnotMatch) {
            const name = typeAnnotMatch[1];
            if (!['if', 'then', 'else', 'case', 'of', 'let', 'in', 'type', 'module', 'import', 'exposing', 'port'].includes(name)) {
                if (!funcNames.includes(name)) funcNames.push(name);
            }
        }
        // Function definition
        const fnMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s+[^=]*=/);
        if (fnMatch) {
            const name = fnMatch[1];
            if (!['if', 'then', 'else', 'case', 'of', 'let', 'in', 'type', 'module', 'import', 'exposing', 'port'].includes(name)) {
                if (!funcNames.includes(name)) funcNames.push(name);
            }
        }
        // Value definition
        const valMatch = line.match(/^([a-z][a-zA-Z0-9_]*)\s*=/);
        if (valMatch) {
            const name = valMatch[1];
            if (!['if', 'then', 'else', 'case', 'of', 'let', 'in', 'type', 'module', 'import', 'exposing', 'port'].includes(name)) {
                if (!funcNames.includes(name)) funcNames.push(name);
            }
        }
    }
    return funcNames;
}

/**
 * Prefix all top-level definitions in a module with the module name
 * Uses lowercase prefix for functions, uppercase for types and constructors
 */
function prefixModuleDefinitions(source, moduleName) {
    // Get prefix forms
    const lowerPrefix = moduleName.replace(/\./g, '_').toLowerCase();
    const upperPrefix = moduleName.replace(/\./g, '_');  // Keep case for types (e.g., AST -> AST_)

    // Collect all names defined in this module
    const typeNames = collectTypeNames(source);
    const ctorNames = collectConstructorNames(source);
    const funcNames = collectFunctionNames(source);

    let result = source;

    // First, rename type declarations: "type alias Foo" -> "type alias Module_Foo"
    for (const typeName of typeNames) {
        // Replace type definition
        result = result.replace(
            new RegExp(`^(type\\s+(?:alias\\s+)?)${typeName}\\b`, 'gm'),
            `$1${upperPrefix}_${typeName}`
        );
        // Replace all references to this type (uppercase, not at start of line)
        // Be careful not to replace inside strings or qualified names
        result = result.replace(
            new RegExp(`(?<!\\.)\\b${typeName}\\b(?!\\.)`, 'g'),
            `${upperPrefix}_${typeName}`
        );
    }

    // Rename constructors in type definitions and usages
    for (const ctorName of ctorNames) {
        // Skip if it's the same as a type name (we already handled it)
        if (typeNames.includes(ctorName)) continue;

        // Replace constructor references (uppercase identifier)
        result = result.replace(
            new RegExp(`(?<!\\.)\\b${ctorName}\\b`, 'g'),
            `${upperPrefix}_${ctorName}`
        );
    }

    // Then, rename function definitions and type annotations
    for (const funcName of funcNames) {
        // Replace type annotation
        result = result.replace(
            new RegExp(`^${funcName}\\s*:`, 'gm'),
            `${lowerPrefix}_${funcName} :`
        );
        // Replace function definition
        result = result.replace(
            new RegExp(`^${funcName}\\s+([^=]*)=`, 'gm'),
            `${lowerPrefix}_${funcName} $1=`
        );
        // Replace value definition (no args)
        result = result.replace(
            new RegExp(`^${funcName}\\s*=`, 'gm'),
            `${lowerPrefix}_${funcName} =`
        );
        // Replace references to this function within the module
        result = result.replace(
            new RegExp(`(?<!\\.)\\b${funcName}\\b(?!\\s*[=:])`, 'g'),
            `${lowerPrefix}_${funcName}`
        );
    }

    return result;
}

/**
 * Merge all modules into a single source
 */
function mergeModules(modules, sortedNames, mainModuleName) {
    const mergedParts = [];
    const nonMainModuleNames = sortedNames.filter(n => {
        const mod = modules.get(n);
        return mod && !mod.isMain;
    });

    // Collect all imports from all modules
    const allImports = new Set();
    for (const name of sortedNames) {
        const mod = modules.get(name);
        if (!mod) continue;
        const imports = extractAllImports(mod.source);
        for (const imp of imports) {
            // Filter out imports of modules we're merging (they'll be inline)
            const importedModule = imp.match(/^import\s+([\w.]+)/)?.[1];
            if (importedModule && !modules.has(importedModule)) {
                allImports.add(imp);
            }
        }
    }

    // Add module header
    mergedParts.push('module Main exposing (main)');
    mergedParts.push('');

    // Add all imports (builtin modules only)
    if (allImports.size > 0) {
        for (const imp of allImports) {
            mergedParts.push(imp);
        }
        mergedParts.push('');
    }

    // Add merged declarations from all modules (in dependency order)
    for (const name of sortedNames) {
        const mod = modules.get(name);
        if (!mod) continue;

        let body = extractModuleBody(mod.source);
        if (!body.trim()) continue;

        mergedParts.push(`-- === Module: ${name} ===`);

        if (mod.isMain) {
            // For main module, rewrite qualified names to prefixed names
            body = rewriteQualifiedNames(body, nonMainModuleNames);
            mergedParts.push(body);
        } else {
            // For non-main modules:
            // 1. Prefix function names with module name
            // 2. Rewrite any qualified references within the module
            body = prefixModuleDefinitions(body, name);
            body = rewriteQualifiedNames(body, nonMainModuleNames);
            mergedParts.push(body);
        }
        mergedParts.push('');
    }

    return mergedParts.join('\n');
}

/**
 * Load and run tcelm2 compiler
 */
function compileWithTcelm2(source, target) {
    return new Promise((resolve, reject) => {
        const elmModule = require('./tcelm2-compiler.js');
        const Elm = elmModule.Elm || global.Elm;

        if (!Elm || !Elm.Compiler) {
            reject(new Error('Failed to load Elm.Compiler module'));
            return;
        }

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
 * Main merge and compile function
 */
async function mergeAndCompile(sourceFile, target = 'main') {
    const baseDir = path.dirname(path.resolve(sourceFile));
    const projectDir = findProjectRoot(baseDir);

    // Read main source
    const mainSource = fs.readFileSync(sourceFile, 'utf8');
    const mainModuleName = extractModuleName(mainSource);

    // Collect all modules
    const modules = collectModules(projectDir, mainSource);

    if (modules.size === 1) {
        // Single module - just compile normally
        const code = await compileWithTcelm2(mainSource, target);
        return code;
    }

    // Sort by dependencies
    const sortedNames = sortModules(modules);

    console.error(`Merging ${modules.size} modules: ${sortedNames.join(', ')}`);

    // Merge into single source
    const mergedSource = mergeModules(modules, sortedNames, mainModuleName);

    // Debug: write merged source
    fs.writeFileSync('/tmp/merged.elm', mergedSource);
    console.error('Merged source written to /tmp/merged.elm');

    // Compile merged source
    const code = await compileWithTcelm2(mergedSource, target);
    return code;
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
    console.error('Usage: tcelm2-merge <source.elm>');
    process.exit(1);
}

const sourceFile = args[0];

mergeAndCompile(sourceFile)
    .then(code => console.log(code))
    .catch(err => {
        console.error('Merge/compile error:', err.message);
        process.exit(1);
    });
