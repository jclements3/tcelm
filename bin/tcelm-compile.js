#!/usr/bin/env node
/**
 * tcelm-compile.js - Node.js wrapper for tcelm Elm compiler
 *
 * Usage: tcelm-compile.js <input.elm> [--target <target>]
 *
 * Reads Elm source, compiles to C code, outputs to stdout.
 */

const fs = require('fs');
const path = require('path');
const vm = require('vm');

// Load the compiled Elm app
const elmPath = path.join(__dirname, 'tcelm-compiler.js');
const elmCode = fs.readFileSync(elmPath, 'utf8');

// Create a context with a global scope for Elm (needs browser-like globals)
const context = {
    Elm: null,
    console: console,
    setTimeout: setTimeout,
    setInterval: setInterval,
    clearTimeout: clearTimeout,
    clearInterval: clearInterval,
    requestAnimationFrame: (cb) => setTimeout(cb, 16),
    cancelAnimationFrame: clearTimeout,
};
vm.createContext(context);
vm.runInContext(elmCode, context);
const Elm = context.Elm;

// Parse arguments
const args = process.argv.slice(2);
let inputFile = null;
let target = 'i386-rtems-nuc';

for (let i = 0; i < args.length; i++) {
    if (args[i] === '--target' && args[i + 1]) {
        target = args[i + 1];
        i++;
    } else if (!args[i].startsWith('-')) {
        inputFile = args[i];
    }
}

if (!inputFile) {
    console.error('Usage: tcelm-compile.js <input.elm> [--target <target>]');
    process.exit(1);
}

// Read input file
let source;
try {
    source = fs.readFileSync(inputFile, 'utf8');
} catch (err) {
    console.error(`Error reading file: ${inputFile}`);
    console.error(err.message);
    process.exit(1);
}

// Initialize Elm app
const app = Elm.Cli.init({
    flags: { target: target }
});

// Handle output from Elm
app.ports.sendOutput.subscribe(function(result) {
    if (result.success) {
        // Output C code to stdout
        console.log(result.code);
        process.exit(0);
    } else {
        // Output error to stderr
        console.error('Compilation error:', result.error);
        process.exit(1);
    }
});

// Send source to Elm
app.ports.receiveSource.send(source);
