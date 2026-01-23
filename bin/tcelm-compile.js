#!/usr/bin/env node
// tcelm compiler wrapper - runs compiled Elm code

const fs = require('fs');
const path = require('path');

// Load the compiled Elm app - it exports to this/global
const elmModule = require('./tcelm-compiler.js');
const Elm = elmModule.Elm || global.Elm || this.Elm;

if (!Elm || !Elm.Cli) {
    console.error('Failed to load Elm.Cli module');
    process.exit(1);
}

// Get command line args
const args = process.argv.slice(2);
if (args.length < 1) {
    console.error('Usage: tcelm-compile <source.elm> [--target <target>]');
    process.exit(1);
}

const sourceFile = args[0];
let target = 'tcc';

// Parse --target argument
const targetIdx = args.indexOf('--target');
if (targetIdx !== -1 && args[targetIdx + 1]) {
    target = args[targetIdx + 1];
}

// Read source file
let source;
try {
    source = fs.readFileSync(sourceFile, 'utf8');
} catch (e) {
    console.error('Error reading file:', sourceFile);
    process.exit(1);
}

// Initialize Elm app
const app = Elm.Cli.init({
    flags: { target: target }
});

// Handle output
app.ports.sendOutput.subscribe(function(result) {
    if (result.success) {
        console.log(result.code);
    } else {
        console.error('Compilation error:', result.error);
        process.exit(1);
    }
});

// Send source to compiler
app.ports.receiveSource.send(source);
