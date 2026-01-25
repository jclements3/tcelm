#!/bin/bash
# Wrapper script for tcelm2 compiler

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SOURCE_FILE="${1:-}"
TARGET="${2:-main}"

if [ -z "$SOURCE_FILE" ]; then
    echo "Usage: tcelm2.sh <source.elm> [target]"
    exit 1
fi

SOURCE_CONTENT=$(cat "$SOURCE_FILE")

node -e "
const mod = require('$SCRIPT_DIR/tcelm2.js');
const app = mod.Elm.Compiler.init({
    flags: {
        source: $(echo "$SOURCE_CONTENT" | node -e "let s=''; require('readline').createInterface({input:process.stdin}).on('line',l=>s+=l+'\n').on('close',()=>console.log(JSON.stringify(s)))"),
        target: '$TARGET'
    }
});

app.ports.printOutput.subscribe(output => {
    console.log(output);
});

app.ports.printErrors.subscribe(errors => {
    errors.forEach(e => console.error('Error:', e));
    process.exit(1);
});
"
