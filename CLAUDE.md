# CLAUDE.md - Project Instructions for Claude Code

## Work Style

- Work autonomously without stopping to summarize progress
- Do not ask "should I continue?" - just keep working
- Only stop when the task is fully complete or you hit a blocker you cannot resolve
- Do not provide progress summaries unless explicitly requested
- Do not ask for confirmation before proceeding with planned work
- When refactoring, keep going until done or until you need clarification on requirements

## Project: tcelm

Elm to C compiler for embedded systems (RTEMS, TCC targets).

### Build & Test

```bash
# Compile the compiler
elm make src/Cli.elm --output=bin/tcelm.js

# Copy to runtime location
cp bin/tcelm.js bin/tcelm-compiler.js

# Test compilation
echo 'module Test exposing (main)
main = 42' > /tmp/test.elm
./bin/tcelm /tmp/test.elm
```

### Architecture

- `src/Cli.elm` - Main CLI and code generation (being refactored)
- `src/Codegen/` - Code generation modules (Builtins, Pattern, Expr, Shared, Lambda, Union)
- `src/Target/` - Target-specific code (TCC, RTEMS, Native)
- `src/Generate/C.elm` - Runtime-based C generation (separate from standalone)
