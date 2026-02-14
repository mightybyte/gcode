# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Coding Standards

Follow the universal coding standards at `~/.openclaw/workspace/coding-standards.md`.

## Project Overview

`gcode` is a pure Haskell library for parsing, rendering, constructing, and validating G-code — the standard language for CNC machine control. It aims to be the canonical, highest-quality G-code library in the Haskell ecosystem.

## Build and Development Commands

### Building
```bash
nix develop --command cabal build
nix build
nix develop
```

### Running Tests
```bash
nix develop --command cabal test
```

### Running the Example CLI
```bash
cabal run gcode-examples -- parse input.gcode
cabal run gcode-examples -- stats input.gcode
cabal run gcode-examples -- validate input.gcode
cabal run gcode-examples -- round-trip input.gcode output.gcode
```

## Architecture

### Module Structure
- `GCode.Types.*` — Core ADTs (Command, Parameter, Block, Program, Units)
- `GCode.Types` — Re-export module
- `GCode.Parse` — Megaparsec-based parser
- `GCode.Render` — Configurable renderer
- `GCode.Build` — Writer monad DSL for constructing programs
- `GCode.Validate` — Static validation and warnings
- `GCode.Checksum` — XOR checksum computation
- `Utils` — Shared utilities including Pretty typeclass

### Key Design Decisions
- Parameters use raw `Double` internally; typed unit newtypes are available separately
- Unknown commands fall through to `GCustom Int` / `MCustom Int`
- `PCustom Char Double` escape hatch for vendor-specific parameters
- Blocks with no command represent comment-only lines or sticky command continuation
- Round-trip fidelity: `parse . render . parse ≡ parse` (modulo whitespace)

## Code Conventions

- **Haskell2010**, `-Wall -Werror -O2`
- Field naming: `_recordName_field`
- Import style: 78-char dash separators between external/internal imports
- No `(^.)` lens getter — use field accessors directly
- No GADTs, type families, functional dependencies
- `::` on new line indented 2 spaces
- No variable-width vertical alignment
- kebab-case for JSON serialization
- `Pretty` typeclass for human-readable output
- `Cover` typeclass from `fake` for QuickCheck test generation

## Dependencies

### Library
megaparsec, text, containers, vector, lens, aeson, deepseq, bytestring, mtl

### Test
hspec, hspec-golden, QuickCheck, fake

### Example exe
optparse-applicative
