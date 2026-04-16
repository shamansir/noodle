# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Build
npm run build          # spago build
npm run bundle         # spago bundle (web app → web/app.js)

# Test
sh ./test.sh           # or: spago test
# No single-test filter — spago runs all specs via test/Main.purs

# Run CLI (TUI)
sh ./run-cli.sh -t starter
sh ./run-cli.sh -f ./ndf/starter.v0.1.ndf

# Run Web UI
sh ./run-web-serve.sh  # bundles + serves on port 15000

# Code generation from NDF
sh ./run-cli.sh -g ./ndf/starter.v0.1.ndf -t starter

# Nix
nix develop            # dev shell
nix build              # full build
nix run                # run app
```

## Architecture

PureScript 0.15 project. Two entry points: `Web.Main` (Halogen web UI) and `Cli.Main` (Blessed terminal UI). Both render the same underlying graph model.

### Core graph engine (`src/Noodle/`)

**Typed layer** (`Noodle.Node`, `Noodle.Patch`, `Noodle.Fn`, `Noodle.Toolkit`) wraps an **untyped Raw layer** (`Noodle.Raw.*`). User-facing code works with typed layer; the raw layer exists for runtime polymorphism.

- `Noodle.Id` — type-safe string newtypes for all identifiers (`FnName`, `NodeR`, `InletR`, `OutletR`, etc.)
- `Noodle.Fn` / `Noodle.Raw.Fn` — functions that run inside nodes (inlet values → outlet values)
- `Noodle.Node` / `Noodle.Raw.Node` — node instances holding state and process
- `Noodle.Patch` — graph of connected nodes + links
- `Noodle.Toolkit` — collection of node families; spawns typed nodes
- `Noodle.Link` — connections between outlets and inlets
- `Noodle.Repr.ValueInChannel` — polymorphic value wrapper used in channel transport

Data flow: **Toolkit** defines node families → **Patch** instantiates nodes → **Fn** processes inlet data → values flow through **Links** → outlets

### Representation system (`src/Noodle/Repr/`)

Two representation typeclasses:
- `StRepr` — state representation (for patch/node state serialization)
- `ChRepr` — channel representation (for value transport between nodes)

`ValueInChannel` is the concrete polymorphic value type passed on links.

### Text/serialization (`src/Noodle/Text/`)

- `NdfFile` — NDF (Node Definition File) format. Line 1: `<toolkit> <major> <minor>`. Subsequent lines: family specs (`family-id : node-name :: inlets => outlets`), inline PureScript code blocks.
- `WsMessage` — WebSocket message format for client/server communication

### Front-end (`src/Front/`)

- `Web/` — Halogen + SVG. `Web.Main` mounts `AppScreen.component`. Components in `Web/Components/`.
- `Cli/` — Blessed TUI. `Cli.Main` → `Cli.App`. Components in `Cli/Components/`.
- `Shared/` — keyboard, panels, help text shared between both UIs

### Toolkits (`src/`)

- `StarterTk/` — demo toolkit: math, color, shape nodes
- `HydraTk/` — Hydra Synth live video synthesis toolkit; JSON scene serialization in `HydraTk/Repr/`

### Key typeclasses

`HasFallback`, `ValueTagged`, `Wiring`, `FromToPatchState` — extend node families and toolkit behavior. Check `src/Noodle/Fn/Process.purs` and `src/Noodle/Toolkit.purs` for interfaces.

### FRP / signals

Heavy use of `signal` library throughout. Node processing and UI updates are reactive. `src/Utils/Signal/` has helpers.

## Tests

Test specs in `test/Spec/`:  `Node`, `Patch`, `Fn`, `Toolkit`, `Repr`, `Ndf/`, `Hydra/`, `UI/`

`test/Example/Toolkit/Minimal/` — minimal toolkit used as test fixture (nodes: `Sample`, `Sum`, `Concat`, `Stateful`, `ModifiesPatch`)

## Key dependencies

- `halogen` + `halogen-hooks` — web UI
- `@shamansir/everblessed` (custom fork of `blessed`) — terminal UI
- `signal` / `wire` — FRP
- `argonaut` — JSON
- `hydra-synth` — video synthesis (JS interop via FFI)
- `spago` 0.93 / `spago-legacy` — build tooling (project uses both; `test.dhall` requires legacy)
