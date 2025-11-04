# GNAT Studio TUI

- [What is GNAT Studio TUI?](#what-is-gnatstudio-tui)
- [Building](#building)
- [Status](#status)

## What is GNAT Studio TUI?

**Branch: strip-down**

GNAT Studio TUI is a terminal user interface (TUI) editor for **Ada** and **SPARK**, derived from GNAT Studio IDE. This is a ground-up rewrite focusing on:

- **TUI-first design** - No GUI dependencies
- **Modeless editing** - Micro-style shortcuts (not Vim/Helix modal)
- **LSP-powered** - Modern language intelligence via Ada Language Server
- **Fast & lean** - Single static binary, minimal dependencies
- **Pure Ada** - No Python, no GTK, no GUI frameworks

**Status:** Phase 1 complete (headless build verified). Phase 2 in progress (TUI bootstrap).

See `PLAN.md` for detailed roadmap.

## Building

### Quick Start (Alire)

```bash
# Prerequisites: Alire package manager
# Install from https://alire.ada.dev

# Build
alr build

# Run test
./test_headless/bin/test_pure_ada
```

### Requirements

**Ada Dependencies (via Alire):**
- GNAT compiler 15.1.2+ (from Alire toolchain)
- gnatcoll 25.0
- vss (Virtual String Subsystem) 25.0
- xmlada 25.0
- libgpr 25.0

**System Dependencies (macOS via Homebrew):**
```bash
brew install autoconf pkg-config
```

### Detailed Instructions

See `BUILD_INSTRUCTIONS.md` for complete build documentation.

See `DEPENDENCIES.md` for dependency details.

## Status

**Completed (Phase 1):**
- ✅ GTK/Python removed
- ✅ Headless build working
- ✅ 56% code reduction (791 files deleted)
- ✅ Pure Ada compilation verified
- ✅ Alire dependency management

**In Progress (Phase 2):**
- 🚧 TUI bootstrap (terminal init, buffer display)

**Planned:**
- Phase 3: Editing basics (insert/delete, undo/redo)
- Phase 4: Splits & panels
- Phase 5: LSP integration (Ada Language Server)
- Phase 6: Project/build support

See `PLAN.md` for full roadmap.
