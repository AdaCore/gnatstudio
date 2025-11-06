# GNATstudio Module Classification

## Analysis Date: 2025-11-04

This document categorizes GNATstudio modules for the TUI conversion per PLAN.md.

## Core Modules (No GTK - Safe for Headless Build)

### Already Headless (_core.gpr variants)
- **kernel_core** (`kernel/kernel_core.gpr`) - Core kernel functionality, no GTK
  - Source: `kernel/src_info/`
  - Dependencies: common, gnatcoll, gnatcoll_sqlite, gnatcoll_xref, language, toolchains_core

- **builder_core** (`builder/builder_core.gpr`) - Build orchestration, headless
  - Dependencies: kernel_core (CHECK THIS)

- **ada_module_core** (`ada_module/core/ada_module_core.gpr`) - Ada language core

- **lal_core** (`lal/lal_core.gpr`) - Libadalang integration core

- **refactoring_core** (`refactoring/core/refactoring_core.gpr`) - Refactoring engine

- **completion_core** (`completion/core/completion_core.gpr`) - Code completion engine


### Standalone Headless Modules
- **vfs** (`vfs/vfs.gpr`) - Virtual file system abstraction
- **lsp_client** (`lsp_client/core/lsp_client_core.gpr`) - Language Server Protocol client (headless)
- **gnatcoll_extras** (`gnatcoll_extras/gnatcoll_extras.gpr`) - GNATCOLL extensions
- **language** (`language/language.gpr`) - Language support infrastructure
- **toolchains_core** (`toolchains_editor/core/toolchains_core.gpr`) - Toolchain management
- **gnatdoc** (`gnatdoc/gnatdoc.gpr`) - Documentation generator
- **cli** (`cli/cli.gpr`) - CLI tools (gnatstudio_cli, gnatdoc3)

## GUI Modules (Require GTK - Must Remove or Stub)

### Kernel with GTK (Removed)
- **kernel** (`kernel/kernel.gpr`) - Deleted. All consumers must depend on
  `kernel_core` to stay headless; GtkAda/spawn_glib were dropped from the tree.

### UI Module Variants (_ui.gpr, removed)
- **ada_module_ui**, **completion_ui**, **refactoring_ui**, **language_ui**  
  These GtkAda builds have been deleted; only the `_core` projects remain.

### GUI-Only Modules (Delete Later)
- **gnatstudio/gps.gpr** - Main GUI application
- **src_editor** - GUI source code editor
- **views** - All GUI panels/views
- **browsers** - Code browsers (GUI)
- **gvd** - Visual debugger (keep DAP for TUI later)
- **widgets** - GTK widget library
- **navigation** - GUI navigation
- **prj_editor** - GUI project editor
- **vsearch** - GUI visual search
- **vcs2** - GUI version control
- **vdiff** - GUI diff viewer
- **help** - GUI help system
- **keymanager** - GUI key bindings
- **aliases** - GUI aliases
- **custom** - GUI customization
- **python** - (removed) legacy scripting engine; repo no longer ships Python
  code or runtime hooks.
- **remote** - GUI remote features
- **socket** - GUI socket features
- **shell** - GUI shell
- **valgrind** - GUI valgrind integration
- **code_analysis** - GUI code analysis
- **memory_usage** - GUI memory profiler
- **docs** - Documentation (keep for building docs)

## Build Strategy

### Phase 1: CLI-Only Build (Current)
1. Update `cli/cli.gpr` to ensure it uses only _core variants
2. Set `alire.toml` project-files to `["cli/cli.gpr"]`
3. Verify no GTK dependencies in dependency chain
4. Build with `alr build`

### Phase 2: TUI Entry Point
1. Create `tui/tui.gpr` depending on core modules only
2. Add ncursesada or Malef dependency
3. Create minimal `tui_main.adb`
4. Add to `alire.toml` executables

### Phase 3: Module Deletion
1. Delete all GUI-only modules listed above
2. Delete _ui/ subdirectories from mixed modules
3. Remove GUI imports from remaining modules

## Dependency Analysis

### CLI Dependency Chain (Target)
```
cli.gpr
├── kernel_core.gpr (✓ no GTK)
│   ├── common/common.gpr (CHECK: might have GTK)
│   ├── language/language.gpr (CHECK)
│   └── toolchains_core.gpr (✓ no GTK)
├── builder_core.gpr (CHECK: which kernel?)
├── ada_module_core.gpr (CHECK)
├── lal_core.gpr (✓ no GTK)
├── gnatdoc.gpr (CHECK: might have GTK)
└── prj_editor/prj_core.gpr (CHECK)
```

**Action Items:**
1. Verify each dependency above is GTK-free
2. If common.gpr has GTK, split into common_core
3. Update any that depend on kernel to use kernel_core
4. Remove python/python.gpr dependency from CLI _(done)_

## Notes

- `kernel_core` uses `src_info/` directory - info-level APIs only
- `kernel` uses full `src/` directory - includes GUI hooks
- Most _core.gpr variants already exist and are headless
- Main work is updating dependency declarations, not creating new modules
