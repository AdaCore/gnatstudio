# GNATstudio - Claude Context

GNATstudio is an IDE for Ada/SPARK development (with C/C++ support), written in Ada with GTK+/GtkAda UI and Python scripting. Copyright AdaCore.

## Build Commands

```bash
make                              # full build (Debug by default)
```

## Test Commands

```bash
cd testsuite
./run.sh                  # run entire testsuite (done by the CI: do not run this)
./run.sh tests/minimal/   # run the test found in the corresponding directory)
```

Tests are in `testsuite/tests/`, one subdirectory per test. Each test has `test.py` (GPS scripting API) and `test.yaml` (metadata). Results go to `testsuite/out/`.

You need a X11/Xvfb display to run most tests.

## Architecture

GPS uses a **plugin/kernel model**: the kernel (`kernel/`) is the mandatory core; everything else is an optional module registered at startup.

### Module Pattern

Each module lives in its own directory with:

- `module_name.gpr` - GPR project file (typically `with "shared.gpr"`)
- `src/` - Ada source files
- `obj/` - object files

Modules are registered in `gnatstudio/src/gps-main.adb`:

1. Add `with Module_Package;` at the top
2. Call `Module_Package.Register_Module (Kernel)` in the body

### Module Registration (Ada pattern)

```ada
-- module_name.ads
with GPS.Kernel;
package Module_Name is
   procedure Register_Module
      (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
end Module_Name;

-- module_name.adb
with GPS.Kernel.Modules; use GPS.Kernel, GPS.Kernel.Modules;
package body Module_Name is
   procedure Register_Module
      (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Module : Module_ID;
   begin
      GPS.Kernel.Modules.Register_Module
         (Module, Kernel, Module_Name => "module_name");
   end Register_Module;
end Module_Name;
```

### Intermodule Communication

Modules communicate via (preferred to least preferred):

- **Hooks** (`gps-kernel-hooks.ads`): named collections of callbacks, fired on events (e.g., project reload, file open). Most powerful mechanism.
- **Context changes**: GTK signals when MDI child selection changes; modules update based on current context type.
- **Direct calls**: avoided except for `Register_*` commands.

Key files: `kernel/src/gps-kernel-hooks.ads`, `kernel/src/gps-kernel-modules.ads`, `kernel/src/gps-kernel-contexts.ads`.

## Key Subsystem Map

| Feature | Directory |
|---------|-----------|
| Editor | `src_editor/` |
| Debugger (DAP) | `dap/` |
| Debugger (GVD/legacy) | `gvd/` |
| LSP client | `lsp_client/` |
| Libadalang integration | `lal/` |
| Project management | `prj_editor/` |
| VCS (git, svn, etc.) | `vcs2/` |
| Code completion | `completion/` |
| Refactoring | `refactoring/` |
| Search/replace | `vsearch/` |
| Navigation | `navigation/` |
| Build integration | `builder/` |
| Python scripting | `python/` |
| Kernel | `kernel/` |
| Entry point | `gnatstudio/src/gps-main.adb` |
| Shared GPR settings | `shared.gpr` |

## Code Conventions

- **Naming**: `Package_Name`, `Procedure_Name`, `Variable_Name` (Mixed_Case with underscores)
- **File naming**: `package-name.ads` / `package-name.adb` (lowercase with hyphens for nested packages)
- **GPL header**: all Ada files have the AdaCore GPL copyright header

## Common Task Patterns

- **Add a new command/action**: register via `GPS.Kernel.Actions.Register_Action` in your module's `Register_Module`
- **Add editor behavior**: look at `src_editor/src/`
- **Add a preference**: use `Default_Preferences` package, register in `Register_Module`

## Python Plugins

Python plugins live in `share/plug-ins/`. They use the `GPS` Python module (the scripting API). Tests use `gs_utils.internal.utils.gps_assert`.
