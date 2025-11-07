# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Overview

This branch hosts the **GNAT Studio TUI** effort: a headless, pure-Ada editor
derived from GNAT Studio. All GtkAda and Python subsystems have been removed.
Only `_core` projects remain; GUI/TUI front-ends will be rebuilt later.

## Build & Test

```bash
# One-time (macOS 26) toolchain fix
./fix_toolchain.sh

# Build the headless integration tests
alr build

# Run the default smoke test
./test_headless/bin/test_pure_ada

# Guard builds (must stay green)
ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P dap/core/dap_core.gpr -p -q
ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P safety_build.gpr -p -q
```

## Repository Norms

- Use `lsp_client/core/lsp_client_core.gpr` and `dap/core/dap_core.gpr` for
  protocol work; never reintroduce GtkAda/GPS.Kernel dependencies into these
  projects.
- Keep documentation in `PLAN.md`, `SESSION_STATE.md`, and `AGENTS.md` current
  with every refactor. These files act as the authoritative roadmap/log.
- When deleting legacy modules, add them to guard builds first to ensure the
  callback/core variants still compile independently.
- All scripting hooks are stubbed; do **not** depend on `GPS` Python modules.
  Any new automation must be implemented in Ada.

## Code Hygiene

- Follow Ada 2022 style (three-space indent, `-gnat2022 -gnatwa -gnata`).
- Introduce comments sparingly—only to clarify non-obvious logic.
- Before committing, run the guard builds listed above and keep changes scoped
  (one topic per commit).
