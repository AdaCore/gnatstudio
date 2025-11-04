# Repository Guidelines

## Project Structure & Module Organization
- `gnatstudio/`, `kernel/`, and `src_editor/` host the core Ada code that drives the IDE back-end, language services, and editors.
- `python/`, `share/`, and `plugins.gpr` bundle Python helpers, UI resources, and project files for extending GNAT Studio.
- `testsuite/` contains the end-to-end harness (`run.sh`, `testsuite.py`) plus individual scenario folders under `testsuite/tests/`.
- `docs/` and `examples/` provide user guides and sample projects; align new documentation with these layouts.

## Build, Test, and Development Commands
- `./configure --prefix=$PWD/_install` — configure GNAT Studio against local GtkAda, GNATcoll, and Python installs.
- `make -j$(nproc)` — build the full toolchain; use `make install` to stage into `_install/`.
- `gprbuild -p -P gps_aggregated.gpr` — rapid rebuild for specific Ada aggregates when iterating on core code.
- `testsuite/run.sh [tests/minimal/]` — run the full test matrix or focus on one test directory.
- `pre-commit run --all-files` — format Python sources with Black before pushing changes.

## Coding Style & Naming Conventions
- Ada files (`.adb/.ads`) rely on `gnatformat`; keep identifiers in `Mixed_Case` and prefer one unit per file.
- Python follows Black defaults (88-char lines, double quotes). Use `snake_case` for functions and modules, `PascalCase` for classes.
- C/C++ helpers mirror existing `*.cxx` files: four-space indents, brace-on-same-line as shown in `cpp_module/`.
- Keep plugin names and resource files lower-case with hyphens (e.g., `share/support/ui/gnatformat.py`).

## Testing Guidelines
- Organize new tests under `testsuite/tests/<ticket-id>-<slug>/` with `test.py` (logic) and `test.yaml` (metadata).
- Reuse `gps_utils.internal.utils.gps_assert` for assertions; prefer descriptive messages to aid triage.
- Run `testsuite/run.sh` locally before opening a PR; capture `testsuite/out/summary.txt` for debugging failures.
- Add targeted tests when touching `gnatformat` or LSP integrations to maintain coverage of regressions.

## Commit & Pull Request Guidelines
- Follow the repository history: short, imperative subjects (`Fix crash in outline view`), optionally prefixed by the subsystem.
- Reference tracking issues or merge-request topics in the body (`Topic: V329-036`) and list noteworthy side effects.
- Squash noisy fixups before submission; leave TODOs documented in PR notes instead of commits.
- Pull requests should describe the change, mention impacted modules, link relevant tests, and include screenshots for UI updates.

## Environment Setup Notes
- Ensure recent GNAT (24 or newer), Gtk+ 3.24, GtkAda, GNATcoll (with project + Python support), and Python with PyGObject/Pycairo as described in `INSTALL`.
- Export `GPR_PROJECT_PATH`, `PATH`, and `LD_LIBRARY_PATH` so `configure` locates the Ada dependencies; document any non-default paths in your PR.
