# Repository Guidelines

## Project Structure & Module Organization
- Core Ada sources live in feature folders such as `kernel/`, `builder/`, `language/`, `refactoring/`, `vfs/`, and `lsp_client/`, with specs in `.ads` and bodies in `.adb`.
- `test_headless/` hosts the default TUI build target (`test_pure_ada.gpr`) and installs binaries under `bin/`; additional sample executables follow the same layout.
- Unit suites reside in `tests/`, using `tests/tests.gpr` with `*_tests.adb` drivers and `*_suite` registries; docs and planning aids live under `docs/`, `MODULES.md`, and `PLAN.md`.
- DAP still relies on the `Minimal_Perfect_Hash` helper exported by the ALS
  `lsp_base` library; `dap/core/dap_core.gpr` already `with`s that project so
  the generated schema keeps compiling headless. The former GtkAda UI under
  `dap/src` has been deleted; the remaining DAP client lives entirely in
  `gps-dap_client-requests.*` until a TUI front end is built.
- Build and dependency metadata stay in `alire.toml`, project files (`*.gpr`), and support scripts such as `fix_toolchain.sh`.

## Build, Test, and Development Commands
- Run `./fix_toolchain.sh` once on macOS Sonoma/Ventura toolchains before any build to patch the GNAT headers.
- `alr build` compiles `test_headless/test_pure_ada.gpr`; append `-- -XBUILD=Production` for optimized builds or `-- -j8` for parallel jobs.
- `alr clean` removes derived objects; pair with `alr build` after large refactors to verify a clean configuration.
- Execute `./test_headless/bin/test_pure_ada` to confirm the headless Ada pipeline and VSS dependency remain intact.
- Build and run the AUnit suites with `alr exec -- gprbuild -P tests/tests.gpr` followed by `alr exec -- ./tests/bin/test_runner`.

## Coding Style & Naming Conventions
- Ada code targets Ada 2022 (`-gnat2022 -gnatwa -gnata`) with three-space indentation, no tabs, and `Package_Name.Child` casing; keep `_core` modules GUI-free and quarantine any `_ui` remnants.
- Align declarations and `is`/`begin` continuations as seen in existing files to preserve Ada column-style formatting.
- Any remaining Python utilities must pass `black` (`pre-commit run black`) before committing.

## Testing Guidelines
- Prefer AUnit for package-level coverage: implement `<feature>_tests.adb`, register the suite in a `<feature>_suite`, and expose it through `test_runner`.
- Keep tests pure Ada—no C shims—to maintain the headless guarantee and keep builds portable across toolchains.
- Use `alr build -- -XBUILD=Coverage` when you need coverage data locally; do not check generated reports into the repository.

## Commit & Pull Request Guidelines
- Follow the prevailing format `type: concise summary` (`docs: refresh session state`) and split unrelated changes into separate commits.
- Reference the relevant roadmap item (e.g., a `PLAN.md` phase) or issue ID in the commit body when applicable.
- PR descriptions should list validation commands (`alr build`, `./test_headless/bin/test_pure_ada`, test suite) and call out any CLI/TUI output diffs with screenshots where meaningful.
- Request the CODEOWNERS listed for GNATSAS components whenever touching `gnatsas*` or `codepeer*` paths to keep reviews compliant.
