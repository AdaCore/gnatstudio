# DAP Client - Headless Refactor Plan

## Goal

Carve the Debug Adapter Protocol stack into a GUI-free core so we can keep
`safety_build.gpr` compiling after deleting GtkAda front-end code. The target
is a `dap_core.gpr` that only depends on kernel primitives, VFS, and the LSP
callback mechanisms, while a future TUI debugger consumes the callback layer.

## Progress

- Created skeleton project `dap/core/dap_core.gpr`, hooked it to `dap.gpr`, and
  linked it against ALS's `lsp_base` project to reuse the upstream
  `Minimal_Perfect_Hash` helper.
- Moved protocol-neutral specs (`dap.ads`, `dap-types.ads/.adb`) under
  `dap/core/src` to begin the core split.
- Core build succeeds (`alr exec -- gprbuild -P dap/core/dap_core.gpr -p`) and
  the guardrail now includes `dap_core` via `safety_build.gpr`.
- Introduced `GPS.DAP_Client.Callbacks` (with `Null_Callback`) as the headless
  facade that replaces direct `GPS.Kernel` usage inside protocol units.
- Added `GPS.DAP_Client.Requests` as the callback-backed base class for future
  protocol refactors.
- Migrated every request family (continue/step/pause, breakpoints, variables,
  evaluate, lifecycle/control) to the callback core and removed the legacy
  `dap-requests-*` units.
- Rewired registers, threads, assembly, and breakpoint helpers to allocate
  callback-based requests prior to deleting their GtkAda hosts.
- Deleted the entire GtkAda debugger shell (`dap/src`, `dap/dap.gpr`), leaving
  only the headless core guarded by `dap/core/dap_core.gpr`.

## Current Status

- The repository now ships only the callback-based DAP core (`dap/core`,
  `generated/`, and helper scripts). There is no debugger UI.
- `dap/core/dap_core.gpr` remains in `safety_build.gpr`, so callback regressions
  are still caught by the guard build.
- Any future debugger experience must be implemented in the new TUI (see
  PLAN.md Phase 10).

## Next Steps (TUI work)

1. **Design TUI panes** – Decide how stack, variables, memory, and breakpoint
   views should render inside the Ncurses/Malef UI, and map every panel to the
   existing callback interfaces (`gps-dap_client-requests.*`).
2. **Integrate debugger commands** – Recreate continue/step/next/pause, toggle
   breakpoints, and display diagnostics/exception filters via the TUI
   keymap/palette so the callback layer processes real traffic again.
3. **Document guardrails** – Keep `dap/core/dap_core.gpr` wired into
   `safety_build.gpr` and note in PLAN/SESSION_STATE that debugging is
   headless-only until the TUI implementation lands.

## Risks & Mitigations

- **Missing UI coverage:** With GtkAda gone, it's easy to forget the debugger
  entirely. Track the TUI work in PLAN.md Phase 10 and treat the guard build as
  mandatory before deleting more code.
- **Callback drift:** The new TUI must stick to `GPS.DAP_Client.Callbacks`.
  Avoid reintroducing direct `GPS.Kernel` or GtkAda dependencies.
- **Toolchain churn:** `dap/core/dap_core.gpr` still depends on ALS's
  `lsp_base`; keep that project up to date so generated schema builds stay
  reproducible.
