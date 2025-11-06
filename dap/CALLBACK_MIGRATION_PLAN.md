# DAP Callback Migration Plan

This checklist tracks the work required to move the Debug Adapter Protocol
(DAP) layer onto the new callback-based core. Follow the steps sequentially;
after **each** numbered step:

1. Build: `ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P dap/core/dap_core.gpr -p -q`
2. Guard build: `ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P safety_build.gpr -p -q`
3. Commit the changes with a focused message.

## Preparation

- [x] Create working branch `dap-callback-refactor`.
- [x] Capture baseline `git status` (clean) and record latest guard build run
      (2025-11-06 `dap/core` + `safety_build`).

## Phase 1 – Adapter Scaffolding

- [x] Design a `GPS.DAP_Client.Callbacks.Kernel_Adapter` that wraps
      `GPS.Kernel` services used by the GUI (hooks, trace, shutdown checks).
- [x] Integrate the adapter into `DAP.Clients` (instantiate once, pass to
      requests and queue entries).
- [x] Update logging to flow through callbacks instead of ad-hoc traces.

## Phase 2 – Request Base Swap

- [x] Refactor `DAP.Requests` spec/body to use `GPS.DAP_Client.Requests.Request`
      while preserving the existing API surface for GUI callers.
- [x] Update `DAP.Clients` request queue (`Requests_Maps`) to use the core
      `Request_Access` type.
- [x] Adjust hook invocations to source method names from callbacks rather than
      `GPS.Kernel` handles.

## Phase 3 – Migrate Request Families

For each group below, migrate spec/body into `dap/core/src` (using the new
callback base), update GUI helpers to rely on the adapter, and remove the GUI
copy once validated.

1. Stack Trace *(see `dap/STACKTRACE_CALLBACK_DESIGN.md` for callback details)*
   - [x] `dap-requests-stacktrace.*`
   - [x] `dap-clients-stack_trace-stacktrace.*`
2. Continue / Step / Pause
   - [ ] `dap-requests-continue.*`
   - [ ] `dap-requests-stepin.*`
   - [ ] `dap-requests-next.*`
   - [ ] `dap-requests-pause.*`
3. Breakpoints
   - [ ] `dap-requests-setbreakpoints.*`
   - [ ] `dap-requests-setfunctionbreakpoints.*`
   - [ ] `dap-requests-setexceptionbreakpoints.*`
   - [ ] `dap-requests-setinstructionbreakpoints.*`
4. Variables / Scopes / Evaluate
   - [ ] `dap-requests-variables.*`
   - [ ] `dap-requests-scopes.*`
   - [ ] `dap-requests-evaluate.*`
   - [ ] `dap-requests-setvariable.*`
   - [ ] `dap-requests-setexpression.*`
5. Lifecycle / Control
   - [ ] `dap-requests-initialize.*`
   - [ ] `dap-requests-launch.*`
   - [ ] `dap-requests-attach.*`
   - [ ] `dap-requests-disconnect.*`
   - [ ] `dap-requests-cancel.*`
   - [ ] `dap-requests-configurationdone.*`
   - [ ] `dap-requests-loadedsources.*`
   - [ ] `dap-requests-readmemory.*`
   - [ ] `dap-requests-writememory.*`

## Phase 4 – Clean-Up

- [ ] Remove legacy `dap/src/dap-requests*` files once all migrations are in
      `dap/core/src`.
- [ ] Drop redundant kernel/glib includes from `DAP.Clients` and related units.
- [ ] Update documentation (`README.md`, `AGENTS.md`, `SESSION_STATE.md`,
      `dap/REMAINING_WORK.md`) to reflect the new callback architecture.
- [ ] Run full validation matrix (unit tests, headless smoke tests, guard build)
      and capture the results in the commit message / session log.

---

Use this file as the authoritative checklist across sessions; tick items as
work lands in committed form.
