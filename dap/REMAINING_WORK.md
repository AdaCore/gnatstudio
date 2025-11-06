# DAP Client - Headless Refactor Plan

## Goal

Carve the Debug Adapter Protocol stack into a GUI-free core so we can keep
`safety_build.gpr` compiling after deleting GtkAda front-end code. The target
is a `dap_core.gpr` that only depends on kernel primitives, VFS, and the LSP
callback mechanisms.

## Progress

- Created skeleton project `dap/core/dap_core.gpr`, hooked it to `dap.gpr`, and
  linked it against ALS's `lsp_base` project to reuse the upstream
  `Minimal_Perfect_Hash` helper.
- Moved protocol-neutral specs (`dap.ads`, `dap-types.ads/.adb`) under
  `dap/core/src` to begin the core split.
- Core build succeeds (`alr exec -- gprbuild -P dap/core/dap_core.gpr -p`) and
  the guardrail now includes `dap_core` via `safety_build.gpr`.
- Introduced `GPS.DAP_Client.Callbacks` (with `Null_Callback`) as the headless
  facade that will replace direct `GPS.Kernel` usage inside protocol units.
- Added `GPS.DAP_Client.Requests` as the callback-backed base class for future
  protocol refactors without touching the existing GUI-bound `DAP.Requests`.

## Current State (Problems)

- `dap.gpr` `with`s GUI projects (`views`, `browsers`, `gvd`, `refactoring/ui`)
  and drags GtkAda into every unit.
- Protocol logic (`dap-clients-*.adb`, `dap-requests-*.adb`, `dap-types.*`)
  sits in the same tree as view/controller code under `dap/src/modules`.
- Utilities (`DAP.Utils`) hard-code `GPS.Kernel` navigation helpers.
- Guardrail builds do not touch the DAP code, so GUI deletions risk breakage.
- Kernel entanglement hotspots (pre-audit):
  - `dap/src/dap-clients*.{ads,adb}`: heavy use of `GPS.Kernel`, hooks,
    messages window, MDI, and markers.
  - `dap/src/dap-utils.*`, `dap/src/dap-contexts.*`,
    `dap/src/dap-module*.{ads,adb}`: relies on kernel context helpers.
  - `dap/src/modules/dap-views*/**`: GtkAda + kernel modules (UI layer).
  - Safer protocol-only candidates (no Gtk/GPS usage): `dap-requests*/**`,
    generated `dap-tools*`, `dap-types-breakpoints.*`, plus most files already
    moved under `dap/core/src`.

## Refactor Plan

1. **Inventory & Tag Dependencies**  
   Use `rg "with Gtk"`/`rg "with GPS."` under `dap/src` to list UI-bound units.
   Mark which packages are safe protocol core (requests, types, contexts).

   - **GUI-tied namespaces** (Gtk/GPS.Debuggers/Generic_Views):  
     `dap-clients*.{adb,ads}`, `dap-module*.{adb,ads}`, `dap-utils.*`, every unit
     under `dap/src/modules/`, and the generated view helpers. These stay in the
     GUI/TUI layer.

   - **Headless-ready protocol units** (no Gtk references):  
     `dap.ads`, `dap-types*.{adb,ads}`, `dap-requests*.{adb,ads}`,
     `dap-contexts*.{adb,ads}`, `dap-module-breakpoints*.{adb,ads}`,
     the generated protocol schema in `generated/dap-tools*.{adb,ads}`, plus
     any `dap-clients-*` request helpers that only marshal JSON (verify against
     Gtk before moving).

2. **Define Callback Boundary**  
   Introduce `GPS.DAP_Client.Callbacks` (mirrors the LSP design) exposing the
   services the debugger needs: editor navigation, breakpoint persistence,
   console I/O, telemetry. Add a `Null_Callback` implementation that no-ops.

3. **Split Source Layout**  
   Move protocol-only units to `dap/core/src`. Keep GUI controllers/views in
   `dap/gui/src` (excluded from safety builds). Update relative `with` paths.

4. **Isolate Build Projects**  
   Create `dap/core/dap_core.gpr` referencing the new core sources. Re-home the
   existing `dap.gpr` as the GUI wrapper project that `with`s both the core and
   TUI/GUI layers.

5. **Cut GUI Dependencies**  
   Refactor `DAP.Utils` and any kernel helpers to depend on the callback
   interface rather than `GPS.Kernel` or Gtk widgets. Drop Gtk types from spec
   files so the core builds headless.

6. **Wire Into Safety Builds**  
   Add `with "dap/core/dap_core.gpr";` to `safety_build.gpr` and document guard
   commands:
   ```
   ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P dap/core/dap_core.gpr -p
   ALR_INDEX_AUTO_UPDATE=0 alr exec -- gprbuild -P safety_build.gpr
   ```

7. **Update Docs & Session Notes**  
   Refresh `PLAN.md` Phase 6+ checkpoints and `SESSION_STATE.md` to note the DAP
   headless boundary and new guardrail builds once the split lands.

## Risks & Mitigations

- **Hidden GUI Coupling:** Some protocol units may call Gtk helpers indirectly.
  Mitigate by compiling `dap_core.gpr` early to expose missing callbacks.
- **Breakpoint Persistence:** If persistence relies on GUI settings, move the
  storage to kernel services or stub it until the TUI owns the UI.
- **Command Surface:** Ensure callback API covers all operations before moving
  files; otherwise refactors will churn.
