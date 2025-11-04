Below is a straight, actionable plan to carve this GNAT Studio codebase down to a lean core and stand up a **TUI Ada/SPARK editor**. Target OSes: Linux, macOS, OpenBSD. **Editing UX is modeless and “micro-like” by default** (familiar Ctrl/Alt shortcuts, command palette, multiple cursors), **not Helix-style modal**.

------

## **0) Deliverable in One Line**

A single, static terminal binary that edits Ada/SPARK with **micro-style modeless UX**, LSP-powered intelligence (Ada Language Server), GPR-aware builds, and a clean path to add DAP-based debugging later.

------

## **1) What to Keep / Replace / Drop**

**Keep (core engine & Ada tooling)**

- Text buffer + undo/redo + file I/O.
- Project model for .gpr (GNATcoll/Project or current internal project loader).
- LSP client code paths needed to talk to **Ada Language Server** (completions, hovers, diagnostics, semantic tokens, code actions, formatting).
- Build orchestration (gprbuild/gnatmake invocation, output parsing).
- DAP client plumbing (if present) for later TUI debugging.

**Replace (front-end & interaction)**

- GTK event loop/widgets → **TUI layer** (ncurses Ada binding recommended; hide behind Terminal_Interface abstraction).
- GUI actions/menus/accelerators → **micro-like keybinding layer** and a **command palette**.
- Panels (Locations, Messages, etc.) → **text panes** (splits) rendered via curses.
- Preferences dialogs → **plain-text config** ($XDG_CONFIG_HOME/ada-tui/config).

**Drop (without hesitation)**

- GtkAda, PyGObject/Python plugin engine.
- Non-Ada language modules.
- Heavy GUI plugins: VCS UI, graphs/diagrams, visual diff, profiler, remote GUI tooling.
- Any GUI-only dialogs, wizards, docking/layout systems.

------

## **2) TUI Stack (Ada-first)**

- **Malef** (Ada 2022 TUI toolkit) for pure-Ada widgets we develop ourselves. We will attempt to wrap this in an abstract `Terminal_Interface` for screen, windows, input, colors, mouse hints, etc. to derisk this choice. 

------

## **3) Editing Model & UX (Micro-Style Defaults)**

**Explicitly not modal.** No Vim/Helix modes. Behavior mirrors “micro”:

- **Modeless editing** with standard shortcuts (e.g., Ctrl‑S save, Ctrl‑O open, Ctrl‑F find, Ctrl‑R replace, Ctrl‑Q quit).
- **Command palette** (e.g., Ctrl‑E): run open, write, quit, build, grep, lsp.*, dap.*.
- **Selections & multiple cursors**: add/remove cursors, operate on all selections for insert/edit/delete; show cursors distinctly.
- **Splits**: horizontal/vertical; simple buffer switcher; quick “close split”.
- **Status line**: file, line/col, RO/RW, diagnostics counts, LSP state.
- **Search**: incremental in-buffer; project grep (external tool or internal walk).
- **Fuzzy open**: quick file picker from project sources.

Note: start single-cursor if necessary; wire the cursor set early so multi-cursor slips in cleanly.

------

## **4) LSP (Ada Language Server) Integration**

- Launch ALS per workspace; JSON-RPC over pipes.
- Wire **didOpen/didChange** with debounce and incremental sync.
- Implement:
  - **Completions** (list under cursor; accept/preview).
  - **Hover** (ephemeral overlay or status popup).
  - **Go to definition/refs** (open target in current/new split).
  - **Diagnostics** (gutter marks + diagnostics panel; jump next/prev).
  - **Semantic tokens** (map to color pairs; themeable).
  - **Code actions** (quick fixes via palette).
  - **Formatting** via ALS (GNATfmt under the hood).
- Optional: GPR LSP for .gpr files when practical.

------

## **5) Project & Build**

- Load .gpr; resolve source/build trees; expose file list to fuzzy-open.
- **Build command**: call gprbuild with project env; stream output to “Build” pane; parse errors to diagnostics list.
- **On-save compile** (opt-in): trigger background build or rely on ALS diagnostics.
- **SPARK** (phase 2): gnatprove command; parse locations into diagnostics.

------

## **6) Debugging (Phase 2)**

- Preserve/port **DAP client**.
- TUI layout: source pane + stacks/locals/watch panes.
- Keys: continue/step/next/finish; toggle breakpoints (gutter mark), breakpoint list panel.
- Open source at stop locations; highlight current line; show inline value hovers (popup).

------

## **7) Repo Surgery & Build Refactor**

**A. Carve a “core” library**

- Extract headless-safe pieces: buffers, project API, LSP client, build runner, diagnostics store, config.
- Remove all GtkAda/Python imports from these units.

**B. New tui-app**

- Depends on core + Terminal_Interface (ncurses).
- Own main loop: input → keymap → editor ops → partial redraw.

**C. Purge GUI/Python**

- Delete Gtk widget trees, menus, actions, docking, GTK main loop, Python engine and scripts.
- Delete non-Ada language modules/plugins.

**D. GPR changes**

- Two GPRs: core.gpr (no Gtk), tui.gpr (links ncurses binding).
- Static link where sensible; no runtime Python.

------

## **8) Minimal Architecture**

```
+---------------------------+
|           TUI             |
|  Terminal_Interface       |
|  Keymap (micro-style)     |
|  Command Palette          |
|  Panes/Splits/Status      |
+-------------+-------------+
              |
              v
+---------------------------+
|           Core            |
|  Buffers + Undo/Redo      |
|  Diagnostics Store        |
|  Project (.gpr) API       |
|  Build Runner (gprbuild)  |
|  LSP Client (ALS)         |
|  (DAP Client - optional)  |
+-------------+-------------+
              |
              v
+---------------------------+
| Ada Toolchain / ALS / DAP |
+---------------------------+
```

------

## 9) Phased Execution (do it in this order)**

1. **Compile GNAT Studio headless**: strip Gtk/Python deps until “core” compiles. Stub GUI calls.
2. **TUI bootstrap**: init curses, draw a file buffer, scrolling, cursor, status line.
3. **Editing basics**: insert/delete, undo/redo, file open/save, micro-like keymap, command palette.
4. **Splits & panels**: diagnostics pane, build pane; simple window manager.
5. **LSP online**: open/changes, completions, diagnostics, hovers, go-to, semantic tokens, format.
6. **Project/build**: load .gpr, fuzzy-open over project files, build command + error plumbing.
7. **Search & grep**: in-buffer find/replace; project grep to panel.
8. **Multi-cursor**: add/remove cursors; apply edits across selections.
9. **Polish & perf**: partial redraw, input latency, large-file tests; macOS/OpenBSD term quirks.
10. **(Optional) Debugging**: bring up DAP panes, breakpoints, stepping.

Ship after step 7–8; debugging can follow.

------

## **10) Config & Theming**

- $XDG_CONFIG_HOME/ada-tui/config.(toml|ini):
  - keybindings (override defaults),
  - theme (map semantic tokens → color pairs),
  - build commands, on-save actions.
- No embedded scripting to start; allow external commands via palette.

------

## **11) Risks & Mitigations**

- **GUI entanglement**: If buffer ops are coupled to Gtk, rehost a lean buffer rather than untangling everything. Keep file formats/undo semantics compatible.
- **Input complexity**: Normalize key events across terminals early; add tests for macOS terminfo, OpenBSD wscons.
- **LSP churn**: Guard with timeouts/cancellation; debounce text changes; robust reconnection.
- **Multi-cursor correctness**: Apply edits back-to-front by span to avoid index drift; fuzz tests on batch operations.

------

## **12) Footprint & Targets**

- **Binary**: single executable, no Python, no Gtk.
- **Start time**: < 150 ms on mid hardware.
- **Idle RAM**: low tens of MB (ALS is separate and can be heavier; that’s fine).
- **Throughput**: smooth typing at 120+ wpm; partial redraws only.

------

## **13) Recommendation Snapshot**

- **TUI lib**: Ada ncurses binding now; wrap it cleanly to swap later if needed.
- **Editing UX**: **Micro-style modeless** from day one (no Helix/Vim modes).
- **Intelligence**: ALS for everything smart; semantic tokens for color; GNATfmt via LSP.
- **Builds**: gprbuild in a pane; pipe errors into diagnostics.
- **Debugging**: keep DAP client; wire UI when core is stable.

This gets us a focused, fast Ada/SPARK terminal editor with modern ergonomics and zero GUI baggage, and it stays close to GNAT Studio’s proven Ada tooling where it matters.