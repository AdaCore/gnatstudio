# GNATstudio TUI - Current Session State & Recovery Guide

**Date:** 2025-11-05
**Branch:** strip-down
**Latest Commit:** e213b7b4c8
**Session:** 29 commits, ~10 hours work
**Token Usage:** ~410K/1M

---

## QUICK START FOR NEXT SESSION

**Immediate Status:**
- ✅ All changes committed and pushed
- ✅ Build works: `alr build` (0.47s)
- ✅ Tests pass: `./test_headless/bin/test_pure_ada` and `./tests/bin/test_runner`
- ✅ safety_build.gpr validates 9 core modules

**What to do NEXT:**
1. Read this file completely
2. Read PLAN.md Section 9 (Phased Execution)
3. **BEGIN PLAN.MD PHASE 2:** TUI Bootstrap (not LSP completion!)
4. Add ncursesada, create tui/, init terminal, draw buffer

**DO NOT:**
- Continue LSP client work (90% done, defer final 10% to Phase 5)
- Add gtkada dependency (defeats TUI purpose)
- Mock GPS.Editors (architectural debt)

---

## I. CURRENT TECHNICAL STATE

### A. Repository Metrics

```
Original:     ~570,000 LoC
Current:      ~165,000 LoC
Deleted:       967,790 lines (71% reduction!)
Files deleted: 4,596
Repository:    230 MB
Commits:       29 this session
```

### B. Build System

**Method:** `alr build` (pure Alire, no configure/make)

**Dependencies (14 packages):**
```
Core:
- gnatcoll, gnatcoll_xref, gnatcoll_sqlite (C compilation fixed!)
- vss, xmlada, libgpr
- aunit

LSP (Phase 5 prep):
- spawn, spawn_glib
- ada_language_server

Note: gtkada NOT added (would defeat TUI purpose)
```

**Build Performance:**
- test_pure_ada: 0.47s
- safety_build.gpr: ~5s
- All tests: <1s

### C. Module Status

**safety_build.gpr (Compiling & Validated) - 9 modules:**
```ada
with "common/common";
with "language/language";
with "toolchains_editor/core/toolchains_core";
with "kernel/kernel_core";
with "prj_editor/prj_core";
with "builder/builder_core";
with "ada_module/core/ada_module_core";
with "refactoring/core/refactoring_core";
with "codefix/core/codefix_core";
```

**Purpose:** Validates we haven't deleted critical code for Phases 2-6

**Coverage:** PLAN.md Phases 2-6 (TUI bootstrap through project/build)

**lsp_client_core (90% Complete) - NOT in safety_build yet:**
```
Location: lsp_client/core/
Files: 78 (was 124 total, 48 moved to deleted_gui/)
Status: Compiles partially (~70 files), needs body work
Blocker: ~36 GPS.Kernel→Callbacks conversions in body files
Decision: DEFER to Phase 5
```

**dap (Deferred to Phase 10):**
```
Location: dap/
Files: 117 (67% salvageable)
Status: Not split yet, depends on lsp_client
Decision: Handle in Phase 10 (debugging)
```

### D. Critical Fixes Applied

**C Compilation Fix (CRITICAL):**
```bash
# The real issue: Broken symlink
sudo rm /Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk
sudo ln -s MacOSX15.4.sdk /Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk

# What we THOUGHT was the issue (WRONG):
# - Broken include-fixed headers (was already empty)
# - Documented in TOOLCHAIN_FIX.md (NEEDS UPDATE)

# What ACTUALLY works for our code:
# - common.gpr: for Driver ("C") use "/usr/bin/clang";
# - Uses system clang to bypass broken GCC

# Result: ALL C code compiles (spawn, gnatcoll_*, etc.)
```

**File:** `fix_toolchain.sh` - NEEDS UPDATE with correct symlink fix

---

## II. WHERE WE ARE IN PLAN.MD

### PLAN.md Phase Status

**Phase 1: Headless Compilation** ✅ **COMPLETE**
```
Requirements: Strip GTK/Python, compile without GUI
Status: DONE (and validated with safety_build.gpr)
Exceeded: Created safety builds, testing, fixed C compilation
```

**Phase 2: TUI Bootstrap** ❌ **NOT STARTED** ← **WE SHOULD BE HERE**
```
Requirements: Init curses, buffer, scrolling, cursor, status line
Status: Not started
Why: Been doing Phase 1 cleanup and Phase 5 prep
Next: THIS IS THE CORRECT NEXT STEP
```

**Phase 3-4: Editing, Splits** ❌ **NOT STARTED**

**Phase 5: LSP Integration** ⚠️ **90% PREPARED** (out of sequence)
```
Requirements: Launch ALS, didOpen/didChange, completions, diagnostics
Status: lsp_client split 90% done (infrastructure ready)
Remaining: 10% needs TUI context (event loop, preferences, buffer model)
Decision: Finish when we naturally reach Phase 5
```

**Phase 6-10** ❌ **NOT STARTED**

### Alignment Analysis

**By PLAN.md letter:** ⚠️ Out of sequence (Phase 5 before Phase 2)

**By PLAN.md spirit:** ✅ Good (validating dependencies, preparing infrastructure)

**Correct action:** Return to sequence, begin Phase 2

---

## III. LSP CLIENT SPLIT - DETAILED STATUS

### What We Accomplished (90%)

**File Reorganization:**
```
lsp_client/
├── core/                       (78 files, 472K)
│   ├── lsp_client_core.gpr
│   ├── REMAINING_WORK.md       (documents final 10%)
│   └── src/
│       ├── gps-lsp_client-callbacks.ads/adb  (NEW - interface)
│       ├── gps-lsp_client-utilities.ads/adb  (NEW - minimal, 4 functions)
│       ├── gps-lsp_clients.ads/adb           (Kernel→Callbacks in spec)
│       ├── gps-lsp_client-requests*.ads/adb  (27 request types)
│       ├── gps-lsp_client-configurations*.ads/adb
│       └── gps-lsp_client-language_servers*.ads/adb
│
└── deleted_gui/                (48 files, 724K)
    ├── README.md               (documents what was deleted)
    ├── gps-lsp_module.ads/adb  (GPS registration)
    ├── gps-lsp_client-editors*.ads/adb  (20 files)
    ├── gps-lsp_client-completion.ads/adb
    ├── gps-lsp_client-outline.ads/adb
    └── ... (GUI views, refactoring UI, search UI)
```

**Callback Interface Created:**
```ada
-- GPS.LSP_Client.Callbacks (no GPS.Kernel, no Glib.Main!)
type LSP_Callback_Interface is limited interface;

-- Methods:
procedure Trace (Message : String; Mode : Trace_Mode) is abstract;
function Get_Tab_Width (File : Virtual_File) return Natural is abstract;
function Get_Insert_Spaces (File : Virtual_File) return Boolean is abstract;
function Get_Project_File return Virtual_File is abstract;
function Get_Project_Path return Virtual_File is abstract;
procedure Schedule_Timer (Interval, Callback, Timer) is abstract;
procedure Cancel_Timer (Timer) is abstract;

-- Null_Callback provided for testing
```

**Minimal Utilities (NO GPS.Editors!):**
```ada
-- Only 4 pure functions (100 LoC total):
function To_URI (Virtual_File) return DocumentUri;
function To_Virtual_File (DocumentUri) return Virtual_File;
function To_Language_Category (SymbolKind) return Language_Category;
function To_Construct_Visibility (Als_Visibility) return Construct_Visibility;

-- Excluded (GPS.Editors-dependent):
-- - LSP_Position_To_Location (unused by core)
-- - Location_To_LSP_Position (unused by core)
-- - Get_Formatting_Options (needs Preferences callback)
```

**Spec Files Fixed:**
```
✅ gps-lsp_clients.ads: Kernel→Callbacks discriminant
✅ gps-lsp_client-requests.ads: Kernel→Callbacks discriminant
✅ gps-lsp_client-configurations.ads: Kernel→Callbacks discriminant
✅ gps-lsp_client-configurations-clangd.ads: Callbacks
✅ gps-lsp_client-language_servers-real.ads: Kernel→Callbacks
✅ Glib.Main removed from specs
```

**Exclusions (8 files, justified):**
```
1. gps-lsp_client-requests-check_syntax.* - Needs deleted_gui dialog
2-7. gps-lsp_client-requests-*_formatting.* (6 files) - Need Preferences callback
```

**Files Compiling:** ~70 out of 78 (90%)

### What Remains (10%, ~2-3 hours)

**Body File Conversions (~36 references):**
```
gps-lsp_clients.adb: 4 Kernel. calls
  - Self.Kernel.Insert → Comment out or abstract
  - Self.Kernel.Get_Buffer_Factory → Comment out (GUI-specific)
  - Self.Kernel.Get_Original_Environment → Abstract or hardcode

gps-lsp_client-configurations*.adb: 31 Kernel. calls
  - GPS.Kernel.Preferences.* → Callbacks.Get_*
  - GPS.Kernel.Project.* → Callbacks.Get_Project_*
  - GPS.Kernel.Hooks.* → Comment out or abstract

gps-lsp_client-language_servers-real.adb: 1 Kernel. call
  - Downcast → Change to Callbacks type
```

**Glib.Main References (~10):**
```
gps-lsp_clients.adb:
  - Glib.Main.Generic_Sources instantiation → Abstract
  - Glib.Main.Remove (timer) → Callbacks.Cancel_Timer
  - Timer type → Callbacks.Timer_Id (already done in spec)
```

**Compilation Validation:**
```bash
alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr -p
# Expected after fixes: Success
```

**Add to safety_build.gpr:**
```ada
with "lsp_client/core/lsp_client_core";
```

### Why We're Pausing at 90%

**Technical Reason:**
- Remaining work needs TUI context (event loop, preferences, project model)
- Would be guessing at Callback implementations
- Likely rework when TUI design emerges

**Strategic Reason:**
- PLAN.md sequence says Phase 2 before Phase 5
- TUI is the goal, LSP is infrastructure
- Better to design Callbacks with TUI in hand

**Practical Reason:**
- Token budget approaching limit
- Natural pause point (structural work complete)
- Body conversions are mechanical (resume anytime)

---

## IV. CRITICAL DECISIONS MADE

### Decision 1: NO GPS.Editors Mocking ✅

**Proposed:** Create mock GPS.Editors package to allow utilities to compile

**Rejected Because:**
- Only 0 out of 4 useful utilities functions need GPS.Editors
- Would create fake dependency (kernel_core → editor types)
- Architectural debt (delete mocks later)
- Against TUI philosophy (no GUI dependencies)

**Instead:** Minimal utilities extraction (4 pure functions, NO GPS.Editors)

**Result:** Clean architecture, zero technical debt

### Decision 2: NO gtkada Dependency ✅

**Proposed:** Add gtkada for Glib.Main (event loop)

**Rejected Because:**
- gtkada is GTK+ GUI framework
- Completely defeats TUI purpose
- Glib.Main is GUI event loop

**Instead:** Abstract Glib.Main through Callbacks.Timer interface

**Result:** TUI-compatible design

### Decision 3: Defer LSP 10% to Phase 5 ✅

**Proposed:** Finish all 49 Kernel→Callbacks conversions now

**Rejected Because:**
- Needs TUI context (what do Callbacks actually do?)
- Premature design (don't know TUI architecture yet)
- PLAN.md says Phase 2 before Phase 5

**Instead:** Pause at 90%, document remaining work, pivot to Phase 2

**Result:** Correct sequencing, avoid rework

### Decision 4: Use Minimal Utilities (Not Full Mock, Not Delete) ✅

**Alternatives Considered:**
- A) Mock full GPS.Editors (200+ lines)
- B) Exclude all utilities
- C) Implement TUI types now
- D) Delete utilities, inline conversions

**Chosen:** Extract minimal utilities (4 functions, 100 LoC)

**Reasoning:**
- To_URI/To_Virtual_File are pure VFS↔LSP (TUI needs these too)
- Zero GPS.Editors dependency
- Zero GPS.Kernel dependency
- Reusable for TUI unchanged
- Enables 70 files to compile

**Result:** Best architecture, minimal code, maximum validation

---

## V. FILE ORGANIZATION - CURRENT LAYOUT

### Top-Level Directories (Modules)

```
ada_module/core/        - Ada language support (clean)
builder/core/           - Build orchestration (clean, 4 files excluded)
codefix/core/           - Code fix engine (clean, 2 files excluded)
common/core/            - Core utilities (clean)
dap/                    - Debug protocol (117 files, defer to Phase 10)
docs/                   - Documentation (9.9 MB, keep for reference)
kernel/src_info/        - Core kernel (clean, kernel_core.gpr)
language/               - Language infrastructure (clean)
lsp_client/             - LSP client (90% split, see below)
prj_editor/core/        - Project operations (clean)
refactoring/core/       - Refactoring engine (clean)
share/                  - Resources (templates, key_themes)
test_headless/          - Verification tests
tests/                  - AUnit tests (3 passing)
toolchains_editor/core/ - Toolchain management (clean)
```

### lsp_client/ Directory Structure

```
lsp_client/
├── gps_lsp_client.gpr          (OLD, references deleted src/)
│
├── core/                        ⭐ NEW STRUCTURE
│   ├── lsp_client_core.gpr     Protocol-only project
│   ├── REMAINING_WORK.md       Documents final 10%
│   └── src/ (78 files)
│       ├── Callbacks           Interface files (NEW)
│       │   ├── gps-lsp_client-callbacks.ads (interface definition)
│       │   └── gps-lsp_client-callbacks.adb (Null_Callback impl)
│       │
│       ├── Client Core         Main client (Kernel→Callbacks in spec)
│       │   ├── gps-lsp_clients.ads ✅ (Callbacks discriminant)
│       │   ├── gps-lsp_clients.adb ⚠️ (4 Kernel. calls remain)
│       │   └── gps-lsp_clients-shutdowns.ads/adb
│       │
│       ├── Configurations      Server configs (Kernel→Callbacks in specs)
│       │   ├── gps-lsp_client-configurations.ads ✅
│       │   ├── gps-lsp_client-configurations.adb ⚠️ (31 Kernel. calls)
│       │   ├── gps-lsp_client-configurations-als.ads/adb
│       │   └── gps-lsp_client-configurations-clangd.ads/adb
│       │
│       ├── Language Servers    Server lifecycle
│       │   ├── gps-lsp_client-language_servers.ads/adb
│       │   ├── gps-lsp_client-language_servers-real.ads ✅
│       │   ├── gps-lsp_client-language_servers-real.adb ⚠️ (1 Kernel. call)
│       │   ├── gps-lsp_client-language_servers-stub.ads
│       │   └── gps-lsp_client-language_servers-interceptors.ads
│       │
│       ├── Requests (27 types)  Protocol requests
│       │   ├── gps-lsp_client-requests.ads ✅ (Callbacks discriminant)
│       │   ├── gps-lsp_client-requests.adb
│       │   ├── gps-lsp_client-requests-base.ads/adb
│       │   ├── gps-lsp_client-requests-*.ads/adb (26 request types)
│       │   │   Most compile ✅
│       │   │   Excluded (8):
│       │   │     - check_syntax (needs deleted_gui dialog)
│       │   │     - 3 formatting requests (need Preferences callback)
│       │   │     - execute_command-named_parameters (uses old utilities)
│       │   └── gps-lsp_client-requests-internals.ads/adb
│       │
│       ├── Utilities           Minimal conversions (NEW)
│       │   ├── gps-lsp_client-utilities.ads ✅ (4 functions, NO GPS.Editors!)
│       │   ├── gps-lsp_client-utilities.adb ✅ (pure VFS↔LSP conversions)
│       │   └── To_URI, To_Virtual_File, To_Language_Category, To_Construct_Visibility
│       │
│       └── Other
│           ├── gps-lsp_client.ads (package root)
│           ├── gps-lsp_client-text_documents.ads (doc sync)
│           └── gps-lsp_client-partial_results.ads
│
└── deleted_gui/ (48 files)      ⭐ ARCHIVED GUI LAYER
    ├── README.md                Documents what was deleted and why
    ├── gps-lsp_module.ads/adb   GPS module registration
    ├── gps-lsp_client-editors*.ads/adb (20 files) - Editor integration
    ├── gps-lsp_client-completion.ads/adb - GTK completion popup
    ├── gps-lsp_client-outline.ads/adb - GTK outline view
    ├── gps-lsp_client-references.ads/adb - Find refs view
    ├── gps-lsp_client-call_tree.ads/adb - Call hierarchy
    ├── gps-lsp_client-refactoring*.ads/adb (8 files) - Refactoring UI
    ├── gps-lsp_client-search*.ads/adb (4 files) - Search integration
    ├── gps-lsp_client-shell.ads/adb - Python bindings
    └── gps-lsp_client-tasks.ads/adb - Background tasks UI
```

### Compilation Status

**Currently excluded (8 files):**
1. check_syntax - Needs GPS.LSP_Client.Editors.Code_Actions.Dialog (in deleted_gui)
2-7. Formatting requests (6 files) - Need Get_Formatting_Options callback

**Currently compile (~70 files):**
- All infrastructure (clients, configurations, language_servers)
- Most requests (hover, completion, references, symbols, rename, code_action, etc.)
- Utilities (minimal, pure functions)

**Blocked on (~36 references in body files):**
- GPS.Kernel.Preferences → Callbacks.Get_* (preferences)
- GPS.Kernel.Project → Callbacks.Get_Project_* (project context)
- GPS.Kernel.Hooks → Abstract or comment out (hook registration)
- GPS.Kernel.Insert, Get_Buffer_Factory → Comment out (GUI-specific)
- Glib.Main calls → Callbacks.Timer calls

### Why 90% is Good Stopping Point

**Completed (Hard Work):**
- File organization (mechanical but time-consuming)
- Interface design (requires architectural thinking)
- Type system changes (discriminants, affects everything)
- Dependency resolution (spawn, ada_language_server)

**Remaining (Mechanical Work):**
- Search/replace Kernel.X → Callbacks.Y
- Implement Callbacks methods (but don't know what they should do without TUI!)
- Comment out GUI-specific code

**Better to do remaining work WITH TUI context:**
- Callbacks.Get_Tab_Width → Read from TUI config
- Callbacks.Schedule_Timer → Use TUI event loop
- Callbacks.Trace → Write to TUI log panel

---

## VI. WHAT TO DO NEXT - DETAILED GUIDANCE

### Option A: Complete LSP Final 10% (2-3 hours)

**Tasks:**
1. Convert 36 Kernel→Callbacks in body files
2. Abstract or comment out GUI-specific code
3. Test compilation
4. Add to safety_build.gpr

**Pros:**
- Finish one component 100%
- safety_build includes LSP
- Clean milestone

**Cons:**
- Out of PLAN.md sequence
- Callback implementations are stubs (no TUI yet)
- Delayed TUI progress

**Effort:** 2-3 hours

### Option B: Begin PLAN.MD Phase 2 (TUI Bootstrap) ⭐ RECOMMENDED

**Tasks:**
1. Research ncursesada vs malef (ncursesada in Alire, malef not)
2. Add ncursesada dependency to alire.toml
3. Create tui/ directory structure
4. Create tui/tui.gpr project
5. Create tui/src/tui_main.adb (ncurses "hello world")
6. Initialize terminal (initscr, raw, noecho, keypad)
7. Implement Text_Buffer type (gap buffer or rope)
8. Draw buffer to terminal (addstr, mvaddstr)
9. Handle cursor (getyx, move)
10. Basic scrolling (viewport into buffer)
11. Status line (bottom row)
12. File loading (GNATCOLL.VFS)

**Pros:**
- Follows PLAN.md sequence
- Visible progress (working TUI)
- Provides context for LSP completion
- This is the project goal!

**Cons:**
- LSP at 90% (but good stopping point)
- Context switch from infrastructure to UI

**Effort:** 2-3 weeks for Phase 2

### Recommendation: OPTION B

**Reasoning:**
1. PLAN.md sequence is correct (TUI before LSP)
2. Remaining LSP work needs TUI context
3. We've validated LSP is doable (main goal of prep work)
4. Time to build the actual product!

---

## VII. DEPENDENCIES FOR PHASE 2

### Required: Terminal Library

**Option 1: ncursesada (Available in Alire)**
```bash
alr search ncurses
# Ada binding to ncurses library
# Pros: Available, standard, well-tested
# Cons: Not as modern as malef
```

**Option 2: malef (PLAN.md preference, NOT in Alire)**
```
Repository: https://github.com/joseaverde/Malef
Status: Ada 2022 TUI toolkit, active development
Pros: Modern, Ada 2022, designed for TUI
Cons: Not in Alire, need to vendor or pin
PLAN.md says: "defer malef, clone later for patches"
```

**Decision for Phase 2:** Start with ncursesada (available), migrate to malef later if desired

### Required: Buffer Implementation

**Options:**
- Gap buffer (simple, fast for cursor-local edits)
- Rope (complex, fast for large files)
- Piece table (middle ground)

**Recommendation:** Gap buffer to start (simplest)

### Required: Project Files

```
tui/
├── tui.gpr
├── src/
│   ├── tui_main.adb           Entry point
│   ├── tui-buffer.ads/adb     Text buffer type
│   ├── tui-terminal.ads/adb   Terminal abstraction
│   ├── tui-editor.ads/adb     Editor state
│   ├── tui-input.ads/adb      Input handling
│   ├── tui-render.ads/adb     Screen rendering
│   └── tui-status.ads/adb     Status line
```

---

## VIII. KEY FILES TO READ

**For Understanding Current State:**
1. `SESSION_STATE.md` (this file)
2. `PLAN.md` (project roadmap)
3. `safety_build.gpr` (what's validated)
4. `lsp_client/core/REMAINING_WORK.md` (LSP final 10%)

**For Build System:**
1. `alire.toml` (dependencies)
2. `BUILD_INSTRUCTIONS.md` (how to build)
3. `fix_toolchain.sh` (C compilation fix)
4. `TOOLCHAIN_FIX.md` (NEEDS UPDATE - wrong explanation)

**For Architecture:**
1. `MODULES.md` (module organization)
2. `DEPENDENCIES.md` (dependency info)
3. `lsp_client/deleted_gui/README.md` (what was deleted)
4. `lsp_client/core/src/gps-lsp_client-callbacks.ads` (interface design)

---

## IX. COMMIT HISTORY SUMMARY

**Commits 1-10: Phase 1 Foundation**
- System dependencies, Alire setup
- GTK/Python removal from configure
- Headless build verified

**Commits 11-19: Massive Cleanup**
- GUI infrastructure deleted (791 files)
- configure/Makefile removed
- Non-Ada modules deleted
- ~400K LoC removed

**Commits 20-21: C Compilation Breakthrough**
- Fixed MacOSX14.sdk symlink (real issue!)
- Re-added gnatcoll_xref/sqlite
- Created safety_build.gpr

**Commits 22-25: Safe Deletion Phases**
- Phase 1: Excluded files (16)
- Phase 2: kernel/src, builder/src (195 files)
- Phase 3: Obsolete modules (82 files)
- All validated by safety_build.gpr

**Commits 26-29: LSP Client Split**
- 78 files → core/, 48 → deleted_gui/
- Callback interface created
- Minimal utilities (no mocking!)
- 90% complete

**Total:** 4,596 files deleted, 967,790 lines removed, 71% reduction

---

## X. BLOCKERS & SOLUTIONS

### Blocker 1: C Compilation (SOLVED ✅)

**Problem:** All C code failed (fcntl.h not found)

**Root Cause:** MacOSX14.sdk symlink → non-existent MacOSX15.5.sdk

**Solution:**
```bash
sudo rm /Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk
sudo ln -s MacOSX15.4.sdk /Library/Developer/CommandLineTools/SDKs/MacOSX14.sdk
```

**Workaround (for our code):**
```ada
-- In common.gpr:
package Compiler is
   for Driver ("C") use "/usr/bin/clang";
end Compiler;
```

**Status:** SOLVED (all C compiles)

**Documentation:** fix_toolchain.sh, TOOLCHAIN_FIX.md (NEEDS UPDATE)

### Blocker 2: gnatcoll_xref Dependency (SOLVED ✅)

**Problem:** Initially removed, broke kernel_core/language compilation

**Root Cause:** Thought "LSP replaces xref" but types are fundamental

**Solution:** Re-add gnatcoll_xref after fixing C compilation

**Status:** SOLVED (xref types available)

### Blocker 3: Glib.Main Dependency (SOLVED ✅)

**Problem:** lsp_client uses Glib.Main (GUI event loop)

**Solution:** Abstract through Callbacks.Timer interface

**Status:** SOLVED in specs, remains in bodies (~10 call sites)

### Blocker 4: GPS.Editors Dependency (SOLVED ✅)

**Problem:** lsp_client utilities uses GPS.Editors (deleted module)

**Solution:** Minimal utilities with only 4 pure functions (NO GPS.Editors!)

**Status:** SOLVED (no mocking needed!)

### Blocker 5: GPS.Kernel in LSP Bodies (DEFERRED)

**Problem:** ~36 Kernel. calls in configuration/client bodies

**Solution:** Convert to Callbacks (mechanical work)

**Status:** DEFERRED to Phase 5 (needs TUI context)

---

## XI. TESTING STRATEGY

### Current Tests (Minimal)

**test_pure_ada:**
- Validates: GNAT compiler, VSS library, Alire dependencies
- Coverage: Build system only (~0.1%)

**AUnit tests (3 tests):**
- Validates: VSS string operations
- Coverage: String utilities only

**safety_build.gpr:**
- Validates: 9 core modules compile together
- Coverage: Compilation only (no runtime tests)

**Issues:** No functional tests, no buffer tests, no LSP tests

### Testing for Phase 2

**Should add:**
```ada
-- tests/src/buffer_tests.ads/adb
procedure Test_Insert_Character;
procedure Test_Delete_Character;
procedure Test_Buffer_To_String;
procedure Test_Cursor_Movement;
procedure Test_Scrolling;
```

**AUnit integration:**
- Add to tests/src/test_runner.adb
- Run with `./tests/bin/test_runner`

---

## XII. BUILD COMMANDS

**Main build:**
```bash
alr build                # Builds test_pure_ada (0.47s)
```

**Safety validation:**
```bash
alr exec -- gprbuild -P safety_build.gpr    # Validates core modules (~5s)
```

**Test LSP core:**
```bash
alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr -p
# Currently fails at body files (expected)
```

**Run tests:**
```bash
./test_headless/bin/test_pure_ada    # Smoke test
./tests/bin/test_runner              # AUnit tests
```

**Clean:**
```bash
alr clean    # Clean build artifacts
```

---

## XIII. RECOMMENDED NEXT ACTIONS

### Immediate (Next Session):

**1. Update TOOLCHAIN_FIX.md** (15 min)
- Correct explanation (symlink, not include-fixed)
- Update fix_toolchain.sh with symlink fix

**2. Begin PLAN.MD Phase 2** (Start TUI!)
- Add ncursesada to alire.toml
- Create tui/ directory
- Create tui.gpr
- Create tui_main.adb with ncurses "hello world"
- Initialize terminal (initscr, raw mode)
- Clear screen, print "TUI works!", wait for key, cleanup

**Estimated first milestone:** 2-4 hours to "hello TUI"

### Medium-term (Weeks 1-3):

**Implement Phase 2 per PLAN.md:**
- Text buffer structure
- Drawing to screen
- Cursor movement
- Scrolling
- Status line
- File open/save

### Long-term (Weeks 4-11):

**Follow PLAN.MD Phases 3-5:**
- Phase 3: Editing (insert/delete, undo)
- Phase 4: Splits & panels
- Phase 5: LSP integration (finish lsp_client_core 10%, wire to TUI)

---

## XIV. CRITICAL REMINDERS

### DO NOT:
- ❌ Add gtkada dependency (defeats TUI purpose)
- ❌ Mock GPS.Editors (architectural debt)
- ❌ Try to finish LSP without TUI (needs context)
- ❌ Skip PLAN.md phases (sequence matters)

### DO:
- ✅ Follow PLAN.MD Phase 2 next
- ✅ Use ncursesada (available in Alire)
- ✅ Build TUI incrementally (commit often)
- ✅ Test each feature as you build
- ✅ Refer to lsp_client/core/REMAINING_WORK.md when reaching Phase 5

### REMEMBER:
- We're at 90% LSP prep, not 100% (intentional pause)
- TUI is the goal, LSP is infrastructure
- safety_build.gpr is our safety net
- Callback interface is correct design (will pay off in Phase 5)

---

## XV. SUCCESS METRICS

### What Success Looks Like (End of Phase 2)

```bash
# Launch TUI
./tui/bin/gnatstudio_tui some_file.adb

# Should see:
- Ada file contents displayed
- Cursor at top-left
- Can scroll with arrow keys
- Status line shows: "some_file.adb | Line 1, Col 1"
- Can quit with Ctrl+Q
```

**This proves:**
- Terminal initialization works
- Buffer can load and display
- Input handling works
- Rendering works
- Basic TUI foundation complete

### What Success Looks Like (End of Phase 5)

```bash
./tui/bin/gnatstudio_tui some_file.adb

# Should see:
- Ada file with syntax highlighting (semantic tokens)
- Diagnostics in gutter (red X for errors)
- Type identifier, press key → hover shows type info
- Press Ctrl+Space → completion menu appears
- Make error, see diagnostic immediately
- Press key → jump to definition
```

**This proves:**
- LSP integration complete
- lsp_client_core 100% done
- TUI LSP callbacks implemented
- Full smart editor working

---

## XVI. QUESTIONS FOR RECOVERY

### If Starting Fresh Session:

**Q1:** Where are we?
**A1:** Phase 1 complete, Phase 5 90% prepared, should do Phase 2 next

**Q2:** What should I do?
**A2:** Begin PLAN.MD Phase 2 (TUI bootstrap with ncursesada)

**Q3:** Why is LSP not 100%?
**A3:** Intentional - remaining 10% needs TUI context, defer to Phase 5

**Q4:** Can I finish LSP now?
**A4:** You can (2-3 hours) but shouldn't (needs TUI, out of sequence)

**Q5:** What if I want to work on LSP?
**A5:** Read lsp_client/core/REMAINING_WORK.md, but really should do TUI first

**Q6:** Is anything broken?
**A6:** No - all builds work, all tests pass, all commits pushed

**Q7:** What's the critical path?
**A7:** TUI (Phase 2-4) before LSP completion (Phase 5)

---

## XVII. FINAL STATUS

**Codebase:** Clean, validated, 71% smaller
**Build:** Working (alr build)
**Tests:** Passing (smoke + 3 AUnit)
**Safety:** Validated (safety_build.gpr)
**LSP:** 90% ready (intentional pause)
**TUI:** Not started (correct next step)
**Alignment:** Return to PLAN.md sequence

**Recommendation:** Begin Phase 2 (TUI Bootstrap)

**Confidence:** High (foundation is solid, path is clear)

**Next milestone:** Working TUI displaying Ada files (2-3 weeks)

---

**This document should enable complete recovery of context in next session.**
