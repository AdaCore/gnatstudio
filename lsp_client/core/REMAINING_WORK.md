# LSP Client Core - Remaining 20% Work for Phase 5

**Current Status:** 80% complete - structural split done, integration work remains

**Created:** 2025-11-05
**Commit:** e9d8a30781 (wip: split lsp_client module - Phases 1-4)

---

## What's Complete (80%)

✅ **File Organization:**
- 77 protocol files in lsp_client/core/src/
- 48 GUI files archived in lsp_client/deleted_gui/
- Clean separation of protocol from GUI

✅ **Callback Interface:**
- Created GPS.LSP_Client.Callbacks interface
- Abstracts: Trace, Get_Tab_Width, Get_Insert_Spaces, Get_Project_File/Path
- Includes Null_Callback for testing

✅ **Main Discriminant Decoupling:**
- gps-lsp_clients.ads: Changed Kernel → Callbacks discriminant
- gps-lsp_client-requests.ads: Changed Kernel → Callbacks discriminant
- Type signatures updated

✅ **Dependencies:**
- spawn ~25.0 added (process management)
- ada_language_server ~25.0 added (LSP protocol types)
- lsp_client_core.gpr created with vss_text, spawn, lsp_client imports

---

## Remaining Work (20%) - Estimated 4-6 Hours

### 1. Event Loop Abstraction (2-3 hours)

**Problem:** Glib.Main dependency for timers/event loop

**Current Usage:**
```ada
-- gps-lsp_clients.ads line 310-311
Restart_Timer : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;

-- gps-lsp_clients.adb ~10 references:
- Line 83: new Glib.Main.Generic_Sources (LSP_Client_Access)
- Line 964: Self.Restart_Timer := Glib.Main.No_Source_Id
- Line 1690: use type Glib.Main.G_Source_Id
- Line 1697-1699: Glib.Main.Remove (Self.Restart_Timer)
```

**Purpose:**
- Timer to automatically restart crashed language servers
- Integrates with GTK+ main event loop
- Provides periodic polling

**Solution Options:**

**Option A: Abstract Through Callbacks (RECOMMENDED)**

Add to GPS.LSP_Client.Callbacks interface:
```ada
type Timer_Id is private;
No_Timer : constant Timer_Id;

procedure Create_Timer
  (Self     : LSP_Callback_Interface;
   Interval : Natural;  -- milliseconds
   Callback : Timer_Callback_Access;
   Timer    : out Timer_Id) is abstract;

procedure Cancel_Timer
  (Self  : LSP_Callback_Interface;
   Timer : in out Timer_Id) is abstract;
```

Changes needed:
- gps-lsp_clients.ads: Replace `Glib.Main.G_Source_Id` with `Callbacks.Timer_Id`
- gps-lsp_clients.adb: Replace `Glib.Main.Remove` with `Callbacks.Cancel_Timer`
- Remove `with Glib.Main;` (becomes GUI-free!)

**Effort:** 2-3 hours
- Define interface: 30 min
- Update gps-lsp_clients.ads/adb: 1-2 hours
- Test with Null_Callback (timers disabled): 30 min

**Option B: Make Restart Timer Optional**

```ada
-- Just comment out timer functionality
-- Self.Restart_Timer := Glib.Main.No_Source_Id;  -- Disabled for TUI
```

**Effort:** 30 min
**Downside:** Loses auto-restart functionality

---

### 2. Fix Remaining GPS.Kernel References in Specs (1 hour)

**4 files still reference GPS.Kernel:**

**File 1: gps-lsp_client-configurations.ads** (~line 30)
```ada
-- CURRENT:
type Server_Configuration
  (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is ...

-- CHANGE TO:
type Server_Configuration
  (Callbacks : not null access GPS.LSP_Client.Callbacks.LSP_Callback_Interface'Class) is ...
```

**File 2: gps-lsp_client-configurations-clangd.ads**
```ada
-- Similar change: Kernel → Callbacks discriminant
```

**File 3: gps-lsp_client-language_servers-real.ads**
```ada
-- Used for: Kernel.Trace (logging)
-- Change to: Callbacks.Trace
```

**File 4: gps-lsp_client-utilities.ads** (line 61)
```ada
-- CURRENT:
function Kernel return GPS.Kernel.Kernel_Handle;

-- OPTIONS:
A. Delete this function (likely GPS-specific utility)
B. Change to: function Callbacks return GPS.LSP_Client.Callbacks.LSP_Callback_Access
```

**Tasks:**
- Read each file
- Change Kernel discriminants → Callbacks
- Update function signatures
- Remove `with GPS.Kernel;` statements
- Add `with GPS.LSP_Client.Callbacks;`

**Effort:** 1 hour (4 files × 15 min each)

---

### 3. Fix GPS.Kernel References in Body Files (1-2 hours)

**Pattern to Replace:**

**OLD (GPS.Kernel pattern):**
```ada
Kernel.Trace ("LSP: Server started");
Width := Kernel.Get_Preferences.Editor_Tab_Width.Get_Pref;
```

**NEW (Callbacks pattern):**
```ada
Callbacks.Trace ("LSP: Server started", Trace_Info);
Width := Callbacks.Get_Tab_Width (File);
```

**Files to Update (~10 locations):**

1. **gps-lsp_clients.adb** (~5-10 Kernel.Trace calls)
2. **gps-lsp_client-configurations.adb** (preference access)
3. **gps-lsp_client-configurations-als.adb** (preference access)
4. **gps-lsp_client-configurations-clangd.adb** (preference access)
5. **gps-lsp_client-language_servers-real.adb** (tracing)
6. **gps-lsp_client-utilities.adb** (formatting preferences)

**Common Patterns:**

```ada
-- Tracing
Kernel.Trace("message")
→ Callbacks.Trace("message", Trace_Info)

-- Preferences (tab width)
Kernel.Get_Preferences.Editor_Tab_Width.Get_Pref
→ Callbacks.Get_Tab_Width(File)

-- Preferences (insert spaces)
Kernel.Get_Preferences.Use_Tabs.Get_Pref
→ not Callbacks.Get_Insert_Spaces(File)

-- Project context
Kernel.Get_Project_Tree.Root_Project.Project_Path
→ Callbacks.Get_Project_Path
```

**Effort:** 1-2 hours
- Search and replace: 30 min
- Handle edge cases: 30-60 min
- Test compilation: 30 min

---

### 4. Handle GPS.Editors References (30 min - 1 hour)

**In gps-lsp_client-utilities.ads:**

```ada
with GPS.Editors;

function LSP_Position_To_Location
  (Editor   : GPS.Editors.Editor_Buffer'Class;
   Position : LSP.Messages.Position)
   return GPS.Editors.Editor_Location'Class;
```

**Problem:** GPS.Editors is from src_editor (deleted module)

**Solutions:**

**Option A: Exclude from lsp_client_core**
```ada
-- Add to lsp_client_core.gpr:
for Excluded_Source_Files use (
   "gps-lsp_client-utilities.ads",
   "gps-lsp_client-utilities.adb"
);
```

**Option B: Move to deleted_gui** (these are editor conversions, not protocol)

**Option C: Stub GPS.Editors types**
```ada
-- Create minimal GPS.Editors stub
package GPS.Editors is
   type Editor_Buffer is null record;
   type Editor_Location is null record;
end GPS.Editors;
```

**RECOMMENDED: Option A** (exclude utilities for now)
- These are editor integration functions
- Not core protocol
- Can implement TUI-specific versions later

**Effort:** 30 min

---

### 5. Test Compilation with Exclusions (1 hour)

**Create exclusions list:**
```ada
-- lsp_client_core.gpr
for Excluded_Source_Files use (
   "gps-lsp_client-utilities.ads",
   "gps-lsp_client-utilities.adb"
);
```

**Test:**
```bash
alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr -p
```

**Expected issues:**
- Files that import utilities will fail
- May need to exclude more files transitively
- Dependency chain analysis needed

**Iterate:**
- Add exclusions as compilation reveals dependencies
- Document what's excluded and why
- Ensure core protocol still compiles

**Effort:** 1 hour (compilation + iteration)

---

### 6. Add to safety_build.gpr (30 min)

**Once lsp_client_core compiles:**

```ada
-- safety_build.gpr
with "common/common";
with "language/language";
with "toolchains_editor/core/toolchains_core";
with "kernel/kernel_core";
with "prj_editor/prj_core";
with "builder/builder_core";
with "ada_module/core/ada_module_core";
with "refactoring/core/refactoring_core";
with "codefix/core/codefix_core";
with "lsp_client/core/lsp_client_core";  -- ADD THIS

project Safety_Build is
   ...
end Safety_Build;
```

**Test:**
```bash
alr exec -- gprbuild -P safety_build.gpr
```

**Effort:** 30 min (test + fix any issues)

---

## Detailed Task Breakdown

### Task 1: Abstract Glib.Main (2-3 hours)

**Subtasks:**
1. Add timer interface to GPS.LSP_Client.Callbacks.ads (30 min)
   ```ada
   type Timer_Id is new Natural;
   No_Timer : constant Timer_Id := 0;

   type Timer_Callback is access procedure;

   procedure Schedule_Timer
     (Self     : LSP_Callback_Interface;
      Interval : Natural;
      Callback : Timer_Callback;
      Timer    : out Timer_Id) is abstract;

   procedure Cancel_Timer
     (Self : LSP_Callback_Interface;
      Timer : in out Timer_Id) is abstract;
   ```

2. Update Null_Callback implementation (15 min)
   ```ada
   overriding procedure Schedule_Timer (...) is
   begin
      Timer := No_Timer;  -- No-op for null callback
   end Schedule_Timer;

   overriding procedure Cancel_Timer (...) is null;
   ```

3. Update gps-lsp_clients.ads (30 min)
   - Remove `with Glib.Main;`
   - Change `Restart_Timer : Glib.Main.G_Source_Id` → `Restart_Timer : Callbacks.Timer_Id`
   - Update Timer_Rec type if present

4. Update gps-lsp_clients.adb (1-1.5 hours)
   - Replace ~10 Glib.Main calls with Callbacks calls
   - Remove Generic_Sources instantiation
   - Update timer creation/cancellation logic
   - Handle timer callbacks (may need restructuring)

5. Test (30 min)

### Task 2: Fix 4 Spec Files GPS.Kernel (1 hour)

**gps-lsp_client-configurations.ads (15 min):**
```ada
-- Line ~30
type Server_Configuration
  (Callbacks : not null access GPS.LSP_Client.Callbacks.LSP_Callback_Interface'Class)
is tagged limited record
   ...
end record;
```

**gps-lsp_client-configurations-als.adb (15 min):**
- Search for Kernel references
- Replace with Callbacks
- Likely in preference access

**gps-lsp_client-configurations-clangd.ads (15 min):**
- Similar Kernel → Callbacks

**gps-lsp_client-language_servers-real.ads (15 min):**
- Used for Kernel.Trace → Callbacks.Trace

### Task 3: Fix Body Files (1-2 hours)

**Search pattern:**
```bash
grep -n "Kernel\." lsp_client/core/src/*.adb | wc -l
```

**Replacement patterns:**

| Old Code | New Code | Location |
|----------|----------|----------|
| `Kernel.Trace("msg")` | `Callbacks.Trace("msg", Trace_Info)` | ~5-10 places |
| `Kernel.Get_Preferences.Editor_Tab_Width.Get_Pref` | `Callbacks.Get_Tab_Width(File)` | configs |
| `Kernel.Get_Preferences.Use_Tabs.Get_Pref` | `not Callbacks.Get_Insert_Spaces(File)` | configs |
| `Kernel.Get_Project_Tree.Root_Project.Project_Path` | `Callbacks.Get_Project_Path` | configs |

**Systematic approach:**
1. Grep all Kernel. references: `grep -rn "Kernel\." lsp_client/core/src/*.adb`
2. Categorize by type (trace, preference, project)
3. Replace each category
4. Compile and fix errors
5. Iterate

### Task 4: Handle Utilities or Exclude (30 min)

**Decision point:** gps-lsp_client-utilities has GPS.Editors

**IF excluding:**
```ada
-- lsp_client_core.gpr
for Excluded_Source_Files use (
   "gps-lsp_client-utilities.ads",
   "gps-lsp_client-utilities.adb"
);
```

**Then find files that import utilities:**
```bash
grep "GPS.LSP_Client.Utilities" lsp_client/core/src/*.ads
```

**May need to exclude transitively:**
- Files that use utilities
- Create "minimal" vs "full" core

### Task 5: Compilation Testing (1-2 hours)

**Iterative process:**
```bash
# Attempt build
alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr -p 2>&1 | tee /tmp/lsp_core_build.log

# For each error:
# 1. Categorize (missing import, wrong type, etc.)
# 2. Fix or exclude file
# 3. Rebuild
# 4. Repeat

# Continue until:
# Success OR clean set of exclusions documented
```

**Expected error categories:**
1. Missing GPS.Kernel methods → Add to Callbacks
2. Missing GPS.Editors → Exclude file
3. Circular dependencies → Restructure imports
4. Glib.Main references → Abstract or exclude
5. Missing src_editor → Exclude file

### Task 6: Add to safety_build.gpr (30 min)

**After successful compilation:**

1. Add import to safety_build.gpr
2. Test: `alr exec -- gprbuild -P safety_build.gpr`
3. Fix any circular dependency issues
4. Document exclusions if any

---

## Effort Summary by Category

| Category | Optimistic | Realistic | Pessimistic |
|----------|-----------|-----------|-------------|
| Event loop abstraction | 1.5h | 2.5h | 4h |
| Fix spec files | 0.5h | 1h | 2h |
| Fix body files | 0.5h | 1.5h | 3h |
| Handle utilities | 0.25h | 0.5h | 1h |
| Compilation testing | 0.5h | 1h | 2h |
| Add to safety_build | 0.25h | 0.5h | 1h |
| **TOTAL** | **3.5h** | **7h** | **13h** |

**Expected:** 7 hours (1 work day with realistic estimate)

---

## Detailed File-by-File Work List

### Files Needing GPS.Kernel → Callbacks Changes

**Spec Files (4):**
1. ☐ gps-lsp_client-configurations.ads
   - Change discriminant
   - Estimate: 15 min

2. ☐ gps-lsp_client-configurations-clangd.ads
   - Change discriminant or imports
   - Estimate: 15 min

3. ☐ gps-lsp_client-language_servers-real.ads
   - Kernel for tracing
   - Estimate: 15 min

4. ☐ gps-lsp_client-utilities.ads
   - Has GPS.Editors (consider excluding)
   - Estimate: 30 min

**Body Files (~6):**
5. ☐ gps-lsp_clients.adb
   - ~10 Kernel references
   - ~10 Glib.Main references
   - Estimate: 1.5-2 hours

6. ☐ gps-lsp_client-configurations.adb
   - Preference access
   - Estimate: 30 min

7. ☐ gps-lsp_client-configurations-als.adb
   - Preference access
   - Estimate: 15 min

8. ☐ gps-lsp_client-configurations-clangd.adb
   - Preference access
   - Estimate: 30 min

9. ☐ gps-lsp_client-language_servers-real.adb
   - Tracing calls
   - Estimate: 30 min

10. ☐ gps-lsp_client-utilities.adb
    - May exclude entire file
    - Estimate: 15 min (if excluding)

### Interface Additions Needed

**GPS.LSP_Client.Callbacks.ads additions:**

```ada
-- Timer abstraction (if Option A)
type Timer_Id is new Natural;
No_Timer : constant Timer_Id := 0;
type Timer_Callback is access procedure;

procedure Schedule_Timer (...) is abstract;
procedure Cancel_Timer (...) is abstract;

-- Null_Callback implementations
overriding procedure Schedule_Timer (Self : Null_Callback; ...) is
begin
   Timer := No_Timer;
end;

overriding procedure Cancel_Timer (Self : Null_Callback; ...) is null;
```

---

## Integration Checklist

**When completing in Phase 5:**

- [ ] All GPS.Kernel references removed or converted
- [ ] All Glib.Main references abstracted or excluded
- [ ] No GPS.Editors dependencies (excluded or removed)
- [ ] Compiles: `alr exec -- gprbuild -P lsp_client/core/lsp_client_core.gpr`
- [ ] Added to safety_build.gpr
- [ ] safety_build.gpr compiles
- [ ] Exclusions documented (if any)
- [ ] TUI callback implementation planned
- [ ] README.md in lsp_client/core/ updated
- [ ] Committed and pushed

---

## Current Blockers

**Hard Blockers (must fix):**
1. Glib.Main dependency (GUI event loop)
2. GPS.Kernel in 4+ spec files
3. GPS.Kernel calls in body files

**Soft Blockers (can exclude):**
1. GPS.Editors utilities
2. Files that transitively depend on utilities

**Not Blockers (already fixed):**
- ✅ File organization
- ✅ Callback interface design
- ✅ Main type discriminants
- ✅ Dependencies added
- ✅ C compilation (symlink fixed)

---

## Why This is the RIGHT Time to Pause

**Per PLAN.md sequencing:**
- Phase 2-4: TUI bootstrap, editing, splits
- **Phase 5: LSP integration** ← This is when lsp_client_core completes
- Phase 6-10: Advanced features

**We're pausing at exactly the right point:**
- Structure is ready (80%)
- TUI infrastructure needed for remaining 20%
- When Phase 5 arrives, we'll have TUI event loop to plug into
- Callbacks.Schedule_Timer will have a real implementation

**Completing now would require:**
- Implementing TUI event loop (doesn't exist)
- Guessing at TUI integration points
- Possibly rework later

**Better:** Complete when TUI exists and we know what callbacks should do.

---

## Success Criteria for "100% Done"

**Phase 5 completion checklist:**
1. lsp_client_core.gpr compiles successfully
2. Zero GPS.Kernel references
3. Zero Glib.Main references
4. Zero gtkada dependency
5. Included in safety_build.gpr
6. TUI callback implementation exists
7. Basic LSP features work (hover, completion, diagnostics)

**Expected: 7 hours of focused work during Phase 5**

**Current: 80% done, correct place to pause**
