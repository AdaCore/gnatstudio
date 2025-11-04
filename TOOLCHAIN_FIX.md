# GNAT Toolchain Header Fix for macOS 26

## Problem

The Alire GNAT 15.1.2 toolchain (gnat_native_15.1.2_60748c54) has **permanently broken headers** that prevent C code compilation on macOS 26 (Darwin 25.0.0).

### Symptoms

All C code compilation fails with errors like:
```
error: unknown type name 'FILE'
note: 'FILE' is defined in header '<stdio.h>'
```

Affects:
- gnatcoll_sqlite (shell.c, sqlite3.c)
- gnatcoll_iconv (iconv_support.c)
- gnatcoll_xref (transitive dependency)
- Any Ada package with C bindings

### Root Cause

**GCC's fixincludes system created broken headers during toolchain build:**

1. **When:** Toolchain built May 22, 2025 for macOS 14 SDK
2. **What:** GCC's fixincludes processed headers and created "fixed" versions
3. **Problem:** Generated `include-fixed/stdio.h` uses `FILE` type BEFORE defining it
4. **Impact:** GCC searches `include-fixed/` FIRST, before system headers
5. **Result:** Broken header found first, compilation fails

**Technical details:**
- Toolchain built for: macOS 14 (darwin23.6.0)
- Running on: macOS 26 (darwin25.0.0)
- GCC configured with hardcoded sysroot for MacOSX14.sdk
- fixincludes extracted content in wrong order
- File: `lib/gcc/aarch64-apple-darwin23.6.0/15.0.1/include-fixed/stdio.h`
- Line 78: `#include <_stdio.h>` (defines FILE)
- Line 83: `extern FILE *__stdinp;` (uses FILE) ← ERROR: Used before defined!

## The Fix

**Solution:** Remove the broken `include-fixed/` headers entirely.

### Automated Fix Script

**Run once after installing Alire toolchain:**

```bash
./fix_toolchain.sh
```

This script:
1. Backs up `include-fixed/` to `include-fixed.backup`
2. Deletes all broken headers
3. Creates README marker
4. Forces GCC to use correct macOS 26 SDK headers

### Manual Fix

If you prefer manual steps:

```bash
cd ~/.local/share/alire/toolchains/gnat_native_15.1.2_60748c54
cd lib/gcc/aarch64-apple-darwin23.6.0/15.0.1/

# Backup
cp -r include-fixed include-fixed.backup

# Remove broken headers
rm -rf include-fixed/*

# Marker
echo "Fixed on $(date)" > include-fixed/README
```

### Verification

Test C compilation works:

```bash
echo '#include <stdio.h>
int main() {
    FILE *f = NULL;
    return 0;
}' | alr exec -- gcc -x c - -o /tmp/test
```

**Expected:** Compiles successfully (only deployment version warning)

**If it fails:** The fix didn't work or wasn't applied

## Persistence

### ⚠️  This Fix is TEMPORARY

**Location:** `~/.local/share/alire/toolchains/` (user's home directory)

**Not in git:** System modification, not part of source code

**Will be lost if:**
- Alire updates the gnat_native toolchain
- Toolchain is manually removed and reinstalled
- `alr toolchain --select` installs a fresh copy

**Must be re-applied:**
- After any toolchain reinstall
- By every developer on macOS 26
- When setting up a new development machine

### Checking if Fix is Applied

```bash
ls ~/.local/share/alire/toolchains/gnat_native_15.1.2_60748c54/lib/gcc/aarch64-apple-darwin23.6.0/15.0.1/include-fixed/
```

**If empty except README:** ✅ Fix is applied
**If contains *.h files:** ❌ Fix needed - run `./fix_toolchain.sh`

## Alternative Solutions

### Option 1: Use Different Toolchain (Not Available)

Only one ARM64 macOS GNAT toolchain exists in Alire (15.1.2).

Waiting for AdaCore to release macOS 26-compatible toolchain.

### Option 2: Build GNAT from Source (Complex)

Build custom GNAT with correct fixincludes for macOS 26.

**Effort:** Several hours
**Benefit:** Permanent fix
**Drawback:** Maintenance burden, not easily sharable

### Option 3: Report to Alire/AdaCore (Long-term)

File issue requesting macOS 26 toolchain rebuild.

**Status:** Should be done, but doesn't help immediately

## Why This Matters

Without this fix:
- ❌ gnatcoll_xref won't compile
- ❌ kernel_core, language modules can't build (need xref types)
- ❌ Most GNATstudio core modules blocked
- ❌ TUI development impossible

With this fix:
- ✅ All gnatcoll packages compile
- ✅ Full core module compilation
- ✅ safety_build.gpr validates all modules
- ✅ TUI development unblocked

## Automation

### Can This Be Automated in alire.toml?

**No.** Alire doesn't provide hooks to modify toolchains post-installation.

Toolchains are managed globally (or per-config), not per-project.

### Best Practice

1. Add to project README/INSTALL
2. Include fix script in repo
3. Document in build instructions
4. Remind developers in error messages if possible

## Future

This workaround is needed until:
- AdaCore releases GNAT toolchain built on macOS 26, OR
- GCC fixincludes is updated to handle modern macOS SDK structure, OR
- Alire supports toolchain post-install hooks

## Questions?

See commit 4f35f65428 for the original fix.

Contact: https://github.com/berkeleynerd/gnatstudio/issues
