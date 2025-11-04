# GNATstudio TUI - Build Instructions

## Quick Start

```bash
# Build
alr build

# Run
./test_headless/bin/test_pure_ada
```

## Build System

**Primary build method:** `alr build`

Alire automatically:
- Resolves and fetches dependencies
- Sets up GPR_PROJECT_PATH
- Configures build environment
- Invokes gprbuild with correct parameters

## Current Build Target

**Project:** `test_headless/test_pure_ada.gpr`
**Executable:** `test_pure_ada`
**Build time:** ~5 seconds (clean), <1s (incremental)
**Size:** 2.0 MB

## Dependencies (Managed by Alire)

```
gnatcoll  25.0.0  - GNAT Components Collection
vss       25.0.0  - Virtual String Subsystem  
xmlada    25.0.0  - XML/Ada toolkit
libgpr    25.0.0  - GNAT Project File Library
gnat      15.1.2  - GNAT compiler (toolchain)
```

## Alternative Build Methods

### Direct gprbuild (not recommended)
```bash
# Must set environment manually
alr exec -- gprbuild -P test_headless/test_pure_ada.gpr
```

### Old configure/make (deprecated for TUI)
```bash
# Still exists but not used for TUI build
./configure --prefix=$PWD/install
make
```

## Build Modes

```bash
# Debug (default)
alr build

# Production (optimized)
alr build -- -XBUILD=Production

# With specific parallelism
alr build -- -j8

# Verbose
alr build -- -v
```

## Clean Build

```bash
# Clean all artifacts
alr clean

# Rebuild from scratch
alr clean && alr build
```

## Troubleshooting

### If build fails:
1. Check Alire index is updated: `alr update`
2. Verify dependencies: `alr show --solve`
3. Check for file system issues
4. Try clean rebuild: `alr clean && alr build`

### Common Issues:

**C compilation errors:**
- Cause: Alire GNAT toolchain has broken headers on macOS 26
- Solution: We avoid C dependencies; use pure Ada libraries

**Missing dependencies:**
- Run: `alr update` to refresh index
- Check: `alr show <package>` to verify availability

## Development Workflow

```bash
# Make code changes
# ...

# Incremental build (fast)
alr build

# Test
./test_headless/bin/test_pure_ada

# Clean when needed
alr clean
```

## Next: TUI Development

To build future TUI version:
1. Add `ncursesada` to `alire.toml`
2. Create `tui/tui.gpr`  
3. Update `alire.toml` project-files to point to `tui/tui.gpr`
4. Run `alr build`

Build system is ready for PLAN.MD Phase 2!
