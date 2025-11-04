# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

GNATstudio is a sophisticated IDE for Ada/SPARK development, written primarily in Ada with extensive Python plugin support. The codebase consists of 48+ modular components with ~468K lines of code.

## Build Commands

### Prerequisites Setup
```bash
# Ensure dependencies are available
export GPR_PROJECT_PATH=/path/to/gtkada:/path/to/xmlada:$GPR_PROJECT_PATH
export PATH=/path/to/gnat/bin:$PATH
```

### Building
```bash
# Configure (first time only)
./configure --prefix=/path/to/install

# Build with different modes
make BUILD=Debug          # Development build with assertions
make BUILD=Production      # Optimized build
make BUILD=AddressSanitizer  # Memory debugging
make BUILD=Coverage        # Code coverage analysis

# Parallel build (recommended)
make -j8                   # Use 8 cores

# Build specific components
make -C gnatstudio         # Build main IDE only
make -C cli               # Build CLI tools only
make -C testsuite         # Build test drivers only
```

### Clean and Install
```bash
make clean                 # Clean all build artifacts
make install              # Install to configured prefix
```

## Testing

### Run Full Testsuite
```bash
cd testsuite
./run.sh                  # Run all tests with Xvfb
./run.sh --noxvfb        # Run without Xvfb
```

### Run Single Test
```bash
cd testsuite
./run.sh tests/minimal/   # Run specific test
./run.sh tests/Z999-999.xfail/  # Run expected-to-fail test
```

### Run with Valgrind
```bash
cd testsuite
./run.sh --valgrind_memcheck  # Memory check mode
```

### Python Coverage
```bash
cd testsuite
./run.sh --pycov          # Generate Python coverage report
```

## Architecture

### Module Organization

The codebase is organized into independent modules that communicate through well-defined interfaces:

**Core Infrastructure Layer:**
- `kernel/`: Central IDE kernel managing hooks, tasks, preferences, modules, and actions. All modules register with the kernel and communicate through its event system.
- `common/`: Shared utilities split into `core/` (backend logic) and `ui/` (GUI components).
- `vfs/`: Virtual file system abstraction enabling uniform file operations across local/remote filesystems.

**Language Support Layer:**
- `ada_module/`: Ada language support with syntax highlighting, navigation, and refactoring.
- `lal/`: Integration with Libadalang for advanced Ada semantic analysis.
- `lsp_client/`: Language Server Protocol client for modern language support.
- `cpp_module/`: C/C++ language support.

**Development Tools Layer:**
- `builder/`: Build system integration supporting gprbuild, make, and custom build commands.
- `gvd/`: Visual debugger with GDB/DAP integration.
- `code_analysis/`: Static analysis integration (CodePeer, GNATcheck).
- `refactoring/`: Automated code transformation tools.

**UI Components Layer:**
- `widgets/`: Custom GTK+ widgets in Ada for specialized IDE displays.
- `views/`: Various IDE panels (outline, project, messages, etc.).
- `src_editor/`: Source code editor with syntax highlighting and code completion.

### Key Design Patterns

**Hook System:** The kernel provides a publish-subscribe event system. Modules register hooks for events (file_saved, project_changed, etc.) enabling loose coupling between components.

**Module Registration:** Each module has a `Register_Module` procedure called at startup, registering commands, menus, preferences, and hooks with the kernel.

**Python Integration:** Python scripts can access the full IDE API through the `GPS` module, enabling dynamic extension of functionality.

**Language Server Protocol:** Modern language support uses LSP for scalability and language-agnostic design.

### Entry Points

- Main application: `gnatstudio/src/gps-main.adb`
- CLI tools: `cli/src/` (gnatdoc3, gnatstudio_cli)
- Python API: `python/src/` provides GPS module for scripts

### Build System Integration

The project uses GNAT Project files (.gpr) for dependency management:
- `gps_aggregated.gpr`: Top-level aggregate project
- `gnatstudio/gps.gpr`: Main IDE project importing all modules
- `shared.gpr.in`: Common build configuration (generated from configure)

Each module has its own .gpr file declaring dependencies, allowing independent compilation and testing.

## Development Workflow

### Adding a New Feature
1. Identify the appropriate module or create a new one
2. Register with kernel in `Register_Module` procedure
3. Add Python API bindings if user-scriptable
4. Write tests in `testsuite/tests/`

### Debugging
```bash
# Run with debug output
ADA_DEBUG_FILE=./gnatdebug gnatstudio

# Run under GDB
gdb gnatstudio/obj/gnatstudio

# Enable specific traces in gnatdebug file
echo "GPS.KERNEL.MODULES=yes" >> gnatdebug
```

### Working with Python Plugins
- User plugins: `~/.gnatstudio/plug-ins/`
- System plugins: `share/plug-ins/`
- Plugin API: Use `GPS` module for IDE interaction

### Common Module Locations
- Ada language features: `ada_module/`, `lal/`
- Debugging: `gvd/`, `dap/`
- Version control: `vcs2/`
- Project management: `prj_editor/`, `toolchains/`
- Code navigation: `navigation/`, `browsers/`, `vsearch/`