# GNATstudio Dependencies

## System Dependencies (macOS)

### Installed via Homebrew

The following packages are required to build GNATstudio on macOS:

```bash
brew install pkg-config
```

### Versions Verified

- **GTK+3**: _not required (GUI removed)_
- **Python**: _not required (scripting removed)_
- **PyGObject3**: _not required_
- **PyCairo**: _not required_
- **pkg-config**: 2.5.1

### Verification

After installation, verify the dependencies:

```bash
pkg-config --modversion libgpr
```

## Ada Dependencies (via Alire)

The following Ada libraries are managed by Alire (see `alire.toml`):

- **gnatcoll**: ^25.0 - GNAT Components Collection (core)
- **xmlada**: ^25.0 - XML/Ada toolkit
- **gtkada**: _removed from the build; kept only for historical context_
- **libgpr**: ^25.0 - GNAT Project Manager library

## Future TUI Dependencies

When converting to TUI (per PLAN.md):

- **malef**: Ada 2022 TUI toolkit (to be cloned from GitHub)
  - Repository: https://github.com/joseaverde/Malef
  - Not yet in Alire index
  - Alternative: **ncursesada** (available in Alire)

## Notes

- This dependency list was established on 2025-11-04
- Python dependencies (PyGObject, PyCairo) have been removed with the GTK
  teardown; no Python runtime is needed for the TUI build.
- GTK+3 dependencies will be removed during TUI conversion
- System-level dependencies are installed via Homebrew on macOS
- Ada dependencies are managed via Alire package manager
