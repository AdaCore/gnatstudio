# GNATstudio Dependencies

## System Dependencies (macOS)

### Installed via Homebrew

The following packages are required to build GNATstudio on macOS:

```bash
brew install gtk+3 pkg-config pygobject3 py3cairo
```

### Versions Verified

- **GTK+3**: 3.24.51
- **Python**: 3.14.0 (Homebrew) or system Python 3.9+
- **PyGObject3**: 3.54.5
- **PyCairo**: 1.28.0
- **pkg-config**: 2.5.1

### Verification

After installation, verify the dependencies:

```bash
# Verify GTK+3
pkg-config --modversion gtk+-3.0

# Verify Python GTK bindings
python3 -c "import gi; gi.require_version('Gtk', '3.0'); from gi.repository import Gtk; print(f'GTK {Gtk.MAJOR_VERSION}.{Gtk.MINOR_VERSION}.{Gtk.MICRO_VERSION}')"

# Verify PyCairo
python3 -c "import cairo; print(f'PyCairo version: {cairo.version}')"
```

## Ada Dependencies (via Alire)

The following Ada libraries are managed by Alire (see `alire.toml`):

- **gnatcoll**: ^25.0 - GNAT Components Collection (core)
- **xmlada**: ^25.0 - XML/Ada toolkit
- **gtkada**: ^25.0 - Ada bindings for GTK+
- **libgpr**: ^25.0 - GNAT Project Manager library

## Future TUI Dependencies

When converting to TUI (per PLAN.md):

- **malef**: Ada 2022 TUI toolkit (to be cloned from GitHub)
  - Repository: https://github.com/joseaverde/Malef
  - Not yet in Alire index
  - Alternative: **ncursesada** (available in Alire)

## Notes

- This dependency list was established on 2025-11-04
- Python dependencies (PyGObject, PyCairo) will be removed during TUI conversion
- GTK+3 dependencies will be removed during TUI conversion
- System-level dependencies are installed via Homebrew on macOS
- Ada dependencies are managed via Alire package manager
