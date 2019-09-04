# GNAT Studio

- [What is GNAT Studio?](#what-is-gps)
- [Building](#building)

## What is GNAT Studio?

GNAT Studio is a lightweight, extensible IDE, intended to develop high-integrity software in **Ada** and **SPARK**, with support for **C** and **C++** as well.

![GPS - Screenshot](/docs/users_guide/gps-main-window.png?raw=true)

## Building

### Requirements

GNAT Studio requires:

- A recent version of [Gtk+](http://www.gtk.org/) (currently using version 3.14)
- An install of Python which includes [PyGObject](https://wiki.gnome.org/action/show/Projects/PyGObject) and [Pycairo](https://cairographics.org/pycairo/)
- An install of [GtkAda](https://github.com/AdaCore/gtkada)
- An install of [GNATcoll](https://github.com/AdaCore/gnatcoll), configured with support for projects and Python scripting (`--enable-project`, `--with-python=...`)

See the `INSTALL` file for details.
