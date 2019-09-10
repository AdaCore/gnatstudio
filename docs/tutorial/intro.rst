************
Introduction
************

This document provides a guide through the major capabilities of the GNAT
Programming Studio by working on a code example: sdc, a simple desktop
calculator.

It is important to realize that the features that you are about to experiment
with are available on multiple platforms, using the same user interface and
capabilities, providing a user-friendly environment with a tight integration
between the tools.

Start GNAT Studio in the directory containing the tutorial files, or if the
directory is read-only, copy the :file:`tutorial` directory and its
subdirectories in a local (writable) area, and start GNAT Studio from the
:file:`tutorial` directory, so that GNAT Studio will load the right context.

By default, the tutorial sources can be found under
`<prefix>/share/examples/gnatstudio/tutorial`, where `<prefix>` is the prefix
directory of the GNAT Studio installation.

Alternatively, if you have already started GNAT Studio in another directory,
you can load the project `sdc.gpr` by using the menu `File->Open Project...`
