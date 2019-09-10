************
Introduction
************

**Important note**: This document is not ready for release yet.

This document explains how to add your own modules to the GNAT Studio
programming system.

GNAT Studio is a fully open architecture, to which one can add new features
ranging from new menu items to launch external tools to full support for new
languages, including cross-references.

.. index:: adding menus
.. index:: menus
.. index:: toolbar
.. index:: key bindings

Some of these additions can be done solely through the use of text files. These
are for instance adding new key bindings to various parts of GNAT Studio,
for instance in the editor. The end-user can also easily add new menus or
toolbar buttons. See the customization chapters in the GNAT Studio user's guide.

This document will focus on these additions that can only be done
through programming languages.

At this point, GNAT Studio can only be extended by programming in **Ada**.
In addition, it is planned for the near future that extensions in **C** or
**C++** can be done. Work is under way to extend python scripting in
GNAT Studio.

Likewise, adding basic support for new languages will be made easier, and
doable through external text files, requiring no programming. This is not
available for this first release of the GNAT Studio environment.

