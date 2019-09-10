************
System Setup
************

As explained in the introduction, GNAT Studio can currently only be extended by
programming in Ada. This assumes that a number of tools are available on your
system, so that you can recompile your new module.

Most of these external tools and libraries are available from
`http://libre.act-europe.fr <http://libre.act-europe.fr>`_.

*GNAT 3.15 or above*

  GNAT is the GNU Ada Compiler, integrated into the gcc tool chain, and
  developed by **Ada Core Technologies** and **ACT Europe**. GNAT Studio will
  not compile with other Ada compilers than GNAT.

*Gtk+ 2.2.0 or above*

  gtk+ is a C toolkit used for the graphical interface of GNAT Studio. It is
  available on a number of platforms, including most UNIX systems and Windows.
  Available from `http://www.gtk.org <http://www.gtk.org>`_.

*GNAT Studio sources*

  GNAT Studio sources include the corresponding GNAT, GtkAda and GVD sources
  needed to build it. If needed, GNAT, GtkAda and GVD sources can be obtained
  seperately from anonymous cvs access from `http://libre.act-europe.fr
  <http://libre.act-europe.fr>`_

GNAT Studio sources contain an INSTALL file that explains how to recompile
GNAT Studio itself. GNAT Studio knows how to dynamically load a module.
As a result, you do not necessarily need to rebuild GNAT Studio itself to add
new modules, although the dynamic loading hasn't been fully tested yet and might
not work on all platforms.

