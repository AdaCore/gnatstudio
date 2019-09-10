************************
Hello World walk through
************************

.. highlight:: ada

Creating a new module is best demonstrated by going through the
classical and simple example 'hello world'. This example will be
refined as new extension possibilities are described later on in this
document.

Declaring the module
====================

A module is generally implemented in a separate source file, at this point
an Ada package. The first thing that needs to be done is to create the specs
of this package. Most of the time, a single function has to be exported,
which is called Register_Module by convention. Therefore, we have to create
a new directory to contain the module (we'll call it :file:`hello_world`), at
the same level as other modules like the source editor.

Still by convention, the sources are put in a directory called :file:`src`, and
the object files are kept in a separate directory called :file:`obj`::

  mkdir hello_world
  mkdir hello_world/src
  mkdir hello_world/obj
  

In the source directory, we create the file :file:`hello_world.ads`, which
contains the declaration of the `Register_Module` subprogram::

  with GPS.Kernel;
  package Hello_World is
     procedure Register_Module
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
  end Hello_World;

Before going over the details of the implementation of `Register_Module`,
we have to make sure that the rest of GNAT Studio knows about this module,
and that we know how to compile it

Publicizing your module
=======================

Until GNAT Studio provides dynamic modules, you have to modify the main
subprogram of GNAT Studio to make it aware of your module.

This is done by modifying the file :file:`gps-main.adb`, and adding
two statements in there: a `with` statement that imports
:file:`hello_world`.ads, and a call to `Hello_World.Register_Module`.
See for instance how this is done for the keymanager module.

Compiling your module
=====================

However, after the addition of the two statements in :file:`gps-main.adb`,
the file :file:`hello_world.ads` will not be found automatically by GNAT Studio.
Therefore, you need to create a project file for your new module (we'll call it
:file:`hello_world.gpr`), and add a dependency to it in the root project file
of GNAT Studio (:file:`gps/gps.gpr`), as is currently done for all other
modules.

The project file :file:`hello_world.gpr` is best created by copying the
project file from any other module, for instance the aliases module
(:file:`aliases/aliases.gpr`), and changing the name of the project to
`Hello_World`.

You must also create a set of two Makfiles, which are used to add files other
than Ada, even if your module only uses Ada files.
Once again, this is best done by copying the two Makefiles from the
directory :file:`aliases`, renaming them into :file:`Makefile` and
:file:`Makefile.hello_world`, and replacing the strings `aliases` and
`ALIASES` by resp. `hello_world` and `HELLO_WORLD`.

These steps will be made easier in the near future, but in any case are
relatively straightforward, and only need to be done once per module. The
resulting setup automatically takes into account all sources files that will
be added later on to the module, either C or Ada, and compile them with the
appropriate compiler.

You might also prefer in your first attempt at creating a new module to add
your new files into the :file:`src` directory of an existing module. In this
case, you don't have to create any of the project files or Makefile, nor to
modify the :file:`gps-main.adb` file.

Once the project file has been created, and a dependency added in
:file:`gps.gpr`, you might want to reload the GNAT Studio project in
GNAT Studio, so that the editing of your sources can be done in an
Ada-friendly context.

Registering the module
======================

Back to the source files of your modules. We now need to create a body for
the procedure `Register_Module`. The minimal thing this function has to
do is indicate to the GNAT Studio kernel that a new module is being declared,
and give it a name. If you only do that, there is no direct impact on the rest
of GNAT Studio. However, as we will see during in this guide, having a specific
`Module_Id` is mandatory for some of the advanced feature, so it is
cleaner to always declare one from the start.

This is done by creating the file :file:`hello_world.adb`, with the following
contents::

  with GPS.Kernel.Modules;  use GPS.Kernel, GPS.Kernel.Modules;

  package Hello_World is
     procedure Register_Module
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
     is
        Module : Module_ID;
     begin
        GPS.Kernel.Modules.Register_Module
           (Module, Kernel, Module_Name => "hello_world");
     end Register_Module;

  end Hello_World;
  

At this point, the hello_world module is compilable, only it won't do anything
but be loaded in GNAT Studio.

The following sections will show how new features can be provided to the
rest of GNAT Studio.

