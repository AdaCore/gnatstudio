**************************************
Welcome to the GNAT Programming Studio
**************************************

GPS is a complete integrated development environment that gives access
to a wide range of tools and integrates them smoothly. It integrates
especially well with AdaCore's tools, but can easily be extended to
drive other tools, through small plug-ing written in python.

Here is a rough list of the GNAT Programming Studio features:

* :ref:`Multiple_Document_Interface`

  GPS is based on a multiple document interface, which allows you to
  organize windows the way you want, float them to other screens,
  drag them to other places to reorganize your desktop, and of course
  restore the desktop the next time GPS is restarted.

* Built-in editor (:ref:`Editing_Files`)

  Fully customizable editor with syntax highlighting, smart completion of text,
  multiple views of the same file, automatic indentation, block-level
  navigation, support for Emacs keybindings, code folding, refactoring, visual
  comparison of files, alias expansion,...

* Support for compile/build/run cycle (:ref:`Compilation/Build`)

  Any command line compiler can be integrated in GPS, with built in
  support for GNAT, gcc and make. Error messages are easily navigable,
  and automatic code fixing is provided for a number of typical error
  messages.
  This includes support for cross-compilers, or running compilers on
  separate hosts than the machine on which GPS itself is running.

* Project management (:ref:`Project_Handling`)

  Project files (editable either graphically or manually) are used to
  describe the location of sources, their naming schemes, how they
  should be built,...
  Graphical browsers exist to analyze the dependencies between your
  projects and the sources within your projects.

* Integration with various :ref:`Version_Control_System`

  CVS, subversion, git and clearcase are supported out of the box, but
  others can be added by customizing some XML plug-ins.

* Intelligent :ref:`Source_Navigation`

  By leveraging on information provided by the compilers, or using
  its own parses, GPS makes it possible to find the declaration of
  entities, their references,... It also provides advanced capabilities
  like call graphs, UML-like entity browsers,...

* Full debugger integration (:ref:`Debugging`)

  GPS integrates fully with gdb, and provides multiple graphical views
  to monitor the state of your application, include a call stack, a
  visual display for the values of the variables, a breakpoint editor,...

* Integration with code analysis tools (:ref:`Tools`)

  GPS integrates well with various command-line tools like gcov and
  GNATcoverage (for the coverage of your code), codepeer and Spark
  (to analyze your code),... In a lot of cases, it provides nice
  graphical rendering of their output, often integrated with the editor
  itself so that the information is available where you need it.

* Fully customizable (:ref:`Customizing_and_Extending_GPS`)

  GPS provides an extensive python API which allows you to customize
  existing features, or develop your own new plug-ins easily.
  Simpler customization can be done through one of the numerous
  preferences or local settings.

