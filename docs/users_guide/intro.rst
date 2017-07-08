**************************************
Welcome to the GNAT Programming Studio
**************************************

GPS is a complete integrated development environment.  It integrates with a
wide range of tools, providing easy access to each. It integrates
especially well with AdaCore's tools but can easily be extended to work
with other tools by writing small plugins in Python.

Here is a summary of the features of the GNAT Programming Studio:

* :ref:`Multiple_Document_Interface`

  GPS uses a multiple document interface, allowing you to organize windows
  the way you want and organize your desktop by floating them to other
  screens or dragging them to any location.  (GPS restores the desktop the
  next time it is restarted.)

* Built-in editor (:ref:`Editing_Files`)

  Fully customizable editor with syntax highlighting, smart completion of text,
  multiple views of the same file, automatic indentation, block-level
  navigation, support for Emacs key bindings, code folding, refactoring, visual
  comparison of files, and alias expansion, among other features.

* Support for compile/build/run cycle (:ref:`Compilation/Build`)

  Any compiler called by a command line can be integrated in GPS, with
  built in support for GNAT, :program:`gcc`, and :program:`make`.  You can
  easily navigate through error messages, and automatic code fixing is
  provided for many common errors.  GPS includes support for
  cross-compilers (running compilers on a different machine than the one on
  which GPS is running).

* Project management (:ref:`Project_Handling`)

  You can use project files (editable either graphically or manually) to
  describe attributes of a project, including the location of sources,
  their naming schemes, and how they should be built.  GPS provides a
  graphical browser to analyze both dependencies between your projects and
  between sources within your projects.

* Integration with various :ref:`Version_Control_System`

  CVS, subversion, :program:`git`, and ClearCase are supported out of the
  box.  You can add support for others by customizing some XML plugins.

* Intelligent :ref:`Source_Navigation`

  By leveraging information provided by the compilers and using its own
  parsers, GPS allows you to find program information such as the
  declaration of entities and their references, that would otherwise be
  hard to locate.  It also provides advanced capabilities such as call
  graphs and UML-like entity browsers.

* Full debugger integration (:ref:`Debugging`)

  GPS fully integrates with :program:`gdb` and provides multiple graphical
  views to monitor the state of your application, including a call stack, a
  visual display for the values of the variables, and a breakpoint editor.

* Integration with code analysis tools (:ref:`Tools`)

  GPS integrates tightly with various command-line tools such as
  :program:`gcov` and GNATcoverage (for the coverage of your code) and
  CodePeer and Spark (to analyze your code). In most cases, it provides
  graphical rendering of their output, often integrated with the editor
  itself so the information is available where and when you need it.

* Fully customizable (:ref:`Customizing_and_Extending_GPS`)

  GPS provides an extensive Python API, allowing you to customize existing
  features or easily develop your own new plugins.  Simpler customization
  can be done through the numerous preferences and local settings.
