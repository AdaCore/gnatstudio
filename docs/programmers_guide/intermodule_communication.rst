*************************
Intermodule communication
*************************

As described above, GNAT Studio is organized into largely independent
modules. For instance, the various views, browsers, help, vcs
support,... are separate modules, that can either be loaded at startup
or not.

When they are not loaded, the correspondings features and menus are not
available to the user.

These modules need to communicate with each other so as to provide the
best possible integration between the tools. There currently exists a
number of ways to send information from one module to
another. However, some of these technics depend on Ada-specific types,
and thus makes it harder to write modules in different languages like
C or Python.

The following communication technics are currently provided:

* Direct calls
  A module can explicitly specify that it depends on another one. This
  is done by changing the project file, and adding the necessary "with"
  statements in the code.  This technics is highly not recommended, and
  should never be used when one of the other technics would do the job,
  since it defeats the module independency.  The only place it is
  currently used at is for direct calls to the Register_* commands.
  Most of the time, these Register_* subprograms are also available through
  XML customization files, and thus limit the direct dependencies between
  modules, while providing greated extensibility to the final user.

* Shell calls
  Each module can register new shell commands for the interactive shell
  window.  Any other module can call these commands. There is no direct
  dependency on the code, although this means that the module that
  provide the command must be loaded before the other module.  This
  technics is used for instance for the codefix module, that needs a
  high degree of integration with the source editor module. It will also
  be used for communicating with Emacs.

* Addition to contextual menus
  A module is free to add entries to the main menu bar or to any
  contextual menus within GNAT Studio.

  Most of the time, a module will decide to add new entries depending on
  what the contextual menu applies to (the current context), although it
  might also decide to do that based on what module is displaying the
  contextual menu. Modules are identified by their name, which can
  easily be tested by other menus.

* Context changes
  Every time a new MDI child is selected, or when a module chooses to
  emit such a signal, a context change is reported via a gtk+ signal. A
  context is an Ada tagged type, created by the currently active
  module. There exists different kinds of contexts, some for files
  (directories and project), others for entities (same as before, but
  with an entity name in addition, other for a location (adding line and
  column),...  New types of contexts can be created by the modules
  without impacting the rest of GNAT Studio. All callbacks must test that the
  context they receive matches what they can handle.

  These contexts are also used for the contextual menus

  A module can choose to emit the signal to report changes to its
  context by emitting the signal. Other modules can they update their
  content accordingly. This is how the switches editor updates the
  project/directory/file it is showing when a new selection is done in
  the project view.

* hooks and action hooks
  Hooks are similar to the usual gtk+ signals.
  Each hook is a named collection of subprograms to be called when the hook is
  executed. Such hooks are executed by various parts of GNAT Studio when
  some actions take place, like reloading the project, loading a file,...

  These are the most powerful way for a module to react to actions taking place
  in other parts of GNAT Studio, and to act appropriately.

  In most cases, all the subprograms in a hook are executed in turn, and thus
  they all get a chance to act.

  However, in some other cases, the subprograms are only executed until one of
  them indicates that it has accomplished a useful action, and that no other
  subprogram from this hook should be called. These are called **action hooks**.
  This is the fundamental mechanism used by GNAT Studio to request for instance
  the edition of a file: the module that wishes to display a file executes the
  hook "open_file_action_hook" with the appropriate argument. At this point, all
  subprograms bound to this hook are executed, until one of them acknowledge that
  it knows how to edit this file (and hopefully opens an editor). Then no other
  subprogram from this hook is called, so that the file is not open multiple
  times.

  This mechanism is used for instance by the module that handles the external
  editors. It is setup so that it binds to the "open_file_action_hook" hook. Any
  time a file needs to be open, the callback from this module is called first.
  If the user has indicated that the external editor should always be used, this
  external editors module opens the appropriate editor, and stops the execution
  of the hook. However, if the user didn't wish to use an external editor, this
  module does nothing, so that the callback from the source editor module is
  called in turn, and can thus open the file itself.

  See :file:`gps-kernel-hooks.ads` for more information on hooks.


