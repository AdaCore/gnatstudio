
import GPS
class Predefined_Hooks:
    """
    This class is not available in GPS itself. It is included in this
    documentation as a way to describe all the predefined hooks that GPS
    exports.

    Each function below describes the name of the hook (as should be used
    as parameter to GPS.Hook constructor), as well as the list of parameters
    that are passed by GPS.

    """

    # activity_checked_hook = 'activity_checked_hook'
    def activity_checked_hook(name):
        """
      Emitted when an activity status has been checked, the last step done
      after the activity has been committed. It is at this point that the
      activity closed status is updated.

      :param str name:

        """

    # after_character_added = 'after_character_added'
    def after_character_added(name,file,char,interactive):
        """
      Emitted when a character has been added in the editor. This hook is also
      called for the backspace key.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.character_added`
      
      .. seealso:: :func:`GPS.Predefined_Hooks.word_added`

      :param str name:
      :param GPS.File file:

        """

    # after_file_changed_detected = 'after_file_changed_detected'
    def after_file_changed_detected(name):
        """
      Emitted when one or more opened file have been changed outside of GPS,
      and GPS needed to resynchronize it. This is called even when the user
      declined to synchronize.

      :param str name:

        """

    # analysis_loading_finsished = 'analysis_loading_finsished'
    def analysis_loading_finsished(name):
        """
      Emitted when all the messages that should be displayed in the Analysis
       Report have been loaded.

      :param str name:

        """

    # annotation_parsed_hook = 'annotation_parsed_hook'
    def annotation_parsed_hook(name):
        """
      Emitted when the last annotation has been parsed

      :param str name:

        """

    # before_exit_action_hook = 'before_exit_action_hook'
    def before_exit_action_hook(name):
        """
      Emitted when GPS is about to exit. If the function returns False,
      the exit is aborted, and you should display a dialog to explain why

      :param str name:
      :return: bool


        """

    # before_file_saved = 'before_file_saved'
    def before_file_saved(name,file):
        """
      Emitted immediately before a file is saved

      :param str name:
      :param GPS.File file:

        """

    # bookmark_added = 'bookmark_added'
    def bookmark_added(name,str):
        """
      Emitted when a new bookmark has been created by the user. The
      parameter is the name of the bookmark

      :param str name:
      :param str str:

        """

    # bookmark_removed = 'bookmark_removed'
    def bookmark_removed(name,str):
        """
      Emitted when a bookmark has been removed by the user. The parameter
      is the name of the bookmark

      :param str name:
      :param str str:

        """

    # buffer_edited = 'buffer_edited'
    def buffer_edited(name,file):
        """
      Emitted after the user has stopped modifying the contents of an editor

      :param str name:
      :param GPS.File file:

        """

    # build_mode_changed = 'build_mode_changed'
    def build_mode_changed(name,str):
        """
      :param str name:
      :param str str:

        """

    # build_server_connected_hook = 'build_server_connected_hook'
    def build_server_connected_hook(name):
        """
      Emitted when GPS connects to the build server in remote mode

      :param str name:

        """

    # character_added = 'character_added'
    def character_added(name,file,char,interactive):
        """
      Emitted when a character is going to be added in the editor. It is also
      called when a character is going to be removed, in which case the last
      parameter is 8 (control-h).
      
      .. seealso:: :func:`GPS.Predefined_Hooks.after_character_added`
      
      .. seealso:: :func:`GPS.Predefined_Hooks.word_added`

      :param str name:
      :param GPS.File file:

        """

    # clipboard_changed = 'clipboard_changed'
    def clipboard_changed(name):
        """
      Emitted when the contents of the clipboard has changed, either
      because the user added a new entry to it (Copy or Cut) or because
      the index of the last paste operation has changed (Paste Previous)

      :param str name:

        """

    # compilation_finished = 'compilation_finished'
    def compilation_finished(name,category,target,mode,shadow,background,status):
        """
      Emitted when a compile operation has finished.
      
      Among the various tasks that GPS connects to this hook are the
      automatic reparsing of all xref information, and the activation of
      the automatic-error fixes. See also the hook "xref_updated"

      :param str name:
      :param str category:
         location or highlighting category that contains the compilation output
      :param str target:
      :param str mode:
      :param int status:

        """

    # compilation_starting = 'compilation_starting'
    def compilation_starting(name,category,quiet,shadow,background,preserve_output):
        """
      Emitted when a compilation operation is about to start.
      
      Among the various tasks that GPS connects to this hook are: check
      whether unsaved editors should be saved (asking the user), and stop the
      background task that parses all xref info. If ``quiet`` is True, no
      visible modification should be done in the MDI, such as raising consoles
      or clearing their content, since the compilation should happen in
      background mode.
      
      Funtions connected to this hook should return False if the compilation
      should not occur for some reason, True if it is OK to start the
      compilation. Typically, the reason to reject a compilation would be
      because the user has explicitly cancelled it through a graphical dialog,
      or because running a background compilation is not suitable at this
      time.
      
      .. code-block:: python
      
         # The following code adds a confirmation dialog to all
         # compilation commands.
         import gps_utils
         @gps_utils.hook("compilation_starting")
         def __compilation_starting(hook, category, quiet, *args):
            if not quiet:
               return MDI.yes_no_dialog("Confirm compilation ?")
            else:
               return True
      
      .. code-block:: python
      
         # If you create a script to execute your own build script, you
         # should always do the following as part of your script. This
         # ensures a better integration in GPS (saving unsaved editors,
         # reloading xref information automatically in the end, raising
         # the GPS console, parsing error messages for automatically
         # fixable errors,...)
      
         if notHook ("compilation_starting").run_until_failure(
              "Builder results", False, False):
            return
      
         # ... spawn your command
         Hook("compilation_finished").run("Builder results")

      :param str name:
      :param str category:
         location or highlighting category that contains the compilation output
      :param bool quiet:
         If False, nothing should be reported to the user unless it is an error
      :param bool shadow:
         Whether the build launched was a Shadow builds, i.e. a secondary build
         launched automatically by GPS after a real build. For instance, when
         multiple toolchains mode is activated, the builds generating xref are
         Shadow builds
      :param bool background:
      :param bool preserve_output:
         Content of Messages view is not cleaned
      :return: bool


        """

    # compute_build_targets = 'compute_build_targets'
    def compute_build_targets(name,str):
        """
      Emitted whenever GPS needs to compute a list of subtargets for a given
      build target. The handler should check whether name is a known build
      target, and if so, return a list of tuples, where each tuple corresponds
      to one target and contains a display name (used in the menus, for
      instance), the name of the target and the full path for the project.
      
      If `str` is not known, it should return an empty list.
      
      The `str` parameter is the name of the target, for instance 'main', 'exec'
      or 'make'.
      
      .. code-block:: python
      
         def compute_targets(hook, name):
            if name == "my_target":
              return [(display_name_1, target_1, ''),
                      (display_name_2, target_2, '')]
            return ""
         GPS.Hook("compute_build_targets").add(compute_targets)

      :param str name:
      :param str str:

        """

    # context_changed = 'context_changed'
    def context_changed(name,context):
        """
      Emitted when the current context changes in GPS, such as when a new
      file or entity is selected, or a window is created

      :param str name:
      :param GPS.Context context:

      :asynchronous 400 (ms)

        """

    # contextual_menu_close = 'contextual_menu_close'
    def contextual_menu_close(name):
        """
      Called just before a contextual menu is destroyed. At this time, the
      value returned by :func:`GPS.contextual_context` is still the one used
      in the hook contextual_menu_open and you can still reference the data
      you stored in the context. This hook is called even if no action was
      selected by the user. However, it is always called before the action is
      executed, since the menu itself is closed first.
      
       .. seealso:: :func:`GPS.Predefined_Hooks.contextual_menu_open`

      :param str name:

        """

    # contextual_menu_open = 'contextual_menu_open'
    def contextual_menu_open(name):
        """
      Called just before a contextual menu is created. It is called before any
      of the filters is evaluated, and can be used to precomputed data shared
      by multiple filters to speed up the computation. Use
      :func:`GPS.contextual_context` to get the context of the contextual menu
      and store precomputed data in it.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.contextual_menu_close`

      :param str name:

        """

    # debugger_breakpoint_added = 'debugger_breakpoint_added'
    def debugger_breakpoint_added(name,debugger,id):
        """
      The breakpoint with ID as parameter is added. The Debugger given in argument
      might actually be set to None when the list of breakpoints is changed
      before the debugger starts.

      :param str name:
      :param GPS.Debugger debugger:
      :param int id:

        """

    # debugger_breakpoint_changed = 'debugger_breakpoint_changed'
    def debugger_breakpoint_changed(name,debugger,id):
        """
      The breakpoint with ID as parameter is changed. The Debugger given in argument
      might actually be set to None when the list of breakpoints is changed
      before the debugger starts.

      :param str name:
      :param GPS.Debugger debugger:
      :param int id:

        """

    # debugger_breakpoint_deleted = 'debugger_breakpoint_deleted'
    def debugger_breakpoint_deleted(name,debugger,id):
        """
      The breakpoint with ID as parameter is deleted. The Debugger given in argument
      might actually be set to None when the list of breakpoints is changed
      before the debugger starts.

      :param str name:
      :param GPS.Debugger debugger:
      :param int id:

        """

    # debugger_breakpoints_changed = 'debugger_breakpoints_changed'
    def debugger_breakpoints_changed(name,debugger):
        """
      The list of breakpoints set in the debugger was reloaded. It might
      not have changed since the last time. The Debugger given in argument
      might actually be set to None when the list of breakpoints is changed
      before the debugger starts.

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_command_action_hook = 'debugger_command_action_hook'
    def debugger_command_action_hook(name,debugger,str):
        """
      Called when the user types a command in the debugger console, or emits
      the command through the GPS.Debugger API. It gives you a chance to
      override the behavior for the command, or even define your own
      commands. Note that you must ensure that any debugger command you
      execute this way does finish with a prompt. The function should return
      the output of your custom command (which is printed in the debugger
      console), or Debugger.Command_Intercepted to indicate the command was
      handled (but this is not output in the console)
      
      .. code-block:: python
      
          ## The following example implements a new gdb command, "hello". When
          ## the user types this command in the console, we end up executing
          ## "print A" instead. This can be used for instance to implement
          ## convenient macros
          def debugger_commands(hook, debugger, command):
             if command == "hello":
                return 'A=' + debugger.send("print A", False)
             else:
                return ""
          GPS.Hook("debugger_command_action_hook").add(debugger_commands)

      :param str name:
      :param GPS.Debugger debugger:
      :param str str:
      :return: str


        """

    # debugger_context_changed = 'debugger_context_changed'
    def debugger_context_changed(name,debugger):
        """
      Emitted when the context of the debuggee has changed, for instance
      after thread switching, frame selection,...

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_executable_changed = 'debugger_executable_changed'
    def debugger_executable_changed(name,debugger):
        """
      Emitted when the executable associated with the debugger has changed,
      for instance via /Debug/Debug/Open File. This is also called initially
      when the executable is given on the command line.

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_location_changed = 'debugger_location_changed'
    def debugger_location_changed(name,debugger):
        """
      Emitted whenever the debugger reports a new current location, for instance
      when it stops at a breakpoint, when the user switches frame or thread,...

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_process_stopped = 'debugger_process_stopped'
    def debugger_process_stopped(name,debugger):
        """
      Called when the debugger has ran and has stopped, for example when
      hitting a breakpoint, or after a next command. If you need to know when
      the debugger just started processing a command, you can connect to the
      debugger_state_changed hook instead. Conceptually, you could connect to
      debugger_state_changed at all times instead of debugger_process_stopped
      and check when the state is now "idle".
      
      .. seealso:: :func:`GPS.Predefined_Hooks.debugger_stated_changed`

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_process_terminated = 'debugger_process_terminated'
    def debugger_process_terminated(name,debugger):
        """
      '
      Emitted when the debugged process has finished

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_question_action_hook = 'debugger_question_action_hook'
    def debugger_question_action_hook(name,debugger,str):
        """
      Emitted just before displaying an interactive dialog, when
      the underlying debugger is asking a question to the user. This hook
      can be used to disable the dialog (and send the reply directly to the
      debugger instead). It should return a non-empty string to pass to the
      debugger if the dialog should not be displayed.
      You cannot send any command to the debugger in this hook.
      The string parameter contains the debugger question.
      
      .. code-block:: python
      
          def gps_question(hook, debugger, str):
             return "1"   ## Always choose choice 1
      
          GPS.Hook("debugger_question_action_hook").add(gps_question)
      
          debug=GPS.Debugger.get()
          debug.send("print &foo")

      :param str name:
      :param GPS.Debugger debugger:
      :param str str:
      :return: str


        """

    # debugger_started = 'debugger_started'
    def debugger_started(name,debugger):
        """
      Emitted after the debugger has been spawned, and when it is possible
      to send commands to it. Better to use debugger_state_changed
      
      .. seealso:: :func:`GPS.Predefined_Hooks.debugger_stated_changed`

      :param str name:
      :param GPS.Debugger debugger:

        """

    # debugger_state_changed = 'debugger_state_changed'
    def debugger_state_changed(name,debugger,new_state):
        """
      Indicates a change in the status of the debugger: ``new_state`` can be
      one of "none" (the debugger is now terminated), "idle" (the debugger is
      now waiting for user input) or "busy" (the debugger is now processing a
      command, and the process is running). As opposed to
      debugger_process_stopped, this hook is called when the command is just
      starting its executing (hence the debugger is busy while this hook is
      called, unless the process immediately stopped).
      
      This hook is also called when internal commands are sent to the
      debugger, and thus much more often than if it was just reacting to user
      input. It is therefore recommended that the callback does the minimal
      amount of work, possibly doing the rest of the work in an idle callback
      to be executed when GPS is no longer busy.
      
      If the new state is "busy", you cannot send additional commands to the
      debugger.
      
      When the state is either "busy" or "idle", GPS.Debugger.command will
      return the command that is about to be executed or the command that was
      just executed and just completed.

      :param str name:
      :param GPS.Debugger debugger:
      :param str new_state:

        """

    # debugger_terminated = 'debugger_terminated'
    def debugger_terminated(name,debugger):
        """
      Emitted just before the connection to the debugger is closed. It
      is still possible to send commands. Better to use debugger_state_changed

      :param str name:
      :param GPS.Debugger debugger:

        """

    # desktop_loaded = 'desktop_loaded'
    def desktop_loaded(name):
        """
      :param str name:

        """

    # diff_action_hook = 'diff_action_hook'
    def diff_action_hook(name,vcs_file,orig_file,new_file,diff_file,title):
        """
      Emitted to request the display of the comparison window

      :param str name:
      :param GPS.File vcs_file:
      :param GPS.File orig_file:
      :param GPS.File new_file:
      :param GPS.File diff_file:
      :param str title: (default: "")
      :return: bool


        """

    # file_changed_detected = 'file_changed_detected'
    def file_changed_detected(name,file):
        """
      Emitted whenever GPS detects that an opened file changed on the
      disk. You can connect to this hook if you want to change the default
      behavior, which is asking if the user wants to reload the file. Your
      function should return 1 if the action is handled by the function, and
      return 0 if the default behavior is desired.
      
      This hook stops propagating as soon as a handler returns True. If you want
      get noticed systematically, use the `after_file_changed_detected` instead.
      
      .. code-block:: python
      
        import GPS
      
        def on_file_changed(hook, file):
            # automatically reload the file without prompting the user
            ed = GPS.EditorBuffer.get(file, force = 1)
            return 1
      
        # install a handler on "file_changed_detected" hook
        GPS.Hook("file_changed_detected").add(on_file_changed)

      :param str name:
      :param GPS.File file:
      :return: bool


        """

    # file_changed_on_disk = 'file_changed_on_disk'
    def file_changed_on_disk(name,file):
        """
      Emitted when some external action has changed the contents of a file on
      the disk, such as a VCS operation. The parameter might be a directory
      instead of a file, indicating that any file in that directory might have
      changed.

      :param str name:
      :param GPS.File file:

        """

    # file_closed = 'file_closed'
    def file_closed(name,file):
        """
      Emitted just before the last editor for a file is closed. You can still
      use :func:`EditorBuffer.get` and :func:`current_view` to access the last
      editor for `file`.

      :param str name:
      :param GPS.File file:

        """

    # file_deleted = 'file_deleted'
    def file_deleted(name,file):
        """
      Emitted whenever GPS detects that a file was deleted on the disk. The
      parameter might be a directory instead of a file, indicating that any
      file within that directory has been deleted.

      :param str name:
      :param GPS.File file:

        """

    # file_deleting = 'file_deleting'
    def file_deleting(name,file):
        """
      +Emitted before GPS delete a file.

      :param str name:
      :param GPS.File file:

        """

    # file_edited = 'file_edited'
    def file_edited(name,file):
        """
      Emitted when a file editor has been opened for a file that was not already
      opened before. Do not confuse with the hook open_file_action, which is
      used to request the opening of a file.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.open_file_action_hook`

      :param str name:
      :param GPS.File file:

        """

    # file_line_action_hook = 'file_line_action_hook'
    def file_line_action_hook(name,identifier,file,every_line,tooltip,info,icon_name):
        """
      Emitted to request the display of new information on the side of the
      editors. You usually will not connect to this hook, but you might want to
      run it yourself to ask GPS to display some information on the side of
      its editors.
      If Info is null or empty, existing line information is removed.
      If the first index in Info is 0, then space is reserved on the side for a
      new column, but no information is added yet. The first item should provide
      info to compute the maximum width of the column (text + icon).
      If the first index is -1, then extra information is added in the status
      bar (not on the side), using the provided Icon_Nam and Tooltip.
      Otherwise, information is added for all the lines with a corresponding
      entry in Info.

      :param str name:
      :param str identifier:
      :param GPS.File file:
      :param bool every_line: (default: True)

        """

    # file_renamed = 'file_renamed'
    def file_renamed(name,file,file2):
        """
      Emitted whenever a GPS action renamed a file on the disk. `file`
      indicates the initial location of the file, while ``renamed`` indicates
      the new location. The parameters might be directories instead of files,
      indicating that the directory has been renamed, and thus any file within
      that directory have their path changed.

      :param str name:
      :param GPS.File file:
      :param GPS.File file2:

        """

    # file_saved = 'file_saved'
    def file_saved(name,file):
        """
      Emitted whenever a file has been saved

      :param str name:
      :param GPS.File file:

        """

    # file_status_changed = 'file_status_changed'
    def file_status_changed(name,file,status):
        """
      Emitted when a file status has changed. The value for the status could
      be one of "UNMODIFIED", "MODIFIED" or "SAVED".

      :param str name:
      :param GPS.File file:
      :param str status:

        """

    # gps_started = 'gps_started'
    def gps_started(name):
        """
      Emitted when GPS is fully loaded and its window is visible to the user.
      You should not do any direct graphical action before this hook has been
      called, so it is recommended that in most cases your start scripts
      connect to this hook.

      :param str name:

        """

    # highlight_range = 'highlight_range'
    def highlight_range(name,phase,file,from_line,to_line):
        """
      Request to highlight range of text in given file.
      Phase 1 is executed on each keystroke and should work fast.
      Phase 2 is executed when semantic information is ready and may use it.

      :param str name:
      :param int phase:
      :param GPS.File file:
      :param int from_line:
      :param int to_line:

        """

    # html_action_hook = 'html_action_hook'
    def html_action_hook(name,url_or_file,enable_navigation,anchor):
        """
      Emitted to request the display of HTML files. It is generally useful
      if you want to open an HTML file and let GPS handle it in the usual
      manner.

      :param str name:
      :param str url_or_file:
      :param bool enable_navigation: (default: True)
      :param str anchor: (default: "")
      :return: bool


        """

    # location_changed = 'location_changed'
    def location_changed(name,file,line,column,project):
        """
      Emitted when the location in the current editor has changed.

      :param str name:
      :param GPS.File file:
      :param int line:
      :param int column:

      :asynchronous 200 (ms)

        """

    # log_parsed_hook = 'log_parsed_hook'
    def log_parsed_hook(name):
        """
      Emitted when the last log has been parsed

      :param str name:

        """

    # marker_added_to_history = 'marker_added_to_history'
    def marker_added_to_history(name,location):
        """
      Emitted when a new marker is added to the history list of previous
      locations, where the user can navigate backwards and forwards.

      :param str name:
      :param GPS.Location location:

        """

    # mdi_child_selected = 'mdi_child_selected'
    def mdi_child_selected(name,child):
        """
      Emitted when the currently focused MDI child has changed in GPS (e.g: when
      switching editors)

      :param str name:
      :param GPS.MDIWindow child:

      :asynchronous 400 (ms)

        """

    # message_selected = 'message_selected'
    def message_selected(name,message):
        """
      :param str name:
      :param GPS.Message message:

        """

    # open_file_action_hook = 'open_file_action_hook'
    def open_file_action_hook(name,file,line,column,column_end,enable_navigation,new_file,force_reload,focus,project,group,initial_position,Areas,Title):
        """
      Emitted when GPS needs to open a file. You can connect to this hook if
      you want to have your own editor open, instead of GPS's internal
      editor. Your function should return 1 if it did open the file or 0 if
      the next function connected to this hook should be called.
      
      The file should be opened directly at `line` and `column`. If
      `column_end` is not 0, the given range should be highlighted if
      possible.  `enable_navigation` is set to True if the new location
      should be added to the history list, so that the user can navigate
      forward and backward across previous locations. `new_file` is set to
      True if a new file should be created when file is not found. If set to
      False, nothing should be done. `force_reload` is set to true if the
      file should be reloaded from the disk, discarding any change the user
      might have done. focus is set to true if the open editor should be given
      the keyboard focus.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.file_edited`
      
      .. code-block:: python
      
          GPS.Hook('open_file_action_hook').run(
                    GPS.File("gps-kernel.ads"),
                    322, # line
                    5,   # column
                    9,   # column_end
                    1,   # enable_navigation
                    1,   # new_file
                    0)   # force_reload

      :param str name:
      :param GPS.File file:
      :param int line: (default: 1)
         If -1, all editors for this file will be closed instead
      :param int column: (default: 1)
      :param int column_end:
      :param bool enable_navigation: (default: True)
      :param bool new_file: (default: True)
      :param bool force_reload:
      :param bool focus: (default: True)
      :param GPS.Project project:
      :return: bool


        """

    # preferences_changed = 'preferences_changed'
    def preferences_changed(name,pref):
        """
      Emitted when the value of some of the preferences changes. Modules should
      refresh themselves dynamically.

      :param str name:

      :asynchronous 400 (ms)

        """

    # project_changed = 'project_changed'
    def project_changed(name):
        """
      Emitted when the project has changed. A new project has been loaded, and
      all previous settings and caches are now obsolete. In the callbacks for
      this hook, the attribute values have not been computed from the project
      yet, and will only return the default values. Connect to the
      project_view_changed hook instead to query the actual values.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.project_view_changed`

      :param str name:

        """

    # project_changing = 'project_changing'
    def project_changing(name,file):
        """
      Emitted just before a new project is loaded

      :param str name:
      :param GPS.File file:

        """

    # project_editor = 'project_editor'
    def project_editor(name):
        """
      Emitted before the :guilabel:`Project Editor` is opened. This allows a
      custom module to perform specific actions before the actual creation of
      this dialog.

      :param str name:

        """

    # project_saved = 'project_saved'
    def project_saved(name,project):
        """
      Emitted when a project is saved to disk. It is called for each
      project in the hierarchy.

      :param str name:
      :param GPS.Project project:

        """

    # project_view_changed = 'project_view_changed'
    def project_view_changed(name):
        """
      Emitted when the project view has been changed, for instance because one
      of the environment variables has changed. This means that the list of
      directories, files or switches might now be different. In the callbacks
      for this hook, you can safely query the new attribute values.

      :param str name:

        """

    # revision_parsed_hook = 'revision_parsed_hook'
    def revision_parsed_hook(name):
        """
      Emitted when the last revision has been parsed

      :param str name:

        """

    # rsync_action_hook = 'rsync_action_hook'
    def rsync_action_hook(name,synchronous,force,to_remote,print_output,print_command,tool_name,host_name,queue_id,file):
        """
      internal use only

      :param str name:
      :param bool synchronous:
      :param bool force:
      :param bool to_remote:
      :param bool print_output:
      :param bool print_command:
      :param str tool_name:
      :param str host_name:
      :param str queue_id:
      :param GPS.File file:
      :return: bool


        """

    # rsync_finished = 'rsync_finished'
    def rsync_finished(name):
        """
      :param str name:

        """

    # search_functions_changed = 'search_functions_changed'
    def search_functions_changed(name):
        """
      Emitted when the list of registered search functions changes.

      :param str name:

        """

    # search_regexps_changed = 'search_regexps_changed'
    def search_regexps_changed(name):
        """
      Emitted when a new regexp has been added to the list of predefined search
      patterns.

      :param str name:

        """

    # search_reset = 'search_reset'
    def search_reset(name):
        """
      Emitted when the current search pattern is reset or changed by the
      user or when the current search is no longer possible because the setup
      of GPS has changed.

      :param str name:

        """

    # semantic_tree_updated = 'semantic_tree_updated'
    def semantic_tree_updated(name,file):
        """
      Emitted when the semantic_tree for a file has been updated.

      :param str name:
      :param GPS.File file:

        """

    # server_config_hook = 'server_config_hook'
    def server_config_hook(name,server,nickname):
        """
      Emitted when a server is assigned to a server operations category.
      
      The `server_type` parameter is the server operations category. It can
      take the values "BUILD_SERVER", "EXECUTION_SERVER" or "DEBUG_SERVER".

      :param str name:
      :param str server:
      :param str nickname:

        """

    # server_list_hook = 'server_list_hook'
    def server_list_hook(name):
        """
      Emitted when the list of configured servers has changed.

      :param str name:

        """

    # source_lines_folded = 'source_lines_folded'
    def source_lines_folded(name,context,line1,line2):
        """
      :param str name:
      :param int line1:
      :param int line2:

        """

    # source_lines_unfolded = 'source_lines_unfolded'
    def source_lines_unfolded(name,context,line1,line2):
        """
      :param str name:
      :param int line1:
      :param int line2:

        """

    # status_parsed_hook = 'status_parsed_hook'
    def status_parsed_hook(name):
        """
      Emitted when the last status has been parsed

      :param str name:

        """

    # stop_macro_action_hook = 'stop_macro_action_hook'
    def stop_macro_action_hook(name):
        """
      You should run this hook to request that the macro currently being
      replayed be stopped. No more events should be processed as part of this
      macro.

      :param str name:

        """

    # task_finished = 'task_finished'
    def task_finished(name):
        """
      Emitted when a background task is finished

      :param str name:

        """

    # task_started = 'task_started'
    def task_started(name):
        """
      Emitted when a new background task is started

      :param str name:

        """

    # variable_changed = 'variable_changed'
    def variable_changed(name):
        """
      Emitted when one of the scenario variables has been renamed, removed or
      when one of its possible values has changed.

      :param str name:

        """

    # vcs_active_changed = 'vcs_active_changed'
    def vcs_active_changed(name):
        """
      Emitted when the active VCS has changed. This is the VCS on which operations
      like commit and log happen.

      :param str name:

        """

    # vcs_file_status_changed = 'vcs_file_status_changed'
    def vcs_file_status_changed(VCS,files,props):
        """
      Emitted when the VCS status of a file has been recomputed. The file might now
      be up to date, staged for commit, locally modified,... It might also have a
      different version number, for file-based systems.
      This hook is only called on actual change of the status, and provides basic
      information on the new status. Check GPS.VCS.file_status to get more
      details.

      :param GPS.VCS VCS:
      :param [GPS.File] files:
      :param int props:

        """

    # vcs_refresh = 'vcs_refresh'
    def vcs_refresh(name,is_file_saved):
        """
      Run this hook to force a refresh of all VCS-related views. They will
      resynchronize their contents from the disk, rather than rely on cached
      information. Set `is_file_saved` parameter to True when the hook is being
      run after saving a file, False otherwise

      :param str name:
      :param bool is_file_saved:

        """

    # word_added = 'word_added'
    def word_added(name,file):
        """
      Emitted when a word has been added in an editor.
      
      .. seealso:: :func:`GPS.Predefined_Hooks.character_added`

      :param str name:
      :param GPS.File file:

        """

    # xref_updated = 'xref_updated'
    def xref_updated(name):
        """
      Emitted when the cross-reference information has been updated.

      :param str name:

        """
GPS.Predefined_Hooks = Predefined_Hooks
