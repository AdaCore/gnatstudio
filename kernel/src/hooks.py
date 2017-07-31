#!/usr/bin/env python
'''
This file describes all hooks exported by GPS. It is used
to generate code, so any change to it requires a rebuilding
of GPS.
'''

class Mapping(object):
    '''
    Describe how to map an Ada type to a python type.
    If 'ada' is set to None, the parameter is ignored on the Ada side.
    If 'python' is set to None, the parameter is ignored in Python.
    %(ada)s is replaced by the name of the actual Ada parameter in 'topython'
    %(idx)d is replaced by the index of the parameter in 'toada'
      In 'toada', there is a local variable K for the kernel.
    'setreturn' is used to store the result of a function and pass it to
      python. The value is in a 'Tmp' variable.
    '''
    def __init__(self, ada, python,
                 topython='%(ada)s',
                 toada='Data.Nth_Arg(%(idx)d)',
                 toada_vars='',
                 toada_init='',
                 setreturn='Data.Set_Return_Value (Tmp);',
                 withs=[],
                 body_withs=[]):
        self.ada = ada
        self.python = python
        self.toada = toada
        self.toada_vars = toada_vars
        self.toada_init = toada_init
        self.topython = topython
        self.withs = withs
        self.body_withs = body_withs
        self.setreturn = setreturn if python is not None else 'null;'


class Hook_Type(object):
    '''Describe a type of hook'''
    def __init__(self, params=[], returns=None, return_default=None,
                 returns_run=-1, descr='', internal_run=False,
                 override_run_from_python=True):
        """
        :param returns_run: the value returned, in Ada, when calling Run.
           By default, this is the same as returns, but could be set to
           None to ignore the returned value.
        :param internal_run: if True, the Run function is generated with a
           special name and should not be called directly by the code.
        :param override_run_from_python: if False, Run_From_Python procedure
           will not be overriden
        :param debounce: set up the timeout for interact asynchronously,
           can be set if no any values are returned
        """
        self.params = params
        self.descr = descr.strip()
        self.internal_run = internal_run
        self.returns = returns
        self.return_default = return_default
        self.returns_run = returns if returns_run == -1 else returns_run
        self.override_run_from_python = override_run_from_python
        if self.descr:
            self.descr = '\n   --  ' + self.descr.replace('\n', '\n   --  ')


class Debounce_Hook_Type(Hook_Type):
    '''Describe a debounce type of hook'''
    def __init__(self, params=[], descr='', internal_run=False,
                 override_run_from_python=True, debounce=None):
        """
        :param debounce: set up the timeout for interact asynchronously
        """
        Hook_Type.__init__(self,params,None,None,-1,descr,internal_run,override_run_from_python)
        self.debounce = debounce


class Param(object):
    '''The description of one parameter for a hook type'''
    def __init__(self, name, type, default=None, descr='', inpython=True, asynch_key=None):
        self.name = name
        self.type = type
        self.descr = descr
        self.default = default
        self.inpython = inpython
        self.asynch_key = asynch_key

    def show_in_ada(self):
        'Whether to show on the Ada side'
        return types[self.type].ada is not None

    def show_in_python(self):
        'Whether to show on the Python side'
        return self.inpython and types[self.type].python is not None

    def is_assign(self):
        'Whether to reassign parameters value for asynch. call'
        if types[self.type].ada is not None:
            if self.asynch_key is None:
                return True

        return False


class Hook(object):
    '''The description of a specific hook'''
    def __init__(self, name, type, descr=''):
        self.name = name
        self.type = type
        self.raw_descr = descr
        self.descr = descr.strip()
        if self.descr:
            self.descr = '\n   --  ' + \
                self.descr.replace('\n', '\n   --  ') + '\n'


types = {
    '__hookname__': Mapping(None, 'str', topython='Self.Name.all'),
    'Task': Mapping(
        ada='String',
        python='GPS.Task',
        topython='Task_Manager.Shell.Get_Or_Create_Instance (Data, %(ada)s)',
        toada='Get_Data (Data.Nth_Arg (%(idx)d), ' +
            'K.Scripts.New_Class ("Task"))',
        body_withs=['Task_Manager.Shell']),

    'Context': Mapping(
        ada='GPS.Kernel.Selection_Context',
        python='GPS.Context',
        topython='Create_Context (Get_Script (Data), %(ada)s)',
        toada='Get_Context (Data.Nth_Arg (%(idx)d))'),

    'String': Mapping(ada='String', python='str'),
    'Integer': Mapping(ada='Integer', python='int'),
    'Boolean': Mapping(ada='Boolean', python='bool'),
    'Float': Mapping(ada='Float', python='float'),
    'Character': Mapping(
        ada='Glib.Gunichar',
        python=None),

    'File_Status': Mapping(
        ada='GPS.Kernel.File_Status',
        python='str',
        topython='File_Status\'Image (%(ada)s)',
        toada='File_Status\'Value (Data.Nth_Arg (%(idx)d))'),

    'Visible_Column': Mapping(
        ada='GNATCOLL.Xref.Visible_Column',
        python='int',
        topython='Integer (%(ada)s)',
        toada='GNATCOLL.Xref.Visible_Column'
            ' (Integer\'(Data.Nth_Arg(%(idx)d)))',
        withs=['GNATCOLL.Xref']),

    'Any_Type': Mapping(
        ada='GNATCOLL.Any_Types.Any_Type',
        python=None,
        withs=['GNATCOLL.Any_Types']),

    'Child_Group': Mapping(
        ada='Gtkada.MDI.Child_Group',
        python='String',
        toada='Gtkada.MDI.Child_Group\'Value (Data.Nth_Arg (%(idx)d))',
        topython='%(ada)s\'Img',
        withs=['Gtkada.MDI']),

    'Child_Position': Mapping(
        ada='Gtkada.MDI.Child_Position',
        python='String',
        toada='Gtkada.MDI.Child_Position\'Value (Data.Nth_Arg (%(idx)d))',
        topython='%(ada)s\'Img',
        withs=['Gtkada.MDI']),

    'Allowed_Areas': Mapping(
        ada='Gtkada.MDI.Allowed_Areas',
        python='String',
        toada='Gtkada.MDI.Allowed_Areas\'Value (Data.Nth_Arg (%(idx)d))',
        topython='%(ada)s\'Img',
        withs=['Gtkada.MDI']),

    'Preference': Mapping(
        ada='Default_Preferences.Preference',
        python=None,  # Not sent to python for backward compatibility
        toada='null',
        withs=['Default_Preferences']),

    'File': Mapping(
        ada='GNATCOLL.VFS.Virtual_File',
        python='GPS.File',
        topython='Create_File (Data.Get_Script, %(ada)s)',
        toada='GPS.Kernel.Scripts.Get_Data (Data.Nth_Arg (%(idx)d, '
            'Get_File_Class (K), Allow_Null => True))',
        withs=['GNATCOLL.VFS']),

    'FileSet': Mapping(
        ada='GPS.Kernel.File_Sets.Set',
        python='[GPS.File]',
        topython='Ada_To_Python_File_Dict (Data.Get_Script, %(ada)s)',
        toada_vars='Tmp_%(idx)d : File_Sets.Set;',
        toada_init='Python_To_Ada_File_Dict (Tmp_%(idx)d, Data, %(idx)d);',
        toada='Tmp_%(idx)d',
        withs=['GNATCOLL.VFS']),

    'VCS_Engine': Mapping(
        ada='not null access GPS.VCS.Abstract_VCS_Engine\'Class',
        python='GPS.VCS',
        topython='GPS.VCS.Create_VCS_Instance (Data.Get_Script, %(ada)s)',
        toada='GPS.VCS.Get_VCS (Data.Nth_Arg (%(idx)d, Allow_Null => False))',
        withs=['GPS.VCS']),
                          
    'VCS_File_Properties': Mapping(
        ada='GPS.VCS.VCS_File_Properties',
        python='int',
        topython='Integer (%(ada)s.Status)',
        toada='GPS.VCS.VCS_File_Properties\'' +
            '(Status => GPS.VCS.VCS_File_Status ' +
              '(Integer\'(Data.Nth_Arg (%(idx)d))), others => <>)',
        withs=['GPS.VCS']),
         
    'Project': Mapping(
        ada='GNATCOLL.Projects.Project_Type',
        python='GPS.Project',
        toada='Get_Data (Data, %(idx)d)',
        topython='Create_Project (Data.Get_Script, %(ada)s)',
        withs=['GNATCOLL.Projects']),

    'Line_Info_Data': Mapping(
        ada='access GPS.Editors.Line_Information.Line_Information_Array',
        python=None,  # Not passed to python
        withs=['GPS.Editors.Line_Information']),

    'Message': Mapping(
        ada='GPS.Kernel.Messages.Message_Access',
        python='GPS.Message',
        topython='GPS.Kernel.Messages.Shell.Create_Message_Instance'
            ' (Get_Script (Data), %(ada)s)',
        toada='GPS.Kernel.Messages.Shell.Get_Data (Data.Nth_Arg (%(idx)d))',
        body_withs=['GPS.Kernel.Messages.Shell']),

    'Location': Mapping(
        ada='Location_Marker',
        python='GPS.Location',
        topython='GPS.Markers.To_String (%(ada)s)',
        toada='GPS.Markers.No_Marker'),

    'Distant_Server': Mapping(
        ada='Remote.Distant_Server_Type',
        python='str',
        toada='Remote.Distant_Server_Type\'Value (String\'(Data.Nth_Arg ' +
            '(%(idx)d)))',
        topython='Remote.Distant_Server_Type\'Image (%(ada)s)',
        withs=['Remote']),

    'Debugger': Mapping(
        ada='access GPS.Debuggers.Base_Visual_Debugger\'Class',
        python='GPS.Debugger',
        topython='GPS.Kernel.Scripts.Get_Or_Create_Instance ' +
            '(Data.Get_Script, %(ada)s)',
        toada='GPS.Debuggers.Base_Visual_Debugger_Access ' +
            '(GNATCOLL.Scripts.Gtkada.Get_Data'
            ' (Data.Nth_Arg (%(idx)d, K.Scripts.New_Class ("Debugger"))))',
        withs=['Glib.Object', 'GPS.Debuggers'],
        body_withs=['GNATCOLL.Scripts.Gtkada']),

    'Debugger_State': Mapping(
        ada='GPS.Debuggers.Debugger_State',
        python='str',
        topython='GPS.Debuggers.To_String (%(ada)s)',
        toada='GPS.Debuggers.From_String (Data.Nth_Arg (%(idx)d))',
        withs=['GPS.Debuggers']),
}

# The following describe hook types (the various hook families with
# their list of parameters and return values). A reference to a 'type'
# is a reference to an entry in the 'types' dict above.

hook_types = {
    'simple_hooks': Hook_Type(
        [Param('name', '__hookname__')]),
    'string_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('str', 'String')]),
    'project_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('project', 'Project')]),
    'file_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File')]),
    'file2_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File'),
         Param('file2', 'File')]),
    'file_return_boolean_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File')],
        returns='Boolean',
        return_default='False'),  # stop when one function returns True
    'task_hooks': Hook_Type(
        [Param('queue_id', 'Task')],
        descr='''
The hooks that report information about a running
background task or process'''),

    'file_location_hooks': Debounce_Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File', asynch_key=True),
         Param('line', 'Integer'),
         Param('column', 'Integer'),
         Param('project', 'Project', inpython=False,
               default='GNATCOLL.Projects.No_Project')],
        debounce=200),

    'string_return_any_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('str', 'String')],
        returns='Any_Type',
        return_default='GNATCOLL.Any_Types.Empty_Any_Type'),

    'file_status_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File'),
         Param('status', 'File_Status')]),

    'return_boolean_hooks': Hook_Type(
        [Param('name', '__hookname__')],
        returns='Boolean',
        return_default='True'),  # Stops when one returns False

    'diff_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('vcs_file', 'File'), # ??? From_Callback_Data_Diff used to
                                    # test Is_Cygin_Path and format it
         Param('orig_file', 'File'),
         Param('new_file', 'File'),
         Param('diff_file', 'File'),
         Param('title', 'String', default='""')],
        returns='Boolean',
        return_default='False'), # stops when one returns True

    'context_hooks': Debounce_Hook_Type(
        [Param('name', '__hookname__'),
         Param('context', 'Context')],
        debounce=400,
        descr='Hooks that take a context as parameter'),

    'two_lines_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('context', 'Context', inpython=False, default='No_Context'),
         Param('line1', 'Integer'),
         Param('line2', 'Integer')]),

    'preferences_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('pref', 'Preference', default='null')]),

    'open_file_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('file', 'File'),
         Param('line', 'Integer', default=1, descr='''
If -1, all editors for this file will be closed instead'''),
         Param('column', 'Visible_Column', default=1),
         Param('column_end', 'Visible_Column', default=0),
         Param('enable_navigation', 'Boolean', default=True),
         Param('new_file', 'Boolean', default=True),
         Param('force_reload', 'Boolean', default=False),
         Param('focus', 'Boolean', default=True),
         Param('project', 'Project'),
         Param('group', 'Child_Group', default='Gtkada.MDI.Group_Default',
               inpython=False),
         Param('initial_position', 'Child_Position', inpython=False,
               default='Gtkada.MDI.Position_Automatic'),
         Param('Areas', 'Allowed_Areas', inpython=False,
               default='Gtkada.MDI.Central_Only'),
         Param('Title', 'String', default='""', inpython=False)
        ],
        returns='Boolean',
        return_default='False',  # Stops when one returns True
        returns_run=None),        # Ignore return value for Run

    # If file is set to No_File, this hook applies to all open_files.
    # ??? We used to emit the hook for all open_files, one by one, but it is
    # more efficient to run it once only
    'line_info_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('identifier', 'String'),
         Param('file', 'File'),
         Param('every_line', 'Boolean', default=True),
         Param('tooltip', 'String', default='""', inpython=False),
         Param('info', 'Line_Info_Data', inpython=False,
               default='null'),
         Param('icon_name', 'String', default='""', inpython=False)],
        descr='Info parameter must be Unchecked_Freed by the caller'),

    'rsync_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('synchronous', 'Boolean'),
         Param('force', 'Boolean'),
         Param('to_remote', 'Boolean'),
         Param('print_output', 'Boolean'),
         Param('print_command', 'Boolean'),
         Param('tool_name', 'String'),
         Param('host_name', 'String'),
         Param('queue_id', 'String'),
         Param('file', 'File')],
        returns='Boolean',
        return_default='False'),

    'compilation_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('category', 'String', descr='''
location or highlighting category that contains the compilation output'''),
         Param('quiet', 'Boolean', descr='''
If False, nothing should be reported to the user unless it is an error'''),
         Param('shadow', 'Boolean', descr='''
Whether the build launched was a Shadow builds, i.e. a secondary build
launched automatically by GPS after a real build. For instance, when
multiple toolchains mode is activated, the builds generating xref are
Shadow builds'''),
         Param('background', 'Boolean')],
        returns='Boolean',
        return_default='True'), # Stops when one returns False

    'compilation_finished_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('category', 'String', descr='''
location or highlighting category that contains the compilation output'''),
         Param('target', 'String'),
         Param('mode', 'String'),
         Param('shadow', 'Boolean', default='False', descr='''
Whether the build launched was a Shadow builds, i.e. a secondary build
launched automatically by GPS after a real build. For instance, when
multiple toolchains mode is activated, the builds generating xref are
Shadow builds''', inpython=False),
         Param('background', 'Boolean', default='False', inpython=False),
         Param('status', 'Integer')]),

    'character_hooks': Hook_Type(
        [Param('name',      '__hookname__'),
         Param('file',      'File'),
         Param('char',      'Character', default=0, inpython=False),
         Param('interactive', 'Boolean', default='True', inpython=False)]),

    'marker_hooks': Hook_Type(
        [Param('name',      '__hookname__'),
         Param('location',  'Location')]),

    'html_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('url_or_file', 'String'),
         Param('enable_navigation', 'Boolean', default=True),
         Param('anchor', 'String', default='""')],
        returns='Boolean',
        return_default='False', # Stop when one function returns True
        returns_run=None),      # Ignore return value for Run

    'message_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('message', 'Message')]),

    'server_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('server', 'Distant_Server'),
         Param('nickname', 'String')]),

    'debugger_hooks': Hook_Type(
        [Param('name', '__hookname__'),
         Param('debugger', 'Debugger')]),

    'debugger_states_hooks': Hook_Type(
        [Param('name',      '__hookname__'),
         Param('debugger',  'Debugger'),
         Param('new_state', 'Debugger_State')]),

    'debugger_string_hooks': Hook_Type(
        [Param('name',      '__hookname__'),
         Param('debugger',  'Debugger'),
         Param('str',       'String')],
        returns='String',
        return_default='""'),  # Stop when one returns non-empty string

    'vcs_file_status_hooks': Hook_Type(
        [Param('VCS',       'VCS_Engine'),
         Param('files',     'FileSet'),
         Param('props',     'VCS_File_Properties')]),       
              
    'vcs_hooks': Hook_Type(
        [Param('VCS',       'VCS_Engine')]),          
}

# The following describe all specific hooks. They all belong to one
# of the families described above

hooks = [
    Hook('activity_checked_hook', 'simple_hooks',
         descr='''
Emitted when an activity status has been checked, the last step done
after the activity has been committed. It is at this point that the
activity closed status is updated.'''),

    Hook('after_character_added', 'character_hooks', descr='''
Emitted when a character has been added in the editor. This hook is also
called for the backspace key.\n
.. seealso:: :func:`GPS.Predefined_Hooks.character_added`\n
.. seealso:: :func:`GPS.Predefined_Hooks.word_added`'''),

    Hook('annotation_parsed_hook', 'simple_hooks',
         descr='Emitted when the last annotation has been parsed'),

    Hook('before_exit_action_hook', 'return_boolean_hooks', descr='''
Emitted when GPS is about to exit. If the function returns False,
the exit is aborted, and you should display a dialog to explain why'''),

    Hook('before_file_saved', 'file_hooks', descr='''
Emitted immediately before a file is saved'''),

    Hook('bookmark_added', 'string_hooks', descr='''
Emitted when a new bookmark has been created by the user. The
parameter is the name of the bookmark'''),

    Hook('bookmark_removed', 'string_hooks', descr='''
Emitted when a bookmark has been removed by the user. The parameter
is the name of the bookmark'''),

    Hook('buffer_edited', 'file_hooks', descr='''
Emitted after the user has stopped modifying the contents of an editor'''),

    Hook('build_mode_changed', 'string_hooks'),

    Hook('build_server_connected_hook', 'simple_hooks', descr='''
Emitted when GPS connects to the build server in remote mode'''),

    Hook('character_added', 'character_hooks', descr='''
Emitted when a character is going to be added in the editor. It is also
called when a character is going to be removed, in which case the last
parameter is 8 (control-h).\n
.. seealso:: :func:`GPS.Predefined_Hooks.after_character_added`\n
.. seealso:: :func:`GPS.Predefined_Hooks.word_added`'''),

    Hook('clipboard_changed', 'simple_hooks', descr='''
Emitted when the contents of the clipboard has changed, either
because the user added a new entry to it (Copy or Cut) or because
the index of the last paste operation has changed (Paste Previous)'''),

    Hook('compilation_starting', 'compilation_hooks', descr='''
Emitted when a compilation operation is about to start.\n
Among the various tasks that GPS connects to this hook are: check
whether unsaved editors should be saved (asking the user), and stop the
background task that parses all xref info. If ``quiet`` is True, no
visible modification should be done in the MDI, such as raising consoles
or clearing their content, since the compilation should happen in
background mode.\n
Funtions connected to this hook should return False if the compilation
should not occur for some reason, True if it is OK to start the
compilation. Typically, the reason to reject a compilation would be
because the user has explicitly cancelled it through a graphical dialog,
or because running a background compilation is not suitable at this
time.\n
.. code-block:: python\n
   # The following code adds a confirmation dialog to all
   # compilation commands.
   import gps_utils
   @gps_utils.hook("compilation_starting")
   def __compilation_starting(hook, category, quiet, *args):
      if not quiet:
         return MDI.yes_no_dialog("Confirm compilation ?")
      else:
         return True

.. code-block:: python\n
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
   Hook("compilation_finished").run("Builder results")'''),

    Hook('compilation_finished', 'compilation_finished_hooks', descr='''
Emitted when a compile operation has finished.\n
Among the various tasks that GPS connects to this hook are the
automatic reparsing of all xref information, and the activation of
the automatic-error fixes. See also the hook "xref_updated"'''),

    Hook('compute_build_targets', 'string_return_any_hooks', descr='''
Emitted whenever GPS needs to compute a list of subtargets for a given
build target. The handler should check whether name is a known build
target, and if so, return a list of tuples, where each tuple corresponds
to one target and contains a display name (used in the menus, for
instance), the name of the target and the full path for the project.\n
If `str` is not known, it should return an empty list.\n
The `str` parameter is the name of the target, for instance 'main', 'exec'
or 'make'.\n
.. code-block:: python\n
   def compute_targets(hook, name):
      if name == "my_target":
        return [(display_name_1, target_1, ''),
                (display_name_2, target_2, '')]
      return ""
   GPS.Hook("compute_build_targets").add(compute_targets) '''),

    Hook('context_changed', 'context_hooks', descr='''
Emitted when the current context changes in GPS, such as when a new
file or entity is selected, or a window is created'''),

    Hook('contextual_menu_close', 'simple_hooks', descr='''
Called just before a contextual menu is destroyed. At this time, the
value returned by :func:`GPS.contextual_context` is still the one used
in the hook contextual_menu_open and you can still reference the data
you stored in the context. This hook is called even if no action was
selected by the user. However, it is always called before the action is
executed, since the menu itself is closed first.\n
 .. seealso:: :func:`GPS.Predefined_Hooks.contextual_menu_open`'''),

    Hook('contextual_menu_open', 'simple_hooks', descr='''
Called just before a contextual menu is created. It is called before any
of the filters is evaluated, and can be used to precomputed data shared
by multiple filters to speed up the computation. Use
:func:`GPS.contextual_context` to get the context of the contextual menu
and store precomputed data in it.\n
.. seealso:: :func:`GPS.Predefined_Hooks.contextual_menu_close`'''),

    Hook('debugger_breakpoints_changed', 'debugger_hooks', descr='''
The list of breakpoints set in the debugger was reloaded. It might
not have changed since the last time. The Debugger given in argument
might actually be set to None when the list of breakpoints is changed
before the debugger starts.'''),

    Hook('debugger_command_action_hook', 'debugger_string_hooks',
         descr='''
Called when the user types a command in the debugger console, or emits
the command through the GPS.Debugger API. It gives you a chance to
override the behavior for the command, or even define your own
commands. Note that you must ensure that any debugger command you
execute this way does finish with a prompt. The function should return
the output of your custom command (which is printed in the debugger
console), or Debugger.Command_Intercepted to indicate the command was
handled (but this is not output in the console)\n
.. code-block:: python\n
    ## The following example implements a new gdb command, "hello". When
    ## the user types this command in the console, we end up executing
    ## "print A" instead. This can be used for instance to implement
    ## convenient macros
    def debugger_commands(hook, debugger, command):
       if command == "hello":
          return 'A=' + debugger.send("print A", False)
       else:
          return ""
    GPS.Hook("debugger_command_action_hook").add(debugger_commands)'''),

    Hook('debugger_context_changed', 'debugger_hooks', descr='''
Emitted when the context of the debuggee has changed, for instance
after thread switching, frame selection,...'''),

    Hook('debugger_executable_changed', 'debugger_hooks', descr='''
Emitted when the executable associated with the debugger has changed,
for instance via /Debug/Debug/Open File. This is also called initially
when the executable is given on the command line.'''),

    Hook('debugger_process_stopped', 'debugger_hooks', descr='''
Called when the debugger has ran and has stopped, for example when
hitting a breakpoint, or after a next command. If you need to know when
the debugger just started processing a command, you can connect to the
debugger_state_changed hook instead. Conceptually, you could connect to
debugger_state_changed at all times instead of debugger_process_stopped
and check when the state is now "idle".\n
.. seealso:: :func:`GPS.Predefined_Hooks.debugger_stated_changed`'''),

    Hook('debugger_process_terminated', 'debugger_hooks', descr=''''
Emitted when the debugged process has finished'''),

    Hook('debugger_question_action_hook', 'debugger_string_hooks', descr='''
Emitted just before displaying an interactive dialog, when
the underlying debugger is asking a question to the user. This hook
can be used to disable the dialog (and send the reply directly to the
debugger instead). It should return a non-empty string to pass to the
debugger if the dialog should not be displayed.
You cannot send any command to the debugger in this hook.
The string parameter contains the debugger question.\n
.. code-block:: python\n
    def gps_question(hook, debugger, str):
       return "1"   ## Always choose choice 1

    GPS.Hook("debugger_question_action_hook").add(gps_question)

    debug=GPS.Debugger.get()
    debug.send("print &foo")'''),

    Hook('debugger_started', 'debugger_hooks', descr='''
Emitted after the debugger has been spawned, and when it is possible
to send commands to it. Better to use debugger_state_changed\n
.. seealso:: :func:`GPS.Predefined_Hooks.debugger_stated_changed`'''),

    Hook('debugger_state_changed', 'debugger_states_hooks',
         descr='''
Indicates a change in the status of the debugger: ``new_state`` can be
one of "none" (the debugger is now terminated), "idle" (the debugger is
now waiting for user input) or "busy" (the debugger is now processing a
command, and the process is running). As opposed to
debugger_process_stopped, this hook is called when the command is just
starting its executing (hence the debugger is busy while this hook is
called, unless the process immediately stopped).\n
This hook is also called when internal commands are sent to the
debugger, and thus much more often than if it was just reacting to user
input. It is therefore recommended that the callback does the minimal
amount of work, possibly doing the rest of the work in an idle callback
to be executed when GPS is no longer busy.\n
If the new state is "busy", you cannot send additional commands to the
debugger.\n
When the state is either "busy" or "idle", GPS.Debugger.command will
return the command that is about to be executed or the command that was
just executed and just completed.'''),

    Hook('debugger_terminated', 'debugger_hooks', descr='''
Emitted just before the connection to the debugger is closed. It
is still possible to send commands. Better to use debugger_state_changed'''),

    Hook('debugger_location_changed', 'debugger_hooks', descr='''
Emitted whenever the debugger reports a new current location, for instance
when it stops at a breakpoint, when the user switches frame or thread,...'''),

    Hook('desktop_loaded', 'simple_hooks'),

    Hook('diff_action_hook', 'diff_hooks', descr='''
Emitted to request the display of the comparison window'''),

    Hook('file_changed_detected', 'file_return_boolean_hooks', descr='''
Emitted whenever GPS detects that an opened file changed on the
disk. You can connect to this hook if you want to change the default
behavior, which is asking if the user wants to reload the file. Your
function should return 1 if the action is handled by the function, and
return 0 if the default behavior is desired.

This hook stops propagating as soon as a handler returns True. If you want
get noticed systematically, use the `after_file_changed_detected` instead.

.. code-block:: python\n
  import GPS

  def on_file_changed(hook, file):
      # automatically reload the file without prompting the user
      ed = GPS.EditorBuffer.get(file, force = 1)
      return 1

  # install a handler on "file_changed_detected" hook
  GPS.Hook("file_changed_detected").add(on_file_changed)'''),

    Hook('after_file_changed_detected', 'simple_hooks', descr='''
Emitted when one or more opened file have been changed outside of GPS,
and GPS needed to resynchronize it. This is called even when the user
declined to synchronize.'''),

    # ??? Can be removed when we get rid of VCS1
    Hook('file_changed_on_disk', 'file_hooks', descr='''
Emitted when some external action has changed the contents of a file on
the disk, such as a VCS operation. The parameter might be a directory
instead of a file, indicating that any file in that directory might have
changed.'''),

    Hook('file_closed', 'file_hooks', descr='''
Emitted just before the last editor for a file is closed. You can still
use :func:`EditorBuffer.get` and :func:`current_view` to access the last
editor for `file`.'''),

    Hook('file_deleting', 'file_hooks', descr='''
+Emitted before GPS delete a file.'''),

    Hook('file_deleted', 'file_hooks', descr='''
Emitted whenever GPS detects that a file was deleted on the disk. The
parameter might be a directory instead of a file, indicating that any
file within that directory has been deleted.'''),

    Hook('file_edited', 'file_hooks', descr='''
Emitted when a file editor has been opened for a file that was not already
opened before. Do not confuse with the hook open_file_action, which is
used to request the opening of a file.\n
.. seealso:: :func:`GPS.Predefined_Hooks.open_file_action_hook`'''),

    Hook('file_line_action_hook', 'line_info_hooks', descr='''
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
entry in Info.'''),

    Hook('file_renamed', 'file2_hooks', descr='''
Emitted whenever a GPS action renamed a file on the disk. `file`
indicates the initial location of the file, while ``renamed`` indicates
the new location. The parameters might be directories instead of files,
indicating that the directory has been renamed, and thus any file within
that directory have their path changed.'''),

    Hook('file_saved', 'file_hooks', descr='''
Emitted whenever a file has been saved'''),

    Hook('file_status_changed', 'file_status_hooks', descr='''
Emitted when a file status has changed. The value for the status could
be one of "UNMODIFIED", "MODIFIED" or "SAVED".'''),

    Hook('gps_started', 'simple_hooks', descr='''
Emitted when GPS is fully loaded and its window is visible to the user.
You should not do any direct graphical action before this hook has been
called, so it is recommended that in most cases your start scripts
connect to this hook.'''),

    Hook('html_action_hook', 'html_hooks', descr='''
Emitted to request the display of HTML files. It is generally useful
if you want to open an HTML file and let GPS handle it in the usual
manner.'''),

    Hook('location_changed', 'file_location_hooks', descr='''
Emitted when the location in the current editor has changed, and the
cursor has stopped moving.'''),

    Hook('log_parsed_hook', 'simple_hooks',
         descr='Emitted when the last log has been parsed'),

    Hook('marker_added_to_history', 'marker_hooks', descr='''
Emitted when a new marker is added to the history list of previous
locations, where the user can navigate backwards and forwards.'''),

    Hook('message_selected', 'message_hooks'),

    Hook('open_file_action_hook', 'open_file_hooks', descr='''
Emitted when GPS needs to open a file. You can connect to this hook if
you want to have your own editor open, instead of GPS's internal
editor. Your function should return 1 if it did open the file or 0 if
the next function connected to this hook should be called.\n
The file should be opened directly at `line` and `column`. If
`column_end` is not 0, the given range should be highlighted if
possible.  `enable_navigation` is set to True if the new location
should be added to the history list, so that the user can navigate
forward and backward across previous locations. `new_file` is set to
True if a new file should be created when file is not found. If set to
False, nothing should be done. `force_reload` is set to true if the
file should be reloaded from the disk, discarding any change the user
might have done. focus is set to true if the open editor should be given
the keyboard focus.\n
.. seealso:: :func:`GPS.Predefined_Hooks.file_edited`\n
.. code-block:: python\n
    GPS.Hook('open_file_action_hook').run(
              GPS.File("gps-kernel.ads"),
              322, # line
              5,   # column
              9,   # column_end
              1,   # enable_navigation
              1,   # new_file
              0)   # force_reload'''),

    Hook('preferences_changed',
        # ??? Should not be emitted if Kernel.Preferences.Is_Frozen
        'preferences_hooks', descr='''
Emitted when the value of some of the preferences changes. Modules should
refresh themselves dynamically.'''),

    Hook('project_changed', 'simple_hooks', descr='''
Emitted when the project has changed. A new project has been loaded, and
all previous settings and caches are now obsolete. In the callbacks for
this hook, the attribute values have not been computed from the project
yet, and will only return the default values. Connect to the
project_view_changed hook instead to query the actual values.\n
.. seealso:: :func:`GPS.Predefined_Hooks.project_view_changed`'''),

    Hook('project_changing', 'file_hooks', descr='''
Emitted just before a new project is loaded'''),

    Hook('project_editor', 'simple_hooks', descr='''
Emitted before the :guilabel:`Project Editor` is opened. This allows a
custom module to perform specific actions before the actual creation of
this dialog.'''),

    Hook('project_saved', 'project_hooks', descr='''
Emitted when a project is saved to disk. It is called for each
project in the hierarchy.'''),

    Hook('project_view_changed', 'simple_hooks', descr='''
Emitted when the project view has been changed, for instance because one
of the environment variables has changed. This means that the list of
directories, files or switches might now be different. In the callbacks
for this hook, you can safely query the new attribute values.'''),

    Hook('revision_parsed_hook', 'simple_hooks',
         descr='Emitted when the last revision has been parsed'),

    Hook('rsync_action_hook', 'rsync_hooks', descr='internal use only'),

    Hook('rsync_finished', 'simple_hooks'),

    Hook('search_functions_changed', 'simple_hooks', descr='''
Emitted when the list of registered search functions changes.'''),

    Hook('search_regexps_changed', 'simple_hooks', descr='''
Emitted when a new regexp has been added to the list of predefined search
patterns.'''),

    Hook('search_reset', 'simple_hooks', descr='''
Emitted when the current search pattern is reset or changed by the
user or when the current search is no longer possible because the setup
of GPS has changed.'''),

    Hook('server_config_hook', 'server_hooks', descr='''
Emitted when a server is assigned to a server operations category.\n
The `server_type` parameter is the server operations category. It can
take the values "BUILD_SERVER", "EXECUTION_SERVER" or "DEBUG_SERVER".'''),

    Hook('server_list_hook', 'simple_hooks', descr='''
Emitted when the list of configured servers has changed.'''),

    Hook('status_parsed_hook', 'simple_hooks',
         descr='Emitted when the last status has been parsed'),

    Hook('stop_macro_action_hook', 'simple_hooks', descr='''
You should run this hook to request that the macro currently being
replayed be stopped. No more events should be processed as part of this
macro.'''),

    Hook('source_lines_folded', 'two_lines_hooks'),

    Hook('source_lines_unfolded', 'two_lines_hooks'),

    Hook('task_started', 'simple_hooks', descr='''
Emitted when a new background task is started'''),

    Hook('variable_changed', 'simple_hooks', descr='''
Emitted when one of the scenario variables has been renamed, removed or
when one of its possible values has changed.'''),

    Hook('word_added', 'file_hooks', descr='''
Emitted when a word has been added in an editor.\n
.. seealso:: :func:`GPS.Predefined_Hooks.character_added`'''),

    Hook('xref_updated', 'simple_hooks', descr='''
Emitted when the cross-reference information has been updated.'''),

    Hook('semantic_tree_updated', 'file_hooks', descr='''
Emitted when the semantic_tree for a file has been updated.'''),
         
    Hook('vcs_file_status_changed', 'vcs_file_status_hooks', descr='''
Emitted when the VCS status of a file has been recomputed. The file might now
be up to date, staged for commit, locally modified,... It might also have a
different version number, for file-based systems.
This hook is only called on actual change of the status, and provides basic
information on the new status. Check GPS.VCS.file_status to get more
details.'''),
         
    Hook('vcs_active_changed', 'simple_hooks', descr='''
Emitted when the active VCS has changed. This is the VCS on which operations
like commit and log happen.'''),
         
    Hook('vcs_refresh', 'simple_hooks', descr='''
Run this hook to force a refresh of all VCS-related views. They will
resynchronize their contents from the disk, rather than rely on cached
information'''),
         
]

#########################################################################
# Code below
#########################################################################


def generate():
    """
    Generate Ada code for the hooks
    """

    withs = {'with %s;' % n
             for t in hook_types.values()
             for p in t.params
             for n in types[p.type].withs}
    withs.update({'with %s;' % n
                  for t in hook_types.values()  if t.returns is not None
                  for n in types[t.returns].withs})
    withs = '\n'.join(sorted(withs))

    f = open('../generated/gps-kernel-hooks.ads', 'wb')
    f.write('''--  Automatically generated from hooks.py
pragma Style_Checks (Off);
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
%(withs)s
package GPS.Kernel.Hooks is

   procedure Register_Hooks
      (Kernel : not null access Kernel_Handle_Record'Class);
   --  Make all hooks available to python

   function Name (Self : not null access Hook_Function'Class) return String
      with Inline => True;
   function Name (Self : Hook_Types'Class) return String with Inline => True;
   --  Return the name for Self. When the hook has not been registered,
   --  it has no Name so we display the type.

   ---------------------------
   -- File_Line_Action_Hook --
   ---------------------------
   --  Wrappers around "file_line_action_hook"

   procedure Create_Line_Information_Column
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Info       : GPS.Editors.Line_Information.Line_Information_Record :=
         GPS.Editors.Line_Information.Empty_Line_Information;
      Every_Line : Boolean := True);
   --  Request the creation of a column on the side of some editors.
   --  Info is used to determine the size of the column: it can contain either
   --  an image or some markup. If Empty_Line_Information is given, the column
   --  will have the default size needed to contain one icon.

   procedure Remove_Line_Information_Column
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String);
   --  Remove the column identified by Identifier for the editors of File.
   --  If File is empty, then the column will be removed for all open files.

   procedure Add_Line_Information
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Info       : GPS.Editors.Line_Information.Line_Information_Array;
      Tooltip    : String := "";
      Icon_Name  : String := "");
   --  Add line information to File.
   --  The range of Info must correspond to the range of line numbers
   --  that are to be modified. If the range is -1 .. -1, the info is added to
   --  the status line of the editors (and tooltip is then used when hovering
   --  that label).
   --  Info will be freed by the editor.
   --  Icon_Name is the name of a stock icon to display

   ---------------
   -- File_Sets --
   ---------------

   procedure Python_To_Ada_File_Dict
      (Files   : out File_Sets.Set;
       Data    : Callback_Data'Class;
       Idx     : Natural);
   --  Stores the Idx-th parameter of Data in Files.

   function Ada_To_Python_File_Dict
      (Script  : not null access Scripting_Language_Record'Class;
       Files   : File_Sets.Set) return List_Instance;
   --  Convert Files to a python list

   procedure Unregister_Debounce_Timeouts;
   -- remove all registered timeouts

   -----------
   -- Hooks --
   -----------

''' % {'withs': withs})

    withs = {'with %s;' % n
             for t in hook_types.values()
             for p in t.params
             for n in types[p.type].body_withs}
    withs.update({'with %s;' % n
                  for t in hook_types.values()  if t.returns is not None
                  for n in types[t.returns].body_withs})
    withs = '\n'.join(sorted(withs))

    b = open('../generated/gps-kernel-hooks.adb', 'wb')
    b.write('''--  Automatically generated from hooks.py
pragma Style_Checks (Off);
pragma Warnings (Off, "comparison with True is redundant");
pragma Warnings (Off, "constant * is not referenced");
with Ada.Tags;                  use Ada.Tags;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Scripts.Hooks;  use GPS.Kernel.Scripts.Hooks;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
%(withs)s
package body GPS.Kernel.Hooks is
   Me : constant Trace_Handle := Create ("HOOKS", GNATCOLL.Traces.Off);
   use type GNATCOLL.Any_Types.Any_Type;

   function Name (Self : Hook_Types'Class) return String is
   begin
      if Self.Name /= null then
         return Self.Name.all;
      else
         return "unregistered " & Self.Type_Name;
      end if;
   end Name;

   function Name (Self : not null access Hook_Function'Class) return String is
   begin
      if Self.all in Python_Hook_Function'Class then
         return Get_Name (Python_Hook_Function (Self.all).Func);
      else
         return External_Tag (Self'Tag);
      end if;
   end Name;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Info       : GPS.Editors.Line_Information.Line_Information_Record :=
         GPS.Editors.Line_Information.Empty_Line_Information;
      Every_Line : Boolean := True)
   is
      Params : aliased GPS.Editors.Line_Information.Line_Information_Array :=
         (0 .. 0 => Info);
   begin
      if File /= GNATCOLL.VFS.No_File then
         File_Line_Action_Hook.Run
            (Kernel       => Kernel,
             File         => File,
             Identifier   => Identifier,
             Info         => Params'Access,
             Every_Line   => Every_Line);
      else
         for F of Kernel.Open_Files loop
            File_Line_Action_Hook.Run
               (Kernel       => Kernel,
                File         => File,
                Identifier   => Identifier,
                Info         => Params'Access,
                Every_Line   => Every_Line);
         end loop;
      end if;
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String)
   is
   begin
      if Kernel.Is_In_Destruction then
         null;
      elsif File /= GNATCOLL.VFS.No_File then
         File_Line_Action_Hook.Run
            (Kernel       => Kernel,
             File         => File,
             Identifier   => Identifier,
             Info         => null);
      else
         for F of Kernel.Open_Files loop
            File_Line_Action_Hook.Run
               (Kernel       => Kernel,
                File         => File,
                Identifier   => Identifier,
                Info         => null);
         end loop;
      end if;
   end Remove_Line_Information_Column;

   --------------------------
   -- Add_Line_Information --
   --------------------------

   procedure Add_Line_Information
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Identifier : String;
      Info       : GPS.Editors.Line_Information.Line_Information_Array;
      Tooltip    : String := "";
      Icon_Name  : String := "") is
   begin
      if File /= GNATCOLL.VFS.No_File then
         File_Line_Action_Hook.Run
            (Kernel       => Kernel,
             File         => File,
             Identifier   => Identifier,
             Info         => Info'Unrestricted_Access,
             Tooltip      => Tooltip,
             Icon_Name    => Icon_Name);
      else
         for F of Kernel.Open_Files loop
            File_Line_Action_Hook.Run
               (Kernel       => Kernel,
                File         => File,
                Identifier   => Identifier,
                Info         => Info'Unrestricted_Access,
                Tooltip      => Tooltip,
                Icon_Name    => Icon_Name);
         end loop;
      end if;
   end Add_Line_Information;

   -----------------------------
   -- Python_To_Ada_File_Dict --
   -----------------------------

   procedure Python_To_Ada_File_Dict
      (Files   : out File_Sets.Set;
       Data    : Callback_Data'Class;
       Idx     : Natural)
   is
      pragma Unreferenced (Files, Data, Idx);
   begin
      raise Program_Error
         with "Can't run vcs_file_status_changed from python";
   end Python_To_Ada_File_Dict;

   -----------------------------
   -- Ada_To_Python_File_Dict --
   -----------------------------

   function Ada_To_Python_File_Dict
      (Script  : not null access Scripting_Language_Record'Class;
       Files   : File_Sets.Set) return List_Instance
   is
      List : List_Instance'Class := New_List (Script);
      Idx  : Positive := 1;
   begin
      for F of Files loop
          Set_Nth_Arg (List, Idx, Create_File (Script, F));
          Idx := Idx + 1;
      end loop;
      return List;
   end Ada_To_Python_File_Dict;
''' % {'withs': withs})

    for type_name in sorted(hook_types.keys()):
        t = hook_types[type_name]
        params = [';\n       %s : %s%s' % (
                        p.name.title(),
                        types[p.type].ada,
                        ' := %s' % p.default if p.default is not None else '')
                  for p in t.params if p.show_in_ada()]

        plist = [', %s' % p.name.title()
                 for p in t.params if p.show_in_ada()]

        python_params = [p for p in t.params if p.show_in_python()]
        pset = ['''
                  Data.Set_Nth_Arg
                     (%d, %s);''' % (
                      index + 1,
                      types[p.type].topython % {
                          'ada': p.name.title()})
                for (index, p) in enumerate(python_params)]

        python_and_ada_params = [
            p for p in t.params if p.show_in_ada() and p.show_in_python()]
        p_in_run = [',\n             %s => %s' % (
            p.name.title(),
            types[p.type].toada % {'idx': idx + 2})  # '+1 to ignore Self'
            for (idx, p) in enumerate(python_and_ada_params)]

        param_descr = '\n'.join(
            '   --  %s : %s' % (p.name.title(),
                                p.descr.strip().replace('\n', '\n   --    '))
            for p in t.params if p.descr)
        if param_descr:
            param_descr = '\n' + param_descr

        subst = {
            'name': type_name.title(),
            'basename': type_name,
            'descr': t.descr,
            'param_descr': param_descr,
            'pcount': len([p for p in t.params if p.show_in_python()]),
            'plist': ''.join(plist),
            'pset': ''.join(pset),
            'p_in_run': ''.join(p_in_run),
            'returndefault': t.return_default,
            'toada_vars': '\n'.join(
                types[p.type].toada_vars % {'idx': idx + 2}
                for (idx, p) in enumerate(python_and_ada_params)
                if types[p.type].toada_vars),
            'toada_init': '      \n'.join(
                types[p.type].toada_init % {'idx': idx + 2}
                for (idx, p) in enumerate(python_and_ada_params)
                if types[p.type].toada_init),
            'run_name': 'Run' if not t.internal_run else 'Run_Internal',
            'params': ''.join(params)}

        if isinstance(t, Debounce_Hook_Type):
            asynch_plist = [', Data.%s' % p.name.title()
                     for p in t.params if p.show_in_ada()]

            compare_keys = ['Data.%s = %s' % (
                            p.name.title(),
                            p.name.title())
                      for p in t.params if p.asynch_key]

            compare = ['%s%s' % (
                            ' and then ' if idx > 0 else '',
                            p)
                      for idx, p in enumerate(compare_keys)]

            assign = ['               Data.%s := %s;\n' % (
                            p.name.title(),
                            p.name.title())
                      for p in t.params if p.is_assign()]

            subst['asynch_plist'] = ''.join(asynch_plist)
            subst['debounce'] = t.debounce
            subst['assign'] = ''.join(assign)

            if len(compare) > 0:
               subst['compare'] = 'if ' + ''.join (compare) + ' then'
               subst['end_compare'] = 'end if;'
            else:
               subst['compare'] = ''
               subst['end_compare'] = ''

        # Settings for the callback function

        if t.returns is None:
            subst['func_proc_or_func'] = 'procedure'
            subst['func_returns'] = ''
            subst['ada_call'] = '''
               %(name)s_Function'Class (F.all).Execute
                  (Kernel%(plist)s);''' % subst
        else:
            subst['func_proc_or_func'] = 'function'
            subst['func_returns'] = ' return %s' % types[t.returns].ada
            subst['func_return_type'] = types[t.returns].ada
            if t.returns_run:
                subst['tmp_return'] = ' Tmp'
            else:
                subst['tmp_return'] = ''
            subst['ada_call'] = '''
               declare
                  Tmp : constant %(func_return_type)s := %(name)s_Function'Class
                     (F.all).Execute (Kernel%(plist)s);
               begin
                  if Tmp /= %(returndefault)s then
                     return%(tmp_return)s;
                  end if;
               end;''' % subst

        # Settings for the Run function

        if t.returns_run:
            subst['returntype'] = types[t.returns_run].ada
            subst['run_proc_or_func'] = 'function'
            subst['run_exit'] = ''
            subst['returns_run'] = ' return %s' % types[t.returns_run].ada
            subst['returns_run_type'] = types[t.returns_run].ada
            subst['run_body'] = '''
                  declare
                     Tmp : constant %(returns_run_type)s := F2.Execute (Data);
                  begin
                     Free (Data);
                     if Tmp /= %(returndefault)s then
                        return Tmp;
                     end if;
                  end;''' % subst
            subst['returns_body'] = '\n      return %s;' % t.return_default
            subst['run_from_python_body'] = types[t.returns_run].setreturn
            subst['run_from_python_var'] = '''
         Tmp : constant %(returntype)s := Self.Run
            (Kernel => K%(p_in_run)s);''' % subst

        # Settings for the Run procedure

        else:
            subst['run_proc_or_func'] = 'procedure'
            subst['returns_run'] = ''
            subst['run_exit'] = ''

            if t.returns is None:
                subst['returns_body'] = ''
                subst['run_body'] = '''
                         declare
                            Tmp : constant Boolean := F2.Execute (Data);
                            pragma Unreferenced (Tmp);
                         begin
                            Free (Data);
                         end;'''
            else:
                subst['returns_body'] = '''
      if Active (Me) then
         Trace (Me, "Default handling for " & Name (Self));
      end if;''' % subst
                subst['run_body'] = '''
                         declare
                            Tmp : constant Boolean := F2.Execute (Data);
                         begin
                            Free (Data);
                            if Tmp /= %(returndefault)s then
                               return;
                            end if;
                         end;''' % subst

            subst['run_from_python_var'] = ''
            subst['run_from_python_body'] = '''Self.Run   --  not dispatching
         (Kernel   => K%(p_in_run)s);''' % subst

        # Output specs

        hooks.sort(key=lambda h: h.name)

        f.write('''
   --------------
   -- %(name)s --
   --------------%(descr)s

   type %(name)s_Function is abstract new Hook_Function
      with null record;

   %(func_proc_or_func)s Execute
      (Self   : %(name)s_Function;
       Kernel : not null access Kernel_Handle_Record'Class%(params)s)%(func_returns)s is abstract;
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           f.write('''
   type %(name)s is new Debounce_Hook_Types with null record;
''' % subst)
        else:
           f.write('''
   type %(name)s is new Hook_Types with null record;
''' % subst)

        f.write('''
   overriding function Type_Name (Self : %(name)s) return String
      is ("%(basename)s");

   procedure Add
      (Self  : in out %(name)s;
       Obj   : not null access %(name)s_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null);
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           f.write('''
   procedure Add_Debounce
      (Self  : in out %(name)s;
       Obj   : not null access %(name)s_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null);
''' % subst)

        f.write('''
   %(run_proc_or_func)s %(run_name)s
      (Self   : in out %(name)s;
       Kernel : not null access Kernel_Handle_Record'Class%(params)s)%(returns_run)s;
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           f.write('''
   procedure Force_Debounce
      (Self    : in out %(name)s;
       Kernel  : not null access Kernel_Handle_Record'Class%(params)s);
''' % subst)

        if t.override_run_from_python:
            f.write('''
   overriding procedure Run_From_Python
      (Self : in out %(name)s; Data : in out Callback_Data'Class);''' % subst)

        f.write('''%(param_descr)s
''' % subst)

        # Output body

        b.write('''
   ---------
   -- Add --
   ---------

   procedure Add
      (Self  : in out %(name)s;
       Obj   : not null access %(name)s_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null) is
   begin
      Self.Add_Hook_Func (Obj, Last, Watch);
   end Add;
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           b.write('''
   ------------------
   -- Add_Debounce --
   ------------------

   procedure Add_Debounce
      (Self  : in out %(name)s;
       Obj   : not null access %(name)s_Function'Class;
       Last  : Boolean := True;
       Watch : access Glib.Object.GObject_Record'Class := null) is
   begin
      Self.Add_Debounce_Hook_Func (Obj, Last, Watch);
   end Add_Debounce;

   ----------
   -- Call --
   ----------

   procedure Call
      (Self   : in out %(name)s;
       Funcs  : Hook_Func_Lists.List;
       Kernel : not null access Kernel_Handle_Record'Class%(params)s)
   is
      List : array (1 .. Natural (Funcs.Length)) of
        access Hook_Function'Class;
      Last : Natural := 0;
   begin
      --  One of the issues here is that running a hook could add or remove
      --  a function from Self.Funcs. We use an explicit copy to compensate

      for F of Funcs loop
         Last := Last + 1;
         List (Last) := F.Func;
         List (Last).Refcount := List (Last).Refcount + 1;
      end loop;

      for F of List (1 .. Last) loop
         begin
            if F.Refcount = 1 then
                --  Skip already deleted hooks
                null;
            elsif F.all in Python_Hook_Function'Class then
               declare
                  F2 : constant Subprogram_Type :=
                     Python_Hook_Function (F.all).Func;
                  Data : Callback_Data'Class :=
                     F2.Get_Script.Create (Arguments_Count => %(pcount)s);
               begin%(pset)s%(run_body)s
               end;
            else%(ada_call)s
            end if;
         exception
            when E : others =>
               Trace (Me, E, "While running "
                  & Name (Self) & ":" & Name (F) & ASCII.LF);
         end;
      end loop;

      for F of List (1 .. Last) loop
         if F.Refcount = 1 then
            Remove_Hook_Func (Self, F);
         end if;
      end loop;
   end Call;

   type %(name)s_Access is access all %(name)s;

   type %(name)s_Params is new Hook_Function_Params with record
      Id : Glib.Main.G_Source_Id := 0;
      Hook : %(name)s_Access;
      Kernel : not null access Kernel_Handle_Record'Class%(params)s;
   end record;

   type %(name)s_Params_Access is
     access all %(name)s_Params;

   %(name)s_Timeout_Value : constant Guint := %(debounce)s;

   package %(name)s_Timeout is
     new Glib.Main.Generic_Sources (%(name)s_Params_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (%(name)s_Params, %(name)s_Params_Access);

   -------------------------
   -- On_%(name)s_Timeout --
   -------------------------

   function On_%(name)s_Timeout
     (Data : %(name)s_Params_Access) return Boolean
   is
      use Hook_Func_Params_Lists;
      use type Glib.Main.G_Source_Id;

      C : Hook_Func_Params_Lists.Cursor := Data.Hook.Asynch_Data.First;
      D : %(name)s_Params_Access;
   begin
      while Has_Element (C) loop
         D := %(name)s_Params_Access (Element (C));
         if D.Id = Data.Id then
            Delete (Data.Hook.Asynch_Data, C);
            exit;
         else
            D := null;
         end if;

         Next (C);
      end loop;

      Call (Data.Hook.all, Data.Hook.Asynch_Funcs, Data.Kernel%(asynch_plist)s);
      Free (D);

      return False;

   exception
      when E : others =>
         Trace (Me, E, "On_%(name)s_Timeout " & Name (Data.Hook.all));
         Free (D);

         return False;
   end On_%(name)s_Timeout;
''' % subst)

        b.write('''
   ---------
   -- %(run_name)s --
   ---------

   %(run_proc_or_func)s %(run_name)s
      (Self   : in out %(name)s;
       Kernel : not null access Kernel_Handle_Record'Class%(params)s)%(returns_run)s
   is
      use Hook_Func_Lists;
      Block_Me : constant Block_Trace_Handle :=
         Create (Me, (if Active (Me) then Name (Self) else ""));
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           b.write('''
   begin
      Call (Self, Self.Funcs, Kernel%(plist)s);

      if Self.Asynch_Funcs.Is_Empty then
         return;
      end if;

      for D of Self.Asynch_Data loop
         declare
            Data : constant %(name)s_Params_Access :=
              %(name)s_Params_Access (D);
         begin
            %(compare)s
               Glib.Main.Remove (Data.Id);
%(assign)s
               Data.Id := %(name)s_Timeout.Timeout_Add
                 (%(name)s_Timeout_Value, On_%(name)s_Timeout'Access, Data);
               return;
            %(end_compare)s
         end;
      end loop;

      declare
         Data : constant %(name)s_Params_Access :=
           new %(name)s_Params'
             ((Glib.Main.No_Source_Id, Self'Unchecked_Access, Kernel%(plist)s));
      begin
         Data.Id := %(name)s_Timeout.Timeout_Add
           (%(name)s_Timeout_Value, On_%(name)s_Timeout'Access, Data);
         Self.Asynch_Data.Append (Hook_Function_Params_Access (Data));
      end;
''' % subst)
        else:
           b.write('''
      List : array (1 .. Natural (Self.Funcs.Length)) of
        access Hook_Function'Class;
      Last : Natural := 0;
   begin
      --  One of the issues here is that running a hook could add or remove
      --  a function from Self.Funcs. We use an explicit copy to compensate

      for J of Self.Funcs loop
         Last := Last + 1;
         List (Last) := J.Func;
         List (Last).Refcount := List (Last).Refcount + 1;
      end loop;

      for F of List (1 .. Last) loop
         begin
            if F.Refcount = 1 then
                --  Skip already deleted hooks
                null;
            elsif F.all in Python_Hook_Function'Class then
               declare
                  F2 : constant Subprogram_Type :=
                     Python_Hook_Function (F.all).Func;
                  Data : Callback_Data'Class :=
                     F2.Get_Script.Create (Arguments_Count => %(pcount)s);
               begin%(pset)s%(run_body)s
               end;
            else%(ada_call)s
            end if;
         exception
            when E : others =>
               Trace (Me, E, "While running "
                  & Name (Self) & ":" & Name (F) & ASCII.LF);
         end;
      end loop;

      for J of List (1 .. Last) loop
         if J.Refcount = 1 then
            Remove_Hook_Func (Self, J);
         end if;
      end loop;
''' % subst)

        b.write('''%(run_exit)s%(returns_body)s
   end %(run_name)s;
''' % subst)

        if isinstance(t, Debounce_Hook_Type):
           b.write('''
   --------------------
   -- Force_Debounce --
   --------------------

   procedure Force_Debounce
      (Self    : in out %(name)s;
       Kernel  : not null access Kernel_Handle_Record'Class%(params)s)
   is
      use Hook_Func_Lists;
      Block_Me : constant Block_Trace_Handle :=
         Create (Me, (if Active (Me) then Name (Self) else ""));

   begin
      if Self.Asynch_Data.Is_Empty then
         return;
      end if;

      for D of Self.Asynch_Data loop
         declare
            Data : %(name)s_Params_Access :=
              %(name)s_Params_Access (D);
         begin
            Glib.Main.Remove (Data.Id);
            Free (Data);
         end;
      end loop;
      Self.Asynch_Data.Clear;

      Call (Self, Self.Funcs, Kernel%(plist)s);
      Call (Self, Self.Asynch_Funcs, Kernel%(plist)s);
   end Force_Debounce;
''' % subst)

        if t.override_run_from_python:
            b.write('''
   ---------------------
   -- Run_From_Python --
   ---------------------

   overriding procedure Run_From_Python
      (Self : in out %(name)s; Data : in out Callback_Data'Class)
   is
      K : constant Kernel_Handle := Get_Kernel (Data);
   begin
      declare
         %(run_from_python_var)s%(toada_vars)s
      begin
         %(toada_init)s
         %(run_from_python_body)s
      end;
   exception
      when E : others =>
         Trace (Me, E, " while running " & Name (Self));
   end Run_From_Python;
''' % subst)

        # Output list of hooks

        f.write('\n')
        for hook in hooks:
            if hook.type == type_name:
                f.write('''   N_%(name)s : aliased constant String := "%(base)s";
   %(varname)s : aliased %(type)s;%(descr)s
''' % {'name': hook.name.title(),
       'varname': '%s_Hook' % hook.name.title()
           if not hook.name.endswith('_hook')
           else hook.name.title(),
       'base': hook.name,
       'type': type_name.title(),
       'descr': hook.descr})

    f.write('end GPS.Kernel.Hooks;\n')
    f.close()

    b.write('''
   ----------------------------------
   -- Unregister_Debounce_Timeouts --
   ----------------------------------

   procedure Unregister_Debounce_Timeouts is
   begin
''')
    for h in hooks:
        if isinstance(hook_types[h.type], Debounce_Hook_Type):
           b.write('''
      for D of %(name)s.Asynch_Data loop
         declare
            Data : constant %(type)s_Params_Access :=
              %(type)s_Params_Access (D);
         begin
            Glib.Main.Remove (Data.Id);
         end;
      end loop;
      %(name)s.Asynch_Data.Clear;
''' % {'type': h.type.title(),
       'name': '%s_Hook' % h.name.title()
           if not h.name.endswith('_hook')
           else h.name.title()})

    b.write('''
   end Unregister_Debounce_Timeouts;

   --------------------
   -- Register_Hooks --
   --------------------

   procedure Register_Hooks
      (Kernel : not null access Kernel_Handle_Record'Class)
   is
   begin
      Trace (Me, "Register predefined hooks");
''')
    for h in hooks:
        varname = ('%s_Hook' % h.name.title()
           if not h.name.endswith('_hook')
           else h.name.title())
        b.write('      %s.Name := N_%s\'Access;\n' % (varname, h.name.title()))
        b.write('      %s.Register (Kernel);\n' % varname)
    b.write('''   end Register_Hooks;
end GPS.Kernel.Hooks;
''')
    b.close()


def generate_doc():
    """
    Generate the documentation for hooks.
    """
    f = open('../../docs/users_guide/GPS/generated_hooks.py', 'wb')
    f.write('''
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
''')

    for h in hooks:
        type = hook_types[h.type]

        def p_param(p):
            descr = p.descr.strip()
            if descr:
                descr = '\n         ' + descr.replace('\n', '\n         ')
            return descr

        params = ['\n      :param %(python)s %(name)s:%(default)s%(doc)s' % {
                     'python': types[p.type].python,
                     'doc': p_param(p),
                     'default': ' (default: %s)' % p.default
                                if p.default else '',
                     'name': p.name}
                  for p in type.params if p.show_in_python()]

        plist = ','.join([p.name for p in type.params])
        descr = h.raw_descr.strip()
        if descr:
            descr = '\n      ' + descr.replace('\n', '\n      ') + '\n'

        if type.returns is not None and types[type.returns].python is not None:
            returns = '\n      :return: %s\n' % types[type.returns].python
        else:
            returns = ''

        subst = {
            'base': h.name,
            'descr': descr,
            'plist': plist,
            'return': returns,
            'params': ''.join(params)
        }

        if isinstance(type, Debounce_Hook_Type):
            subst['asynch'] =  type.debounce

        f.write('''
    # %(base)s = '%(base)s'
    def %(base)s(%(plist)s):
        """%(descr)s%(params)s%(return)s
''' % subst)

        if isinstance(type, Debounce_Hook_Type):
            f.write('''
      :asynchronouse %(asynch)s (ms)
''' % subst)

        f.write('''
        """
''')

    f.write('GPS.Predefined_Hooks = Predefined_Hooks\n')
    f.close()

generate()
generate_doc()
