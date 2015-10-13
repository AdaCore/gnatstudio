"""
This plug-in adds support for QGen (the GNAT Modeling Compiler) which generates
Ada (SPARK 2014?) and C code from Simulink models.

To use this plugin, you must have "mdl2json" available on your
PATH. Your project must also add the "Simulink" language to its
Languages attribute. At this point, opening an .mdl file will
show a diagram instead of showing the text of the .mdl file.
Double-click on a system block to open it and see other diagrams.

The project can optionally include an output directory for the
generated code:

    package QGen is
       for Output_Dir use "generated_src";
    end QGen;

The project can also be used to override the types file used when
generating code. The default is to use a file with the same name
as the .mdl, but with extension _types.txt. If some other file is
needed, you can use:

    package QGen is
       for Switches ("myfile.mdl") use ("-t", "mytypes.txt");
    end QGen;

A contextual menu is provided when you right-click on an .mdl file,
to generate code from that file. This menu is available in particular
in the project view and in the diagrams themselves.
Whenever qgen has finished running, GPS will automatically reload the
project to make the newly generated files available.
"""

import json
import GPS
import GPS.Browsers
import glob
import gps_utils
import gpsbrowsers
import modules
import os
import os.path
import os_utils
import re
from workflows.promises import Promise, ProcessWrapper


logger = GPS.Logger('MODELING')


class Project_Support(object):
    """
    This class provides an interface to the project facilities, to be
    used by QGen.
    """

    def register_languages(self):
        """Add support for the Simulink language"""
        GPS.parse_xml("""<?xml version='1.0' ?>
          <GPS>
            <Language>
              <Name>Simulink</Name>
              <Body_Suffix>.mdl</Body_Suffix>
              <Obj_Suffix>-</Obj_Suffix>
            </Language>
            <Language>
              <Name>Simulink_Json</Name>
              <Body_Suffix>.mdl.json</Body_Suffix>
              <Obj_Suffix>-</Obj_Suffix>
            </Language>
          </GPS>""")

    def register_tool(self):
        """Register the QGENC tool and its switches"""

        GPS.parse_xml("""<?xml version='1.0' ?>
           <GPS>
             <project_attribute
              package="QGen"
              name="Output_Dir"
              editor_page="QGen"
              label="Output directory"
              description="The location of all generated source code files"
              hide_in="wizard library_wizard">
                <string type="directory"/>
             </project_attribute>

             <project_attribute
              package="QGen"
              name="Switches"
              editor_page="QGen"
              list="true"
              label="Switches"
              hide_in="wizard library_wizard">
                <index attribute='Languages'>
                   <string />
                </index>
             </project_attribute>

             <target-model name="QGenc" category="">
               <description>Generic launch of QGen</description>
               <iconname>gps-build-all-symbolic</iconname>
               <switches>
               </switches>
             </target-model>

             <target model="QGenc" category="_File_" name="QGen for file">
               <in-toolbar>FALSE</in-toolbar>
               <in-menu>FALSE</in-menu>
               <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
               <read-only>TRUE</read-only>
               <command-line>
                 <arg>qgenc</arg>
                 <arg>%(option_incremental)s</arg>
                 <arg>%(option_language)s</arg>
                 <arg>ada</arg>
               </command-line>
             </target>

             <tool
              name="QGENC"
              package="QGen"
              index="Simulink">
               <language>Simulink</language>
               <switches>
                 <title line="1">Files</title>
                 <title line="2">Generation</title>
                 <title line="3">Output</title>

                 <field
                  line="1"
                  label="Matlab file"
                  switch="%(option_matlab)s"
                  separator=" "
                  as-file="true"
                 tip="Provides variable declarations of the Matlab workspace"/>
                 <field
                  line="1"
                  label="Typing file"
                  switch="%(option_typing)s"
                  separator=" "
                  as-file="true"
                  tip="Provides Simulink block typing information"/>
                 <field
                  line="1"
                  label="Library directory"
                  switch="%(option_library)s"
                  separator=" "
                  as-directory="true"
                  tip=""/>

                 <combo
                  line="2"
                  label="Target language"
                  switch="%(option_language)s"
                  separator=" "
             tip="The language used by QGENC to produce the generated files">
                    <combo-entry label="Ada" value="ada"/>
                    <combo-entry label="C" value="c"/>
                 </combo>
                 <check
                  line="2"
                  label="Flatten model"
                  switch="%(option_flatten)s"
                  tip=""/>

                 <radio line="3">
                   <radio-entry
                    label="Delete"
                    switch="%(option_clean)s"
             tip="Delete contents of output directory between compilations"/>
                   <radio-entry
                    label="Preserve"
                    switch="%(option_incremental)s"
             tip="Preserve contents of output directory between compilations"/>
                 </radio>
               </switches>
             </tool>
           </GPS>""" % {
            "option_clean": CLI.OPTION_CLEAN,
            "option_flatten": CLI.OPTION_FLATTEN,
            "option_incremental": CLI.OPTION_INCREMENTAL,
            "option_language": CLI.OPTION_LANGUAGE,
            "option_library": CLI.OPTION_LIBRARY,
            "option_matlab": CLI.OPTION_MATLAB,
            "option_typing": CLI.OPTION_TYPING
        })

    def get_output_dir(self, file):
        """
        Return the output directory to use when generating code for file.
        It default to the project's object directory.

        :param GPS.File file: the .mdl file
        """
        dir = file.project().get_attribute_as_string(
            package='QGen', attribute='Output_Dir')
        if not dir:
            try:
                return file.project().object_dirs()[0]
            except:
                return GPS.Project.root().object_dirs()[0]
        return dir

    def get_switches(self, file):
        """
        Return the wswitches to use for a specific file
        :param GPS.File file: the .mdl file
        :return str: the list of switches
        """
        try:
            switches = file.project().get_attribute_as_string(
                attribute='Switches', package='QGen',
                index=os.path.basename(file.name()))
            if not switches:
                switches = file.project().get_attribute_as_string(
                    attribute='Switches', package='QGen',
                    index='simulink')
        except:
            switches = ''

        return switches


class CLI(GPS.Process):
    """
    An interface to the mdl2json executable. This is responsible for
    converting an mdl file to a JSON format that can be displayed by GPS.
    """

    mdl2json = os_utils.locate_exec_on_path('mdl2json')
    # path to mdl2json

    qgenc = os_utils.locate_exec_on_path('qgenc')
    # path to qgenc

    OPTION_CLEAN = "-c"
    OPTION_FLATTEN = "--full-flattening"
    OPTION_INCREMENTAL = "-i"
    OPTION_LANGUAGE = "-l"
    OPTION_LIBRARY = "-b"
    OPTION_MATLAB = "-m"
    OPTION_OUTPUT = "-o"
    OPTION_TYPING = "-t"
    OPTION_DEBUG = "--debug"
    # The names of various QGENC and CLI options

    @staticmethod
    def is_available():
        """
        Whether mdl2json is available on the system.
        """
        # Testing None or empty string
        if CLI.mdl2json:
            return True
        else:
            return False

    @staticmethod
    def get_json(file):
        """
        Compute the JSON to display the given .mdl file
        :param GPS.File file: the .mdl file to analyze
        :return: a promise, that passes the full output of the process
           when resolved
        """

        promise = Promise()

        # Get switches, but remove the ones that do not apply to mdl2json
        switches = re.sub(
            CLI.OPTION_FLATTEN, "", project_support.get_switches(file))
        outdir = project_support.get_output_dir(file)

        # ??? Should output result on stdout
        cmd = ' '.join(
            [CLI.mdl2json,
             file.name(),
             switches])

        def __on_exit(proc, exit_status, output):
            if exit_status == 0:
                promise.resolve(output)
            else:
                GPS.Console().write('When running mdl2json: %s\n' % (
                    output), mode='error')

        # mdl2json is relatively fast, and since the user is waiting for
        # its output to see the diagram, we run in active mode below.
        GPS.Process(command=cmd, on_exit=__on_exit, active=True)
        return promise

    ###########
    # Compiling models
    ###########

    @staticmethod
    def is_model_file(ctx):
        """
        Whether the current context is a model file.
        :param GPS.Context ctx:
        """
        try:
            return ctx.file().language() == 'simulink'
        except:
            return False

    @staticmethod
    def __compile_file_to_source_code(file, on_exit=None):
        """
        Generate code for a specific source file
        :param GPS.File file:
        :param func on_exit: called when the code generation finishes
        """
        # Compute the extra switches. The user can override -t, for instance,
        # by setting the project attribute Switches("file.mdl") with a
        # proper version of -t.
        switches = [
            CLI.OPTION_OUTPUT,
            project_support.get_output_dir(file),
            CLI.OPTION_TYPING,
            "%s_types.txt" % os.path.splitext(
                os.path.basename(file.name()))[0]]
        switches = (' '.join(switches) +
                    ' ' + project_support.get_switches(file) +
                    ' ' + file.name())

        target = GPS.BuildTarget('QGen for file')
        target.execute(
            synchronous=False,
            file=file,
            extra_args=switches,
            on_exit=on_exit)

    @staticmethod
    def compile_context_to_source_code():
        """
        Generate code from the model file from the current context.
        This function should only be called when is_model_file returns
        True.
        """
        ctxt = GPS.contextual_context() or GPS.current_context()

        # On exit, recompute the project to include generated sources
        def on_exit(status):
            if not status:
                GPS.Project.recompute()

        CLI.__compile_file_to_source_code(
            file=ctxt.file(),
            on_exit=on_exit)

    @staticmethod
    def compile_project_to_source_code():
        """
        Generate code for all simulink files in the project
        """
        # ??? qgenc should allow multipe files on the command line
        def all_simulink_files():
            for s in GPS.Project.root().sources(recursive=True):
                if s.language() == 'simulink':
                    yield s

        all_files = all_simulink_files()

        def on_exit(status):
            try:
                next_file = next(all_files)
                CLI.__compile_file_to_source_code(next_file, on_exit=on_exit)
            except:
                # no more files
                GPS.Project.recompute()

        on_exit(0)  # start processing the files


class QGEN_Diagram(gpsbrowsers.JSON_Diagram):
    def on_selection_changed(self, item, *args):
        """React to a change in selection of an item."""
        pass


class QGEN_Diagram_Viewer(GPS.Browsers.View):
    """
    A Simulink diagram viewer. It might be associated with several
    diagrams, which are used as the user opens blocks.
    """

    file = None   # The associated .mdl file
    diags = None  # The list of diagrams read from this file

    @staticmethod
    def __get_or_create_view(file):
        """
        Get an existing viewer for file, or create a new empty view.
        :return: (view, newly_created)
        """
        for win in GPS.MDI.children():
            if hasattr(win, '_gmc_viewer'):
                v = win._gmc_viewer
                if v.file == file:
                    win.raise_window()
                    return (v, False)

        v = QGEN_Diagram_Viewer()
        v.file = file
        v.diags = None   # a gpsbrowsers.JSON_Diagram_File
        v.create(
            diagram=GPS.Browsers.Diagram(),  # a temporary diagram
            title=os.path.basename(file.name()),
            save_desktop=v.save_desktop)
        v.set_read_only(True)

        c = GPS.MDI.get_by_child(v)
        c._gmc_viewer = v

        return (v, True)

    @staticmethod
    def get_or_create(file):
        """
        Get an existing diagram for the file, or create a new one.
        The actual diagrams are loaded asynchronously, so might not be
        available as soon as the instance is constructed. They are however
        automatically loaded in the view as soon as possible.

        :param GPS.File file: the file to display
        :return QGEN_Diagram_Viewer: the diagram
        """
        v, newly_created = QGEN_Diagram_Viewer.__get_or_create_view(file)

        if newly_created:
            def __on_json(json):
                v.diags = GPS.Browsers.Diagram.load_json_data(
                    json, diagramFactory=QGEN_Diagram)
                if v.diags:
                    v.diagram = v.diags.get()

            CLI.get_json(file).then(__on_json)

        return v

    @staticmethod
    def open_json(file, data):
        """
        Open an existing JSON file that contains a Simulink diagram.
        :param GPS.File file: the file associated with the JSON data,
           so that we do not open multiple viewers for the same file.
        :param data: the actual json data to display.
        """
        v, newly_created = QGEN_Diagram_Viewer.__get_or_create_view(file)
        if newly_created:
            v.diags = GPS.Browsers.Diagram.load_json_data(
                data, diagramFactory=QGEN_Diagram)
            if v.diags:
                v.diagram = v.diags.get()
        return v

    def save_desktop(self, child):
        """Save the contents of the viewer in the desktop"""
        info = {
            'file': self.file.name(),
            'scale': self.scale,
            'topleft': self.topleft}
        return (module.name(), json.dumps(info))

    def perform_action(self, action, item):
        """
        Perform the action described by the parameter.
        :param str action: an action described in the JSON file, to be
           executed when the user interacts with an item. The list of
           valid actions is documented in the code below.
        """

        # Split the command into its name and arguments
        (name, args) = action.split('(', 1)
        if args and args[-1] != ')':
            GPS.Console().write(
                "Invalid command: %s (missing closing parenthesis)\n" % (
                    action, ))
            return

        args = args[:-1].split(',')  # ??? fails if arguments contain nested ,
        for idx, a in enumerate(args):
            if a[0] in ('"', "'") and a[-1] == a[0]:
                args[idx] = a[1:-1]
            elif a.isdigit():
                args[idx] = int(a)
            else:
                GPS.Console().write("Invalid command: %s\n" % (action, ))
                return

        if name == 'showdiagram':
            self.diagram = self.diags.get(args[0])

    # @overriding
    def on_item_double_clicked(self, topitem, item, x, y, *args):
        """
        Called when the user double clicks on an item.
        """
        action = topitem.data.get('dblclick')
        if action:
            self.perform_action(action, topitem)

    # @overriding
    def on_create_context(self, context, topitem, item, x, y, *args):
        """
        Called when the user right-clicks in an item.
        """
        context.set_file(self.file)


project_support = Project_Support()
project_support.register_languages()  # available before project is loaded

if not CLI.is_available():
    logger.log('mdl2json not found on the PATH')

else:
    project_support.register_tool()

    class QGEN_Module(modules.Module):

        @staticmethod
        @gps_utils.hook('open_file_action_hook', last=False)
        def __on_open_file_action(file, *args):
            if file.language() == 'simulink':
                logger.log('Open %s' % file)
                viewer = QGEN_Diagram_Viewer.get_or_create(file)
                return True
            if file.language() == 'simulink_json':
                logger.log('Open %s' % file)
                viewer = QGEN_Diagram_Viewer.open_json(
                    file, open(file.name()).read())
                return True
            return False

        def load_desktop(self, data):
            """Restore the contents from the desktop"""
            info = json.loads(data)
            f = GPS.File(info['file'])
            if f.name().endswith('.mdl'):
                viewer = QGEN_Diagram_Viewer.get_or_create(f)
            else:
                viewer = QGEN_Diagram_Viewer.open_json(
                    f, open(f.name()).read())
            viewer.scale = info['scale']
            viewer.topleft = info['topleft']
            return GPS.MDI.get_by_child(viewer)

        def setup(self):
            """
            Initialize the support for modeling in GPS.
            This is only called when the necessary command line executables
            are found.
            """

            gps_utils.make_interactive(
                name='compile model file',
                filter=CLI.is_model_file,
                contextual='Generate code for %f',
                callback=CLI.compile_context_to_source_code)

            gps_utils.make_interactive(
                name='compile model',
                contextual='Generate code for project',
                callback=CLI.compile_project_to_source_code)

    module = QGEN_Module()
