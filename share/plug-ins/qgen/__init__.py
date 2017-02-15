"""
This plugin adds support for QGen and generates Ada and C code from Simulink
models.

To use this plugin, you must install qgen, and have "qgenc" available
in your PATH.

Your project must also add the "Simulink" language to its Languages attribute.
At this point, opening an .mdl file will show a diagram instead of showing the
text of the .mdl file.  Double-click on a system block to open it and see other
diagrams.

The project can optionally include an output directory for the
generated code. This directory defaults to the project's object_dir.

    project Default is
       for Languages use ("Ada", "C");
       for Source_Dirs use (".", "generated");
       package QGen is
          for Output_Dir use "generated";
       end QGen;
    end Default;

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
Convenient toolbar buttons are enabled to generate-then-build or even
generate-then-build-then-debug.
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
import workflows
import constructs
from workflows.promises import Promise, ProcessWrapper, TargetWrapper
from project_support import Project_Support
from . import mapping
from sig_utils import Signal
from diagram_utils import Diagram_Utils


logger = GPS.Logger('MODELING')


class MDL_Language(GPS.Language):
    """
    A class that describes the MDL and Simulink language for GPS.
    """

    # @overriding
    def __init__(self):
        # The constant string used to create a construct id
        self.const_id = "#QGEN#"
        pass

    @staticmethod
    def register():
        """Add support for the Simulink language"""
        GPS.Language.register(
            MDL_Language(),
            name="Simulink",
            body_suffix=".mdl",
            spec_suffix=".slx",
            obj_suffix="-")

    # @overriding
    def should_refresh_constructs(self, file):
        # Always refresh when GPS believes the file has changed (for instance
        # after the buffer_edited hook).
        return True

    # @overriding
    def clicked_on_construct(self, construct):
        def __on_loaded(view):
            construct_id = construct.id.split(self.const_id, 1)[1]
            diag = view.diags.get(construct_id)
            # If the construct was a diagram, open it
            # otherwise highlight the item
            if diag.id == construct_id:
                view.set_diagram(diag)
            else:
                info = QGEN_Module.modeling_map.get_diagram_for_item(
                    view.diags, construct_id)
                if info:
                    diagram, item = info
                    view.diags.clear_selection()
                    diagram.select(item)
                    view.set_diagram(diagram)
                    view.scroll_into_view(item)

        QGEN_Diagram_Viewer.get_or_create(
            construct.file, on_loaded=__on_loaded)

    # @overriding
    def parse_constructs(self, clist, file, file_contents):
        """
        Provides support for the Outline view
        """

        flat = GPS.Preference('outline-flat-view').get()

        def process_item(item, item_id, children, start_offset):
            """
            Return an item and its simulated sloc.
            The source locations are simulated so that nesting can be computed
            automatically by GPS based on the line/column info. GPS uses
            indexes when they are positive.

            :return: (item, sloc_start, sloc_end, constructs_CAT)
            """
            max_offset = start_offset
            subsystem_list = []
            # When processing the current diagram, this list stores what
            # subsystems where already added in the Outline to avoid
            # duplication when adding items
            exclude_subsystem = item_id == viewer.diagram.id

            # The index entry contains a JSON_Array of entries with
            # a 'name' and 'diagram' fields. They respectively correspond
            # to the simulink name of the item and its corresponding JSON id
            if children is not []:
                for child in children:
                    child_name = child["name"]
                    child_id = child["diagram"]
                    if exclude_subsystem:
                        subsystem_list.append(child_name)
                    for child_entry_name, child_children in viewer.diags.index:
                        # Each child of the current diagram is processed
                        if child_entry_name == child_id:
                            for result in process_item(
                                    child_name, child_id, child_children,
                                    start_offset=max_offset + 1):
                                max_offset = result[3][2]
                                yield result
                            break

            # Adding all generic blocks to the outline for the current diagram
            if exclude_subsystem:
                for it in viewer.diagram.items:
                    if isinstance(it, GPS.Browsers.Link):
                        continue
                    try:
                        for child in it.children:
                            # The qgen_navigation_info is a custom item
                            # used by this plugin for navigation we do not
                            # want to display it
                            if child.id != 'qgen_navigation_info' and \
                               child.id not in subsystem_list:
                                max_offset = max_offset + 2
                                # A block item will have the same name and id
                                # both set to child.id
                                yield (child.id, child.id,
                                       (0, 0, max_offset - 1),
                                       (0, 0, max_offset),
                                       constructs.CAT_ENTRY)
                    except:
                        None
            yield (item, item_id,
                   (0, 0, start_offset),
                   (0, 0, max_offset + 1),
                   constructs.CAT_CLASS)

        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()

        if viewer and viewer.diags:
            offset = 1
            id, val = viewer.diags.index[0]
            for it_name, it_id, sloc_start, sloc_end, type in process_item(
                    id, id, val, 0):
                offset = max(offset, sloc_end[2]) + 1
                clist.add_construct(
                    category=type,
                    is_declaration=True,
                    visibility=constructs.VISIBILITY_PUBLIC,
                    name=it_name if flat else Diagram_Utils.block_split(
                        it_name, count=1, backward=True)[-1],
                    profile='',
                    # We combine the name of the block with its id and a #QGEN#
                    # string to both create a unique id (it_name is unique) and
                    # be able to retrieve the id to display the correct diagram
                    # when the construct is clicked (as long as the name did
                    # not contain #QGEN# already, which is unlikely).
                    id=it_name + self.const_id + it_id,
                    sloc_start=sloc_start,
                    sloc_end=sloc_end,
                    sloc_entity=sloc_start)


class CLI(GPS.Process):
    """
    An interface to the qgenc executable. This is responsible for
    converting an mdl file to a JSON format that can be displayed by GPS.
    """

    qgenc = os_utils.locate_exec_on_path('qgenc')
    # path to qgenc

    @staticmethod
    def is_available():
        """
        Whether qgenc is available on the system.
        """
        # Testing None or empty string
        if CLI.qgenc:
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

        filepath = file.path
        typefile = os.path.splitext(filepath)[0]
        switches = Project_Support.get_switches(
            file) + " -t %s_types.txt" % typefile
        outdir = Project_Support.get_output_dir(file)

        result_path = os.path.join(
            outdir, '.' + os.path.basename(filepath) + '_mdl2json')

        if os.path.isfile(result_path):
            # If the mdl file is older than the previous json file no need
            # to recompute
            if os.path.getmtime(result_path) >= os.path.getmtime(filepath):
                promise.resolve(result_path)
                return promise

        if outdir:
            switches += ' -o ' + outdir

        cmd = ' '.join(
            [CLI.qgenc, filepath, switches + ' --with-gui-only --incremental'])

        def __on_exit(proc, exit_status, output):
            if exit_status == 0:
                promise.resolve(result_path)
            else:
                GPS.Console().write('When running qgenc for gui: %s\n' % (
                    output), mode='error')
                promise.reject()

        # qgenc is relatively fast, and since the user is waiting for
        # its output to see the diagram, we run in active mode below.
        GPS.Process(command=cmd, on_exit=__on_exit, active=True)
        return promise

    ###########
    # Compiling models
    ###########

    @staticmethod
    def is_model_file(ctx_or_file):
        """
        Whether the current context is a model file.
        :param ctx: either a `GPS.Context` or a `GPS.File`
        """
        try:
            if isinstance(ctx_or_file, GPS.Context):
                f = ctx_or_file.file()
            else:
                f = ctx_or_file
            return f.language() == 'simulink'
        except:
            return False

    @staticmethod
    def __compile_files_to_source_code(files):
        """
        A python generator that generates code for the `mdl` source file.
        :param files: A list of `GPS.File`, from which to generate code.
        :return: the last yield is the status (0 if everything succeeded)
        """
        # Compute the extra switches. The user can override -t, for instance,
        # by setting the project attribute Switches("file.mdl") with a
        # proper version of -t.

        st = 1
        for f in files:
            if CLI.is_model_file(f):
                typefile = os.path.splitext(f.path)[0]
                switches = [
                    "-o", Project_Support.get_output_dir(f),
                    "-t", "%s_types.txt" % typefile, "--with-gui"]
                switches = (' '.join(switches) + ' ' +
                            Project_Support.get_switches(f) +
                            ' ' + f.path)
                w = TargetWrapper(target_name='QGen for file')
                st = yield w.wait_on_execute(file=f, extra_args=switches)
                if st != 0:
                    break

        if st == 0:
            GPS.Project.recompute()  # Add generated files to the project

        yield st

    @staticmethod
    def workflow_compile_context_to_source_code():
        """
        Generate code from the model file for a specific MDL file
        """
        ctxt = GPS.contextual_context() or GPS.current_context()
        return CLI.__compile_files_to_source_code([ctxt.file()])

    @staticmethod
    def workflow_compile_project_to_source_code():
        """
        Generate code for all MDL files in the project
        """
        s = GPS.Project.root().sources(recursive=True)
        return CLI.__compile_files_to_source_code(s)

    @staticmethod
    def workflow_generate_from_mdl_then_build(main_name):
        """
        Generate the code for all simulink files, then compile the project.
        This works best if you have defined the `Main` attribute in your
        project, so that gprbuild knows what to link.
        This is a workflow, and should be used via the functions in
        workflows.py.
        """
        models = Project_Support.get_models(main_name)

        if not models:
            GPS.Console().write(
                "No models specified for %s: use the 'Target' property "
                "in the project file to fix. See QGen Model Debugger "
                "user guide for more detail.\n" % main_name)

        status = yield CLI.__compile_files_to_source_code(models)

        if status == 0:
            w = TargetWrapper(target_name='Build Main')
            yield w.wait_on_execute(main_name=main_name)

    @staticmethod
    def workflow_generate_from_mdl_then_build_then_debug(main_name):
        """
        Generate the code for all simulink files, then compile the specified
        main, then debug it.
        This is a workflow, and should be used via the functions in
        workflows.py.
        """
        exe = GPS.File(main_name).executable_path
        debuggers_to_close = [
            d for d in GPS.Debugger.list()
            if d.get_executable().executable_path == exe]

        if len(debuggers_to_close) > 0:
            if GPS.MDI.yes_no_dialog(
                    "One or more debuggers are already running for that"
                    " executable, do you want to terminate them ?"):
                for debugger in debuggers_to_close:
                    debugger.close()

        models = Project_Support.get_models(main_name)

        if not models:
            GPS.Console().write(
                "No models specified for %s: use the 'Target' property "
                "in the project file to fix. See QGen Model Debugger "
                "user guide for more detail.\n" % main_name)

        status = yield CLI.__compile_files_to_source_code(models)

        if status == 0:
            w = TargetWrapper(target_name='Build Main')
            status = yield w.wait_on_execute(main_name=main_name)
        if status == 0:
            GPS.Debugger.spawn(exe)

    @staticmethod
    def delete_logfile_dialog(filename):
        """
        Spawns a dialog if the file named filename exists
        and returns whether the file exists or not anymore
        """
        # Ask if we need to overwrite the file if this is the
        # first signal to log
        ask_overwrite = True
        for itid, sig in QGEN_Module.signal_attributes.iteritems():
            if sig.logged:
                ask_overwrite = False
                break

        if ask_overwrite and os.path.exists(filename):
            res = GPS.MDI.yes_no_dialog(
                "%s already exists, do you want to delete it?" % filename)
            if res:
                os.remove(filename)
            else:
                return False

        GPS.Console().write(
            "Logfile will be written in %s, open it in the Matlab"
            " web browser.\n" % filename)
        return True

    @staticmethod
    def log_subsystem_values():
        """
        Logs all values for signals of the subsystem in relation to their
        containing variables in an html file that should be
        opened in the Matlab browser in order to have working links.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()

        if viewer:
            v = GPS.MDI.input_dialog("Log current subsystem signals in",
                                     "filename")
            if v:
                QGEN_Module.log_values_in_file([viewer.diagram], v[0])

    @staticmethod
    def stop_logging_subsystem_values():
        """
        Stop logging values for signals of the current subsystem
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()

        if viewer:
            QGEN_Module.stop_logging_values([viewer.diagram])

    @staticmethod
    def action_goto_previous_subsystem():
        """
        Retrieves the focused QGen Diagram viewer and changes the
        diagram to display based on the `text` field of the
        `qgen_navigation_info` item of the current diagram.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            diag_name = viewer.diagram.get_item('qgen_navigation_info').text
            if diag_name != "":
                new_diag = viewer.diags.get(diag_name)
                viewer.set_diagram(new_diag, update_prev=False)
                # ??? Disable previous button when no previous

    @staticmethod
    def action_goto_parent_subsystem():
        """
        Retrieves the focused QGen Diagram viewer and changes the
        diagram to display based on the `parent` field of the
        `qgen_navigation_info` item of the current diagram.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            navigation_info = viewer.diagram.get_item('qgen_navigation_info')
            diag_name = navigation_info.data.get('parent')
            if diag_name != "":
                new_diag = viewer.diags.get(diag_name)
                new_diag.get_item(
                    'qgen_navigation_info').text = viewer.diagram.id
                viewer.set_diagram(new_diag, update_prev=False)
                # ??? Disable parent button when no parent


class QGEN_Diagram(gpsbrowsers.JSON_Diagram):

    def __init__(self, file, json):
        self.current_priority = 0
        self.reset_priority = False
        super(QGEN_Diagram, self).__init__(file, json)

    def on_selection_changed(self, item, *args):
        """React to a change in selection of an item."""
        pass


class QGEN_Diagram_Viewer(GPS.Browsers.View):
    """
    A Simulink diagram viewer. It might be associated with several
    diagrams, which are used as the user opens blocks.
    """
    # The associated .mdl file
    file = None
    # The list of diagrams read from this file a JSON_Diagram_File instance
    diags = None

    def __init__(self):
        # The set of callbacks to call when a new diagram is displayed
        super(QGEN_Diagram_Viewer, self).__init__()

    @staticmethod
    def retrieve_qgen_viewers():
        """
        Returns the list of all qgen viewer instances currently open
        in GPS
        """
        for win in GPS.MDI.children():
            if hasattr(win, '_gmc_viewer'):
                yield win._gmc_viewer

    @staticmethod
    def retrieve_active_qgen_viewer():
        """
        Returns the focused qgen viewer if possible
        :return: a QGEN_Diagram_Viewer instance
        """
        try:
            win = GPS.MDI.current()
            if hasattr(win, '_gmc_viewer'):
                return win._gmc_viewer
        except:
            return None

    @staticmethod
    def get_or_create_view(file):
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
            title=os.path.basename(file.path),
            save_desktop=v.save_desktop,
            toolbar="MDL Browser")
        v.set_read_only(True)

        c = GPS.MDI.get_by_child(v)
        c._gmc_viewer = v

        GPS.Hook('file_edited').run(file)
        return (v, True)

    @staticmethod
    def get_or_create(file, on_loaded=None):
        """
        Get an existing diagram for the file, or create a new one.
        The actual diagrams are loaded asynchronously, so might not be
        available as soon as the instance is constructed. They are however
        automatically loaded in the view as soon as possible.

        :param GPS.File file: the file to display
        :param callable on_loaded: called when the diagram is loaded, or
           immediately if the diagram was already loaded. The function
           receives one parameter:
               - the viewer itself
        :return QGEN_Diagram_Viewer: the viewer.
           It might not contain any diagram yet, since those are read
           asynchronously.
        """
        v, newly_created = QGEN_Diagram_Viewer.get_or_create_view(file)

        if newly_created:
            def __on_json(jsonfile):
                v.diags = GPS.Browsers.Diagram.load_json(
                    jsonfile, diagramFactory=QGEN_Diagram)
                if v.diags:
                    v.set_diagram(v.diags.get(), update_prev=False)

                if on_loaded:
                    on_loaded(v)

            def __on_fail(reason):
                pass

            CLI.get_json(file).then(__on_json, __on_fail)

        else:
            if on_loaded:
                on_loaded(v)

        return v

    @staticmethod
    def open_json(file, data):
        """
        Open an existing JSON file that contains a Simulink diagram.
        :param GPS.File file: the file associated with the JSON data,
           so that we do not open multiple viewers for the same file.
        :param data: the actual json data to display.
        """
        v, newly_created = QGEN_Diagram_Viewer.get_or_create_view(file)
        if newly_created:
            v.diags = GPS.Browsers.Diagram.load_json_data(
                data, diagramFactory=QGEN_Diagram)
            if v.diags:
                v.diagram = v.diags.get()
        return v

    def save_desktop(self, child):
        """Save the contents of the viewer in the desktop"""
        info = {
            'file': self.file.path,
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
            self.set_diagram(self.diags.get(args[0]))

    def set_diagram(self, diag, update_prev=True):
        """
        Sets the diagram that has to be displayed and calls
        the callbacks that have to be executed when the diagram changes
        If diag is None a refresh on the current diagram is performed
        :param diag: The new diagram to display
        :param update_prev: If true, store the previous subsystem
        for navigation purposes
        """
        if diag and diag != self.diagram:
            if update_prev:
                navigation_info = diag.get_item('qgen_navigation_info')
                navigation_info.text = self.diagram.id
            self.diagram = diag
            self.scale_to_fit(2)

        QGEN_Module.on_diagram_changed(self, self.diagram)

        # Let GPS views, in particular the outline, know when we select
        # a new diagram
        GPS.SemanticTree(self.file).update()
        GPS.Hook('semantic_tree_updated').run(self.file)

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
        context.modeling_item = item
        context.modeling_topitem = topitem

MDL_Language.register()   # available before project is loaded

if not CLI.is_available():
    logger.log('qgenc not found')

else:
    Project_Support.register_tool()

    class QGEN_Module(modules.Module):

        modeling_map = None   # a Mapping_File instance

        # id => Signal object
        signal_attributes = {}

        previous_breakpoints = []
        debugger = None

        @staticmethod
        @gps_utils.hook('project_view_changed')
        def __on_project_view_changed():
            """
            Load all mapping files used in this project.
            We need them for the debugger interaction (so we could load
            them only when the debugger is started), but we also need them
            when interacting with the diagram, for instance to show the
            source associated with a block.
            """

            QGEN_Module.modeling_map = mapping.Mapping_File()
            for f in GPS.Project.root().sources(recursive=True):
                if CLI.is_model_file(f):
                    QGEN_Module.modeling_map.load(f)

        @staticmethod
        def __clear(debugger):
            """
            Resets the diagram display when a new debugger is used
            """
            for id, sig in QGEN_Module.signal_attributes.iteritems():
                sig.reset()

            for viewer in QGEN_Diagram_Viewer.retrieve_qgen_viewers():
                for diag in viewer.diags.diagrams:
                    diag.clear_selection()
                    QGEN_Module.on_diagram_changed(viewer, diag, clear=True)

            QGEN_Module.previous_breakpoints = debugger.breakpoints

            QGEN_Module.signal_attributes.clear()
            del QGEN_Module.previous_breakpoints[:]

        @staticmethod
        def compute_item_values(debugger, diagram, toplevel, item):
            """
            Recompute the value for all symbols related to item, and update
            the display of item to show their values.
            :param GPS.Debugger debugger: the debugger to query.
               It can be None when no debugger is running.
            :param (GPS.Browsers.Item|GPS.Browsers.Link) toplevel: the
               toplevel item (generally a link)
            :param GPS.Browsers.Item item: the item that has an "auto"
               property that indicates its value should be displayed.
            """

            # Find the parent with an id. When item is the label of a link, the
            # parent will be set to None, so we default to toplevel (the link,
            # in that case)

            parent = item.get_parent_with_id() or toplevel

            # Find the parent to hide (the one that contains the label)

            item_parent = item
            p = item.parent
            while p is not None:
                item_parent = p
                p = item_parent.parent

            # The list of symbols to compute from the debugger

            symbols = QGEN_Module.modeling_map.get_symbols(blockid=parent.id)

            if symbols and debugger is not None:
                s = next(iter(symbols))  # Get the first symbol
                # Function calls do not have a '/'
                if '/' in s:
                    ss = s.split('/')[-1].strip()  # Remove the "context/" part

                    # ??? Should check that the context matches the current
                    # debugger frame. For now, we assume this is true since the
                    # diagram corresponds to the current frame.

                    value = debugger.value_of("%s" % (ss, ))

                    # Skip case when the variable is unknown
                    if value is None:
                        item_parent.hide()

                    else:
                        # Check whether the value is a float or is an integer
                        # with more than 6 digits then display it
                        # in scientific notation.
                        # Otherwise no formatting is done on the value

                        try:
                            if (len(value) >= 7 or
                                    float(value) != int(value)):
                                value = '%.2e' % float(value)
                        except ValueError:
                            if len(value) >= 7:
                                value = '%s ..' % value[:6]

                        if value:
                            item_parent.show()
                            item.text = value
                        else:
                            item_parent.hide()

            else:
                item_parent.hide()

        @staticmethod
        def on_diagram_changed(viewer, diag, clear=False):
            """
            Called whenever a new diagram is displayed in viewer. This change
            might have been triggered either by the user or programmatically.
            :param viewer: a QGEN_Diagram_Viewer
            :param diag: a QGEN_Diagram
            :param clear: a boolean which is true when we are clearing
            the diagram for a new debugger instance
            """

            assert(isinstance(viewer.diags, gpsbrowsers.JSON_Diagram_File))

            try:
                debugger = GPS.Debugger.get()
            except:
                debugger = None

            # Compute the value for all items with an "auto" property
            for diag, toplevel, it in Diagram_Utils.forall_auto_items([diag]):
                QGEN_Module.compute_item_values(
                    debugger, diag, toplevel=toplevel, item=it)

            # Restore default style for previous items with breakpoints
            map = QGEN_Module.modeling_map

            # This will contain the list of blocks with breakpoints
            # that have their priority style updated before the
            # update_priority_style call and can therefore be excluded
            # from the update
            bp_blocks = []

            for b in QGEN_Module.previous_breakpoints:
                Diagram_Utils.set_block_style(viewer, diag, map, b, bp_blocks)

            Diagram_Utils.update_signal_style(
                QGEN_Module.signal_attributes, viewer, diag)

            # Remove False elements from the lists
            QGEN_Module.signal_attributes = {
                k: sig for k, sig in QGEN_Module.signal_attributes.iteritems()
                if sig.style is not None}

            # The clear method will reset the breakpoints after all the
            # diagrams have been processed
            if clear or not debugger:
                diag.reset_priority = True
                diag.current_priority = 0
                Diagram_Utils.update_priority_style(viewer, [diag], bp_blocks)
                # Update the display
                diag.changed()
                return

            if debugger:
                QGEN_Module.previous_breakpoints = debugger.breakpoints

            Diagram_Utils.highlight_breakpoints(
                viewer, diag, map,
                bp_blocks, QGEN_Module.previous_breakpoints)

            Diagram_Utils.update_priority_style(viewer, [diag], bp_blocks)
            diag.changed()

        @staticmethod
        @gps_utils.hook('debugger_location_changed')
        def __on_debugger_location_changed(debugger):
            debugger.send("qgen_breakpoint_action", output=False)
            QGEN_Module.__show_diagram_and_signal_values(debugger)

        @staticmethod
        def log_values_from_item(itid, filename, model_filename, debug):
            """
            Logs all symbols for the signal 'itid' in 'filename' by registering
            it in the debugger 'debug'
            """
            for s in QGEN_Module.modeling_map.get_symbols(blockid=itid):
                symbol = Diagram_Utils.block_split(s)
                context = symbol[0]
                symbol = symbol[-1].strip()
                src_file, lines = QGEN_Module.modeling_map.get_func_bounds(
                    context)

                debug.send("qgen_logpoint %s %s '%s' %s '%s:%s' %s" % (
                    symbol, context, itid, filename, src_file, lines[1],
                    model_filename), output=False)
                sig_obj = QGEN_Module.signal_attributes.get(itid, None)
                if sig_obj is None:
                    sig_obj = Signal(itid)
                    sig_obj.logged = True
                    QGEN_Module.signal_attributes[itid] = sig_obj
                else:
                    sig_obj.logged = True

        @staticmethod
        def stop_logging_value_for_item(itid, debug):
            """
            Removes the symbols for the signal 'itid' from the list of logged
            signals and updates this information in the debugger 'debug'
            """

            for s in QGEN_Module.modeling_map.get_symbols(blockid=itid):
                symbol = Diagram_Utils.block_split(s)
                context = symbol[0]
                symbol = symbol[-1].strip()
                debug.send("qgen_delete_logpoint %s %s" % (symbol, context),
                           output=False)

            sig_obj = QGEN_Module.signal_attributes.get(itid, None)
            sig_obj.logged = False

        @staticmethod
        def stop_logging_values(diagrams):
            """
            Stop logging all signal values in the given diagrams
            """
            ctxt = GPS.current_context()
            debug = GPS.Debugger.get()

            for diag, toplvl, it in Diagram_Utils.forall_auto_items(diagrams):
                parent = it.get_parent_with_id() or toplvl
                sig_obj = QGEN_Module.signal_attributes.get(parent.id, None)
                if sig_obj is not None and sig_obj.logged:
                    QGEN_Module.stop_logging_value_for_item(parent.id, debug)

            QGEN_Module.__show_diagram_and_signal_values(debug, force=True)

        @staticmethod
        def log_values_in_file(diagrams, filename):
            """
            This function retrieves all the values from the signals (items
            with an 'auto' property), adds them to the list of logged
            signal and forwards that information to gdb that will do the
            logging
            :param diagrams: a list of QGEN_Diagram to look into
            :param filename: the name of the logfile to write in
            """
            ctxt = GPS.current_context()
            debug = GPS.Debugger.get()
            filename = os.path.join(
                Project_Support.get_output_dir(
                    ctxt.file()), filename + '.html')

            if not CLI.delete_logfile_dialog(filename):
                return

            for diag, toplvl, it in Diagram_Utils.forall_auto_items(diagrams):
                parent = it.get_parent_with_id() or toplvl
                QGEN_Module.log_values_from_item(parent.id, filename,
                                                 ctxt.file(), debug)
            QGEN_Module.__show_diagram_and_signal_values(debug,
                                                         force=True)

        @staticmethod
        def __load_debug_script(debugger):
            script = "%sshare/gps/plug-ins/qgen/gdb_scripts.py" \
                     % GPS.get_system_dir()

            # gdb-mi handles the cli source command aswell
            debugger.send("source %s" % script, output=False)

        @gps_utils.hook('debugger_breakpoints_changed')
        def __on_debugger_breakpoints_changed(debugger):
            # ??? Could highlight blocks even though the debugger is not
            # started
            if debugger is None:
                return

            if QGEN_Module.debugger != debugger:
                QGEN_Module.debugger = debugger
                # Load qgen gdb script for custom commands
                QGEN_Module.__load_debug_script(debugger)
                debug_args = debugger.current_file.project(
                ).get_attribute_as_string(attribute="Debug_Args",
                                          package="QGen")
                if debug_args != "":
                    debugger.send("set args %s" % debug_args)

                QGEN_Module.__clear(debugger)
            else:
                # Show blocks with breakpoints
                QGEN_Module.__show_diagram_and_signal_values(
                    debugger, force=True)

        @staticmethod
        def __show_diagram_and_signal_values(
                debugger, force=False):
            """
            Show the model corresponding to the current debugger file and line
            :param force boolean: Used to force an update of the view after a
            user interaction
            """
            filename = debugger.current_file
            line = debugger.current_line

            def __on_viewer_loaded(viewer):
                """
                Called when a MDL browser needs refreshing.
                :param QGEN_Diagram_Viewer viewer: the viewer that is now
                   ready. It should contain the list of diagrams.
                """
                assert isinstance(viewer, QGEN_Diagram_Viewer)

                # User interaction happened, update the current diagram
                if force:
                    viewer.set_diagram(None)
                    return

                # Select the blocks corresponding to the current line
                block = QGEN_Module.modeling_map.get_block(filename, line)
                scroll_to = None
                if block:

                    info = QGEN_Module.modeling_map.get_diagram_for_item(
                        viewer.diags, block)
                    if info:
                        diagram, item = info
                        if item:
                            scroll_to = item
                            # Unselect items from the previous step
                            viewer.diags.clear_selection()
                            diagram.select(item)
                            item_prio = item.data.get('priority')
                            if item_prio is not None and item_prio > 0:
                                if diagram.current_priority > item_prio:
                                    # If the priority is going down then we
                                    # reached a new iteration and have
                                    # to reset the processed styles
                                    diagram.reset_priority = True
                                diagram.current_priority = item_prio
                        viewer.set_diagram(diagram)  # calls on_diagram_changed

                if scroll_to:
                    viewer.scroll_into_view(scroll_to)

            if filename:
                mdl = QGEN_Module.modeling_map.get_mdl_file(filename)
                if mdl:
                    QGEN_Diagram_Viewer.get_or_create(
                        mdl, on_loaded=__on_viewer_loaded)
                else:
                    for viewer in QGEN_Diagram_Viewer.retrieve_qgen_viewers():
                        __on_viewer_loaded(viewer)

        def block_source_ranges(self, blockid):
            """
            Whether there are any breakpoint associated with
            blockid. Returns the list of source locations.
            """
            return self.modeling_map.get_source_ranges(blockid)

        def get_item_with_sources(self, item):
            """
            Return item or its closest parent with corresponding source
            lines
            """
            while (item and
                   (not item.id or not self.block_source_ranges(item.id))):
                item = item.parent
            return item

        @staticmethod
        @gps_utils.hook('open_file_action_hook', last=False)
        def __on_open_file_action(file, *args):
            """
            When an ".mdl" file is opened, use a diagram viewer instead of a
            text file to view it.
            """
            if file.language() == 'simulink':
                logger.log('Open %s' % file)
                viewer = QGEN_Diagram_Viewer.get_or_create(file)
                return True
            return False

        def load_desktop(self, data):
            """Restore the contents from the desktop"""
            info = json.loads(data)
            f = GPS.File(info['file'])
            if f.path.endswith('.mdl') or f.path.endswith('.slx'):
                viewer = QGEN_Diagram_Viewer.get_or_create(f)
            else:
                viewer = QGEN_Diagram_Viewer.open_json(
                    f, open(f.path).read())
            viewer.scale = info['scale']
            viewer.topleft = info['topleft']
            return GPS.MDI.get_by_child(viewer)

        def __contextual_filter_debug_and_sources(self, context):
            """
            Whether the current context is a model block with
            source lines (or one of its parents has source lines). The
            debugger must have been started too.
            """
            try:
                d = GPS.Debugger.get()   # or raise exception
                return self.__contextual_filter_sources(context)
            except:
                return False

        def __contextual_filter_debug_and_symbols(self, context):
            """
            Whether the current context is a model block with
            symbols. The debugger must have been started too.
            """
            try:
                d = GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                return len(self.modeling_map.get_symbols(blockid=it.id)) != 0
            except:
                return False

        def __contextual_filter_debug_and_watchpoint(self, context):
            try:
                d = GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)

                return sig_obj.watched
            except:
                return False

        def __contextual_filter_debug_and_logpoint(self, context):
            """
            Whether the current context is a logged signal.
            The debugger must have been started too.
            """

            try:
                d = GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)

                return sig_obj.logged
            except:
                return False

        def __contextual_filter_debugger_active(self, context):
            """
            Whether the debugger has started or not.
            """

            try:
                d = GPS.Debugger.get()   # or raise exception
                return True
            except:
                return False

        def __contextual_filter_viewer_active(self, context):
            try:
                return QGEN_Diagram_Viewer.retrieve_active_qgen_viewer() \
                    is not None
            except:
                return False

        def __contextual_filter_sources(self, context):
            """
            Whether the current context is a model block with
            source lines (or one of its parents has source lines)
            """
            try:
                it = self.get_item_with_sources(context.modeling_item)
                return it is not None
            except:
                return False

        def __contextual_name_for_break_on_block(self, context):
            it = self.get_item_with_sources(context.modeling_item)
            return 'Debug/Break on %s' % (it.id.replace("/", "\\/"), )

        def __contextual_name_for_unbreak_on_block(self, context):
            it = self.get_item_with_sources(context.modeling_item)
            id = it.id.replace("/", "\\/")
            return 'Debug/Delete breakpoints on %s' % (id, )

        def __contextual_set_signal_value(self):
            ctx = GPS.contextual_context() or GPS.current_context()
            it = ctx.modeling_item
            debug = GPS.Debugger.get()

            for s in self.modeling_map.get_symbols(blockid=it.id):
                ss = s.split('/')[-1].strip()  # Remove the "context/" part
                current = debug.value_of(ss)
                added = False

                if current is not None:
                    v = GPS.MDI.input_dialog(
                        "Value for block %s" % it.id,
                        "value=%s" % current)
                    if v:
                        added = True
                        debug.set_variable(ss, v[0])

            if added:
                QGEN_Module.__show_diagram_and_signal_values(debug, force=True)

        def __contextual_log_signal_value(self):
            """
            Adds the selected signal to the lists of logged signals
            and forwards that information to gdb
            """
            ctx = GPS.contextual_context() or GPS.current_context()
            it = ctx.modeling_item
            debug = GPS.Debugger.get()

            v = GPS.MDI.input_dialog("Log signal %s in" % it.id, "filename")

            if v:
                filename = os.path.join(
                    Project_Support.get_output_dir(
                        ctx.file()), v[0] + '.html')
                if not CLI.delete_logfile_dialog(filename):
                    return

                QGEN_Module.log_values_from_item(
                    it.id, filename, ctx.file(), debug)

                QGEN_Module.__show_diagram_and_signal_values(debug,
                                                             force=True)

        def __contextual_disable_log_signal_value(self):
            ctx = GPS.contextual_context() or GPS.current_context()
            it = ctx.modeling_item
            debug = GPS.Debugger.get()

            QGEN_Module.stop_logging_value_for_item(it.id, debug)
            QGEN_Module.__show_diagram_and_signal_values(debug, force=True)

        def __contextual_set_persistent_signal_value(self):
            ctx = GPS.contextual_context() or GPS.current_context()
            it = ctx.modeling_item
            debug = GPS.Debugger.get()
            added = False

            for s in self.modeling_map.get_symbols(blockid=it.id):
                v = GPS.MDI.input_dialog(
                    "Persistent value for signal %s" % it.id,
                    "value=")
                if v:
                    added = True
                    debug.send("qgen_watchpoint %s %s" % (s, v[0]),
                               output=False)

            if added:
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)
                if sig_obj is None:
                    sig_obj = Signal(it.id)
                    QGEN_Module.signal_attributes[it.id] = sig_obj
                sig_obj.watched = True

                QGEN_Module.__show_diagram_and_signal_values(debug, force=True)

        def __contextual_disable_persistent_signal_value(self):
            ctx = GPS.contextual_context() or GPS.current_context()
            it = ctx.modeling_item
            debug = GPS.Debugger.get()
            removed = False

            for s in self.modeling_map.get_symbols(blockid=it.id):
                removed = True
                debug.send("qgen_delete_watchpoint %s" % s, output=False)

            if removed:
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)
                sig_obj.watched = False
                QGEN_Module.__show_diagram_and_signal_values(debug, force=True)

        def __contextual_set_breakpoint(self):
            """
            Set a breakpoint, in the current debugger, on the current block
            """
            ctx = GPS.contextual_context() or GPS.current_context()
            it = self.get_item_with_sources(ctx.modeling_item)
            debug = GPS.Debugger.get()

            # Create the breakpoints

            ranges = self.block_source_ranges(it.id)
            filename = None
            line = None
            if ranges:
                for file, rg in ranges:
                    filename = filename or file
                    line = line or rg[0]
                    # Set a breakpoint only on the first line of a range
                    debug.break_at_location(file, line=rg[0])

                # Force a refresh to show blocks with breakpoints
                QGEN_Module.__show_diagram_and_signal_values(
                    debug, force=True)

        def __contextual_delete_breakpoint(self):
            """
            Set a breakpoint, in the current debugger, on the current block
            """
            ctx = GPS.contextual_context() or GPS.current_context()
            it = self.get_item_with_sources(ctx.modeling_item)
            ranges = self.block_source_ranges(it.id)
            if ranges:
                debug = GPS.Debugger.get()
                for file, rg in ranges:
                    filename = file
                    line = rg[0]
                    debug.unbreak_at_location(file, line=line)

                QGEN_Module.__show_diagram_and_signal_values(
                    debug, force=True)

        def __contextual_show_source_code(self):
            """
            Show the first line of code for an item
            """
            ctx = GPS.contextual_context() or GPS.current_context()
            it = self.get_item_with_sources(ctx.modeling_item)
            init = False
            # Return the first line corresponding to that item that
            # is in the comp function, or if none is available
            # the first referenced line
            for file, rg in self.block_source_ranges(it.id):
                if not init:
                    line = rg[0]
                    init = True
                for f, b in QGEN_Module.modeling_map.get_file_funcinfos(
                        os.path.basename(file.path)):
                    if f.endswith('comp'):
                        for l in rg:
                            if l >= b[0] and l <= b[1]:
                                line = l
                                break
                        break

            if init:
                buffer = GPS.EditorBuffer.get(file)
                view = buffer.current_view()
                view.goto(buffer.at(line, 1))
                view.center()
                GPS.MDI.get_by_child(view).raise_window()

        def setup(self):
            """
            Initialize the support for modeling in GPS.
            This is only called when the necessary command line executables
            are found.
            """

            gps_utils.make_interactive(
                callback=CLI.workflow_compile_context_to_source_code,
                name='MDL generate code for file',
                category='QGen',
                filter=CLI.is_model_file,
                contextual='Generate code for %f')

            gps_utils.make_interactive(
                callback=CLI.workflow_compile_project_to_source_code,
                name='MDL generate code for whole project',
                category='QGen')

            # ??? Should work when no debugger is currently running
            gps_utils.make_interactive(
                name='MDL break debugger on block',
                contextual=self.__contextual_name_for_break_on_block,
                filter=self.__contextual_filter_debug_and_sources,
                callback=self.__contextual_set_breakpoint)

            gps_utils.make_interactive(
                name='MDL delete breakpoints on block',
                contextual=self.__contextual_name_for_unbreak_on_block,
                filter=self.__contextual_filter_debug_and_sources,
                callback=self.__contextual_delete_breakpoint)

            gps_utils.make_interactive(
                name='MDL set signal value',
                contextual='Debug/Set value for signal',
                filter=self.__contextual_filter_debug_and_symbols,
                callback=self.__contextual_set_signal_value)

            gps_utils.make_interactive(
                name='MDL set persistent signal value',
                contextual='Debug/Set persistent value for signal',
                filter=self.__contextual_filter_debug_and_symbols,
                callback=self.__contextual_set_persistent_signal_value)

            gps_utils.make_interactive(
                name='MDL log signal value',
                contextual='Debug/Log this signal',
                filter=self.__contextual_filter_debug_and_symbols,
                callback=self.__contextual_log_signal_value)

            gps_utils.make_interactive(
                name='MDL disable persistent signal value',
                contextual='Debug/Disable persistent value for signal',
                filter=self.__contextual_filter_debug_and_watchpoint,
                callback=self.__contextual_disable_persistent_signal_value)

            gps_utils.make_interactive(
                name='MDL disable log signal value',
                contextual='Debug/Stop logging this signal',
                filter=self.__contextual_filter_debug_and_logpoint,
                callback=self.__contextual_disable_log_signal_value)

            gps_utils.make_interactive(
                callback=CLI.action_goto_parent_subsystem,
                name='MDL goto parent subsystem',
                category='Browsers',
                filter=self.__contextual_filter_viewer_active,
                icon='gps-upward-symbolic')

            gps_utils.make_interactive(
                callback=CLI.action_goto_previous_subsystem,
                name='MDL goto previous subsystem',
                category='Browsers',
                filter=self.__contextual_filter_viewer_active,
                icon='gps-backward-symbolic')

            gps_utils.make_interactive(
                callback=CLI.stop_logging_subsystem_values,
                name='Stop logging subsystem values',
                category='Browsers',
                filter=self.__contextual_filter_debugger_active,
                icon='gps-stop-save-symbolic')

            gps_utils.make_interactive(
                callback=CLI.log_subsystem_values,
                name='Log subsystem values',
                category='Browsers',
                filter=self.__contextual_filter_debugger_active,
                icon='gps-save-symbolic')

            gps_utils.make_interactive(
                name='MDL show source for block',
                contextual='Models/Show source code',
                filter=self.__contextual_filter_sources,
                callback=self.__contextual_show_source_code)

            workflows.create_target_from_workflow(
                parent_menu='/Build/MDL generate & build/',
                target_name="MDL Generate code then build",
                workflow_name="generate-from-mdl-then-build",
                workflow=CLI.workflow_generate_from_mdl_then_build,
                icon_name="gps-build-mdl-symbolic")

            workflows.create_target_from_workflow(
                parent_menu='/Build/MDL generate, build & debug/',
                target_name="MDL Generate code then build then debug",
                workflow_name="generate-from-mdl-then-build-then-debug",
                workflow=CLI.workflow_generate_from_mdl_then_build_then_debug,
                icon_name="gps-qgen-debug-symbolic",
                in_toolbar=True)

    module = QGEN_Module()
