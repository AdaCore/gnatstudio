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
import gps_utils
import gpsbrowsers
import modules
import os
import os.path
import os_utils
import sys
import re
import workflows
import constructs
from workflows.promises import Promise, TargetWrapper, timeout
from project_support import Project_Support
from sig_utils import Signal


logger = GPS.Logger('MODELING')


class MDL_Language(GPS.Language):
    """
    A class that describes the MDL and Simulink language for GPS.
    """
    const_split = re.compile('#QGEN.*#')
    # The constant string used to create a construct id
    const_id = "#QGEN"

    # @overriding
    def __init__(self):
        pass

    @staticmethod
    def register():
        """Add support for the Simulink language"""
        GPS.Language.register(
            MDL_Language(),
            name="Simulink",
            body_suffix=".mdl",
            spec_suffix=".slx")

        GPS.Language.register(
            MDL_Language(),
            name="QGen",
            body_suffix=".xmi")
    # @overriding

    def should_refresh_constructs(self, file):
        """
        If we did not generate constructs for the model yet, parse the
        file asynchronously, otherwise use the stored constructs
        """

        flat = GPS.Preference('outline-flat-view').get()
        logger.log("Asking if refreshing constructs")

        @workflows.run_as_workflow
        def workflow_gen_constructs(viewer):
            logger.log("Starting constructs parsing")
            workflows.task_workflow(
                "Creating outline for %s" % file.name(),
                parse_model_tree, viewer=viewer)

        @workflows.run_as_workflow
        def fill_tree(task, viewer, it_name, it_id,
                      sloc_start, sloc_end, type, idx, offset, task_max):
            c_id = "{0}{1}{2}#{3}".format(
                it_name, MDL_Language.const_id, sloc_end[-1], it_id)
            c_name = it_name if flat else Diagram_Utils.block_split(
                it_name, count=1, backward=True)[-1]
            viewer.constructs.append(
                (c_name, c_id, sloc_start, sloc_end, type, it_id))
            viewer.constructs_map[c_id] = (c_name, sloc_start,
                                           sloc_end, type, it_id)
            task.set_progress(idx, task_max)

        def parse_model_tree(task, viewer):
            GPS.Console().write(
                "Generating outline view for the model %s...\n" % viewer.file)
            id, val = viewer.diags.index[0]
            idx = 0
            offset = 1
            for it_name, it_id, sloc_start, sloc_end, type, task_max in\
                    process_item(viewer):
                idx += 1
                offset = max(offset, sloc_end[2]) + 1
                yield fill_tree(task, viewer, it_name, it_id, sloc_start,
                                sloc_end, type, idx, offset, task_max)
            viewer.parsing_done()
            GPS.Console().write("Outline view generated\n")
            GPS.Hook('file_edited').run(file)

        def process_item(viewer):
            """
            Return an item and its simulated sloc.
            The source locations are simulated so that nesting can be computed
            automatically by GPS based on the line/column info. GPS uses
            indexes when they are positive.

            :return: (item, sloc_start, sloc_end, constructs_CAT, items_len)
            """
            offset = 0
            item, children = viewer.diags.index[0]
            items_len = len(children)
            item_stack = [(item, item, children, 0)]
            item, item_id, children, start_offset = item_stack[-1]
            # The index entry contains a JSON_Array of entries with
            # a 'name' and 'diagram' fields. They respectively correspond
            # to the simulink name of the item and its corresponding JSON id
            while item_stack:
                children_cpy = list(children)
                for child in children_cpy:
                    child_name = child["name"]
                    child_id = child["diagram"]
                    for child_entry_name, child_children in viewer.diags.index:
                        # Look for the child diagram in the index table
                        if child_entry_name == child_id:
                            offset = offset + 1
                            items_len += len(child_children)
                            item_stack.append((child_name, child_id,
                                               list(child_children), offset))
                            break
                    children.remove(child)
                    # If the subsystem has no child, add the construct as a
                    # leaf
                    if not item_stack[-1][2]:
                        it, it_id, _, start_offset = item_stack.pop()
                        offset = offset + 1
                        yield (it, it_id, (0, 0, start_offset),
                               (0, 0, offset), constructs.CAT_CLASS, items_len)
                    else:
                        break

                item, item_id, children, start_offset = item_stack[-1]

                # If all the children of that subsystem have been processed add
                # the containing subsystem construct
                while not children:
                    offset = offset + 1
                    yield (item, item_id, (0, 0, start_offset),
                           (0, 0, offset), constructs.CAT_CLASS, items_len)
                    item_stack.pop()

                    if item_stack:
                        item, item_id, children, start_offset = item_stack[-1]
                    else:
                        break

        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer_for_file(file)

        if viewer and viewer.loading_complete and not viewer.parsing_complete\
           and viewer.diags and not viewer.constructs:
            logger.log("Creating constructs for outline of %s" % file.name)
            workflow_gen_constructs(viewer)
        return True

    # @overriding
    def get_last_selected_construct_id(self, file):
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer_for_file(file)
        if viewer is not None:
            return viewer.selected_construct_id()
        return None

    # @overriding
    def clicked_on_construct(self, construct):
        def __on_loaded(view):
            _, _, sloc, _, construct_id = view.constructs_map[construct.id]
            diag = view.diags.get(construct_id)
            # If the construct was a diagram, open it
            # otherwise highlight the item
            if diag.id == construct_id:
                try:
                    sloc = sloc[-1]
                    view.update_nav_status(construct.id)
                except ValueError:
                    logger.log(
                        "Malformed construct id : %s, not added to history" %
                        construct.id)
                finally:
                    view.set_diagram(diag)
                    logger.log('Set diagram for construct : %s' % diag.id)
            else:
                info = QGEN_Module.modeling_map.get_diagram_for_item(
                    view.diags, construct_id)
                if info:
                    diagram, item = info
                    view.diags.clear_selection()
                    diagram.select(item)
                    view.set_diagram(diagram)
                    view.scroll_into_view(item)

        QGEN_Diagram_Viewer.get_or_create_from_model(
            construct.file, on_loaded=__on_loaded)

    # @overriding
    def parse_constructs(self, clist, file, file_contents):
        """
        Provides support for the Outline view
        """

        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer_for_file(file)
        if not viewer:
            return

        def add_construct(c_name, c_id, sloc_start, sloc_end, type):
            clist.add_construct(
                category=type,
                is_declaration=True,
                visibility=constructs.VISIBILITY_PUBLIC,
                name=c_name,
                profile='',
                # We combine the name of the block with its id and a #QGEN#
                # string to both create a unique id (it_name is unique) and
                # be able to retrieve the id to display the correct diagram
                # when the construct is clicked (as long as the name did
                # not contain #QGEN# already, which is unlikely).
                id=c_id,
                sloc_start=sloc_start,
                sloc_end=sloc_end,
                sloc_entity=sloc_start)

        if viewer.constructs and not viewer.loading_complete:
            clist.clear
            logger.log("Adding constructs to outline")
        for c_name, c_id, sloc_start, sloc_end, type, _ in viewer.constructs:
            add_construct(c_name, c_id, sloc_start, sloc_end, type)
        GPS.Hook('semantic_tree_updated').run(file)


class CLI(GPS.Process):
    """
    An interface to the qgenc executable. This is responsible for
    converting an mdl file to a JSON format that can be displayed by GPS.
    """

    qgenc = os_utils.locate_exec_on_path('qgenc')
    plugins_dir = None
    # path to qgenc

    @staticmethod
    def is_available():
        """
        Whether qgenc is available on the system.
        """
        # Testing None or empty string
        if CLI.qgenc:
            # Check that the python modules are stored in the qgen directory
            CLI.plugins_dir = os.path.join(
                os.path.dirname(os.path.dirname(CLI.qgenc)),
                'libexec', 'qgen', 'share', 'gps', 'plug-ins', 'qgen')
            has_qgen_modules = os.path.exists(os.path.join(
                CLI.plugins_dir, 'mapping.py')) and os.path.exists(
                    os.path.join(CLI.plugins_dir, 'diagram_utils.py'))
            if not has_qgen_modules:
                logger.log(
                    'mapping.py and/or diagram_utils.py not '
                    'found in the qgen installation')
            else:
                return True
        return False

    @staticmethod
    def get_qgenc_switches(file, extra=[], remove_list=[]):
        """
        Creates the necessary arguments to call qgenc on the
        model file, using those defined in the project file
        if possible.
        :param GPS.File file: The model file to generate arguments for.
        :param str list extra: Extra arguments to add to the switches.
        :param (str, bool) list remove_list: Arguments to remove if found,
        associated with a boolean specifying whether they have a positional
        argument as well.
        :return str: The string containing the cli arguments for qgenc.
        """
        def remove_arg(l, val, positional):
            res = []
            skip = False
            for arg in l:
                if arg == val:
                    skip = positional
                    continue
                if skip:
                    skip = False
                else:
                    res.append(arg)
            return res

        switches = ""

        if CLI.is_model_file(file):
            outdir = Project_Support.get_output_dir(file)
            switches = Project_Support.get_switches(file)

            # always ignoring -o and --output should be defined
            # using the Output_Dir attribute instead
            remove_list.extend([('-o', True), ('--output', True)])
            for arg, has_param in remove_list:
                switches = remove_arg(switches, arg, has_param)

            if os.path.splitext(file.path)[1] == ".xmi":
                extra.extend(['--pre-process-xmi'])
            elif '--typing' not in switches and '-t' not in switches:
                typing_file = os.path.splitext(file.path)[0] + '_types.txt'
                extra.extend(['-t', typing_file])
            extra.extend(['-o', outdir])
            switches.extend(extra)
            return ' '.join(switches)

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

        switches = CLI.get_qgenc_switches(
            file, extra=['--with-gui-only', '--incremental', "--no-misra"],
            remove_list=[('-c', False), ('--clean', False)])
        if os.path.splitext(filepath)[1] == ".xmi":
            switches += " --pre-process-xmi"
        outdir = Project_Support.get_output_dir(file)
        result_path = os.path.join(
            outdir, '.qgeninfo',
            Diagram_Utils.get_diagram_hash(
                os.path.splitext(os.path.basename(filepath))[0]) + '.qmdl')

        logger.log("Looking for diagram %s" % result_path)

        if os.path.isfile(result_path):
            logger.log("Diagram %s exists" % result_path)
            # If the mdl file is older than the previous json file no need
            # to recompute
            diagram_dt = os.path.getmtime(result_path)
            mdl_dt = os.path.getmtime(filepath)
            if diagram_dt >= mdl_dt:
                promise.resolve(result_path)
                return promise
            logger.log("Model file has been modified recently,"
                       "{0} >= {1}  recomputing diagram info".format(
                           mdl_dt, diagram_dt))

        cmd = ' '.join(
            [CLI.qgenc, filepath, switches])

        def __on_exit(proc, exit_status, output):
            if exit_status == 0:
                promise.resolve(result_path)
            else:
                GPS.Console().write('When running qgenc for gui: %s\n' % (
                    output), mode='error')
                promise.reject()

        # qgenc is relatively fast, and since the user is waiting for
        # its output to see the diagram, we run in active mode below.
        GPS.Console().write("Generating diagram file, please wait...\n")
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
            return f.language() == 'simulink' or f.language() == 'qgen'
        except Exception:
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
                switches = (f.path + ' ' + CLI.get_qgenc_switches(
                    f, extra=['--with-gui', '--trace']))
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
        Displays the previous subsystem.
        """
        # Retrieves the focused QGen Diagram viewer and get the previous
        # diagram from the history list.
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            diag_name = viewer.get_prev_diag()
            if diag_name is not None:
                new_diag = viewer.diags.get(diag_name)
                viewer.set_diagram(new_diag)

    @staticmethod
    def action_goto_parent_subsystem():
        """
        Displays the parent subsystem.
        """
        # Retrieves the focused QGen Diagram viewer and changes the
        # diagram to display based on the `parent` field of the
        # `qgen_navigation_info` item of the current diagram or if non
        # existent retrieves it from the history list.
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            diag_name = viewer.get_parent_diag()
            if diag_name is not None:
                new_diag = viewer.diags.get(diag_name)
                viewer.set_diagram(new_diag)

    @staticmethod
    def action_scale_to_fit():
        """
        Scales the diagram to the window size.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            viewer.scale_to_fit()

    @staticmethod
    def action_zoom_in():
        """
        Zoom in.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            GPS.execute_action("browser zoom in")

    @staticmethod
    def action_zoom_out():
        """
        Zoom out.
        """
        viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
        if viewer:
            GPS.execute_action("browser zoom out")


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

    def __init__(self):
        # The set of callbacks to call when a new diagram is displayed
        super(QGEN_Diagram_Viewer, self).__init__()
        self.constructs = []

        # construct_id => (c_name, sloc_start, sloc_end, type, it_id)
        # it_id is a diagram name, c_name is the instance name for the diagram
        self.constructs_map = {}
        self.loading_complete = False
        self.parsing_complete = False

        self.root_diag_id = ""
        self.last_dblclicked = ""
        # A construct or None list, used for parent browsing
        self.nav_status = []
        self.nav_index = -1
        # A list keeping track of the diagram browsed before
        # outline was loaded
        self.pre_load_browsing = []

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
    def retrieve_active_qgen_viewer_for_file(f):
        """
        Returns the qgen viewer for the file f if possible
        :param f: a GPS.File that should correspond to the active viewer.
        :return: a QGEN_Diagram_Viewer instance
        """
        try:
            for win in GPS.MDI.children():
                if hasattr(win, '_gmc_viewer'):
                    v = win._gmc_viewer
                    if v.file == f:
                        return v
            return None
        except Exception:
            return None

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
        except Exception:
            return None

    @staticmethod
    def get_or_create_view(file, raise_win=True):
        """
        Get an existing viewer for file, or create a new empty view.
        :return: (view, newly_created)
        """
        for win in GPS.MDI.children():
            if hasattr(win, '_gmc_viewer'):
                v = win._gmc_viewer
                if v.file == file:
                    if raise_win:
                        win.raise_window()
                    return (v, False)

        v = QGEN_Diagram_Viewer()
        v.file = file
        v.diags = None  # A JSON_Diagram_File acting as the container of
        # diagrams and index
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
    @workflows.run_as_workflow
    def launch_diagram_loading_task(viewer, jsonfile, style, on_loaded=None):
        """
        Loads diagrams from the root jsonfile into the QGen_Diagram_Viewer
        viewer object.
        The style object is a JSON data representing styles information
        defined in GPS.Browsers.Styles object used to skip Styles parsing
        when possible (i.e. styles are copied from an existing diagram).
        The on_loaded callback is called after the loading is finished if it
        exists.
        """

        logger.log("Lauching background diagram loading")
        workflows.task_workflow(
            'Loading referenced diagrams',
            QGEN_Diagram_Viewer.load_all_referenced_diagrams,
            viewer=viewer, jsonfile=jsonfile, style=style, on_loaded=on_loaded)

    @staticmethod
    def load_all_referenced_diagrams(task, viewer, jsonfile, style, on_loaded):
        json_dir = os.path.dirname(jsonfile)
        logger.log("Loading referenced diagrams from %s" % (jsonfile))
        idx = 0
        task_max = 0
        for _, children in viewer.diags.index:
            task_max += len(children)
            for child in children:
                diag_to_load = child["diagram"]
                logger.log("Searching diagram %s" % diag_to_load)
                yield QGEN_Diagram_Viewer.load_referenced_diagram(
                    viewer, diag_to_load, json_dir, style)
                idx += 1
                task.set_progress(idx, task_max)
        viewer.loading_complete = True

        if on_loaded:
            on_loaded(viewer)

        MDL_Language().should_refresh_constructs(viewer.file)
        GPS.Hook('file_edited').run(viewer.file)

    @staticmethod
    @workflows.run_as_workflow
    def load_referenced_diagram(viewer, diag_to_load, json_dir, style):
        # We loaded the first diagram as the one requested was not
        # found so we need to retrieve it from the directory
        if not viewer.diags.contains(diag_to_load):
            f = os.path.join(json_dir, Diagram_Utils.get_diagram_hash(
                diag_to_load) + '.qmdl')
            if os.path.exists(f):
                logger.log("Loading contained %s" % f)
                loaded_diag = GPS.Browsers.Diagram.load_json(
                    open(f), diagramFactory=QGEN_Diagram,
                    load_styles=style)
                logger.log("Done loading")
                viewer.diags.index.extend(loaded_diag.index)
                viewer.diags.diagrams.extend(loaded_diag.diagrams)

    @staticmethod
    def get_or_create_from_model(model, on_loaded=None):
        """
        Find an existing viewer that can display the model
        requested or create a new viewer for it.
        :param string diag: The id of the diagram to display
        :param callable on_loaded: called when the diagram is loaded, or
           immediately if the diagram was already loaded. The function
           receives one parameter:
               - the viewer itself
        """
        model_id = os.path.basename(os.path.splitext(model.path)[0])
        logger.log("Searching viewer containing diagram %s" % model_id)
        for viewer in QGEN_Diagram_Viewer.retrieve_qgen_viewers():
            if viewer.diags.contains(model_id):
                on_loaded(viewer)
                logger.log("Found viewer of %s" % viewer.file)
                return viewer

        logger.log("Creating a new viewer")
        return QGEN_Diagram_Viewer.get_or_create_as_root(model, on_loaded)

    @staticmethod
    def get_or_create_as_root(file, on_loaded=None):
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
                    QGEN_Diagram_Viewer.launch_diagram_loading_task(
                        v, jsonfile, v.diags.styles, on_loaded)
                    root_diag = v.diags.get()
                    v.set_diagram(root_diag)
                    v.root_diag_id = root_diag.id
                    v.update_preloading_nav(root_diag)

            def __on_fail(reason):
                pass

            CLI.get_json(file).then(__on_json, __on_fail)

        else:
            if on_loaded:
                on_loaded(v)

        GPS.Hook('file_edited').run(file)
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

    def update_nav_status(self, construct_id):
        """
        Insert the current construct selected in this viewer history.
        :param str construct_id: A construct id to add to the history.
        """
        if construct_id != "":
            self.nav_index += 1
            self.nav_status.insert(self.nav_index, construct_id)

    def update_preloading_nav(self, diag):
        self.nav_index += 1
        self.pre_load_browsing.insert(self.nav_index, diag.id)

    def highlight_gps_construct(self, c):
        """
        Highlight the construct in the outline view.
        :param str c: a construct id to highlight
        """
        try:
            GPS.OutlineView.select_construct(c)
        except GPS.Exception:
            # The outline was not focused on the diagram, discard the exception
            pass

    def parsing_done(self):
        """
        Called when constructs have been parsed, stores the root construct
        in the history.
        """
        self.parsing_complete = True
        logger.log("Parsing done, status is %d %s" % (self.nav_index,
                                                      self.nav_status))
        if self.nav_index > -1:
            for i in range(0, self.nav_index + 1):
                # The user browsed diagrams using double click and
                # previous/parent, we need to recreate this history.
                # First we create a reverse list for each path to
                # a diagram
                path_l = self.pre_load_browsing[:i+1]
                c_id = self.get_construct_from_list(path_l, (-1, None))
                self.nav_status.insert(i, c_id)
        else:
            self.nav_index += 1
            self.nav_status.insert(0, self.constructs[-1][1])

    def selected_construct_id(self):
        """
        :return str: The last construct selected.
        """
        logger.log("Last construct index is %d from %s\n" % (
            self.nav_index, str(self.nav_status)))
        if self.parsing_complete and self.nav_index >= 0:
            return self.nav_status[self.nav_index]
        return None

    def get_prev_diag(self):
        """
        Select the previous diagram in the outline and returns its id.
        :return str: The previous diagram id.
        """
        if self.nav_index <= 0:
            return None

        self.nav_index -= 1
        if self.parsing_complete:
            construct = self.nav_status[self.nav_index]
            self.highlight_gps_construct(construct)
            return self.constructs_map[construct][-1]
        else:
            return self.pre_load_browsing[self.nav_index]

    def get_construct_from_list(self, l, sloc_range=(-1, None), res=""):
        """
        Given a list of diagram ids representing a path in the model, find
        the closest related construct.
        :param str list l: A list of diagram ids.
        :param sloc_range: The range of constructs location to consider.
        :return str: The construct id or "" if not found.
        """
        if not l:
            return ""

        start_sloc = sloc_range[0]
        end_sloc = sloc_range[1]
        last_child_sloc = start_sloc
        name = l[0]

        for _, cons_id, sloc_start, sloc_end, _, d_id in self.constructs:
            if sloc_start[-1] > start_sloc and (
                    end_sloc is None or sloc_end[-1] < end_sloc):

                if sloc_start[-1] == last_child_sloc + 1:
                    last_child_sloc = sloc_end[-1]
                    if name == d_id:
                        if len(l) == 1:
                            return cons_id
                        else:
                            return self.get_construct_from_list(
                                l[1:], (sloc_start[-1], sloc_end[-1]), cons_id)

        return res

    def get_construct_for_dblclick(self):
        """
        Returns the construct corresponding to the last dlbcliked diagram.
        :return string: The construct id for the dblclicked item.
        """
        logger.log("Looking for construct %d from %s\n" % (
            self.nav_index, str(self.nav_status)))
        if self.parsing_complete and self.nav_index >= 0:
            current_construct = self.constructs_map[
                self.nav_status[self.nav_index]]
            start_sloc = current_construct[1][-1]
            last_child_sloc = start_sloc
            item_len = len(self.last_dblclicked)
            idx = item_len
            res = ""

            for d_name, cons_id, sloc_start, sloc_end, _, _ in self.constructs:
                if sloc_start[-1] == last_child_sloc + 1:
                    last_child_sloc = sloc_end[-1]
                    if self.last_dblclicked.endswith(d_name):
                        n_idx = item_len - len(d_name)
                        logger.log("Supposing construct %s for %s" % (
                            d_name, self.last_dblclicked))
                        if n_idx < idx and self.last_dblclicked[
                                n_idx - 1] == '/':
                            res = cons_id
                            idx = n_idx
            return res

    def get_parent_diag(self, update_history=True):
        """
        Returns the parent diagram of the current diagram and updates
        the selected constructs and history if necessary.
        :param boolean update_history: Whether the history should be modified
        when returning the parent diagram.
        """
        if self.nav_index >= 0:
            if not self.parsing_complete:
                self.nav_index -= 1
                return self.pre_load_browsing[self.nav_index]
            construct_id = self.nav_status[self.nav_index]
            if construct_id not in self.constructs_map:
                GPS.Console().write("Outline not loaded, please wait...\n")
                return None
            _, _, sloc, _, diag_name = self.constructs_map[construct_id]
            parent = diag_name
            sloc = sloc[-1]
            # The block was visited by clicking on a construct
            # and is library block that can be referenced multiple
            # times, find out the parent by browsing the construct_list

            # Find the construct with sloc_start < sloc
            # and sloc_end > sloc with sloc_start - sloc smallest
            # which correspond to the encapsulating construct
            current_sloc = -1
            for _, cons_id, sloc_start, sloc_end, _, it_id in self.constructs:
                if sloc_start[-1] < sloc and \
                   sloc_start[-1] - sloc > current_sloc - sloc and \
                   sloc_end[-1] > sloc:
                    current_sloc = sloc_start[-1]
                    parent = it_id
                    parent_cons = cons_id

            if parent != diag_name:
                if update_history:
                    self.highlight_gps_construct(parent_cons)
                    self.nav_index += 1
                    self.nav_status.insert(self.nav_index, parent_cons)
                return parent
            return None

    def get_existing_diagram(self, diag_id, showerror=False):
        res = self.diags.get(diag_id)
        if res.id != diag_id:
            if showerror:
                GPS.Console().write(
                    "%s was not found, " % diag_id)
                if not self.parsing_complete:
                    GPS.Console().write(
                        "wait for the loading to complete...\n")
                else:
                    GPS.Console().write(
                        " it might not be supported by the debugger.\n")
            return None
        return res

    def save_desktop(self, child):
        """Save the contents of the viewer in the desktop"""
        info = {'file': self.file.path}
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
            diag = self.get_existing_diagram(args[0], showerror=True)
            if diag is not None:
                self.set_diagram(diag, is_dblclick=True)

    def set_diagram(self, diag, is_dblclick=False):
        """
        Sets the diagram that has to be displayed and calls
        the callbacks that have to be executed when the diagram changes
        If diag is None a refresh on the current diagram is performed
        :param diag: The new diagram to display
        :param update_prev: If true, store the previous subsystem
        for navigation purposes
        """
        QGEN_Module.cancel_workflows()
        if diag and diag != self.diagram:
            if is_dblclick:
                c_id = self.get_construct_for_dblclick()
                if c_id is not None:
                    self.highlight_gps_construct(c_id)
                    self.update_nav_status(c_id)
                elif not self.parsing_complete:
                    self.update_preloading_nav(diag)
            self.diagram = diag
            self.scale_to_fit(2)

        QGEN_Module.on_diagram_changed(self, self.diagram)

    # @overriding
    def on_item_double_clicked(self, topitem, item, x, y, *args):
        """
        Called when the user double clicks on an item.
        """
        action = topitem.data.get('dblclick')
        if action:
            if hasattr(item, 'id'):
                self.last_dblclicked = item.id
                self.perform_action(action, topitem)

    # @overriding
    def on_item_clicked(self, topitem, item, x, y, *args):
        def is_link(i):
            return hasattr(i, "data") and i.data.get('auto') == "true"

        if is_link(item):
            QGEN_Module.display_variable_in_view(item)
        else:
            for it in item.recurse():
                if is_link(it):
                    QGEN_Module.display_variable_in_view(item)
                    return

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
    logger.log('QGen Debugger not found')

else:
    sys.path.append(CLI.plugins_dir)
    import mapping
    from diagram_utils import Diagram_Utils

    Project_Support.register_tool()

    class AsyncDebugger(object):
        __query_interval = 5

        def __init__(self, debugger):
            self._debugger = debugger
            self._this_promise = None
            self._symbol = None
            self._output = None
            self._timer = None
            self._deadline = None

        def _is_busy(self, timeout):
            """
            Called by GPS at each interval.
            """

            # if the debugger is not busy
            if not self._debugger.is_busy():

                # remove all timers
                self._remove_timers()

                # and if there's cmd to run, send it
                if self._symbol is not None:

                    if self._symbol is not "":
                        self._output = self._debugger.value_of(self._symbol)
                        self._symbol = None
                        self._remove_timers()
                        self._this_promise.resolve(self._output)

                    # "" cmd are default value when making promise,
                    # it's also a maker for pure checker
                    else:
                        self._this_promise.resolve(True)

        def _on_cmd_timeout(self, timeout):
            """
            Called by GPS at when the deadline defined by user is reached
            """

            # remove all timers
            self._remove_timers()

            # answer the promise with the output
            if self._this_promise:
                self._symbol = None
                self._this_promise.resolve(self._output)

        def _remove_timers(self):
            """
            Called in timers to remove both: prepare for new timer registration
            """
            if self._deadline:
                try:
                    self._deadline.remove()
                except Exception:
                    pass
                self._deadline = None

            if self._timer:
                try:
                    self._timer.remove()
                except Exception:
                    pass
                self._timer = None

        def async_print_value(self, symbol, timeout=0, block=False):
            """
            Called by user on request for command within deadline (time)
            Promise returned here will be answered with: output

            This method may also function as a pure block-debugger-and-wait-
            until-not-busy call, when block=True.
            Promise returned for this purpose will be answered with: True/False
            """

            self._this_promise = Promise()
            self._symbol = symbol
            self._output = None

            self._timer = GPS.Timeout(self.__query_interval, self._is_busy)

            # only register deadline for real command waiting
            if not block:
                if timeout > 0:
                    self._deadline = GPS.Timeout(timeout, self._on_cmd_timeout)
            return self._this_promise

    class QGEN_Module(modules.Module):

        display_tasks = []
        modeling_map = None   # a Mapping_File instance
        models = []  # List of model files in the project
        # id => Signal object
        signal_attributes = {}

        previous_breakpoints = []
        debugger = None

        @staticmethod
        def load_debug_info_for(f, d=None):
            if QGEN_Module.modeling_map is None:
                QGEN_Module.modeling_map = mapping.Mapping_File()

            if f is not None:
                logger.log("Loading debug information for %s" % f.path)
            else:
                logger.log("Loading debug information from %s" % d)
            QGEN_Module.modeling_map.load(
                f, d if d is not None else Project_Support.get_output_dir(f))

        @staticmethod
        def get_model_for_source(file):
            """
            Looks into the models loaded for the project and retrieves
            the model file that generated the file given as a parameter.
            Returns None if no such file was found.
            """
            if QGEN_Module.modeling_map is not None:
                m_id = QGEN_Module.modeling_map.get_mdl_id(file)
                for m in QGEN_Module.models:
                    if os.path.splitext(os.path.basename(m.path))[0] == m_id:
                        return m
            return None

        @staticmethod
        def cancel_workflows():
            for t in QGEN_Module.display_tasks:
                logger.log("Canceling running display workflows\n")
                t.interrupt()
            QGEN_Module.display_tasks = []

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
            out_dirs = set()
            for f in GPS.Project.root().sources(recursive=True):
                if CLI.is_model_file(f):
                    QGEN_Module.models.append(f)
                    out_dirs.add(Project_Support.get_output_dir(f))

            for d in out_dirs:
                QGEN_Module.load_debug_info_for(None, d)

        @staticmethod
        def __clear(debugger):
            """
            Resets the diagram display when a new debugger is used
            """
            # Starting the debugger kills running workflows
            QGEN_Module.cancel_workflows()
            for id, sig in QGEN_Module.signal_attributes.iteritems():
                sig.reset()

            for viewer in QGEN_Diagram_Viewer.retrieve_qgen_viewers():
                viewer.diagram.clear_selection()
                QGEN_Module.on_diagram_changed(
                    viewer, viewer.diagram, clear=True)

            QGEN_Module.previous_breakpoints = debugger.breakpoints
            QGEN_Module.signal_attributes.clear()
            del QGEN_Module.previous_breakpoints[:]

        @staticmethod
        def get_item_parent_to_display(item):
            """
            Returns the parent of an item that is displayed, which
            means that it contains the label
            """
            item_parent = item
            p = item.parent
            while p is not None:
                item_parent = p
                p = item_parent.parent
            return item_parent

        @staticmethod
        def clear_all_item_values(diagram):
            """
            Clears the signal values displayed on a diagram
            """

            for _, _, it in Diagram_Utils.forall_auto_items(
                    [diagram]):
                item_parent = QGEN_Module.get_item_parent_to_display(it)
                item_parent.hide()
            diagram.changed()

        @staticmethod
        def compute_all_item_values(task, debugger, diagram, viewer):
            # Compute the value for all items with an "auto" property
            QGEN_Module.display_tasks.append(task)
            auto_items_list = list(Diagram_Utils.forall_auto_items(
                [diagram]))
            auto_items_len = len(auto_items_list)
            idx = 0
            for diag, toplevel, it in \
                    Diagram_Utils.forall_auto_items([diagram]):
                yield QGEN_Module.compute_item_values(
                    debugger, diag, toplevel=toplevel, item=it)
                diagram.changed()
                idx = idx + 1
                task.set_progress(idx, auto_items_len)
            QGEN_Module.display_tasks.remove(task)

        @staticmethod
        def get_var_from_item(debugger, item):
            """
            Returns the variable name corresponding to the given item
            if possible.
            """
            symbols = QGEN_Module.modeling_map.get_symbols(blockid=item.id)
            # The list of symbols to compute from the debugger
            if symbols:
                frames = debugger.frames()
                cur_frame = None
                ret = None
                if frames:
                    cur_frame = frames[0][2]

                for s in symbols:
                    # Signals can have a symbol that is a function call
                    # Those won't have a '/' in the name, discard them as
                    # we are looking for variables here.
                    if '/' in s:
                        inf = s.rsplit('/', 1)
                        # Sometimes gdb adds info in the func name, remove it
                        context = inf[0].split(' ', 1)[0]
                        ret = inf[-1].strip()
                        if context == cur_frame:
                            return ret
                        # Check that the context matches the current
                        # debugger frame. If no matching frame is found we
                        # return the last symbol (should empirically be the
                        # correct one).
                return ret
            return None

        @staticmethod
        def display_variable_in_view(item):
            """
            Adds a variable to the Variables view corresponding
            to the given item if possible.
            """
            try:
                debugger = GPS.Debugger.get()
            except Exception:
                return
            ss = QGEN_Module.get_var_from_item(debugger, item)
            if ss is not None:
                debugger.send("tree display %s\n" % ss, output=False)

        @staticmethod
        @workflows.run_as_workflow
        def compute_item_values(debugger, promise, toplevel, item):
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

            item_parent = QGEN_Module.get_item_parent_to_display(item)

            def update_item_value(value):
                # Skip case when the variable is unknown
                if value is None or "":
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

            # Find the parent with an id. When item is the label of a link, the
            # parent will be set to None, so we default to toplevel (the link,
            # in that case)
            parent = item.get_parent_with_id() or toplevel
            logger.log("Computing value from link for %s " % parent.id)
            ss = QGEN_Module.get_var_from_item(debugger, parent)
            logger.log("Associating it to variable %s " % str(ss))
            if ss is not None:
                async_debugger = AsyncDebugger(debugger)
                yield async_debugger.async_print_value(ss).then(
                    update_item_value)
            else:
                item_parent.hide()

        @staticmethod
        @workflows.run_as_workflow
        def on_diagram_changed(viewer, diag, clear=False):
            """
            Called whenever a new diagram is displayed in viewer. This change
            might have been triggered either by the user or programmatically.
            :param viewer: a QGEN_Diagram_Viewer
            :param diag: a QGEN_Diagram
            :param clear: a boolean which is true when we are clearing
            the diagram for a new debugger instance
            """

            try:
                debugger = GPS.Debugger.get()
            except Exception:
                debugger = None

            QGEN_Module.update_bp_labels(viewer, diag)

            # If the debugger is not started, is not running, or is
            # not stopped at an execution point, we do not display any
            # signal value.
            if debugger is None or debugger.current_frame() == -1:
                logger.log("Clearing no debugger running %s" %
                           (debugger.current_frame() if debugger
                            is not None else "No debugger"))
                QGEN_Module.clear_all_item_values(diag)
            else:
                count = 0
                while debugger.is_busy():
                    if count > 20:
                        break
                    logger.log("Busy debugger")
                    count = count + 1
                    yield timeout(100)

                if debugger.is_busy():
                    logger.log("Clearing diagram because debugger busy")
                    QGEN_Module.clear_all_item_values(diag)
                else:
                    logger.log("Computing item values")

                    # Compute the value for all items with an "auto" property
                    if QGEN_Module.display_tasks:
                        QGEN_Module.cancel_workflows()
                        while QGEN_Module.display_tasks:
                            yield timeout(100)

                    workflows.task_workflow(
                        'Updating signal values',
                        QGEN_Module.compute_all_item_values,
                        debugger=debugger, diagram=diag, viewer=viewer)

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
            logger.log("Debugger location changed")
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
            logger.log("Debugger breakpoints changed")
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
        def __update_current_construct(debugger, viewer):
            """
            Goes through the frame list of the debugger
            and adds the construct corresponding to the
            current code line in the history.
            It will be highlighted automatically by set_diagram.
            """
            if debugger is None:
                return
            frames = debugger.frames()
            cur_frame = debugger.current_frame()
            frame_infos = []
            for s in frames:
                frame_num = s[0]
                # Only analyze the construct up to the active frame
                if frame_num < cur_frame:
                    continue
                fileloc = s[3]
                try:
                    f = fileloc.file()
                    line = fileloc.line()
                    blocks = QGEN_Module.modeling_map.get_block(f, line)
                    logger.log("Block {0} for {1}:{2}".format(
                        str(blocks), str(f), str(line)))
                # If the backtrace was not available no file is found
                except AttributeError:
                    blocks = None

                if not blocks:
                    break
                for block in blocks:
                    diag = QGEN_Module.modeling_map.get_diagram_for_item(
                        viewer.diags, block)[0]
                    insert = True
                    for id, _ in frame_infos:
                        if diag.id == id:
                            insert = False
                            break
                    if insert:
                        frame_infos.insert(0, (diag.id, block))

            logger.log("Frames info is %s" % str(frame_infos))
            i = 0
            frames_len = len(frame_infos)

            while i < frames_len:
                # Resolve Frames info is:
                #                [(u'alias_model_ref',
                #                  u'alias_model_ref/Model2'),
                #                 (u'switchaction', u'switchaction/Model5'),
                #                 (u'matrixdiv', u'matrixdiv/Subsystem'),
                #                 (u'Subsystem_Subsystem',
                #                  u'Subsystem/Subsystem/Unit Delay')]
                # We are trying to recreate the correct list of construct in
                # the outline.
                model_split = Diagram_Utils.block_split(frame_infos[i][1])
                current_d_name = frame_infos[i][0]
                diagram_name = model_split[0]
                split_len = len(model_split)
                split_idx = 1

                # Split the path in the right pair element and join paths
                # members until they match the diagram on left pair element
                # Each non matching joined path is a diagram to be added
                # after mangling, e.g. Subsystem/Subsystem/Unit Delay
                # needs to generate 'Subsystem', 'Subsystem_Subsystem'
                while split_idx < split_len and diagram_name != current_d_name:
                    frame_infos.insert(i + split_idx - 1, (diagram_name, ''))
                    diagram_name = Diagram_Utils.mangle_block_name(
                        diagram_name + '/' + model_split[split_idx])
                    split_idx += 1

                i += split_idx
                frames_len += split_idx - 1

            frame_infos = [it[0] for it in frame_infos]
            logger.log("Frames info after adding subsystems is %s" % str(
                frame_infos))
            # Try to find a construct starting with the longest model path
            # and narrowing it down until a construct is found or
            # the path is empty
            while frame_infos:
                cons_id = viewer.get_construct_from_list(frame_infos)
                if cons_id != "":
                    logger.log('Got construct %s from %s' % (
                        cons_id, str(frame_infos)))
                    viewer.update_nav_status(cons_id)
                    return
                frame_infos.pop()

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

                if not viewer.parsing_complete:
                    MDL_Language().should_refresh_constructs(
                        viewer.file)

                # User interaction happened, update the current diagram
                if force:
                    viewer.set_diagram(None)
                    return

                block = QGEN_Module.modeling_map.get_block(filename, line)
                scroll_to = None
                if block:
                    # We will only select the first block corresponding to the
                    # current line
                    info = QGEN_Module.modeling_map.get_diagram_for_item(
                        viewer.diags, block[0])
                    logger.log("Block for {0}:{1} is {2}".format
                               (str(filename), str(line), str(block[0])))
                    if info:
                        logger.log("Got info : %s" % repr(info))
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
                        QGEN_Module.__update_current_construct(
                            debugger, viewer)
                        viewer.set_diagram(diagram)  # calls on_diagram_changed

                if scroll_to:
                    logger.log("Scroll to " + str(scroll_to.id))
                    viewer.scroll_into_view(scroll_to)

            if filename:
                mdl = QGEN_Module.get_model_for_source(filename)
                logger.log("Mdl file for current file {0} is {1}".format(
                    filename, mdl))

                if mdl:
                    QGEN_Diagram_Viewer.get_or_create_from_model(
                        mdl, on_loaded=__on_viewer_loaded)
                else:
                    for viewer in QGEN_Diagram_Viewer.retrieve_qgen_viewers():
                        __on_viewer_loaded(viewer)

        @staticmethod
        def update_bp_labels(viewer, d):
            """
            Goes through the displayed names in the diagram and highlights them
            if it is possible to break on the associated block.
            :param viewer: a QGen Diagram Viewer instance
            :param d: the diagram to process
            """
            textitem = None

            for item in d.items:
                for it in item.recurse():
                    # Update the textitem style if it exists and debug infos
                    # are available for the corresponding block (textitem
                    # preceeds block rectitem)
                    if hasattr(it, 'id'):
                        if not QGEN_Module.modeling_map.get_source_ranges(
                                it.id):
                            break
                        if textitem is not None:
                            viewer.diags.set_item_style(textitem,
                                                        '#name-style-bp')
                            textitem = None
                    # If an item does not have an id it can contain the textid
                    # we want, recurse to find the name
                    # that needs to have its style updated, if existing it will
                    # be the last.
                    else:
                        for i in it.recurse():
                            if isinstance(i, GPS.Browsers.TextItem):
                                textitem = i

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
            while item is not None:
                if not hasattr(
                        item, 'id') or not self.block_source_ranges(item.id):
                    item = item.parent
                else:
                    break
            return item

        @staticmethod
        @gps_utils.hook('open_file_action_hook', last=False)
        def __on_open_file_action(file, *args):
            """
            When a model file is opened, use a diagram viewer instead of a
            text file to view it.
            """
            if file.language() == 'simulink' or file.language() == 'qgen':
                logger.log('Open %s' % file)
                QGEN_Diagram_Viewer.get_or_create_as_root(file)
                QGEN_Module.load_debug_info_for(file)
                return True
            return False

        def load_desktop(self, data):
            """Restore the contents from the desktop"""
            info = json.loads(data)
            f = GPS.File(info['file'])
            if f.path.endswith(
                    '.mdl') or f.path.endswith(
                        '.slx') or f.path.endswith('.xmi'):
                viewer = QGEN_Diagram_Viewer.get_or_create_as_root(f)
                MDL_Language().should_refresh_constructs(f)
            else:
                viewer = QGEN_Diagram_Viewer.open_json(
                    f, open(f.path).read())
            GPS.Hook('file_edited').run(f)

            return GPS.MDI.get_by_child(viewer)

        def __contextual_filter_debug_and_sources(self, context):
            """
            Whether the current context is a model block with
            source lines (or one of its parents has source lines). The
            debugger must have been started too.
            """
            try:
                GPS.Debugger.get()   # or raise exception
                return self.__contextual_filter_sources(context)
            except Exception:
                return False

        def __contextual_filter_debug_and_symbols(self, context):
            """
            Whether the current context is a model block with
            symbols. The debugger must have been started too.
            """
            try:
                GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                return len(self.modeling_map.get_symbols(blockid=it.id)) != 0
            except Exception:
                return False

        def __contextual_filter_debug_and_watchpoint(self, context):
            try:
                GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)

                return sig_obj.watched
            except Exception:
                return False

        def __contextual_filter_debug_and_logpoint(self, context):
            """
            Whether the current context is a logged signal.
            The debugger must have been started too.
            """

            try:
                GPS.Debugger.get()   # or raise exception
                it = context.modeling_item
                sig_obj = QGEN_Module.signal_attributes.get(it.id, None)

                return sig_obj.logged
            except Exception:
                return False

        def __contextual_filter_debugger_active(self, context):
            """
            Whether the debugger has started or not.
            """

            try:
                GPS.Debugger.get()   # or raise exception
                return True
            except Exception:
                return False

        def __contextual_filter_viewer_active_history_parent(self, context):
            try:
                viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
                return viewer.nav_index \
                    != -1 and viewer.diagram.id != viewer.root_diag_id
            except Exception:
                return False

        def __contextual_filter_viewer_active_history(self, context):
            try:
                viewer = QGEN_Diagram_Viewer.retrieve_active_qgen_viewer()
                return viewer.nav_index > 0
            except Exception:
                return False

        def __contextual_filter_viewer_active(self, context):
            try:
                return QGEN_Diagram_Viewer.retrieve_active_qgen_viewer() \
                    is not None
            except Exception:
                return False

        def __contextual_show_original_block(self):

            def __on_viewer_loaded(viewer):
                d, it = QGEN_Module.modeling_map.get_diagram_for_item(
                    viewer.diags, b[0])
                if it is not None:
                    d.select(it)
                    viewer.set_diagram(d)
                    GPS.MDI.get_by_child(viewer).raise_window()

            ctxt = GPS.contextual_context() or GPS.current_context()
            b = QGEN_Module.modeling_map.get_block(
                ctxt.file(), ctxt.location().line())
            mdl_file = QGEN_Module.get_model_for_source(ctxt.file())
            if mdl_file:
                QGEN_Diagram_Viewer.get_or_create_from_model(
                    mdl_file, on_loaded=__on_viewer_loaded)
            else:
                GPS.Console().write("No model found\n")

        def __contextual_filter_qgen_code(self, context):
            """
            Whether the current context is code generated by QGen
            that can be associated to a block.
            """

            try:
                b = QGEN_Module.modeling_map.get_block(
                    context.file(), context.location().line())
                return b is not None
            except Exception:
                return False

        def __contextual_filter_sources(self, context):
            """
            Whether the current context is a model block with
            source lines (or one of its parents has source lines)
            """
            try:
                it = self.get_item_with_sources(context.modeling_item)
                return it is not None
            except Exception:
                return False

        def __contextual_name_for_break_on_block(self, context):
            it = self.get_item_with_sources(context.modeling_item)
            return 'Break on %s' % it.id.replace("/", "\\/")

        def __contextual_name_for_unbreak_on_block(self, context):
            it = self.get_item_with_sources(context.modeling_item)
            return 'Delete breakpoints on %s' % it.id.replace("/", "\\/")

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
                        for ln in rg:
                            if ln >= b[0] and ln <= b[1]:
                                line = ln
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
                callback=self.__contextual_set_breakpoint,
                static_path='Debug/Break')

            gps_utils.make_interactive(
                name='MDL delete breakpoints on block',
                contextual=self.__contextual_name_for_unbreak_on_block,
                filter=self.__contextual_filter_debug_and_sources,
                callback=self.__contextual_delete_breakpoint,
                static_path='Debug/DelBreak')

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
                name='Goto parent subsystem',
                category='QGen Debugger',
                filter=self.__contextual_filter_viewer_active_history_parent,
                icon='gps-upward-symbolic',
                key='Escape',
                for_learning=True)

            gps_utils.make_interactive(
                callback=CLI.action_goto_previous_subsystem,
                name='Goto previous subsystem',
                category='QGen Debugger',
                filter=self.__contextual_filter_viewer_active_history,
                icon='gps-backward-symbolic',
                key='Left',
                for_learning=True)

            gps_utils.make_interactive(
                callback=CLI.action_scale_to_fit,
                name='Fit window to view',
                category='QGen Debugger',
                filter=self.__contextual_filter_viewer_active,
                icon='gps-zoom-fit-symbolic',
                key='space',
                for_learning=True)

            gps_utils.make_interactive(
                callback=CLI.action_zoom_in,
                name='Zoom in',
                category='QGen Debugger',
                filter=self.__contextual_filter_viewer_active,
                key='plus',
                for_learning=True)

            gps_utils.make_interactive(
                callback=CLI.action_zoom_out,
                name='Zoom out',
                category='QGen Debugger',
                filter=self.__contextual_filter_viewer_active,
                key='minus',
                for_learning=True)

            gps_utils.make_interactive(
                callback=CLI.stop_logging_subsystem_values,
                name='Stop logging subsystem values',
                category='QGen Debugger',
                filter=self.__contextual_filter_debugger_active,
                icon='gps-stop-save-symbolic')

            gps_utils.make_interactive(
                callback=CLI.log_subsystem_values,
                name='Log subsystem values',
                category='QGen Debugger',
                filter=self.__contextual_filter_debugger_active,
                icon='gps-save-symbolic')

            gps_utils.make_interactive(
                name='MDL show source for block',
                contextual='Models/Show source code',
                filter=self.__contextual_filter_sources,
                callback=self.__contextual_show_source_code)

            gps_utils.make_interactive(
                name='MDL show block from source code',
                contextual='Models/Show block from source',
                filter=self.__contextual_filter_qgen_code,
                callback=self.__contextual_show_original_block)

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
