"""
This plug-in provides support for displaying SPARK global contracts generated
by the GNATprove --flow-show-gg switch.
"""

import distutils.dep_util
import json
import os

import GPS
from gs_utils import in_ada_file, interactive
import libadalang as lal
from lal_utils import node
import os_utils


GLOBAL_MARKS = {}
# A dictionary that maps a source filename to a list of GPS.EditorMark()
# { "filename": [GPS.EditorMark(..), GPS.EditorMark(..), ..] }
# This is used to show/hide the generated global contracts GNATstudio.


COMMAND = "gnatprove {project} -u {unit} --mode=flow " + \
          "--flow-show-gg -j 0 --quiet --warnings=off " + \
          "--output=brief --ide-progress-bar"
# The command used to produce a JSON file containing the generated global
# contracts.

HIGHLIGHTING = "Editor code annotations"
# The generated global contracts are displayed in GNATstudio via 'Special
# lines', labelled with this string.


def _log(msg, mode="error"):
    """ Facility logger. """
    GPS.Console("Messages").write(msg + "\n", mode=mode)


def find_object_directory(project):
    """
    Return the object directory for a given project.
    """
    try:
        list_dir = project.object_dirs(recursive=False)
    except Exception:
        _log("Could not obtain project information for this file")

    if list_dir:
        return list_dir[0]
    else:
        return None


def reset_state(buffer, file):
    """ Clear global information for file from GNATstudio. """
    global GLOBAL_MARKS

    for (mark, nlines) in GLOBAL_MARKS.pop(file.name(), []):
        buffer.remove_special_lines(mark, nlines)
        mark.delete()


def get_subp_decl(file, line, column):
    """ Return the subprogram declaration node at line, column.
    Return None if none is found.
    """

    decl = node(file, line, column, 'SubpSpec')
    return decl.parent


def has_aspects(subp_decl_node):
    """ Return True if the subprogram declaration node has an aspect. """

    return bool(subp_decl_node.f_aspects)


def get_aspect_line(line, subp_decl_node):
    """ Get the line number at which the final aspect is, or should go. """

    # If it's an expression function, obtain the line number where it ends
    # (including any aspects that follow) and return that line number.
    if isinstance(subp_decl_node, lal.ExprFunction):
        return int(subp_decl_node.sloc_range.end.line)

    # If there are already aspects, return the line where the aspect list ends
    if has_aspects(subp_decl_node):
        return int(subp_decl_node.f_aspects.sloc_range.end.line)

    subp_spec = subp_decl_node.f_subp_spec

    # If it's a function, return the line of the result type
    if isinstance(subp_spec.f_subp_kind, lal.SubpKindFunction):
        return int(subp_spec.f_subp_returns.sloc_range.end.line)

    # If it's a procedure and has params, return line where the param list ends
    if isinstance(subp_spec.f_subp_kind, lal.SubpKindProcedure):
        if subp_spec.f_subp_params:
            return int(subp_spec.f_subp_params.
                       sloc_range.end.line)

    return line


def edit_file(file, json_name):
    """ Parse the json output and add the global information into the currently
    edited file.
    """
    global GLOBAL_MARKS

    buffer = GPS.EditorBuffer.get()

    def pretty_printed_contracts(contracts):
        for contract in contracts:
            subp_file = contract['file']
            # File where the subprogram declaration occurs

            # If the file in which the subprogram declaration occurs is
            # not the currently edited file, we do not pretty-print
            # these globals
            if subp_file != file.base_name():
                continue

            globals = contract['globals']
            # Subprogram globals

            sloc_line = contract["line"]
            sloc_column = contract["col"]

            for aspect in globals:
                # ??? skip Refined_Globals until a later version of this plugin
                if aspect == u'Refined_Global':
                    continue

                subp_node = get_subp_decl(file, sloc_line, sloc_column)

                insert_line = get_aspect_line(sloc_line, subp_node) + 1

                if has_aspects(subp_node):
                    # Use the start column from the last aspect
                    insert_column = \
                        subp_node.f_aspects.\
                        f_aspect_assocs.children[-1].sloc_range.start.column
                else:
                    # Use the start column from the subprogram node
                    insert_column = \
                        subp_node.sloc_range.start.column

                # Adjust from Libadalang column which is indexed from 1
                indent = " " * (insert_column - 1)

                # Start building the pretty-printed contract

                pp_contract = ""

                # If we don't already have a 'with', insert one
                if not has_aspects(subp_node):
                    pp_contract += "%swith\n" % indent
                    # Increase the indent by 3 spaces
                    indent += " " * 3

                pp_contract += "%s%s => " % (indent, aspect)

                # If we have any globals
                if globals[aspect]:
                    pp_contract += "("

                    pp_contract += "\n"

                    # For each mode (Input, Proof_In, Output, In_Out), build up
                    # the pretty-printed contract
                    for i, mode in enumerate(globals[aspect], start=1):
                        pp_contract += "%s   %s => " % (indent, mode)

                        # Add parentheses if there is > 1 variable for this mode
                        paren_vars = len(globals[aspect][mode]) > 1

                        var_indent = indent + (" " * 6)

                        if paren_vars:
                            pp_contract += "(\n%s" % var_indent

                        # Comma separate each variable, and place on new lines
                        pp_contract += \
                            (",\n%s" % var_indent).join(globals[aspect][mode])

                        # Close parentheses for variables
                        if paren_vars:
                            pp_contract += ")"

                        # If we have more modes remaining, add a comma
                        if i < len(globals[aspect]):
                            pp_contract += ","
                        pp_contract += "\n"

                    # Close parentheses for modes
                    pp_contract += "%s)" % indent

                # Otherwise we have Global => null
                else:
                    pp_contract += "null"

                yield insert_line, pp_contract

    # Start of processing for edit_file

    try:
        with open(json_name, 'r') as fp:
            gg_json = json.load(fp)
    except ValueError:
        if os.stat(json_name).st_size:
            _log("Failed to parse global information file %s: "
                 "the JSON is invalid." % json_name)
        else:
            _log("No global information found: %s is empty." % json_name)
        return
    except IOError:
        _log("No global contracts need to be generated for file %s" %
             file.base_name(), mode="text")
        return

    # Clean the previous special lines if needed
    reset_state(buffer, file)

    # Go through the pretty-printed contracts and insert the special lines into
    # the GNATstudio editor window.
    for line, pp_contract in pretty_printed_contracts(gg_json['contracts']):
        mark = buffer.add_special_line(line, pp_contract, HIGHLIGHTING)
        mark_num = (mark, len(pp_contract))
        # We store these so they can be hidden when the user clicks
        # "Hide generated Global contracts"
        GLOBAL_MARKS.setdefault(file.name(), []).append(mark_num)


def on_exit(process, status, full_output):
    """ Triggered when GNATprove process has exited
    """
    if status:
        _log(process.get_result())
    else:
        edit_file(process.file, process.gg_json)


def show_generated_global_contracts():
    """ Display the currently edited file's generated global contracts in
    GNATstudio.
    """

    context = GPS.current_context()
    file = context.file()
    unitname = file.base_name().split(".")[0]
    project = context.project()
    objdir = find_object_directory(project)
    if not objdir:
        objdir = GPS.get_tmp_dir()
        _log("Could not find an object directory for %s, reverting to %s" %
             (file, objdir))
    gg_json = os.path.join(objdir, "gnatprove", unitname + ".gg")

    if distutils.dep_util.newer(file.name(), gg_json):
        prj = (' -P """%s"""' % project.file().name("Build_Server"))
        scenario = project.scenario_variables_cmd_line("-X")
        cmd = COMMAND.format(project=prj, unit=file.base_name())
        if scenario:
            cmd += ' ' + scenario
        proc = GPS.Process(cmd, on_exit=on_exit, remote_server="Build_Server")
        proc.file = file
        proc.gg_json = gg_json
    elif not os.path.isfile(gg_json):
        _log("Aborting operation: Can't find %s." % gg_json)
        return
    else:
        edit_file(file, gg_json)


#################################
# Register the contextual menus #
#################################

# Only register if gnatprove is on the path
if os_utils.locate_exec_on_path("gnatprove"):
    @interactive("Ada", in_ada_file,
                 contextual="SPARK/Globals/Show generated Global contracts",
                 name="Show generated Global contracts",
                 contextual_group=GPS.Contextual.Group.EXTRA_INFORMATION)
    def show_global_contracts():
        """ Add special lines showing the global contracts. """
        show_generated_global_contracts()

    @interactive("Ada", in_ada_file,
                 contextual="SPARK/Globals/Hide generated Global contracts",
                 name="Hide generated Global contracts",
                 contextual_group=GPS.Contextual.Group.EXTRA_INFORMATION)
    def hide_global_contracts():
        """ Clear the added special lines. """
        reset_state(GPS.EditorBuffer.get(), GPS.current_context().file())
