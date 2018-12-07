#!/usr/bin/python
# -*- coding: utf-8 -*-

############################################################################
# No user customization below this line
############################################################################

"""
This file provides support for using the SPARK 2014 toolset.
"""

import GPS
import os_utils
import os.path
import tool_output
import json
import re
import sys
import fnmatch

# We create the actions and menus in XML instead of python to share the same
# source for GPS and GNATbench (which only understands the XML input for now).

# Note that we use GPS.current_context instead of GPS.contextual_context below,
# because GPS.contextual_context does not work when clicking on the right of a
# line of code (see OB05-033).

# This plugin now depends on gnatprove_menus.xml and gnatprove_file.xml which
# are located in spark2014/. itp_lib which implements interactive theorem
# proving is also in this directory.

# Path to this executable
cur_exec_path = os.path.dirname(os.path.abspath(__file__))

# The xml information are under spark2014
spark2014_dir = os.path.join(cur_exec_path, "spark2014")
sys.path.append(spark2014_dir)
import itp_lib  # noqa

gnatprove_menus_file = os.path.join(spark2014_dir, "gnatprove_menus.xml")
gnatprove_file = os.path.join(spark2014_dir, "gnatprove.xml")

OUTPUT_PARSERS = """
    output_chopper
    utf_converter
    progress_parser
    job_recorder
    gnatprove_parser
    console_writer
    location_parser
    end_of_build"""

with open(gnatprove_menus_file, "r") as input_file:
    xml_gnatprove_menus = input_file.read()

with open(gnatprove_file, "r") as input_file2:
    xml_gnatprove = input_file2.read()

# constants that are required by the plugin

toolname = 'gnatprove'
messages_category = 'GNATprove'
obj_subdir_name = toolname
report_file_name = toolname + '.out'
prefix = 'SPARK'
menu_prefix = '/' + prefix
# hook on exit when ITP has been launched
hook_itp = False


examine_all = 'Examine All'
examine_root_project = 'Examine All Sources'
examine_file = 'Examine Single File'
examine_subp = 'Examine Subprogram'

# proof targets when user profile is 'Basic'

basic_prove_all = 'Basic Prove All'
basic_prove_root_project = 'Basic Prove All Sources'
basic_prove_file = 'Basic Prove File'
basic_prove_subp = 'Basic Prove Subprogram'
basic_prove_line = 'Basic Prove Line'
basic_prove_line_loc = 'Basic Prove Line Location'
basic_prove_check = 'Basic Prove Check'
basic_prove_region = 'Basic Prove Selected Region'

# proof targets when user profile is 'Advanced'

advanced_prove_all = 'Prove All'
advanced_prove_root_project = 'Prove All Sources'
advanced_prove_file = 'Prove File'
advanced_prove_subp = 'Prove Subprogram'
advanced_prove_line = 'Prove Line'
advanced_prove_line_loc = 'Prove Line Location'
advanced_prove_check = 'Prove Check'
advanced_prove_region = 'Prove Selected Region'

# Name of the messages console
MESSAGES = "Messages"


def print_error(message):
    """print errors on the messages console"""
    console = GPS.Console(MESSAGES)
    console.write(message + '\n', mode="error")


# getters for proof target depending on user profile


def prove_all():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_all
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_all


def prove_root_project():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_root_project
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_root_project


def prove_file():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_file
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_file


def prove_subp():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_subp
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_subp


def prove_line():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_line
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_line


def prove_region():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_region
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_region


# used to launch Prove Line from Location View
def prove_line_loc():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_line_loc
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_line_loc


# in case of manual provers, prove_check is
# the only one allowed to open editors
def prove_check():
    if GPS.Preference(User_Profile_Pref_Name).get() == 'Basic':
        return basic_prove_check
    elif GPS.Preference(User_Profile_Pref_Name).get() == 'Advanced':
        return advanced_prove_check


show_report = 'Show Report'
show_log = 'Show Log'
clean_up = 'Clean Proofs'
check_msg_prefix = 'medium: '

# moderate blue with 0.7 transparency
Default_Trace_Color = "rgba(107,174,214, 0.7)"
Overlay_Name = "Gnatprove_Trace_Overlay"
Ce_Spec_Lines_Name = "Gnatprove_Ce_Special_Lines"
Ce_Highlighting = "Editor code annotations"

User_Profile_Pref_Name = 'SPARK/user_profile'
Display_Analysis_Report = "SPARK:General/display_analysis_report"

GPS.Preference(Display_Analysis_Report).create(
    'Display analysis report',
    'boolean',
    'Display an analysis report after running GNATprove.',
    False)

GPS.Preference(User_Profile_Pref_Name).create(
    'User profile',
    'enum',
    'Basic user profile for simple proof panel,' +
    ' advanced user profile for more complex proof panel.',
    0,
    'Basic',
    'Advanced')

Color_Pref_Name = 'SPARK:Colors/color_trace'

GPS.Preference(Color_Pref_Name).create(
    'Highlight color for trace',
    'color',
    'Color to highlight trace lines (restart needed).',
    Default_Trace_Color)


def get_root():
    """retrieve the prefix of the spark install found on the path """
    return os.path.dirname(os.path.dirname(gnatprove)).replace('\\', '/')


def get_example_root():
    """retrieve the full path to the directory containing the examples as
       installed locally.
    """
    return get_root() + '/share/examples/spark'


def update_project_path(paths):
    """update GPR_PROJECT_PATH with the paths given as input, taking into account
       the existing setting of both GPR_PROJECT_PATH and ADA_PROJECT_PATH.
    """
    import os
    os.environ["GPR_PROJECT_PATH"] = \
        ':'.join(paths) + ':' + \
        os.environ["GPR_PROJECT_PATH"] + ':' + \
        os.environ["ADA_PROJECT_PATH"]


def load_example_crazyflie():
    """ load Crazyflie example project, which requires specific code to set
        MODE environment variable.
    """
    import os
    crazyflie_root = os.path.join(get_example_root(), 'crazyflie')
    os.environ["MODE"] = 'Analyze'
    GPS.Project.load(os.path.join(crazyflie_root, 'test.gpr'))


def load_example_ipstack():
    """ load IPstack example project, which requires specific code to set
        GPR_PROJECT_PATH and change working directory.
    """
    import os
    ipstack_root = os.path.join(get_example_root(), 'ipstack')
    update_project_path([os.path.join(ipstack_root, 'projects'),
                         os.path.join(ipstack_root, 'projects.native')])
    os.chdir(os.path.join(ipstack_root, 'build'))
    GPS.Project.load(os.path.join(ipstack_root,
                     'projects.native', 'ipstack_dev.gpr'))


def load_example_thumper():
    """ load Thumper example project, which requires specific code to set
        MODE and GPR_PROJECT_PATH environment variables.
    """
    import os
    thumper_root = os.path.join(get_example_root(), 'thumper')
    update_project_path([os.path.join(thumper_root, 'dummy_projects')])
    os.environ["MODE"] = 'Analyze'
    GPS.Project.load(os.path.join(thumper_root,
                     'thumper', 'src', 'thumper.gpr'))


def get_trace_overlay(buf):
    """retrieve the trace overlay for a buffer. If the buffer hasn't got one
       yet, create it, add it to the buffer, and return it.
    """

    if not hasattr(buf, Overlay_Name):
        o = buf.create_overlay('trace overlay')
        o.set_property('paragraph-background',
                       GPS.Preference(Color_Pref_Name).get())
        setattr(buf, Overlay_Name, o)
    return getattr(buf, Overlay_Name)


def get_ce_special_lines(buf):
    """retrieve the list of special lines for counterexample for a
       buffer. If the buffer hasn't got one yet, create it, add it to the
       buffer, and return it.
    """

    if not hasattr(buf, Ce_Spec_Lines_Name):
        setattr(buf, Ce_Spec_Lines_Name, [])
    return getattr(buf, Ce_Spec_Lines_Name)


def add_ce_special_line(buf, line, text):
    """create a special line in the buffer"""

    line_marker = buf.add_special_line(line+1, text, Ce_Highlighting)
    spec_lines = get_ce_special_lines(buf)
    spec_lines.append(line_marker)
    setattr(buf, Ce_Spec_Lines_Name, spec_lines)


def remove_ce_special_lines(buf):
    """remove all special lines for counterexample in the buffer"""

    line_markers = get_ce_special_lines(buf)
    for line_marker in line_markers:
        buf.remove_special_lines(line_marker, 1)


# helper functions that do not really fit elsewhere
def goto_location(sloc):
    """go to the location defined by the given GPS.FileLocation"""

    buf = GPS.EditorBuffer.get(sloc.file())
    v = buf.current_view()
    GPS.MDI.get_by_child(v).raise_window()
    v.goto(buf.at(sloc.line(), sloc.column()))
    v.center()


# This is the on_exit callback for the editor process
def check_proof_after_close(proc, ex_st, outp):
    """run gnatprove to check a proof after the external editor has been
       closed
    """
    if not proc._is_killed:
        try:
            vc_kind = get_vc_kind(proc._proc_msg)
            llarg = limit_line_option(proc._proc_msg, vc_kind)
            GPS.BuildTarget(prove_check()).execute(extra_args=[llarg],
                                                   synchronous=False)
        except TypeError:
            pass


# For some reason, when the process is killed,
# on_exit function of the process is called twice
# and asking twice to relaunch GNATprove.
# So instead of that if the process was killed we don't do anything
def editor_before_kill(proc, outp):
    """check if editor was killed"""
    proc._is_killed = True


trace_msg = None
trace_lines = []
counterexample = {}


def show_trace(lines):
    """show the trace given by the lines"""
    f = None
    for sloc in lines or []:
        if sloc.file() != f:
            f = sloc.file()
            buf = GPS.EditorBuffer.get(f)
            goto_location(sloc)
            overlay = get_trace_overlay(buf)
        buf.apply_overlay(overlay,
                          buf.at(sloc.line(), 1),
                          buf.at(sloc.line(), 1))


def remove_trace(lines):
    """remove the trace given by the lines"""
    f = None
    for sloc in lines or []:
        if sloc.file() != f:
            f = sloc.file()
            buf = GPS.EditorBuffer.get(f, open=False)
            if buf:
                overlay = get_trace_overlay(buf)
                buf.remove_overlay(overlay)


def get_ce_text_for_line(line_info):
    """Generates the text to be displayed in counterexample for given
       line."""
    return " and ".join(['%s = %s' % (ce_element["name"], ce_element["value"])
                         for ce_element in line_info])


def get_str_indent(buf, line):
    """Returns a string of white spaces that indents up to the indentation of
       the given line."""
    line_loc = buf.at(line, 1)
    last_column = line_loc.end_of_line().column
    indent = 1
    while line_loc.get_char() == " " and indent < last_column:
        indent += 1
        line_loc = buf.at(line, indent)
    return "" if (indent == last_column) else " " * (indent-1)


def show_ce(ce):
    for file in ce:
        if GPS.File(file).language() == "ada":
            first = next(iter(ce[file]), None)
            if first:
                first_sloc = GPS.FileLocation(GPS.File(file),
                                              int(first),
                                              1)
                buf = GPS.EditorBuffer.get(first_sloc.file())
                goto_location(first_sloc)
                for line in ce[file]:
                    text = get_str_indent(buf, int(line)) + \
                           "[Counterexample] " + \
                           get_ce_text_for_line(ce[file][line])
                    add_ce_special_line(buf, int(line), text)


def remove_ce(ce):
    for file in ce:
        if GPS.File(file).language() == "ada":
            buf = GPS.EditorBuffer.get(GPS.File(file))
            remove_ce_special_lines(buf)


def disable_trace_and_ce():
    """remove any traces and counter examples in the editor"""
    global trace_msg, trace_lines, counterexample
    remove_trace(trace_lines)
    remove_ce(counterexample)
    trace_msg = None


def toggle_trace(msg, lines, ce):
    """toggle the trace for the given msg and lines"""
    global trace_msg, trace_lines, counterexample
    if trace_msg is None:
        trace_msg = msg
        trace_lines = lines
        counterexample = ce
        # preferably show the counterexample without trace
        if ce != {}:
            show_ce(ce)
        else:
            show_trace(lines)
    elif trace_msg == msg:
        disable_trace_and_ce()
    else:
        remove_trace(trace_lines)
        remove_ce(counterexample)
        trace_msg = msg
        trace_lines = lines
        counterexample = ce
        # preferably show the counterexample without trace
        if ce != {}:
            show_ce(ce)
        else:
            show_trace(lines)


def build_msg_full_text(file, line, col, text):
    """Given a msg text and location, return the string
       "file:line:col:msg"
       Note that the returned text must be identical to the text that is
       produced by GNATprove - this  text is used to match the message
       produced by GNATprove and extra information about this message
       stored in *.spark file. See on_stdout and on_exit.
    """
    str_col = str(col)

    # In the message produced by GNATprove, '0' is always prepended
    # to column number that is less than 10. Do it also here.
    if col < 10:
        str_col = '0' + str_col
    return "%s:%s:%s: %s" % (file, line, str_col, text)


def get_comp_text(m):
    """Returns the computed text of a message"""
    file = os.path.basename(m.get_file().path)
    text = build_msg_full_text(
        file,
        m.get_line(),
        m.get_column(),
        m.get_text())
    return (text)


def get_norm_text(text):
    """Returns the normalized text of a message text.

       This amounts to getting the basename of the file in the location, so
       that it matches the result of calling [get_comp_text], even in cases
       where GNATprove uses the complete path name in its messages, when
       switch -gnatef is used.
    """
    reg = re.compile(r"([^:]*):(.*)")
    m = re.match(reg, text)
    file = os.path.basename(m.group(1))
    text = file + ':' + m.group(2)
    return text


# Any change to the regular expressions below should follow changes
# in messages issued by GNATprove in Compute_Message in
# flow_error_messages.adb

# This regexp parses the message to get the filename and line number
# given after the "in instantiation at". The filename can be any
# alphanumeric string with '_', '.', and '-' allowed. It does not look
# at the rest of the message because it would be difficult to parse (in
# particular because of counterexamples).
reg1 = re.compile(r"in instantiation at ([\w\.-]+):[0-9]+")
reg2 = re.compile(r"in call inlined at ([\w\.-]+):[0-9]+")
reg3 = re.compile(r"in inherited contract at ([\w\.-]+):[0-9]+")


def get_compunit_for_message(msg):
    """ Return the compilation unit for a given message, so that extra
    information for the message will be found in the file
    unit.spark. For generic instantiations, inlined calls and
    inherited contracts, this corresponds to the last unit in the
    chain of locations. Otherwise, this is simply the compilation
    unit where the message is reported."""

    text = msg.get_text()
    m = re.search(reg1, text)
    if not m:
        m = re.search(reg2, text)
    if not m:
        m = re.search(reg3, text)
    if m:
        fname = m.group(1)
    else:
        fname = os.path.basename(msg.get_file().path)
    return os.path.splitext(fname)[0]


class GNATprove_Parser(tool_output.OutputParser):

    """Class that parses messages of the gnatprove tool, and creates
       decorates the messages coming from GNATprove with actions (showing
       traces) when needed.
       In IDE mode, and when extra info is available for a message, Gnatprove
       appends a symbol [#id] to a message, where "id" is a number  which is
       unique for this unit and this message.
       The GNATprove parser strips the extra symbol from the message so that
       it's not visible in GPS, and builds up a mapping
         msg -> id
       Once GNATprove is terminated, for each msg which has an entry in this
       mapping, the parser opens the JSON file "unit.spark".
       See the :func:`parsejson()` function for the format of this file.
       Once this file is parsed, the GNATprove parser now knows the extra
       information associated to a message, if any. See
       :func:`act_on_extra_info()` to know what is done with this extra
       information.
    """
    analysis_tool = None
    # The GPS.AnalysisTool instance that will be responsible to add messages
    # in the Analysis Report.

    command = None
    # The GNATprove command being parsed.

    def __init__(self, child):
        # Remove the previous GNATprove messages first
        GPS.Locations.remove_category(
            messages_category, GPS.Message.Flags.INVISIBLE)

        tool_output.OutputParser.__init__(self, child)

        gnatprove_plug.output_parser = self

        # holds the unit names for which extra info is retrieved
        self.units_with_extra_info = []
        # holds the mapping "msg" -> msg_id
        self.msg_id = {}
        self.regex = re.compile(r"(.*)\[#([0-9]+)\]$")
        # holds the mapping "unit,msg_id" -> extra_info
        self.extra_info = {}

        # Create a GPS.AnalysisTool instance to collect the messages that will
        # be shown in the report.
        self.analysis_tool = GPS.AnalysisTool(messages_category)

        # create rules for all the messages not related with SPARK itself
        # (e.g: GNAT warnings etc.).
        self.analysis_tool.add_rule('warnings', 'WARNINGS')
        self.analysis_tool.add_rule('informational', 'INFORMATIONAL')
        self.analysis_tool.add_rule('errors', 'ERRORS')

        # create the SPARK rules from the '--list-categories' switch
        process = GPS.Process("gnatprove --list-categories")
        output = process.get_result()
        for line in output.split('\n'):
            splitted_line = line.split(' - ')
            if len(splitted_line) == 3:
                self.analysis_tool.add_rule(splitted_line[1],
                                            splitted_line[0])

    def print_output(self, text):
        """print the text on to the Messages view"""
        if text:
            GPS.Console().write(text + '\n')

    def parse_trace_file(self, filename):
        """ parse the trace file as a list of "file:line" information and
            return the result
        """

        lines = []
        if os.path.isfile(filename):
            with open(filename, 'r') as f:
                for line in f:
                    sl = line.split(':')
                    if len(sl) >= 2:
                        lines.append(
                            GPS.FileLocation(GPS.File(sl[0]),
                                             int(sl[1]),
                                             1))
        return lines

    def handle_entry(self, unit, list):
        """code do handle one entry of the JSON file. See :func:`parsejson()`
           for the details of the format.
        """

        for entry in list:
            if 'msg_id' in entry:
                full_id = unit, entry['msg_id']
                self.extra_info[full_id] = entry

    def parsejson(self, unit, file):
        """parse the json file "file", which belongs to unit "unit" and fill
           the "extra_info" mapping for any entry.
           The json file, if it exists and is a valid JSON value, is a dict
           with two entries "flow" and "proof" (both entries may be absent).
           Each entry is mapped to a list of dictionaries. Some of these
           dictionaries have the field "msg_id", these dictionaries are extra
           information for the corresponding message for the current unit. For
           those messages, we simply build up a mapping
             (unit, id) -> extra_info
           which is later used to act on this extra information for each
           message.
        """
        if os.path.isfile(file):
            with open(file, 'r') as f:
                try:
                    dict = json.load(f)
                    if 'flow' in dict:
                        self.handle_entry(unit, dict['flow'])
                    if 'proof' in dict:
                        self.handle_entry(unit, dict['proof'])
                except ValueError:
                    pass

    def get_rule_id_for_msg(self, m, extra):
        """return the rule ID associated to the message m.
           The rule ID is retrieved from "extra" when it exists: otherwise,
           the rule ID defined for all non-SPARK messages is returned.
        """
        if 'rule' in extra:
            return extra['rule']
        elif 'warning:' in m.get_text():
            return 'WARNINGS'
        elif 'info:' in m.get_text():
            return 'INFORMATIONAL'
        else:
            return 'ERRORS'

    def act_on_extra_info(self, m, extra, objdir, command):
        """act on extra info for the message m. More precisely, if the message
           has a tracefile or counterexample, add an action to the message
           which will show/hide the corresponding trace or counterexample,
           and if the message has manual proof information, run the external
           editor.
        """

        # We associate the real check location to the text of the message. The
        # locations of the message is not always the same as the one of the
        # check. And we need this information for manual proof.
        text_msg = get_comp_text(m)
        if 'check_file' in extra:
            map_msg[text_msg, 'check_file'] = extra['check_file']
        if 'check_line' in extra:
            map_msg[text_msg, 'check_line'] = extra['check_line']
        if 'check_col' in extra:
            map_msg[text_msg, 'check_col'] = extra['check_col']

        counterexample = {}
        if 'cntexmp' in extra:
            counterexample = extra['cntexmp']

        lines = []
        if 'tracefile' in extra and extra['tracefile'] != '':
            tracefile = os.path.join(objdir, extra['tracefile'])
            lines = self.parse_trace_file(tracefile)

        if counterexample != {} or lines != []:
            if counterexample != {}:
                msg = 'Show counterexample'
            else:
                msg = 'Show path'
            m.set_subprogram(lambda m: toggle_trace(m, lines, counterexample),
                             'gps-gnatprove-symbolic',
                             msg)
        # We don't want to open hundreds of editors if a Prove All
        # or Prove File was launched with a manual prover.
        # We only open an editor for prove check.
        editor_dialog = "The condition couldn't be verified\n" \
                        + "Would you like to edit the VC file?\n"

        if command and command.name() == prove_check() and 'vc_file' in extra \
           and GPS.MDI.yes_no_dialog(editor_dialog):
            if 'editor_cmd' in extra:
                cmd = extra['editor_cmd']
                manual_prover_file_path = extra['vc_file']
                process_cwd = os.path.dirname(manual_prover_file_path)
                try:
                    # We have to change the directory for an unknown reason
                    # related to issue 6153 of Coq.
                    proc = GPS.Process(cmd,
                                       on_exit=check_proof_after_close,
                                       before_kill=editor_before_kill,
                                       directory=process_cwd)
                    proc._proc_msg = m
                    proc._is_killed = False
                except OSError:
                    GPS.MDI.dialog("Editor " + cmd[0] +
                                   " not found\n" +
                                   "Manual proof file saved as: " +
                                   extra['vc_file'] + "\n")
            else:
                GPS.MDI.dialog("No editor configured for this prover\n" +
                               "Manual proof file saved as: " +
                               extra['vc_file'] + "\n")

    def on_exit(self, status, command):
        """When GNATprove has finished, check if extra information can be
        attached to them. Display the Analysis Report if the corresponding
        preference is set."""

        self.command = command
        display_in_report = GPS.Preference(Display_Analysis_Report).get()

        self.add_extra_info_on_messages(
            command, display_in_report=display_in_report)

        if display_in_report:
            GPS.Analysis.display_report(self.analysis_tool)

        if self.child is not None:
            self.child.on_exit(status, command)

    def on_stdout(self, text, command):
        """for each GNATprove message, check for a msg_id tag of the form
           [#id] where id is a number. If no such tag is found, just pass the
           text on to the next parser. Otherwise, add a mapping
              msg text -> msg id
           which will be used later (in on_exit) to associate more info to the
           message
        """

        lines = text.splitlines()
        for line in lines:
            m = re.match(self.regex, line)
            if m:
                text = get_norm_text(m.group(1))
                GPS.Locations.parse(text, category=messages_category)
                self.print_output(text)
                self.msg_id[text] = int(m.group(2))
            else:
                # the line doesn't have any extra info, go on
                GPS.Locations.parse(line, category=messages_category)
                self.print_output(line)

    def add_extra_info_on_messages(self, command, display_in_report=False):
        """Scan through messages to see if extra info has been attached to
           them. If so, parse the .spark file of the corresponding unit to
           get the extra info, and act on the extra info.

           Also, display the messages in Analysis Report if the corresponding
           preference is enabled.

        """
        # Global map that associates messages text to the location of the
        # check. Messages already contain a location but it cannot be trusted
        # for launching manual prover. Messages locations records precisely
        # what is failing in a vc not the location of said vc.
        global map_msg

        artifact_dirs = \
            [os.path.join(f, obj_subdir_name)
             for f in GPS.Project.root().object_dirs(recursive=True)]

        map_msg = {}
        imported_units = {}  # map from unit to corresponding object directory
        extra = {}

        for m in GPS.Message.list(messages_category):
            text = get_comp_text(m)
            if text in self.msg_id:
                id = self.msg_id[text]
                unit = get_compunit_for_message(m)
                full_id = unit, id
                # First time this unit is seen, identify the corresponding
                # object directory where extra info can be found for that unit.
                if unit not in imported_units:
                    for artifact_dir in artifact_dirs:
                        sparkfile = os.path.join(artifact_dir, unit + ".spark")
                        if os.path.exists(sparkfile):
                            self.parsejson(unit, sparkfile)
                            imported_units[unit] = artifact_dir
                            break
                # If no object directory was identified, associate the default
                # artifacts directory.
                if unit not in imported_units:
                    imported_units[unit] = GPS.Project.root().artifacts_dir()
                extra = {}
                if full_id in self.extra_info:
                    extra = self.extra_info[full_id]

            if display_in_report:
                self.analysis_tool.add_message(
                    m, self.get_rule_id_for_msg(m, extra))

            if extra:
                self.act_on_extra_info(m, extra, imported_units[unit], command)


def is_file_context(self):
    """This is the context in which "Show Path" may appear."""
    return self.file() is not None


# It's more convenient to define these callbacks outside of the plugin class

def generic_on_analyze(target, args=[]):
    disable_trace_and_ce()
    GPS.BuildTarget(target).execute(extra_args=args, synchronous=False)


def on_examine_all(self):
    generic_on_analyze(examine_all)


def on_examine_root_project(self):
    generic_on_analyze(examine_root_project)


def on_examine_file(self):
    generic_on_analyze(examine_file)


def on_prove_all(self):
    generic_on_analyze(prove_all())


def on_prove_root_project(self):
    generic_on_analyze(prove_root_project())


def on_prove_file(self):
    generic_on_analyze(prove_file())


def on_prove_line(self):
    args = []
    lsparg = build_limit_subp_string(self)
    if lsparg is not None:
        args.append(lsparg)
        if inside_generic_unit_context(self):
            args.append("-U")

    target = ""
    try:
        llarg = "--limit-line=" \
                + os.path.basename(self.message().get_file().path) \
                + ":" + str(self.message().get_line())
        args.append(llarg)
        target = prove_line_loc()
    except Exception:  # No message in context
        target = prove_line()

    generic_on_analyze(target, args=args)


def on_prove_region(self):
    args = []
    lsparg = build_limit_subp_string(self)
    if lsparg is not None:
        args.append(lsparg)
        if inside_generic_unit_context(self):
            args.append("-U")

    target = prove_region()
    lrarg = "--limit-region=" \
            + str(os.path.basename(self.file().path)) \
            + ":" + str(self.start_line()) \
            + ":" + str(self.end_line())
    args.append(lrarg)

    generic_on_analyze(target, args=args)


def on_show_report(self):
    gnatprove_plug.show_report()


def on_show_log(self):
    gnatprove_plug.show_log()


def on_clean_up(self):
    generic_on_analyze(clean_up)


def mk_debug_loc_string(sloc):
    """Return a location for debugging purpose. sloc can be a FileLocation or
       an EditorLocation."""

    curfile = sloc.file() if isinstance(sloc, GPS.FileLocation) \
        else sloc.buffer().file()
    locstring = os.path.basename(curfile.path) + ':' \
        + str(sloc.line()) + ':' + str(sloc.column())
    return locstring


def mk_loc_string(sloc):
    """Return a location suitable to pass to switch --limit-subp.
       sloc should be a FileLocation."""

    locstring = os.path.basename(sloc.file().path) + ':' \
        + str(sloc.line())
    return locstring


def subprogram_start(cursor):
    """Return the start of the subprogram that we are currently in"""

    # This function has been copied and modified from plugin "expanded_code"

    blocks = {'CAT_PROCEDURE': 1, 'CAT_FUNCTION': 1, 'CAT_ENTRY': 1}

    if cursor.block_type() == 'CAT_UNKNOWN':
        return None

    min = cursor.buffer().beginning_of_buffer()
    while not cursor.block_type() in blocks and cursor > min:
        cursor = cursor.block_start() - 1

    if cursor > min:
        return cursor.block_start()
    else:
        return None


def compute_subp_sloc(self):
    """Return the location of the declaration of the subprogram that we are
       currently in"""

    try:
        curloc = self.location()
        buf = GPS.EditorBuffer.get(curloc.file(), open=False)
        if buf is not None:
            edloc = buf.at(curloc.line(), curloc.column())
            start_loc = subprogram_start(edloc)
        else:
            return None
    except Exception:
        return None

    if not start_loc:
        return None
    name = edloc.subprogram_name()

    # [subprogram_start] returns the beginning of the line of the
    # definition/declaration. To be able to call GPS.Entity, we need to be
    # closer to the actual subprogram name. We get closer by skipping the
    # keyword that introduces the subprogram (procedure/function/entry etc.)

    start_loc = start_loc.forward_word(1)
    try:
        entity = GPS.Entity(name, start_loc.buffer().file(),
                            start_loc.line(), start_loc.column())
    except Exception:
        return None
    if entity is not None:
        return entity.declaration()
    else:
        return None


def build_limit_subp_string(self):
    loc = compute_subp_sloc(self)
    if loc is not None:
        return '--limit-subp=' + mk_loc_string(loc)
    else:
        return None


def inside_subp_context(self):
    """Return True if the context is inside a subprogram declaration or body"""

    if compute_subp_sloc(self) is not None:
        return 1
    else:
        return 0


def region_selected(self):
    """Return True if there is a selected region"""

    return int(self.end_line()) > 0


def inside_generic_unit_context(self):
    """Return True if the context is inside a generic unit"""

    try:
        curloc = self.location()
        buf = GPS.EditorBuffer.get(curloc.file(), open=False)
        if buf is not None:
            start_loc = buf.at(1, 1)
            unit_loc, _ = start_loc.search('package|function|procedure',
                                           regexp=True, whole_word=True,
                                           dialog_on_failure=False)
            if unit_loc is None:
                return False
            # reach the start of the next word. We need to forward 2 words and
            # backward 1 in order to get the cursor at the start of the next
            # word instead of the end of the current word with forward 1.
            unit_loc = unit_loc.forward_word(2)
            unit_loc = unit_loc.forward_word(-1)
            tok, _, _ = unit_loc.get_word()
            if tok == 'body':
                # reach the start of the next word
                unit_loc = unit_loc.forward_word(2)
                unit_loc = unit_loc.forward_word(-1)
            name, _, _ = unit_loc.get_word()
            try:
                entity = GPS.Entity(name, unit_loc.buffer().file(),
                                    unit_loc.line(), unit_loc.column())
            except Exception:
                return False
            return entity.is_generic()
        else:
            return False
    except Exception:
        return False


def generic_action_on_subp(self, action):
    """execute the action on the given subprogram entity
    """

    # The argument --limit-subp is not defined in the examine_subp/prove_subp
    # build targets, because we have no means of designating the proper
    # location at that point.  A mild consequence is that --limit-subp does not
    # appear in the editable box shown to the user, even if it appears in the
    # uneditable argument list displayed below it.

    arg = build_limit_subp_string(self)
    if arg is not None:
        args = [arg]
        if inside_generic_unit_context(self):
            args.append("-U")
        target = GPS.BuildTarget(action)
        target.execute(extra_args=args,
                       synchronous=False)


def on_examine_subp(self):
    """execute the "examine subprogram" action on the given subprogram
       entity
    """

    generic_action_on_subp(self, examine_subp)


def on_prove_subp(self):
    """execute the "prove subprogram" action on the given subprogram entity
    """

    generic_action_on_subp(self, prove_subp())


class GNATProve_Plugin:

    """Class to contain the main functionality of the GNATProve_Plugin"""

    output_parser = None
    # The GNATprove_Parser instance used to parse the tool's output

    def __init__(self):
        process = GPS.Process("gnatprove -h")
        help_msg = process.get_result()
        GPS.parse_xml(xml_gnatprove.format(help=help_msg,
                                           output_parsers=OUTPUT_PARSERS))
        GPS.parse_xml(xml_gnatprove_menus % {'prefix': prefix})

    def show_report(self):
        """Display the Analysis Report with the GNATprove messages."""
        if self.output_parser:
            if not GPS.Preference(Display_Analysis_Report).get():
                self.output_parser.add_extra_info_on_messages(
                    self.output_parser.command,
                    display_in_report=True)

            GPS.Analysis.display_report(self.output_parser.analysis_tool)
        else:
            GPS.Analysis.display_report(GPS.AnalysisTool(messages_category))

    def show_log(self):
        """Display the gnatprove.out log"""

        artifact_dir = GPS.Project.root().artifacts_dir()
        report_file = os.path.join(artifact_dir, obj_subdir_name,
                                   report_file_name)

        # if build mode is not the default one, the report file may be found in
        # the parent directory of the current object directory

        if not os.path.exists(report_file):
            if artifact_dir.endswith(os.sep):
                artifact_dir = artifact_dir[:len(os.sep)]
            artifact_dir = os.path.dirname(artifact_dir)
            candidate_report_file = \
                os.path.join(artifact_dir,
                             obj_subdir_name,
                             report_file_name)

            # if the report file is still not found, leave the original path
            # so that the error message mentions this one

            if os.path.exists(candidate_report_file):
                report_file = candidate_report_file
        if not os.path.exists(report_file):
            GPS.Console().write("The file " + report_file +
                                " does not exist; run gnatprove first.",
                                mode='error')
        else:
            buf = GPS.EditorBuffer.get(GPS.File(report_file))
            v = buf.current_view()
            GPS.MDI.get_by_child(v).raise_window()


# Manual proof


class UnknownVCError(Exception):

    def __init__(self, msg):
        self.msg = msg


vc_msg_dict = {
    # VC_RTE_Kind - run-time checks

    'divide by zero might fail': 'VC_DIVISION_CHECK',
    'array index check might fail': 'VC_INDEX_CHECK',
    'overflow check might fail': 'VC_OVERFLOW_CHECK',
    'float overflow check might fail': 'VC_FP_OVERFLOW_CHECK',
    'range check might fail': 'VC_RANGE_CHECK',
    'predicate check might fail': 'VC_PREDICATE_CHECK',
    'predicate check might fail on default value':
        'VC_PREDICATE_CHECK_ON_DEFAULT_VALUE',
    'length check might fail': 'VC_LENGTH_CHECK',
    'discriminant check might fail': 'VC_DISCRIMINANT_CHECK',
    'tag check might fail': 'VC_TAG_CHECK',
    'ceiling priority might not be in Interrupt_Priority':
        'VC_CEILING_INTERRUPT',
    'interrupt might be reserved': 'VC_INTERRUPT_RESERVED',
    'ceiling priority protocol might not be respected':
        'VC_CEILING_PRIORITY_PROTOCOL',
    'the task might terminate': 'VC_TASK_TERMINATION',

    # VC_Assert_Kind - assertions

    'initial condition might fail': 'VC_INITIAL_CONDITION',
    'default initial condition might fail': 'VC_DEFAULT_INITIAL_CONDITION',
    'precondition might fail': 'VC_PRECONDITION',
    'call to nonreturning subprogram might be executed': 'VC_PRECONDITION',
    'precondition of main program might fail': 'VC_PRECONDITION_MAIN',
    'postcondition might fail': 'VC_POSTCONDITION',
    'refined postcondition might fail': 'VC_REFINED_POST',
    'contract case might fail': 'VC_CONTRACT_CASE',
    'contract cases might not be disjoint': 'VC_DISJOINT_CONTRACT_CASES',
    'contract cases might not be complete': 'VC_COMPLETE_CONTRACT_CASES',
    'loop invariant might fail': 'VC_LOOP_INVARIANT',
    'loop invariant might fail in first iteration': 'VC_LOOP_INVARIANT_INIT',
    'loop invariant might fail after first iteration':
    'VC_LOOP_INVARIANT_PRESERV',
    'loop variant might fail': 'VC_LOOP_VARIANT',
    'assertion might fail': 'VC_ASSERT',
    'exception might be raised': 'VC_RAISE',

    # VC_LSP_Kind - Liskov Substitution Principle

    'precondition might be stronger than class-wide precondition':
    'VC_WEAKER_PRE',
    'precondition is stronger than the default class-wide precondition '
    'of True':
    'VC_TRIVIAL_WEAKER_PRE',
    'postcondition might be weaker than class-wide postcondition':
    'VC_STRONGER_POST',
    'class-wide precondition might be stronger than overridden one':
    'VC_WEAKER_CLASSWIDE_PRE',
    'class-wide postcondition might be weaker than overridden one':
    'VC_STRONGER_CLASSWIDE_POST'
}


def is_unproved_check_message(msg):
    for vc_warn in vc_msg_dict.keys():
        # get rid of "medium: "
        if msg.get_text()[len(check_msg_prefix):].startswith(vc_warn):
            return True
    return False


def get_line_warn(context):
    def msg_filter(msg):
        return msg.get_line() == context.location().line() \
            and is_unproved_check_message(msg)
    if len(context.files()) > 0:
        return filter(msg_filter, GPS.Message.list(file=context.file()))
    else:
        return None


def prove_check_context(context, edit_session):
    if context.file() is not None:
        try:
            context._loc_msg = context.message()
            return is_unproved_check_message(context._loc_msg)
        except Exception:  # No message in context
            tmp = get_line_warn(context)
            if len(tmp) == 1:
                context._loc_msg = tmp[0]
                return True
            if edit_session:
                return True
            else:
                return False
    return False


def can_show_report():
    return len(GPS.Message.list(category=messages_category)) > 0 \
        and gnatprove_plug.output_parser is not None


def get_vc_kind(msg):
    # get rid of "medium: "
    clean_msg = msg.get_text()[len(check_msg_prefix):]

    def best_match(acc, elem):
        if clean_msg.startswith(elem):
            if not acc or len(acc) < len(elem):
                return elem
            else:
                return acc
        else:
            return acc

    msg_key = reduce(best_match,
                     vc_msg_dict.keys(), None)
    if not msg_key:
        raise UnknownVCError(clean_msg)
    return vc_msg_dict[msg_key]


def limit_line_option(msg, line, col, vc_kind):
    return "--limit-line=" + os.path.basename(msg.get_file().path) \
                           + ":" + str(line) \
                           + ":" + str(col) \
                           + ":" + vc_kind


def on_prove_check(context):
    msg = context._loc_msg
    text_msg = get_comp_text(msg)
    msg_line = map_msg[text_msg, 'check_line']
    msg_col = map_msg[text_msg, 'check_col']
    vc_kind = get_vc_kind(msg)
    llarg = limit_line_option(msg, msg_line, msg_col, vc_kind)
    args = [llarg]
    if inside_generic_unit_context(context):
        args.append("-U")
    GPS.BuildTarget(prove_check()).execute(extra_args=args,
                                           synchronous=False)


# Check for GNAT toolchain: gnatprove

gnatprove = os_utils.locate_exec_on_path(toolname)

if gnatprove:
    xml_gnatprove_menus = \
        xml_gnatprove_menus.format(root=get_root(), example=get_example_root())

    gnatprove_plug = GNATProve_Plugin()


def compute_gnatserver_path():
    """ Compute the position of the gnat_server tool from the one of gnatprove.
        We do this because gnat_server is not in the PATH (and should not be).
    """

    bin_dir = os.path.dirname(gnatprove)
    gs_rel = os.path.join("..", "libexec", "spark", "bin", "gnat_server")
    return (os.path.abspath(os.path.join(bin_dir, gs_rel)))


gnat_server = compute_gnatserver_path()
itp_lib.print_debug("[gnat_server path]:" + gnat_server)


# Checking for existence of gnat_server (compatibility with spark version < 18)
is_itp = False
if os.path.exists(gnat_server) or os.path.exists(gnat_server + ".exe"):
    is_itp = True


def has_proof_dir():
    """ This does a (too) simple analysis of the .gpr to find the attribute
        Proof_Dir which gives the location of the session file.
    """
    # ??? This function is only used to return the proof_dir if any. It
    # should use GPS's get_attribute but it does not work.

    root_project_file = str(GPS.Project.root().file())
    proof_dir = ""
    with open(root_project_file, 'r') as project_file:
        data = project_file.read()
        # We matched the string for: "for Proof_Dir use "match";". This has to
        # be used to be able to use manual proof. So this allows us to retrieve
        # the directory where proofs are located.
        match = re.search('for\s+Proof_Dir\s+use\s+\"(.*?)\";',
                          data,
                          flags=re.UNICODE)
        if match is not None:
            proof_dir = match.group(1)
            root_project_dir = os.path.dirname(root_project_file)
            proof_dir = os.path.join(root_project_dir, proof_dir)
    return(proof_dir)


def manual_proof_started():
    """ This is used to grey the "Exit Manual Proof" when Manual Proof has not
        been started.
    """
    return itp_lib.itp_started


def start_ITP(tree, file_name, abs_fn_path, args=[], edit_session=False):
    """ Function used to start interactive theorem proving. It actually build
        the command line, find appropriate files and then use the start method
        of tree.
    """

    itp_lib.print_debug("[ITP] Launched")

    # TODO ??? start_ITP and prove_check to be merged.
    artifact_dir = GPS.Project.root().artifacts_dir()
    obj_subdir_name = "gnatprove"
    # gnat_server must be launched from gnatprove dir to find why3.conf
    dir_gnat_server = os.path.join(artifact_dir, obj_subdir_name)
    mlw_file = ""
    file_name_no_ext = os.path.splitext(file_name)[0]
    for dir_name, sub_dir_name, files in os.walk(dir_gnat_server):
        for file in files:
            file_name_string = file_name_no_ext + '.mlw'
            if fnmatch.fnmatch(file, file_name_string) and mlw_file == "":
                mlw_file = os.path.join(dir_name, file)
                itp_lib.print_debug(mlw_file)
    if mlw_file == "":
        itp_lib.print_debug("TODO")

    proof_dir = has_proof_dir()
    if itp_lib.debug_file == "":
        command = gnat_server
    else:
        command = gnat_server + " --debug-stack-trace"
    if proof_dir == "":
        command = command + " "
    else:
        command = command + " " + "--proof-dir " + proof_dir + " "
    if edit_session:
        if itp_lib.debug_file == "":
            command = command + mlw_file
        else:
            command = command + mlw_file + " 2> " + itp_lib.debug_file
    else:
        # The arguments passed are of the following form (remove '='):
        # --limit-line=a.adb:42:42:VC_POSTCONDITION
        arg_limit_line = args[0].replace('=', ' ')
        command = command + arg_limit_line + " " + mlw_file
    itp_lib.print_debug(command)
    tree.start(command, abs_fn_path, dir_gnat_server, mlw_file)


def on_prove_itp(context, edit_session=False):
    """ Parses the context location provided by GPS and use it to call start_ITP
        which is used to start interactive theorem proving
    """

    global tree
    global hook_itp

    if not is_itp:
        # If itp is not detected do not run the tool
        print_error("manual proof requires more recent version of SPARK")
        return
    # ITP part
    tree = itp_lib.Tree_with_process()
    if edit_session:
        abs_fn_path = context.file().name()
        args = []
    else:
        msg = context._loc_msg
        vc_kind = get_vc_kind(msg)
        text_msg = get_comp_text(msg)
        msg_line = map_msg[text_msg, 'check_line']
        msg_col = map_msg[text_msg, 'check_col']
        llarg = limit_line_option(msg, msg_line, msg_col, vc_kind)
        # This is not required in on_prove_check because the .mlw file is
        # computed in gnatprove.
        abs_fn_path = get_compunit_for_message(msg)
        args = [llarg]
    file_name = os.path.basename(abs_fn_path)
    if inside_generic_unit_context(context):
        args.append("-U")
    GPS.Locations.remove_category(messages_category,
                                  GPS.Message.Flags.INVISIBLE)
    start_ITP(tree, file_name, abs_fn_path, args, edit_session)
    # Add a hook to exit ITP before exiting GPS. Add the hook after ITP
    # launched last = False so that it is the first hook to be run
    if hook_itp is False:
        GPS.Hook("before_exit_action_hook").add(exit_ITP, last=False)
        hook_itp = True


def exit_ITP(dummy_arg):
    """ This function is used to exit ITP when exiting GPS (it is added as a
        hook. We don't use tree.exit directly because we want to make sure that
        this function always succeeds otherwise we cannot exit GPS.
    """
    global tree, hook_itp
    if is_itp:
        try:
            tree.exit()
            GPS.Hook("before_exit_action_hook").remove(exit_ITP)
            hook_itp = False
            return True
        except Exception:
            return True
    else:
        print_error("Interactive theorem proving requires version 18 of SPARK")
