"""
This file provides support for using the SPARK 2014 toolset.
"""

############################################################################
## No user customization below this line
############################################################################

import GPS, os_utils, os.path, tool_output, re, gps_utils, json

# We create the actions and menus in XML instead of python to share the same
# source for GPS and GNATbench (which only understands the XML input for now).

xml_gnatprove_menus = """<?xml version="1.0"?>
  <GNATPROVE>
    <filter name="Inside Subprogram Context" language="Ada" shell_lang="python"
        shell_cmd="spark2014.inside_subp_context(GPS.current_context())" />

    <action name="Examine All Action" category="GNATprove" output="none">
       <shell lang="python">spark2014.on_examine_all(GPS.current_context())</shell>
    </action>
    <action name="Examine Root Project Action" category="GNATprove" output="none">
       <shell lang="python">spark2014.on_examine_root_project(GPS.current_context())</shell>
    </action>
    <action name="Examine File Action" category="GNATprove" output="none">
       <filter_and>
          <filter language="Ada" />
          <filter id="Source editor" />
       </filter_and>
       <shell lang="python">spark2014.on_examine_file(GPS.current_context())</shell>
    </action>
    <action name="Examine Subprogram Action" category="GNATprove" output="none">
       <filter id="Inside Subprogram Context" />
       <shell lang="python">spark2014.on_examine_subp(GPS.current_context())</shell>
    </action>
    <action name="Prove All Action" category="GNATprove" output="none">
       <shell lang="python">spark2014.on_prove_all(GPS.current_context())</shell>
    </action>
    <action name="Prove Root Project Action" category="GNATprove" output="none">
       <shell lang="python">spark2014.on_prove_root_project(GPS.current_context())</shell>
    </action>
    <action name="Prove File Action" category="GNATprove" output="none">
       <filter_and>
          <filter language="Ada" />
          <filter id="Source editor" />
       </filter_and>
       <shell lang="python">spark2014.on_prove_file(GPS.current_context())</shell>
    </action>
    <action name="Prove Subprogram Action" category="GNATprove" output="none">
       <filter id="Inside Subprogram Context" />
       <shell lang="python">spark2014.on_prove_subp(GPS.current_context())</shell>
    </action>
    <action name="Prove Line Action" category="GNATprove" output="none">
       <filter id="Inside Subprogram Context" />
       <shell lang="python">spark2014.on_prove_line(GPS.current_context())</shell>
    </action>
    <action name="Show Report Action" category="GNATprove" output="none">
        <shell lang="python">spark2014.on_show_report(GPS.current_context())</shell>
    </action>
    <action name="Clean Proofs Action" category="GNATprove" output="none">
        <shell lang="python">spark2014.on_clean_up(GPS.current_context())</shell>
    </action>
    <action name="Remove Editor Highlighting Action" category="GNATprove" output="none">
       <shell lang="python">spark2014.on_clear_highlighting(GPS.current_context())</shell>
    </action>

    <submenu before="Window">
      <Title>_%(prefix)s</Title>
        <menu action="Examine All Action">
          <Title>Examine All</Title>
        </menu>
        <menu action="Examine Root Project Action">
          <Title>Examine Root Project</Title>
        </menu>
        <menu action="Examine File Action">
          <Title>Examine File</Title>
        </menu>
        <menu><title/></menu>
        <menu action="Prove All Action">
          <Title>Prove All</Title>
        </menu>
        <menu action="Prove Root Project Action">
          <Title>Prove Root Project</Title>
        </menu>
        <menu action="Prove File Action">
          <Title>Prove File</Title>
        </menu>
        <menu><title/></menu>
        <menu action="Show Report Action">
          <Title>Show Report</Title>
        </menu>
        <menu action="Clean Proofs Action">
          <Title>Clean Proofs</Title>
        </menu>
        <menu action="Remove Editor Highlighting Action">
          <Title>Remove Editor Highlighting</Title>
        </menu>
    </submenu>

    <contextual action="Examine File Action">
      <Title>%(prefix)s/Examine File</Title>
    </contextual>
    <contextual action="Examine Subprogram Action">
      <Title>%(prefix)s/Examine Subprogram</Title>
    </contextual>
    <contextual action="Prove File Action">
      <Title>%(prefix)s/Prove File</Title>
    </contextual>
    <contextual action="Prove Subprogram Action">
      <Title>%(prefix)s/Prove Subprogram</Title>
    </contextual>
    <contextual action="Prove Line Action">
      <Title>%(prefix)s/Prove Line</Title>
    </contextual>

    <doc_path>share/doc/spark</doc_path>

    <documentation_file>
      <name>html/ug/index.html</name>
      <descr>SPARK 2014 Toolset User's Guide</descr>
      <category>%(prefix)s</category>
      <menu before="About">/Help/%(prefix)s/SPARK 2014 Toolset User's Guide</menu>
    </documentation_file>

    <documentation_file>
      <name>html/lrm/index.html</name>
      <descr>SPARK 2014 Reference Manual</descr>
      <category>%(prefix)s</category>
      <menu before="About">/Help/%(prefix)s/SPARK 2014 Reference Manual</menu>
    </documentation_file>

    <action name="spark2014_example_binary_search" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/binary_search/test.gpr"</shell>
      <shell>Editor.edit "binary_search.ads"</shell>
      <shell>Editor.edit "binary_search.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_binary_search">
        <title>Binary_Search</title>
      </menu>
    </submenu>

    <action name="spark2014_example_database" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/database/test.gpr"</shell>
      <shell>Editor.edit "database.ads"</shell>
      <shell>Editor.edit "database.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_database">
        <title>Database</title>
      </menu>
    </submenu>

    <action name="spark2014_example_intro" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/intro/test.gpr"</shell>
      <shell>Editor.edit "pricing.ads"</shell>
      <shell>Editor.edit "pricing.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_intro">
        <title>Intro</title>
      </menu>
    </submenu>

    <action name="spark2014_example_longest_common_prefix" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/longest_common_prefix/test.gpr"</shell>
      <shell>Editor.edit "lcp.ads"</shell>
      <shell>Editor.edit "lcp.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_longest_common_prefix">
        <title>Longest_Common_Prefix</title>
      </menu>
    </submenu>

    <action name="spark2014_example_segway" category=""
            show-command="false" output="none">
      <shell>Project.load "@EXAMPLE@/segway/test.gpr"</shell>
      <shell>Editor.edit "segway.ads"</shell>
      <shell>Editor.edit "segway.adb"</shell>
    </action>

    <submenu before="About">
      <title>/Help/%(prefix)s/Examples</title>
      <menu action="spark2014_example_segway">
        <title>Segway</title>
      </menu>
    </submenu>
  </GNATPROVE>
"""

xml_gnatprove = """<?xml version="1.0"?>
  <GNATPROVE>
    <tool name="GNATprove" package="Prove" attribute="switches" index="">
      <language>Ada</language>
      <switches columns="2" lines="3" switch_char="-">
        <title line="1">Proof</title>
         <combo label="Main mode" switch="--mode" noswitch="all"
               separator="=" column="1"
               tip="Main mode of formal verification" >
            <combo-entry label="check" value="check"
                         tip="Check SPARK restrictions for code where SPARK_Mode=On"/>
            <combo-entry label="flow" value="flow"
                         tip="Prove object initialization, globals and depends contracts"/>
            <combo-entry label="prove" value="prove"
                         tip="Prove subprogram contracts and absence of run-time errors"/>
            <combo-entry label="all" value="all"
                         tip="Activates all modes"/>
         </combo>
        <combo line="1" label="Report mode" switch="--report" separator="="
               noswitch="fail" tip="Amount of information reported">
          <combo-entry label="fail" value="fail"
                       tip="Only failed proof attempts"/>
          <combo-entry label="all" value="all"
                       tip="All proof attempts"/>
          <combo-entry label="statistics" value="statistics"
                       tip="Detailed proof attempts"/>
        </combo>
         <combo label="One proof by" switch="--proof" noswitch="no_split"
               separator="=" column="1"
               tip="Formulas generated for each check (faster) or each path (more precise)" >
            <combo-entry label="check" value="no_split"
                         tip="Generate one formula per check"/>
            <combo-entry label="check first, then path" value="then_split"
                         tip="Start with one formula per check, then split into paths when needed"/>
            <combo-entry label="path" value="path_wp"
                         tip="Generate one formula per path for each check"/>
         </combo>
        <spin label="Prover timeout" switch="--timeout="
              default="1" min="1" max="3600"
              tip="Set the prover timeout (in s) for individual proofs" />
        <spin label="Prover max steps" switch="--steps="
              default="0" min="0" max="1000000"
              tip="Set the prover maximum number of steps for individual proofs" />
        <title line="1" column="2">Process control</title>
        <spin label="Multiprocessing" column="2" switch="-j"
              default="1" min="1" max="100"
              tip="Use N processes to compile and prove. On a multiprocessor machine compilation and proof will occur in parallel" />
      </switches>
    </tool>

    <target-model name="gnatprove-examine">
       <description>Target model for GNATprove Examine commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
       </command-line>
       <icon>gps-build-all</icon>
    </target-model>

    <target-model name="gnatprove-prove">
       <description>Target model for GNATprove Prove commands</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="2" lines="4">
         <title column="1" line="1" >General</title>
         <check label="Force re-analysis" switch="-f" column="1"
                tip="Re-start analysis from scratch, ignoring previous results" />
         <check label="Report checks proved" switch="--report=all" column="1"
                tip="Report the status of all checks, including those proved" />
         <title column="2" line="1" >Prover</title>
         <combo label="One proof by" switch="--proof" noswitch="no_split"
               separator="=" column="2"
               tip="Formulas generated for each check (faster) or each path (more precise)" >
            <combo-entry label="check" value="no_split"
                         tip="Generate one formula per check"/>
            <combo-entry label="check first, then path" value="then_split"
                         tip="Start with one formula per check, then split into paths when needed"/>
            <combo-entry label="path" value="path_wp"
                         tip="Generate one formula per path for each check"/>
         </combo>
         <spin label="Prover timeout" switch="--timeout=" column="2"
                default="1" min="1" max="3600"
                tip="Set the prover timeout (in s) for individual proofs" />
         <field label="Alternate prover" switch="--prover=" column="2"
                tip="Alternate prover to use instead of Alt-Ergo" />
       </switches>
    </target-model>

    <target model="gnatprove-examine" name="Examine All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-U</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine" name="Examine Root Project" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine" name="Examine Single File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-examine" name="Examine Subprogram" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=flow</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-U</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Root Project" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Subprogram" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target model="gnatprove-prove" name="Prove Line" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--limit-line=%f:%l</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    <target-model name="gnatprove_clean">
       <description>Target model for GNATprove for cleaning</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
    </target-model>

    <target model="gnatprove_clean" name="Clean Proofs" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--clean</arg>
       </command-line>
    </target>
  </GNATPROVE>
"""

# constants that are required by the plug-in
toolname             = "gnatprove"
obj_subdir_name      = toolname
report_file_name     = toolname + ".out"
prefix               = "SPARK"
menu_prefix          = "/" + prefix
examine_all          = "Examine All"
examine_root_project = "Examine Root Project"
examine_file         = "Examine Single File"
examine_subp         = "Examine Subprogram"
prove_all            = "Prove All"
prove_root_project   = "Prove Root Project"
prove_file           = "Prove File"
prove_subp           = "Prove Subprogram"
prove_line           = "Prove Line"
show_report          = "Show Report"
clean_up             = "Clean Proofs"
clear_highlighting   = "Remove Editor Highlighting"

Default_colors = {
  "info"    : "#88eeaa",
  "error"   : "#f75d59",
  "trace"   : "#00ffff" }

Pref_Names = {}

Overlays = {
    "trace" : "gnatprove_trace_overlay",
    "error" : "gnatprove_error_overlay",
    "info"  : "gnatprove_info_overlay"
    }

Color_pref_prefix = "Plugins/gnatprove/color_"

# The default colors
for k in Default_colors:
    pref_name = Color_pref_prefix + k
    GPS.Preference (pref_name).create(
          "Highlight color for " + k, "color",
          "color used to highlight corresponding lines"
          " You must restart gps to take changes into account",
          Default_colors[k])

# helper functions that do not really fit elsewhere
def goto_location(sloc):
    """go to the location defined by the given GPS.FileLocation"""
    buf = GPS.EditorBuffer.get(sloc.file())
    v = buf.current_view()
    GPS.MDI.get_by_child(v).raise_window()
    v.goto(GPS.EditorLocation(buf, sloc.line(),sloc.column()))
    v.center()

def get_overlay(buf,overlay_name):
    """ For a buffer and an overlay name, return the corresponding overlay
        object of the buffer.

        The Overlay name must be a key of the Overlays object. This function
        will take care of creating the needed overlays in the buffer when they
        are not there, and will also take care of creating them in the right
        order.
    """
    needed_attr = Overlays[overlay_name]
    if not hasattr(buf,needed_attr):
        # the overlay is not there, it means we need to create all three in
        # the right order.
        info  = buf.create_overlay ("info overlay")
        error = buf.create_overlay ("error overlay")
        trace = buf.create_overlay ("trace overlay")
        my_overlays = {
            "info"  : info,
            "trace" : trace,
            "error" : error }
        for k in Overlays:
            o = my_overlays[k]
            pref_name = Color_pref_prefix + k
            o.set_property(
                "paragraph-background",
                GPS.Preference(pref_name).get())
            setattr(buf,Overlays[k],o)
    # we are all set now, get the required attribute
    return getattr(buf,needed_attr)

class GNATprove_Message(GPS.Message):
    """Class that defines gnatprove messages, which are richer than plain GPS
    messages. In particular, a gnatprove message can have an explanation
    attached, which is shown in the form of a path."""

    def __init__(self, category, f, line, col, text, flags,tracefile=""):
        """initialize state for GNATprove_Message"""
        GPS.Message.__init__(self, category, f, line, col, text, flags)
        self.trace_visible = False
        self.lines = []
        if tracefile != "":
            objdirs = GPS.Project.root().object_dirs()
            self.tracefile = os.path.join(objdirs[0], obj_subdir_name, tracefile)
        else:
            self.tracefile = ""
        self.is_info_message = (text.find("info:") != -1)
        self.highlight_message()
        self.load_trace_if_needed()
        # register ourselves in the gnatprove plugin
        gnatprove_plug.add_msg(self)

    def load_trace_if_needed(self):
        if os.path.isfile(self.tracefile):
            self.parse_trace_file()
            if len(self.lines) > 0:
                self.set_subprogram(
                    lambda m : m.toggle_trace(),
                    "gps-gnatprove-path",
                    "show path information")

    def highlight_message(self):
        msg_buffer = GPS.EditorBuffer.get(self.get_file())
        if self.is_info_message:
            needed_overlay = "info"
        else:
            needed_overlay = "error"
        overlay = get_overlay(msg_buffer, needed_overlay)
        loc = GPS.EditorLocation(msg_buffer, self.get_line(),1)
        msg_buffer.apply_overlay(overlay, loc, loc)

    def clear_trace(self):
        """clear the trace of the message"""
        f = None
        for sloc in (self.lines or []):
            if sloc.file() != f:
                f = sloc.file()
                buf = GPS.EditorBuffer.get(f)
                goto_location(sloc)
                if buf:
                    overlay = get_overlay(buf,"trace")
                    buf.remove_overlay(overlay)
        self.trace_visible = False

    def clear_highlighting(self):
        """simply remove all overlays. Note that this will also affect the
           highlighting of other messages
        """
        self.clear_trace()
        buf = GPS.EditorBuffer.get(self.get_file())
        for k in Overlays:
            o = get_overlay(buf, k)
            buf.remove_overlay(o)

    def remove(self):
        """remove the message and clear the trace"""
        GPS.Message.remove(self)
        self.clear_highlighting()

    def toggle_trace(self):
        """Toggle visibility of the trace information"""
        if self.trace_visible:
            gnatprove_plug.remove_trace(self)
            self.clear_trace()
        else:
            gnatprove_plug.register_trace(self)
            self.show_trace()

    def parse_trace_file(self):
        """parse the trace file (a list of file:line information) and store it
            in the list self.lines
        """
        with open(self.tracefile,"r") as f:
            for line in f:
                sl = line.split(':')
                if len(sl) >= 2:
                    self.lines.append(GPS.FileLocation(GPS.File(sl[0]),int(sl[1]), 1))

    def show_trace(self):
        """show the trace of the message. If necessary, read the associated
           trace file to load the information.
        """
        self.trace_visible = True
        f = None
        for sloc in (self.lines or []):
            if sloc.file() != f:
                f = sloc.file()
                buf = GPS.EditorBuffer.get(f)
                goto_location(sloc)
                overlay = get_overlay(buf, "trace")
                buf.remove_overlay(overlay)
            buf.apply_overlay(
                overlay,
                GPS.EditorLocation(buf, sloc.line(), 1),
                GPS.EditorLocation(buf, sloc.line(), 1))

# this variable is used to clear GNATprove messages whenever a new
# builder action is run. It is too early to do it in the menu entry
# callbacks, so we have to do it when starting to parse the tool
# output. See GNATprove_Parser.on_stdout.
# This variable cannot be a member of GNATprove_Parser, because we don't have
# access to the instance in this plugin.
should_clear_messages = True

class GNATprove_Parser(tool_output.OutputParser):
    """Class that parses messages of the gnatprove tool, and creates
    GNATprove_Message objects instead of GPS.Message objects"""

    def __init__(self, child):
        tool_output.OutputParser.__init__(self, child)
        should_clear_messages = True
        regex = "([^:]+):([0-9]+):([0-9]+):(.*)"
        self.comp_msg_regex = re.compile(regex)

    def error_msg_from_json(self,msg):
        """Given a JSON dict that contains the data for a message, print a
        corresponding "compiler-like" message on the GPS Console"""
        text = msg["file"] + ":" + str(msg["line"]) + ":" + str(msg["col"]) +\
                ": " + msg["message"] + "\n"
        GPS.Console("Messages").write(text)

    def on_stdout(self,text,command):
        # On the first message, we assume the tool has been run; clear the
        # locations view of the gnatprove messages.
        global should_clear_messages
        if should_clear_messages:
            should_clear_messages = False
            gnatprove_plug.clean_locations_view()
        lines = text.splitlines()
        for line in lines:
            try:
                # we first try to parse a JSON dict
                msg = json.loads(line)
                # it actually was a JSON dict, we print something that looks
                # like regular GNATProve output on the console
                self.error_msg_from_json(msg)
                if msg.has_key("tracefile"):
                    tracefile = msg["tracefile"]
                else:
                    tracefile = ""
                m = GNATprove_Message(
                        toolname,
                        GPS.File(msg["file"]),
                        msg["line"],
                        msg["col"],
                        msg["message"],
                        0,
                        tracefile=tracefile)
            except ValueError:
                # it's a non-JSON message
                # we can print it as-is
                GPS.Console("Messages").write(line + "\n")
                # we try to parse a file:line:msg format
                m = re.match(self.comp_msg_regex, line)
                if m:
                    fn, line, col, msg_text = m.group(1,2,3,4)
                    msg_text = msg_text.replace('<','&lt;')
                    msg_text = msg_text.replace('>','&gt;')
                    m = GNATprove_Message(
                            toolname,
                            GPS.File(fn),
                            int(line),
                            int(col),
                            msg_text,
                            0)

# not used anymore. kept for possible future use.
def is_subp_decl_context(self):
    """Check whether the given context is the context of a subprogram
       declaration."""
    if isinstance (self, GPS.EntityContext) and \
       self.entity() and \
       self.entity().is_subprogram() and \
       self.entity().declaration() and \
       self.location().file() == self.entity().declaration().file() and \
       self.location().line() == self.entity().declaration().line():
        return True
    else:
        return False

# not used anymore. kept for possible future use.
def is_subp_body_context(self):
    """Check whether the given context is the context of a subprogram
       body."""
    if isinstance (self, GPS.EntityContext) and \
       self.entity() and \
       self.entity().is_subprogram() and \
       self.entity().body() and \
       self.location().file() == self.entity().body().file() and \
       self.location().line() == self.entity().body().line():
        return True
    else:
        return False

# not used anymore. kept for possible future use.
def is_subp_context(self):
    """Check whether the given context is the context of a subprogram
       body or declaration."""
    return is_subp_decl_context(self) or is_subp_body_context(self)

def is_msg_context(self):
    """This is the context in which "Show Path" may appear."""
    return isinstance(self, GPS.FileContext)

# It's more convenient to define these callbacks outside of the plugin class
def generic_on_analyze(target):
    global should_clear_messages
    should_clear_messages = True
    GPS.BuildTarget(target).execute(synchronous=False)

def on_examine_all(self):
    generic_on_analyze(examine_all)

def on_examine_root_project(self):
    generic_on_analyze(examine_root_project)

def on_examine_file(self):
    generic_on_analyze(examine_file)

def on_prove_all(self):
    generic_on_analyze(prove_all)

def on_prove_root_project(self):
    generic_on_analyze(prove_root_project)

def on_prove_file(self):
    generic_on_analyze(prove_file)

def on_prove_line(self):
    generic_on_analyze(prove_line)

def on_show_report(self):
    gnatprove_plug.show_report()

def on_clear_highlighting(self):
    gnatprove_plug.clear_highlighting()

def on_clean_up(self):
    generic_on_analyze(clean_up)

def mk_loc_string (sloc):
    locstring = os.path.basename(sloc.file().name()) + ":" + str(sloc.line())
    return locstring

def subprogram_start(cursor):
   """Return the start of the subprogram that we are currently in"""
   # This function has been copied and modified from plug-in "expanded_code"
   blocks = {"CAT_PROCEDURE":1, "CAT_FUNCTION":1}

   if cursor.block_type() == "CAT_UNKNOWN":
      return None

   min = cursor.buffer().beginning_of_buffer()
   while not blocks.has_key (cursor.block_type()) and cursor > min:
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
        buf = GPS.EditorBuffer.get(curloc.file())
        edloc = GPS.EditorLocation(buf, curloc.line(), curloc.column())
        start_loc = subprogram_start(edloc)
    except:
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
        entity = GPS.Entity(
                    name,
                    start_loc.buffer().file(),
                    start_loc.line(),
                    start_loc.column())
    except:
        return None
    if entity:
        return entity.declaration()
    else:
        return None

def inside_subp_context(self):
    """Return True if the context is inside a subprogram declaration or body"""
    if compute_subp_sloc(self):
        return 1
    else:
        return 0

def generic_action_on_subp(self,action):
    """execute the action on the the given subprogram entity
    """
    # The argument --limit-subp is not defined in the examine_subp/prove_subp
    # build targets, because we have no means of designating the proper
    # location at that point.  A mild consequence is that --limit-subp does not
    # appear in the editable box shown to the user, even if appears in the
    # uneditable argument list displayed below it.
    global should_clear_messages
    should_clear_messages = True
    loc = compute_subp_sloc(self)
    if loc:
        target = GPS.BuildTarget(action)
        target.execute(
            extra_args="--limit-subp="+mk_loc_string (loc),
            synchronous=False)

def on_examine_subp(self):
    """execute the "examine subprogram" action on the the given subprogram
       entity
    """
    generic_action_on_subp(self,examine_subp)

def on_prove_subp(self):
    """execute the "prove subprogram" action on the the given subprogram entity
    """
    generic_action_on_subp(self,prove_subp)

class GNATProve_Plugin:
    """Class to contain the main functionality of the GNATProve_Plugin"""

    def __init__(self):
        GPS.parse_xml(xml_gnatprove)
        GPS.parse_xml(xml_gnatprove_menus % {"prefix":prefix})
        self.messages = []
        self.trace_msg = None

    def add_msg(self,msg):
        """register the given message as a gnatprove message. objects of
        GNATprove_Message are automatically registered.
        """
        self.messages.append(msg)

    def clear_messages(self):
        """reset the list of messages"""
        self.messages = []

    def show_report(self):
        """show report produced in gnatprove/gnatprove.out"""
        objdirs = GPS.Project.root().object_dirs()
        default_objdir = objdirs[0]
        report_file = os.path.join(default_objdir, obj_subdir_name, report_file_name)
        # if build mode is not the default one, the report file may be found in
        # the parent directory of the current object directory
        if not os.path.exists(report_file):
            if default_objdir.endswith(os.sep):
                default_objdir = default_objdir[:-(len(os.sep))]
            default_objdir = os.path.dirname(default_objdir)
            candidate_report_file = os.path.join(default_objdir, obj_subdir_name, report_file_name)
            # if the report file is still not found, leave the original path
            # so that the error message mentions this one
            if os.path.exists(candidate_report_file):
                report_file = candidate_report_file
        buf = GPS.EditorBuffer.get(GPS.File(report_file))
        v = buf.current_view()
        GPS.MDI.get_by_child(v).raise_window()

    def clear_highlighting(self):
        """delete the traces for all registered messages"""
        for msg in self.messages:
            msg.clear_highlighting()

    def register_trace(self,msg):
        if self.trace_msg:
            self.trace_msg.toggle_trace()
        self.trace_msg = msg

    def remove_trace(self,msg):
        if self.trace_msg == msg:
            self.trace_msg = None

    def clean_locations_view(self):
        """clean up the locations view: delete the "gnatprove" category,
           remove traces of the gnatprove messages, and remove the messages
           themselves
        """
        self.clear_highlighting()
        self.clear_messages()
        self.trace_msg = None
        GPS.Locations.remove_category(toolname)

# Check for GNAT toolchain: gnatprove
gnatprove = os_utils.locate_exec_on_path(toolname)

if gnatprove:
    # Check for SPARK 2005 toolchain: spark
    spark2005 = os_utils.locate_exec_on_path ("spark")

    # Rename menu into "SPARK 2014" if there is already a menu "SPARK" for
    # SPARK 2005 toolset.

    if spark2005:
        prefix = "SPARK 2014"
        menu_prefix = "/" + prefix

    example_root=os.path.dirname (os.path.dirname(gnatprove)).replace('\\', '/')+\
                  '/share/examples/spark'
    xml_gnatprove_menus = xml_gnatprove_menus.replace('@EXAMPLE@', example_root)

    gnatprove_plug = GNATProve_Plugin()
