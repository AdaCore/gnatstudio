"""This file provides support for using the GNATprove tool.

   GNATprove is a tool to apply automatic program proof to Ada programs.
   This plugin allows the user to perform a proof run and integrate its output
   into GPS.
   See menu Prove.
"""

############################################################################
## No user customization below this line
############################################################################

import GPS, os_utils, os.path, tool_output, re

xml_gnatprove = """<?xml version="1.0"?>
  <GNATPROVE>

    <tool name="GNATprove" package="Prove" attribute="switches" index="">
      <language>Ada</language>
      <switches columns="2" lines="3" switch_char="-">
        <title line="1">Proof</title>
        <combo line="1" label="Report mode" switch="--report" separator="="
               noswitch="fail" tip="Amount of information reported">
          <combo-entry label="fail" value="fail"
                       tip="Only failed proof attempts"/>
          <combo-entry label="all" value="all"
                       tip="All proof attempts"/>
          <combo-entry label="detailed" value="detailed"
                       tip="Detailed proof attempts"/>
        </combo>
        <spin label="Prover timeout" switch="--timeout="
              default="1" min="1" max="3600"
              tip="Set the prover timeout (in s) for individual VCs" />
        <spin label="Prover max steps" switch="--steps="
              default="0" min="0" max="1000000"
              tip="Set the prover maximum number of steps for individual VCs" />
        <title line="1" column="2">Process control</title>
        <spin label="Multiprocessing" column="2" switch="-j"
              default="1" min="1" max="100"
              tip="Use N processes to compile and prove. On a multiprocessor machine compilation and proof will occur in parallel" />
      </switches>
    </tool>

    <target-model name="gnatprove">
       <description>Target model for GNATprove</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="2" lines="2">
         <title column="1" line="1" >Compilation</title>
         <check label="Ignore cached results" switch="-f" column="1"
                tip="All actions are redone entirely, including compilation and proof" />
         <check label="Report Proved VCs" switch="--report=all" column="1"
                tip="Report the status of all VCs, including those proved" />
         <title column="2" line="1" >Proof</title>
         <spin label="Prover Timeout" switch="--timeout=" column="2"
                default="1" min="1" max="3600"
                tip="Set the prover timeout (in s) for individual VCs" />
         <field label="Alternate Prover" switch="--prover=" column="2"
                tip="Alternate prover to use instead of Alt-Ergo" />
       </switches>
    </target-model>

    <target model="gnatprove" name="Prove All" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--show-tag</arg>
          <arg>-U</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
       </output-parsers>
    </target>

    <target model="gnatprove" name="Prove Root Project" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--show-tag</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
       </output-parsers>
    </target>

    <target model="gnatprove" name="Prove File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--show-tag</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
       </output-parsers>
    </target>

    <target model="gnatprove" name="Prove Subprogram" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--show-tag</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
       </output-parsers>
    </target>

    <target model="gnatprove" name="Prove Line" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--ide-progress-bar</arg>
          <arg>--show-tag</arg>
          <arg>--limit-line=%f:%l</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf_converter
         progress_parser
         gnatprove_parser
       </output-parsers>
    </target>

    <target-model name="gnatprovable">
       <description>Target model for GNATprove in detection mode</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="1" lines="1">
         <title column="1" line="1" >Compilation</title>
         <check label="Ignore cached results" switch="-f" column="1"
                tip="All actions are redone entirely, including compilation and proof" />
       </switches>
    </target-model>

    <target model="gnatprovable" name="Show Unprovable Code" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=detect</arg>
          <arg>-f</arg>
       </command-line>
    </target>

    <target-model name="gnatprove_clean">
       <description>Target model for GNATprove for cleaning</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="1" lines="1">
         <title column="1" line="1" >Compilation</title>
         <check label="Ignore cached results" switch="-f" column="1"
                tip="All actions are redone entirely, including compilation and proof" />
       </switches>
    </target-model>

    <target model="gnatprove_clean" name="Clean Proofs" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
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
prefix               = "Prove"
menu_prefix          = "/" + prefix
prove_all            = "Prove All"
prove_root_project   = "Prove Root Project"
prove_file           = "Prove File"
prove_line           = "Prove Line"
prove_subp           = "Prove Subprogram"
show_unprovable_code = "Show Unprovable Code"
clean_up             = "Clean Proofs"
clear_highlighting   = "Remove Editor Highlighting"

Default_colors = {
  "info"    : "#88eeaa",
  "error"   : "#f75d59",
  "trace"   : "#00ffff" }

Pref_Names = {}

Tag_regex = re.compile("\[(.*)\]$")

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

    def __init__(self, category, f, line, col, text, flags):
        """initialize state for GNATprove_Message"""
        match = Tag_regex.search(text)
        if match:
            self.tag = match.group(1)
            text = text[0:match.start()-1]
        else:
            self.tag = None
        GPS.Message.__init__(self, category, f, line, col, text, flags)
        self.trace_visible = False
        self.lines = []
        self.is_info_message = (text.find("info:") != -1)
        self.highlight_message()
        self.load_trace_if_needed()
        # register ourselves in the gnatprove plugin
        gnatprove_plug.add_msg(self)

    def load_trace_if_needed(self):
        if not self.is_info_message and self.tag:
            fn = self.compute_trace_filename()
            if os.path.isfile(fn):
                self.parse_trace_file(fn)
                self.set_subprogram(
                    lambda m : m.toggle_trace(),
                    "gps-semantic-check",
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
        if self.lines:
            first_sloc = self.lines[0]
            buf = GPS.EditorBuffer.get(first_sloc.file())
            if buf:
                overlay = get_overlay(buf,"trace")
                buf.remove_overlay(overlay)
        self.trace_visible = False

    def compute_trace_filename(self):
        """compute the trace file name in which the path information is
           stored
        """
        objdirs = GPS.Project.root().object_dirs()
        gnatprove_dir = os.path.join(objdirs[0], "gnatprove")
        fn = "%(file)s_%(line)d_%(col)d_%(tag)s.trace" % \
             { "file" : os.path.basename(self.get_file().name()),
               "line" : self.get_line(),
               "col"  : self.get_column(),
               "tag"  : self.tag }
        return os.path.join(gnatprove_dir, fn)

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

    def parse_trace_file(self, fn):
        """parse the trace file (a list of file:line information) and store it
            in the list self.lines
        """
        with open(fn,"r") as f:
            for line in f:
                sl = line.split(':')
                self.lines.append(GPS.FileLocation(GPS.File(sl[0]),int(sl[1]), 1))


    def show_trace(self):
        """show the trace of the message. If necessary, read the associated
           trace file to load the information.
        """
        self.trace_visible = True
        if self.lines:
            first_sloc = self.lines[0]
            buf = GPS.EditorBuffer.get(first_sloc.file())
            goto_location(first_sloc)
            overlay = get_overlay(buf, "trace")
            buf.remove_overlay(overlay)
            for sloc in self.lines:
                buf.apply_overlay(
                    overlay,
                    GPS.EditorLocation(buf, sloc.line(), 1),
                    GPS.EditorLocation(buf, sloc.line(), 1))

class GNATprove_Parser(tool_output.OutputParser):
    """Class that parses messages of the gnatprove tool, and creates
    GNATprove_Message objects instead of GPS.Message objects"""

    def on_stdout(self,text):
        lines = text.splitlines()
        for line in lines:
            sl = line.split(':')
            if len(sl) >= 4:
                m = GNATprove_Message(
                        toolname,
                        GPS.File(sl[0]),
                        int(sl[1]),
                        int(sl[2]),
                        ':'.join(sl[3:]).strip(),
                        0)
            else:
                print line

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

def is_subp_context(self):
    """Check whether the given context is the context of a subprogram
       body or declaration."""
    return is_subp_decl_context(self) or is_subp_body_context(self)

def is_ada_file_context(self):
    """Check whether the given context is the context of an Ada file. This is
    used to check whether one can do "Prove File" although it is a necessary,
    but not sufficient condition (e.g. one cannot run gnatprove on a spec file
    which has a body)."""
    # self.file() may raise an exception even if the context is a
    # GPSFileContext, so we wrap the whole thing in a except block
    try:
        return isinstance (self, GPS.FileContext) \
            and self.file() \
            and self.file().language().lower() == "ada"
    except:
        return False

def is_msg_context(self):
    """This is the context in which "Show Path" may appear."""
    return isinstance(self, GPS.FileContext)

# It's more convenient to define these callbacks outside of the plugin class
def generic_on_prove(target):
    gnatprove_plug.clean_locations_view()
    GPS.BuildTarget(target).execute(synchronous=False)

def on_prove_all(self):
    generic_on_prove(prove_all)

def on_prove_root_project(self):
    generic_on_prove(prove_root_project)

def on_prove_file(self):
    generic_on_prove(prove_file)

def on_prove_line(self):
    generic_on_prove(prove_line)

def on_clear_highlighting(self):
    gnatprove_plug.clear_highlighting()

def on_show_unprovable_code(self):
    GPS.BuildTarget(show_unprovable_code).execute(synchronous=False)

def on_clean_up(self):
    generic_on_prove(clean_up)

def mk_loc_string (sloc):
    locstring = os.path.basename(sloc.file().name()) + ":" + str(sloc.line())
    return locstring

def on_prove_subp(self):
    """execute the "prove subprogram" action on the the given subprogram entity
    """
    # The argument --limit-subp is not defined in the prove_subp build target,
    # because we have no means of designating the proper location at that point.
    # A mild consequence is that --limit-subp does not appear in the editable
    # box shown to the user, even if appears in the uneditable argument list
    # displayed below it.
    gnatprove_plug.clean_locations_view()
    loc = self.entity().declaration()
    target = GPS.BuildTarget(prove_subp)
    target.execute(
        extra_args="--limit-subp="+mk_loc_string (loc),
        synchronous=False)

class GNATProve_Plugin:
    """Class to contain the main functionality of the GNATProve_Plugin"""

    def __init__(self):
        GPS.Menu.create(menu_prefix, ref = "Window", add_before = True)
        GPS.Menu.create(menu_prefix + "/" + prove_all, on_prove_all)
        GPS.Menu.create(
            menu_prefix + "/" + prove_root_project,
            on_prove_root_project)
        GPS.Menu.create(
            menu_prefix + "/" + prove_file,
            on_prove_file,
            filter = is_ada_file_context)
        GPS.Menu.create(
            menu_prefix + "/" + show_unprovable_code,
            on_show_unprovable_code)
        GPS.Menu.create(
            menu_prefix + "/" + clean_up,
            on_clean_up)
        GPS.Menu.create(
            menu_prefix + "/" + clear_highlighting,
            on_clear_highlighting)
        GPS.Contextual(prefix + "/" + prove_file).create(
            on_activate = on_prove_file,
            filter = is_ada_file_context)
        GPS.Contextual(prefix + "/" + prove_line).create(
            on_activate = on_prove_line,
            filter = is_ada_file_context)
        GPS.Contextual(prefix + "/" + prove_subp).create(
            on_activate = on_prove_subp,
            filter = is_subp_context)
        GPS.parse_xml(xml_gnatprove)
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
        GPS.Locations.remove_category(toolname)

# Check for GNAT toolchain: gnatprove
gnatprove = os_utils.locate_exec_on_path(toolname)

if gnatprove:
    gnatprove_plug = GNATProve_Plugin()
