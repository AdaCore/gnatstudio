"""GNAT support for GPS

This file provides support for Ada and GNAT:
This includes switches in the project properties editor.
This also includes predefined search patterns.
And finally a number of predefined text aliases are defined.
"""

import GPS
import os
import os.path
import re
import sys
import traceback
import os_utils
import gnat_switches
import gps_utils
from gps_utils.switches import Check, Spin
from xml.sax.saxutils import escape

gnatmakeproc = None


def EnsureInitialized():
    global gnatmakeproc
    if gnatmakeproc is None:
        gnatmakeproc = gnatMakeProc()
    gnatmakeproc.init_switches()


def get_warnings_list(cmd="", args="make -h"):
    #  Get list of GNAT warning options using given cmd if provided
    global gnatmakeproc
    if gnatmakeproc is None:
        gnatmakeproc = gnatMakeProc()
    gnatmakeproc.ensure_switches(cmd, args)

    return gnatmakeproc.warnings_list


def Label(switch, default):
    str = None

    if switch in gnat_switches.switches_comments:
        str = re.sub("^(Activate warnings|Validity checks) (on|for) ",
                     "",
                     gnat_switches.switches_comments[switch][0].strip())
    if not str:
        str = re.sub("^turn on (checking|warnings) (on|for) ",
                     "",
                     default.strip())
    try:
        str = str[0].upper() + str[1:]
        str = re.sub("[.] *$", "", str)
    except Exception:
        GPS.Logger("GNAT_SWITCHES").write(
            "Label could not be parsed for switch: %s" % (switch))

    return escape(str)


def Tip(switch, default):
    if switch in gnat_switches.switches_comments:
        str = gnat_switches.switches_comments[switch][1].strip()
    else:
        str = default.strip()

    return escape(str)


def Warning(switch, defaulttip, defaultstate, before=False):
    """Returns a Warning switch object"""
    sw = "-gnatw" + switch
    return Check(sw, "-gnatw" + switch.upper(),
                 Label(sw, defaulttip),
                 Tip(sw, defaulttip),
                 defaultstate, before)


def Validity(switch, defaulttip, defaultstate, before=False):
    """Returns a Validity Check switch object"""
    sw = "-gnatV" + switch
    return Check(sw, "-gnatV" + switch.upper(),
                 Label(sw, defaulttip),
                 Tip(sw, defaulttip),
                 defaultstate, before)


def StyleSpin(switch, defaulttip, defaultval, minval, maxval, before=False):
    """Returns a SyleSpin switch object"""
    sw = "-gnaty" + switch
    return Spin(sw, "",
                Label(sw, defaulttip),
                Tip(sw, defaulttip), "",
                defaultval, minval, maxval, before)


def Style(switch, defaulttip, defaultstate, before=False):
    """Returns a Syle switch object"""
    sw = "-gnaty" + switch
    return Check(sw, "",
                 Label(sw, defaulttip),
                 Tip(sw, defaulttip),
                 defaultstate, before)


class gnatMakeProc:

    """This class controls the gnatmake execution"""

    def __init__(self):
        self.warnings_list = []
        self.validity_checks_list = []
        self.style_checks_list = []
        self.gnatCmd = ""

    def init_switches(self):
        # ensure_switches returns true if the gnat command is not identical to
        # the previous one. In this case, we need to recreate the whole xml
        # tree and call GPS.parse_xml to update the switch editor.
        if self.ensure_switches():
            try:
                xmlCompiler = self.__get_xml()
            except Exception:
                GPS.Console("Messages").write(
                    "Exception thrown in ada_support.py:\n")
                name = sys.exc_info()[1]
                tb = traceback.format_list(
                    traceback.extract_tb(sys.exc_info()[2]))
                for info in tb:
                    GPS.Console("Messages").write(info)
                GPS.Console("Messages").write(str(name) + "\n")
                xmlCompiler = xmlCompilerHead + \
                    xmlCompilerDefault + xmlCompilerTrailer
            GPS.parse_xml(
                """<?xml version="1.0" ?><GPS>""" + xmlCompiler + "</GPS>")

    def get_warnings_list(self):
        self.init_switches()
        return self.warnings_list

    def get_validity_checks_list(self):
        self.init_switches()
        return self.validity_checks_list

    def get_style_checks_list(self):
        self.init_switches()
        return self.style_checks_list

    def ensure_switches(self, cmd="", args="make -h"):
        prev_cmd = self.gnatCmd
        if cmd == "":
            self.gnatCmd = gps_utils.get_gnat_driver_cmd()
        else:
            self.gnatCmd = cmd

        if self.gnatCmd == "":
            self.gnatCmd = "gnat"

        if GPS.is_server_local("Build_Server"):
            if not os.path.isfile(self.gnatCmd):
                cmd = os_utils.locate_exec_on_path(self.gnatCmd)
                if cmd == "":
                    GPS.Console("Messages").write(
                        "Error: '%s' is not in the path.\n" % self.gnatCmd)
                    GPS.Console("Messages").write(
                        "Error: Could not initialize the ada_support module.\n"
                    )
                    return

                self.gnatCmd = cmd

        # gnat check command changed: we reinitialize the rules list
        if prev_cmd != self.gnatCmd:
            self.__get_switches_from_help(self.gnatCmd, args)
            return True
        else:
            return False

    def __get_xml(self):
        xml = """
            <popup label="Warnings" line="2" column="1" lines="2" columns="3">
             <title line="1" column="1" column-span="3">Global switches</title>
             <title line="2" column="1" column-span="3">Warnings</title>
        """
        n = 0
        for switch in self.warnings_list:
            if switch.Switch() in (
                "-gnatwa", "-gnatw.e", "-gnatws", "-gnatwe"
            ):
                line = "1"
                if switch.Switch() in ("-gnatwa", "-gnatwe"):
                    col = "1"
                else:
                    col = "2"
            else:
                line = "2"
                if n <= (len(self.warnings_list) - 4) / 3:
                    col = "1"
                elif n <= (len(self.warnings_list) - 4) * 2 / 3:
                    col = "2"
                else:
                    col = "3"
                n = n + 1
            xml += switch.Xml(line, col)
        xml += """
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatwa" master-status="on"
                    slave-switch="-gnatws" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatwa" master-status="on"
                    slave-switch="-gnatw.e" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatw.e" master-status="on"
                    slave-switch="-gnatws" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatw.e" master-status="on"
                    slave-switch="-gnatwa" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatws" master-status="on"
                    slave-switch="-gnatwa" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatws" master-status="on"
                    slave-switch="-gnatw.e" slave-status="off" />
        <expansion switch="-gnatw" />
      </popup>
      <popup label="Validity checking mode"
             line="2" column="1" lines="2" columns="1" >
        <title line="1" column="1">Global switches</title>
        <title line="2" column="1">Checks</title>
"""
        for switch in self.validity_checks_list:
            if switch.Switch() == "-gnatVa" or switch.Switch() == "-gnatVn":
                line = "1"
            else:
                line = "2"
            xml += switch.Xml(line, "1")
        xml += """
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatVa" master-status="on"
                    slave-switch="-gnatVn" slave-status="off" />
        <dependency master-page="Ada" slave-page="Ada"
                    master-switch="-gnatVn" master-status="on"
                    slave-switch="-gnatVa" slave-status="off" />
        <expansion switch="-gnatV" />
      </popup>
      <popup label="Style checks" line="2" column="1">
"""
        default_style_alias = "-gnaty3abcefhiklmnprst"
        if "-gnatyy" in gnat_switches.switches_comments:
            style_alias_res = re.split(
                "^ *This is equivalent to ([^., ]*).*",
                gnat_switches.switches_comments["-gnatyy"][1]
            )
            if len(style_alias_res) > 1:
                default_style_alias = "-" + style_alias_res[1]

        for switch in self.style_checks_list:
            if switch.Switch() == "-gnatyy":
                xml += '<expansion switch="-gnatyy" alias="-gnaty" />'
            elif switch.Switch() == "-gnatyg":
                xml += '<expansion switch="-gnatyg" alias="%s" />' % (
                    default_style_alias + "dISux")
            else:
                xml += switch.Xml("1", "1")
        xml += '<expansion switch="-gnatym" alias="-gnatyM79" />'
        xml += """<expansion switch="-gnaty" alias="%s" />""" % (
            default_style_alias)
        xml += """
         <expansion switch="-gnaty"/>
      </popup>
"""
        xmlCompiler = xmlCompilerHead + xml + xmlCompilerTrailer
        return xmlCompiler

    def __add_switch_callback(self, process, matched, unmatched):
        if unmatched.startswith("\n"):
            line = self.msg
            self.msg = ""
            if re.search("^ +[-]gnatwxx", line):
                self.warnings_analysis = True
            elif re.search("^ +[-]gnatVxx", line):
                self.validity_checks_analysis = True
            elif re.search("^ +[-]gnatyxx", line):
                self.style_checks_analysis = True
            elif re.search("^ +[-]gnat", line):
                self.warnings_analysis = False
                self.validity_checks_analysis = False
            elif self.style_checks_analysis and re.search("^ *[-]", line):
                self.style_checks_analysis = False

            elif self.warnings_analysis:
                res = re.split("^ *([^ *+]+)([*]?)([+]?) +(.+) *$", line)
                i_sw = 1
                i_star = 2
                i_plus = 3
                i_desc = 4
                if len(res) > 2:
                    if res[i_sw] == "a":
                        # retrieve the list of warnings not activated by
                        # -gnatwa Note that this is not used anymore with
                        # recent GNAT version: the all_warnings_exception_list
                        # is now determined by '+' sign after the switch
                        # definition
                        exception = re.split(
                            "\(except ([a-zA-Z. ]*)\) *$", res[i_desc])
                        if len(exception) > 1:
                            self.all_warnings_exception_list = re.findall(
                                "[.]?[a-zA-Z]", exception[1] + ".e")
                        self.warnings_list.append(
                            Warning(res[i_sw], res[i_desc], False, True))

                    elif res[i_sw] in ("e", ".e", "s"):
                        # include the global switches directly.
                        self.warnings_list.append(
                            Warning(res[i_sw], res[i_desc], False, True))

                    # include only on warnings, and a limited list of global
                    # warnings (gnatwa, gnatws, gnatw.e)
                    elif (
                        not re.search("turn off", res[i_desc]) and not
                            re.search("(all|every)", res[i_desc]) and not
                            re.search("^normal warning", res[i_desc])
                    ):
                        # two ways to determine if the switch is part of gnatwa
                        # or not: the old way used the gnatwa exception list,
                        # that was part of the -gnatwa description, while we
                        # now use with recent gnat versions the '+' sign after
                        # the switch description
                        if len(self.all_warnings_exception_list) == 0:
                            is_alias_part = res[i_plus] == "+"
                        else:
                            is_alias_part = False
                            # switches activated by default are not part of
                            # gnatwa
                            if res[i_star] == "*":
                                is_alias_part = False
                            else:
                                if re.search("turn on", res[i_desc]):
                                    # part of gnatwa, unless explicitely part
                                    # of the gnatwa exception list search if
                                    # warning is not part of gnatwa
                                    is_alias_part = True
                                    for ex in self.all_warnings_exception_list:
                                        if ex == res[i_sw]:
                                            is_alias_part = False
                                            break

                        # warnings_list is a list of [switch, description,
                        # default value, part_of_gnatwa] remove the 'turn on'
                        # in the description
                        warn = Warning(
                            res[i_sw], res[i_desc], res[i_star] == "*")
                        if is_alias_part:
                            warn.Add_Default_Val_Dependency("-gnatwa", True)
                        warn.Add_Default_Val_Dependency("-gnatw.e", True)
                        warn.Add_Default_Val_Dependency("-gnatws", False)
                        self.warnings_list.append(warn)

            elif self.validity_checks_analysis:
                res = re.split("^ *([^ *]+) +(.+) *$", line)
                if len(res) > 2:
                    if res[1] == "a" or res[1] == "n":
                        self.validity_checks_list.append(
                            Validity(res[1], res[2], False, True))
                    elif res[1].lower() == res[1]:
                        val = Validity(res[1], res[2], res[1] == "d")
                        if res[1] != "d":
                            val.Add_Default_Val_Dependency("-gnatVa", True)
                        val.Add_Default_Val_Dependency("-gnatVn", False)
                        self.validity_checks_list.append(val)

            elif self.style_checks_analysis:
                res = re.split("^ *(1[-]9|.)(n*) +(.+) *$", line)
                if len(res) > 2:
                    if res[1] == "1-9":
                        self.style_checks_list.append(StyleSpin(
                            switch="", defaulttip=res[3], defaultval=0,
                            minval=0, maxval=9, before=True))

                    # no parameters. Do not include -gnatyN (remove all checks)
                    # and -gnatym (alias of -gnatyM79)
                    elif res[1] != "N" and res[1] != "m":
                        if res[2] == "":
                            self.style_checks_list.append(
                                Style(switch=res[1], defaulttip=res[3],
                                      defaultstate=False))
                        else:
                            self.style_checks_list.append(StyleSpin(
                                switch=res[1], defaulttip=res[3],
                                defaultval=0, minval=0, maxval=32768))

        self.msg += matched

    def __get_switches_from_help(self, gnatCmd, args):
        # Verify we have the correct gnatcheck executable
        self.warnings_list = []
        self.validity_checks_list = []
        self.style_checks_list = []
        self.all_warnings_exception_list = []
        self.warnings_analysis = False
        self.validity_checks_analysis = False
        self.style_checks_analysis = False

        if gnatCmd != "":
            # Then retrieve warnings/style/restriction checks from gnatmake
            self.msg = ""
            # ??? We don't spawn this process on the build server as this leads
            # to undesired results: this spawn command becomes asynchronous
            # because of the rsync commands that are enqueued. Thus the result
            # of the gnatmake -h analysis arrives after the switches dialog is
            # created, leading to empty boxes.
            # The behavior is then to try getting a valid gnat make command
            # from the local machine, and fallback to the default switches if
            # not found.
            process = GPS.Process(
                "\"\"\"" + gnatCmd + "\"\"\" " + args,
                "^.+\r?$",
                on_match=self.__add_switch_callback,
                remote_server="Build_Server")
            process.get_result()
        return True

# Constant definitions: those are switches that we define for all versions of
# GNAT. Those constants also contain the default values that we use when we
# did not manage to correctly parse the gnatmake output.


xmlCompilerHead = """
   <tool name="Builder" package="Builder" index="ada" override="True">
      <language>Ada</language>
      <initial-cmd-line></initial-cmd-line>
      <switches>
         <title column="1" line="1" >Dependencies</title>
         <title column="2" line="1" >Compilation</title>
         <check label="Recompile if switches changed" switch="-s"
                tip="Recompile if compiler switches have changed since last\
 compilation" />
         <check label="Minimal recompilation" switch="-m"
                tip="Specifies that the minimum necessary amount of\
 recompilation be performed. In this mode, gnatmake ignores time stamp\
 differences when the only modification to a source file consist in adding or\
 removing comments, empty lines, spaces or tabs" />

         <spin label="Multiprocessing" switch="-j" min="0" max="100"\
 default="1"
               column="2"
               tip="Use N processes to carry out the compilations. On a\
 multiprocessor machine compilations will occur in parallel" />
         <check label="Keep going" switch="-k" column="2"
                tip="Continue as much as possible after a compilation error" />
         <check label="Debug information" switch="-g" column="2"
                tip="Add debugging information. This forces the corresponding\
 switch for the compiler, binder and linker" />
         <check label="Use mapping file" switch="-C" column="2"
                tip="Use a mapping file. A mapping file is a way to\
 communicate to the compiler two mappings: from unit name to file names, and\
 from file names to path names. This will generally improve the compilation\
 time" />
      </switches>
   </tool>

   <tool name="Ada" package="Compiler" index="ada" override="true">
      <language>Ada</language>
      <initial-cmd-line>-g -gnatQ</initial-cmd-line>
      <switches>
         <title line="1" column="1" >Code generation</title>
         <title line="1" column="2" >Run-time checks</title>
         <title line="2" column="1" line-span="2" >Messages</title>
         <title line="3" column="1" line-span="0" />
         <title line="2" column="2" >Debugging</title>
         <title line="3" column="2" >Syntax</title>
         <title line="4" column="1" >Stack usage</title>
         <title line="4" column="2" ></title>

         <check label="Generate stack usage information"\
 switch="-fcallgraph-info=su,da" line="4" column="1"/>
         <combo switch="-O" nodigit="1" noswitch="0"
                tip="Controls the optimization level">
            <combo-entry label="No optimization" value="0" />
            <combo-entry label="Some optimization" value="1" />
            <combo-entry label="Full optimization" value="2" />
            <combo-entry label="Full + Automatic inline" value="3" />
         </combo>
         <check label="Inlining" switch="-gnatn"
                tip="Enable inlining of Ada subprograms marked with a pragma\
 Inline" />
         <check label="Unroll loops" switch="-funroll-loops"
                tip="Perform the optimization of loop unrolling. This is only\
 done for loops whose number of iterations can be determined at compile time\
 or run time" />
         <check label="Position independent code" switch="-fPIC"
                tip="If supported for the target machine, emit\
 position-independent code, suitable for dynamic linking and avoiding any\
 limit of the size of the global offset table" />

         <check label="Always generate ALI file" switch="-gnatQ"
                tip="Don't quit, write ali/tree file even if compile errors" />

         <check label="Separate function sections" switch="-ffunction-sections"
                tip="Generate each function in a separate section. See also\
 -fdata-sections and --gc-sections linker flag" />

         <check label="Separate data sections" switch="-fdata-sections"
                tip="Generate each global data in a separate section. See also\
 -ffunction-sections and --gc-sections linker flag" />

         <check label="Full errors" switch="-gnatf" line="2" column="1"
                tip="Full Errors. Multiple errors per line, all undefined\
 references" />
         <check label="Overflow checking" switch="-gnato" column="2"
                tip="Enable numerics overflow checking" />
         <check label="Suppress all checks" switch="-gnatp" column="2"
                tip="Suppress all checks" />
         <check label="Stack checking" switch="-fstack-check" column="2"
                tip="Generate code to verify that you do not go beyond the\
 boundary of the stack. You should specify this flag if you are running in an\
 environment with multiple threads, but only rarely need to specify it in a\
 single-threaded environment" />
         <check label="Dynamic elaboration" switch="-gnatE" column="2"
                tip="Full dynamic elaboration checks" />

          <check label="Debug Information" switch="-g" line="2" column="2"
                 tip="Debug Information" />
          <dependency master-page="Builder" slave-page="Ada"
                      master-switch="-g" slave-switch="-g"
                      master-status="on" slave-status="on" />
          <check label="Enable assertions" switch="-gnata" line="2" column="2"
                 tip="Assertions enabled. Pragma Assert and pragma Debug are\
 activated" />

          <!--  Do not use radio buttons here, since this would always generate
                at least one of the switches, and the default is different for
                each compiler anyway (GNATPro, GNAT GAP,...) -->
          <check label="Ada 83 mode" switch="-gnat83" line="3" column="2"
                 tip="Override the compiler's default, and enforces Ada 83\
 restrictions" />
          <check label="Ada 95 mode" switch="-gnat95" line="3" column="2"
                 tip="Override the compiler's default, and compile in Ada 95\
 mode" />
          <check label="Ada 2005 mode" switch="-gnat05" line="3" column="2"
                 tip="Override the compiler's default, and activate Ada 2005\
 language features" />
          <check label="Ada 2012 mode" switch="-gnat12" line="3" column="2"
                 tip="Override the compiler's default, and activate Ada 2012\
 language features" />
"""

xmlCompilerDefault = """
         <popup label="Warnings" line="2" column="1">
            <check label="Turn on all optional warnings (except dhl.ot.w)"\
 switch="-gnatwa"
                   tip="Activates all optional warnings" />
            <check label="Failing assertions" switch="-gnatw.a"\
 switch-off="-gnatw.A" default="on" />
            <check label="Bad fixed value (not multiple of small)"\
 switch="-gnatwb" switch-off="-gnatwB" default="off" />
            <check label="Constant conditional" switch="-gnatwc"\
 switch-off="-gnatwC" default="off"
                   tip="Activates warnings for conditional expression used in\
 tests that are known to be True or False at compile time" />
            <check label="Unrepped components" switch="-gnatw.c"\
 switch-off="-gnatw.C" default="off" />
            <check label="Implicit dereference" switch="-gnatwd"\
 switch-off="-gnatwD" default="off"
                   tip="If set, the use of a prefix of an access type in an\
 indexed component, slice or selected component without an explicit .all will\
 generate a warning. With this warning enabled, access checks occur only at\
 points where an explicit .all appears" />
            <check label="Warnings=Errors" switch="-gnatwe"
                   tip="Causes warning messages to be treated as errors" />
            <check label="Unreferenced formal" switch="-gnatwf"\
 switch-off="-gnatwF" default="off"
                   tip="Causes warnings to be generated if a formal parameter\
 is not referenced in the body" />
            <check label="Unrecognized pragmas" switch="-gnatwg"\
 switch-off="-gnatwG" default="on"
                   top="Causes warnings to be generated if a pragma is not\
 recognized" />
            <check label="Hiding variable" switch="-gnatwh"\
 switch-off="-gnatwH" default="off"
                   tip="This switch activates warnings on hiding declarations.\
 A declaration is considered hiding if it is for a non-overloadable entity,\
 and if it declares an entity with the same name as some other entity that is\
 directly or use-visible" />
            <check label="Implementation unit" switch="-gnatwi"\
 switch-off="-gnatwI" default="on"
                   tip="This switch activates warnings for a with of an\
 internal GNAT implementation unit" />
            <check label="Obsolescent feature" switch="-gnatwj"\
 switch-off="-gnatwJ" default="off" />
            <check label="Constant variable" switch="-gnatwk"\
 switch-off="-gnatwK" default="off" />
            <check label="Missing elaboration pragma" switch="-gnatwl"\
 switch-off="-gnatwL" default="off"
                   tip="This switch activates warnings on missing pragma\
 Elaborate_All statements" />
            <check label="Variable assigned but not read" switch="-gnatwm"\
 switch-off="-gnatwM" default="off"/>
            <check label="Address clause overlay" switch="-gnatwo"\
 switch-off="-gnatwO" default="on"
                   tip="This switch activates warnings for possible unintended\
 initialization effects of defining address clauses that cause one variable to\
 overlap another" />
            <check label="Out parameters assigned but not read"\
 switch="-gnatw.o" switch-off="-gnatw.O" default="off" />
            <check label="Ineffective pragma inline" switch="-gnatwp"\
 switch-off="-gnatwP" default="off"
                   tip="This switch activates warnings for a failure of front\
 end inlining to inline a particular call" />
            <check label="Suspicious parameter order" switch="-gnatw.p"\
 switch-off="-gnatw.P" default="off"
                   tip="This switch activates warnings for a failure of front\
 end inlining to inline a particular call" />
            <check label="Questionable missing parentheses" switch="-gnatwq"\
 switch-off="-gnatwQ" default="on" />
            <check label="Redundant construct" switch="-gnatwr"
 switch-off="-gnatwR" default="off"
                   tip="This switch activates warnings for redundant\
 constructs:
 - Assignment of an item to itself
 - Type conversion that converts an expression to its own type
 - ..." />
           <check label="Object renaming function" switch="-gnatw.r"\
 switch-off="-gnatw.R" default="off" />
           <check label="Tracking deleted code" switch="-gnatwt"\
 switch-off="-gnatwT" default="off" />
           <check label="Unused entity" switch="-gnatwu" switch-off="-gnatwU"\
 default="off"
                  tip="This switch activates warnings to be generated for\
 entities that are defined but not referenced" />
           <check label="Unassigned variable" switch="-gnatwv"\
 switch-off="-gnatwV" default="on"/>
           <check label="Wrong low bound assumption" switch="-gnatww"\
 switch-off="-gnatwW" default="on"/>
           <check label="Pragma Warnings off" switch="-gnatw.w"\
 switch-off="-gnatw.W" default="off"/>
           <check label="Export/import" switch="-gnatwx" switch-off="-gnatwX"\
 default="on"/>
           <check label="Non-local exceptions" switch="-gnatw.x"\
 switch-off="-gnatw.X" default="off"/>
           <check label="Ada 2005 incompatibility" switch="-gnatwy"\
 switch-off="-gnatwY" default="on"/>
           <check label="Size/align warnings for unchecked conversion"
                  switch="-gnatwz" switch-off="-gnatwZ" default="on" />
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwb"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwc"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatw.c"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwf"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwj"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwk"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwm"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwp"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatw.p"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwr"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatw.r"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatwu"/>
           <default-value-dependency master-switch="-gnatwa"\
 slave-switch="-gnatw.x"/>
           <expansion switch="-gnatw" />
         </popup>

         <popup label="Validity checking mode" line="2" column="1" >
            <check label="Turn on all validity checking options"\
 switch="-gnatVa" />
            <check label="Checking for copies" switch="-gnatVc"\
 switch-off="-gnatVC" default="off"
                   tip="The right hand side of assignments, and the\
 initializing values of object declarations are validity checked" />
            <check label="Default Reference Manual Checking" switch="-gnatVd"\
 switch-off="-gnatVD" default="off" />
            <check label="Checking for elementary components" switch="-gnatVe"\
 switch-off="-gnatVE" default="off" />
            <check label="Checking for floating-point" switch="-gnatVf"\
 switch-off="-gnatVF" default="off" />
            <check label="Checking for 'in' parameters" switch="-gnatVi"\
 switch-off="-gnatVI" default="off"
                   tip="Arguments for parameters of mode in are validity\
 checked in function and procedure calls at the point of call" />
            <check label="Checking for 'in out' parameters" switch="-gnatVm"\
 switch-off="-gnatVM" default="off"
                   tip="Arguments for parameters of mode in out are validity\
 checked in procedure calls at the point of call" />
            <check label="Checking for operators and attributes"\
 switch="-gnatVo" switch-off="-gnatVO" default="off"
                   tip="Arguments for predefined operations and attributes are\
 validity checked" />
            <check label="Checking for returns" switch="-gnatVr"\
 switch-off="-gnatVR" default="off"
                   tip="The expression in return statements in functions is\
 validity checked" />
            <check label="Checking for subscripts" switch="-gnatVs"\
 switch-off="-gnatVS" default="off"
                   tip="All subscripts expressions are checked for validty" />
            <check label="Checking for tests" switch="-gnatVt"\
 switch-off="-gnatVT" default="off"
                   tip="Expressions used as conditions in if, while or exit\
 statements are checked, as well as guard expressions in entry calls" />
            <expansion switch="-gnatV" />
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVc"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVd"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVe"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVf"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVi"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVm"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVo"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVr"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVs"/>
           <default-value-dependency master-switch="-gnatVa"\
 slave-switch="-gnatVt"/>
         </popup>

         <popup label="Style checks" line="2" column="1">
            <spin label="indentation" switch="-gnaty" min="1" max="9"
                  default="3" />
            <check label="Check casing" switch="-gnatya" />
            <check label="Check end of line blanks" switch="-gnatyb"\
 default="off"/>
            <check label="Check comment format" switch="-gnatyc" />
            <check label="Check end/exit labels" switch="-gnatye" />
            <check label="Check no form feeds" switch="-gnatyf" />
            <check label="Check no horizontal tabs" switch="-gnatyh" />
            <check label="Check if-then layout" switch="-gnatyi" />
            <check label="Check casing rules" switch="-gnatyk" />
            <check label="Check reference manual layout" switch="-gnatyl" />
            <check label="Check line length &lt;= 79 characters"\
 switch="-gnatym" />
            <check label="Check casing of Standard identifiers"\
 switch="-gnatyn"/>
            <check label="Check subprogram bodies in alphabetical order"
                   switch="-gnatyo" />
            <check label="Check pragma casing" switch="-gnatyp" />
            <check label="Check RM column layout" switch="-gnatyr" />
            <check label="Check separate specs present" switch="-gnatys" />
            <check label="Check token separation rules" switch="-gnatyt" />
            <spin label="Line length" switch="-gnatyM"
                  min="0" max="255" default="79" />
            <expansion switch="-gnaty" alias="-gnaty3abcefhiklmnprst" />
            <expansion switch="-gnaty" />
         </popup>
"""

xmlCompilerTrailer = """
      </switches>
   </tool>

   <tool name="Binder" package="Binder" index="ada" override="True">
      <language>Ada</language>
      <switches>
         <check label="Store call stack in exceptions" switch="-E"
                tip="Store tracebacks in exception occurrences when the target\
 supports it" />
         <check label="List possible restrictions" switch="-r" />
         <check label="Shared GNAT run time" switch="-shared" />
      </switches>
   </tool>

   <tool name="Ada Linker" package="Linker" index="ada" override="True">
      <language>Ada</language>
      <initial-cmd-line>-g</initial-cmd-line>
      <switches>
         <check label="Strip symbols" switch="-s" />
         <check label="Debug information" switch="-g" />
         <dependency master-page="Builder" slave-page="Ada Linker"
                     master-switch="-g" slave-switch="-g"
                     master-status="on" slave-status="on" />

         <check label="Remove unused sections (GNU ld only)"
                switch="-Wl,--gc-sections"
                tip="Remove all unused sections from the link output. This is\
 a GNU ld switch. See also -ffunction-sections and -fdata-sections compiler\
 flags" />
      </switches>
   </tool>
"""
