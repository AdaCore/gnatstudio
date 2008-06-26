"""GNAT support for GPS

This file provides support for Ada and GNAT:
This includes switches in the project properties editor.
This also includes predefined search patterns.
And finally a number of predefined text aliases are defined.
"""


import GPS, os, os.path, re, string, traceback
import os_utils, gnat_switches

gnatmakeproc = None

def label (prefix, switch):
   if gnat_switches.switches_comments.has_key (prefix+switch[0]):
      return re.sub ("^Activate warnings (on|for) ", "", gnat_switches.switches_comments[prefix+switch[0]][0].strip())
   else:
      return re.sub ("^turn on warnings (on|for) ", "", switch[1].strip())

def tip (prefix, switch):
   if gnat_switches.switches_comments.has_key (prefix+switch[0]):
      return gnat_switches.switches_comments[prefix+switch[0]][1].strip()
   else:
        return switch[1].strip()

class gnatMakeProc:
   """This class controls the gnatmake execution"""
   def __init__ (self):
      self.warnings_list = []
      self.style_checks_list = []
      self.gnatCmd = ""

   def initSwitches(self):
      global ruleseditor, xmlCompilerHead, xmlCompilerPopupValidity, xmlCompilerPopupStyles, xmlCompilerTrailer
      prev_cmd = self.gnatCmd
      self.gnatCmd = GPS.Project.root().get_attribute_as_string("gnat", "ide")

      if self.gnatCmd == "":
         self.gnatCmd = "gnat"
      if not os.path.isfile (self.gnatCmd):
         self.gnatCmd = os_utils.locate_exec_on_path (self.gnatCmd)
      if self.gnatCmd == "":
         GPS.Console ("Messages").write ("Error: 'gnat' is not in the path.\n")
         GPS.Console ("Messages").write ("Error: Could not initialize the ada_support module.\n")
         return

      # gnat check command changed: we reinitialize the rules list
      if prev_cmd != self.gnatCmd:
         self.get_switches()
         xml = """<popup label="Warnings" line="2" column="1" lines="2" columns="1">"""
         xml += """<title line="1" column="1">Global switches</title>"""
         xml += """<title line="2" column="1">Warnings</title>"""
         for switch in self.warnings_list:
            if switch[2]:
               # active by default
               default="on"
            else:
               default="off"
            if switch[0] == "a" or switch[0] == ".e" or switch[0] == "s" or switch[0] == "e":
               xml += '<check label="'+label ("-gnatw",switch)+'" switch="-gnatw'+switch[0]+'" line="1">\n'
               xml += '  <tip>'+tip("-gnatw",switch)+'</tip>\n'
               xml += '</check>\n'
            else:
               xml += '<check label="'+label ("-gnatw",switch)+'" switch="-gnatw'+switch[0]+'" switch-off="-gnatw'+switch[0].upper()+'" default="'+default+'" line="2">'
               xml += '  <tip>'+tip("-gnatw",switch)+'</tip>\n'
               xml += '</check>\n'
               if switch[3]:
                  # activated by -gnatwa
                  xml += '<default-value-dependency master-switch="-gnatwa" slave-switch="-gnatw'+switch[0]+'"/>\n'
               if re.search ("^turn on warning", switch[1]):
                  xml += '<default-value-dependency master-switch="-gnatw.e" slave-switch="-gnatw'+switch[0]+'"/>\n'
                  xml += '<default-value-dependency master-switch="-gnatws" slave-switch="-gnatw'+switch[0].upper()+'"/>\n'
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
"""
         xml += xmlCompilerValidity
         xml += """<popup label="Style checks" line="2" column="1" >"""
         for switch in self.style_checks_list:
            if switch[3]=="0":
               xml += '<check label="'+label ("-gnaty", switch)+'" switch="-gnaty'+switch[0]+'">\n'
               xml += '  <tip>'+tip("-gnaty",switch)+'</tip>\n'
               xml += '</check>'
            else:
               xml += '<spin label="'+label ("-gnaty", switch)+'" switch="-gnaty'+switch[0]+'" min="'+switch[2]+'" max="'+switch[3]+'" default="'+switch[4]+'" separator="">\n'
               xml += '  <tip>'+tip("-gnaty",switch)+'</tip>\n'
               xml += '</spin>'
         xml += '<expansion switch="-gnatyy" alias="-gnaty" />'
         xml += '<expansion switch="-gnatym" alias="-gnatyM79" />'
         xml += '<expansion switch="-gnaty" alias="'+self.style_alias+'" />'
         xml += """
            <expansion switch="-gnaty" />
         </popup>
"""
         xmlCompiler = xmlCompilerHead+xml+xmlCompilerTrailer
         GPS.parse_xml ("""<?xml version="1.0" ?><GPS>"""+xmlCompiler+"</GPS>")

   def add_switch (self, process, matched, unmatched):
      if unmatched == "\n":
        line = self.msg
        self.msg = ""
        if re.search ("^ +[-]gnatwxx", line):
          self.warnings_analysis = True
        elif re.search ("^ +[-]gnatyxx", line):
          self.style_checks_analysis = True
        elif re.search ("^ +[-]gnat", line):
          self.warnings_analysis = False
        elif self.style_checks_analysis and re.search ("^ *[-]", line):
          self.style_checks_analysis = False
        elif self.warnings_analysis:
          res = re.split ("^ *([^ *]+)([*]?) +(.+) *$", line)
          if len (res) > 2:
            if res[1] == "a":
              # retrieve the list of warnings not activated by -gnatwa
              exception = re.split ("\(except ([a-zA-Z.]*)\) *$", res[3])
              self.all_warnings_exception_list = re.findall("[.]?[a-zA-Z]", exception[1]+".e")
              self.warnings_list.append ([res[1], res[3], False, False])
            elif res[1] == "e" or res[1] == ".e" or res[1] == "s":
              # include the global switches directly.
              self.warnings_list.append ([res[1], res[3], False, False])

            # include only on warnings, and a limited list of global warnings (gnatwa, gnatws, gnatw.e)
            if not re.search ("turn off", res[3]) and not re.search ("(all|every)", res[3]) and not re.search ("^normal warning", res[3]):
              is_alias_part = False
              # switches activated by default are not part of gnatwa
              if res[2] != "*":
                if re.search ("turn on", res[3]):
                  # part of gnatwa, unless explicitely part of the gnatwa exception list
                  # search if warning is not part of gnatwa
                  is_alias_part = True
                  for ex in self.all_warnings_exception_list:
                    if ex == res[1]:
                      is_alias_part = False
                      break
                else:
                  is_alias_part = False

              # warnings_list is a list of [switch, description, default value, part_of_gnatwa]
              # remove the 'turn on' in the description
              self.warnings_list.append ([res[1], res[3], res[2] == "*", is_alias_part])

        elif self.style_checks_analysis:
          res = re.split ("^ *(1[-]9|.)(n*) +(.+) *$", line)
          if len (res) > 2:
            if res[1] == "1-9":
               self.style_checks_list.append(["", res[3], "0", "9", "0"])

            elif res[1] == "y":
               sw = ["y", ""]
               style_alias_res = re.split ("^ *This is equivalent to ([^., ]*).*", tip ("-gnaty", sw));
               if len (style_alias_res) > 1:
                 self.style_alias = "-"+style_alias_res[1]
               else:
                 self.style_alias = "-gnaty3abcefhiklmnprst"

            # no parameters. Do not include -gnatyN (remove all checks), -gnatyg (GNAT checks) and -gnatym (alias of -gnatyM79)
            elif res[1] != "N" and res[1] != "g" and res[1] != "m":
               if res[2] == "":
                  self.style_checks_list.append([res[1], res[3], "0", "0", "0"])
               else:
                  self.style_checks_list.append([res[1], res[3], "0", "32768", "0"])

      self.msg += matched

   def get_switches (self):
      # Verify we have the correct gnatcheck executable
      self.warnings_list = []
      self.style_checks_list = []
      self.all_warnings_exception_list = []
      self.warnings_analysis = False
      self.style_checks_analysis = False

      if self.gnatCmd != "":
         # Then retrieve warnings/style/restriction checks from gnatmake
         self.msg = ""
         process = GPS.Process (self.gnatCmd + " make -h", "^.+$",
                                on_match=self.add_switch)
         process.get_result()
      return True

xmlCompilerHead = """
   <tool name="Ada" package="Compiler" index="ada" override="true">
      <language>Ada</language>
      <initial-cmd-line>-g -gnatQ</initial-cmd-line>
      <switches lines="3" columns="2">
         <title line="1" column="1" >Code generation</title>
         <title line="1" column="2" >Run-time checks</title>
         <title line="2" column="1" line-span="2" >Messages</title>
         <title line="3" column="1" line-span="0" />
         <title line="2" column="2" >Debugging</title>
         <title line="3" column="2" >Syntax</title>

         <combo switch="-O" nodigit="1" noswitch="0"
                tip="Controls the optimization level">
            <combo-entry label="No optimization" value="0" />
            <combo-entry label="Some optimization" value="1" />
            <combo-entry label="Full optimization" value="2" />
            <combo-entry label="Full + Automatic inline" value="3" />
         </combo>
         <check label="Inlining" switch="-gnatn"
                tip="Enable inlining of Ada subprograms marked with a pragma Inline" />
         <check label="Unroll loops" switch="-funroll-loops"
                tip="Perform the optimization of loop unrolling. This is only done for loops whose number of iterations can be determined at compile time or run time" />
         <check label="Position independent code" switch="-fPIC"
                tip="If supported for the target machine, emit position-independent code, suitable for dynamic linking and avoiding any limit of the size of the global offset table" />
         <check label="Code coverage" switch="-ftest-coverage"
                tip="Create data files for the gcov code-coverage utility" />
         <check label="Instrument arcs" switch="-fprofile-arcs"
                tip="Instrument arcs during compilation. For each function of your program, gcc creates a program flow graph, then finds a spanning tree for the graph. Only arcs that are not on the spanning tree have to be instrumented: the compiler adds code to count the number of times that these arcs are executed" />
         <dependency master-page="Ada" slave-page="Ada"
                     master-switch="-ftest-coverage"
                     slave-switch="-fprofile-arcs"
                     master-status="on" slave-status="on" />

         <check label="Always generate ALI file" switch="-gnatQ"
                tip="Don't quit, write ali/tree file even if compile errors" />

         <check label="Separate function sections" switch="-ffunction-sections"
                tip="Generate each function in a separate section. See also -fdata-sections and --gc-sections linker flag" />

         <check label="Separate data sections" switch="-fdata-sections"
                tip="Generate each global data in a separate section. See also -ffunction-sections and --gc-sections linker flag" />

         <check label="Full errors" switch="-gnatf" line="2" column="1"
                tip="Full Errors. Multiple errors per line, all undefined references" />
         <check label="Overflow checking" switch="-gnato" column="2"
                tip="Enable numerics overflow checking" />
         <check label="Suppress all checks" switch="-gnatp" column="2"
                tip="Suppress all checks" />
         <check label="Stack checking" switch="-fstack-check" column="2"
                tip="Generate code to verify that you do not go beyond the boundary of the stack. You should specify this flag if you are running in an environment with multiple threads, but only rarely need to specify it in a single-threaded environment" />
         <check label="Dynamic elaboration" switch="-gnatE" column="2"
                tip="Full dynamic elaboration checks" />

          <check label="Debug Information" switch="-g" line="2" column="2"
                 tip="Debug Information" />
          <dependency master-page="Gnatmake" slave-page="Ada"
                      master-switch="-g" slave-switch="-g"
                      master-status="on" slave-status="on" />
          <check label="Enable assertions" switch="-gnata" line="2" column="2"
                 tip="Assertions enabled. Pragma Assert and pragma Debug are activated" />

          <!--  Do not use radio buttons here, since this would always generate
                at least one of the switches, and the default is different for
                each compiler anyway (GNATPro, GNAT GAP,...) -->
          <check label="Ada 83 mode" switch="-gnat83" line="3" column="2"
                 tip="Override the compiler's default, and enforces Ada 83 restrictions" />
          <check label="Ada 95 mode" switch="-gnat95" line="3" column="2"
                 tip="Override the compiler's default, and compile in Ada 95 mode" />
          <check label="Ada 2005 mode" switch="-gnat05" line="3" column="2"
                 tip="Override the compiler's default, and activate Ada 2005 language features" />
"""

xmlCompilerValidity="""
          <popup label="Validity checking mode" line="2" column="1" >
            <check label="Turn on all validity checking options" switch="-gnatVa" />
            <check label="Checking for copies" switch="-gnatVc" switch-off="-gnatVC" default="off"
                   tip="The right hand side of assignments, and the initializing values of object declarations are validity checked" />
            <check label="Default Reference Manual Checking" switch="-gnatVd" switch-off="-gnatVD" default="off" />
            <check label="Checking for elementary components" switch="-gnatVe" switch-off="-gnatVE" default="off" />
            <check label="Checking for floating-point" switch="-gnatVf" switch-off="-gnatVF" default="off" />
            <check label="Checking for 'in' parameters" switch="-gnatVi" switch-off="-gnatVI" default="off"
                   tip="Arguments for parameters of mode in are validity checked in function and procedure calls at the point of call" />
            <check label="Checking for 'in out' parameters" switch="-gnatVm" switch-off="-gnatVM" default="off"
                   tip="Arguments for parameters of mode in out are validity checked in procedure calls at the point of call" />
            <check label="Checking for operators and attributes" switch="-gnatVo" switch-off="-gnatVO" default="off"
                   tip="Arguments for predefined operations and attributes are validity checked" />
            <check label="Checking for returns" switch="-gnatVr" switch-off="-gnatVR" default="off"
                   tip="The expression in return statements in functions is validity checked" />
            <check label="Checking for subscripts" switch="-gnatVs" switch-off="-gnatVS" default="off"
                   tip="All subscripts expressions are checked for validty" />
            <check label="Checking for tests" switch="-gnatVt" switch-off="-gnatVT" default="off"
                   tip="Expressions used as conditions in if, while or exit statements are checked, as well as guard expressions in entry calls" />
            <expansion switch="-gnatV" />
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVc"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVd"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVe"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVf"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVi"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVm"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVo"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVr"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVs"/>
           <default-value-dependency master-switch="-gnatVa" slave-switch="-gnatVt"/>
         </popup>
"""

xmlCompilerTrailer="""
      </switches>
   </tool>
"""

def on_switch_editor (hook_name):
   global gnatmakeproc
   if gnatmakeproc == None:
     gnatmakeproc = gnatMakeProc()
   gnatmakeproc.initSwitches()

GPS.Hook ("project_editor").add (on_switch_editor)
