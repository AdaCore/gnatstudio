"""This file provides support for using the GNATprove tool.

   GNATprove is a tool to apply automatic program proof to Ada programs.
   This plugin allows the user to perform a proof run and integrate its output
   into GPS.
   See menu Prove.
"""

############################################################################
## No user customization below this line
############################################################################

import GPS, os_utils, os.path

xml_gnatprove = """<?xml version="1.0"?>
  <GNATPROVE>

    <target-model name="gnatprove">
       <description>Target model for GNATprove</description>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
       </command-line>
       <icon>gps-build-all</icon>
       <switches command="%(tool_name)s" columns="2" lines="1">
         <title column="1" line="1" >Compilation</title>
         <check label="Force Recompilation" switch="-f" column="1"
                tip="All actions are redone entirely, including compilation and proof" />
         <title column="2" line="1" >Proof</title>
         <check label="Report Proved VCs" switch="--report=all" column="2"
                tip="Report the status of all VCs, including those proved" />
         <spin label="Prover Timeout" switch="--timeout=" column="2"
                default="1" min="1" max="3600"
                tip="Set the prover timeout (in s) for individual VCs" />
       </switches>
    </target-model>

    <target model="gnatprove" name="Prove Root Project" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=prove</arg>
       </command-line>
    </target>

    <target model="gnatprove" name="Prove File" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=prove</arg>
          <arg>-u</arg>
          <arg>%fp</arg>
       </command-line>
    </target>

    <target model="gnatprove" name="Prove Subprogram" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=prove</arg>
          <arg>--limit-subp=%f:%l</arg>
       </command-line>
    </target>

    <target model="gnatprove" name="Prove Line" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=prove</arg>
          <arg>--limit-line=%f:%l</arg>
       </command-line>
    </target>

    <target model="gnatprove" name="Show Unprovable Code" category="GNATprove">
       <in-menu>FALSE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=detect</arg>
       </command-line>
    </target>

  </GNATPROVE>
"""

prefix               = "Prove"
menu_prefix          = "/" + prefix
prove_root_project   = "Prove Root Project"
prove_file           = "Prove File"
prove_line           = "Prove Line"
prove_subp           = "Prove Subprogram"
show_unprovable_code = "Show Unprovable Code"

# Check for GNAT toolchain: gnatprove

gnatprove = os_utils.locate_exec_on_path("gnatprove")

# This is the context of a subprogram declaration if there is a subprogram,
# there is a corresponding declaration, and their file:line locations match.
def is_subp_decl_context(self):
    if isinstance (self, GPS.EntityContext) and \
       self.entity() and \
       self.entity().category() == "subprogram" and \
       self.entity().declaration() and \
       self.location().file() == self.entity().declaration().file() and \
       self.location().line() == self.entity().declaration().line():
        return True
    else:
        return False

def mk_loc_string (sloc):
    locstring = os.path.basename(sloc.file().name()) + ":" + str(sloc.line())
    return locstring

def on_prove_root_project(self):
    GPS.BuildTarget(prove_root_project).execute()

def on_prove_file(self):
    GPS.BuildTarget(prove_file).execute()

def on_prove_line(self):
    GPS.BuildTarget(prove_line).execute()

def on_prove_subp(self):
    GPS.BuildTarget(prove_subp).execute()

def on_show_unprovable_code(self):
    GPS.BuildTarget(show_unprovable_code).execute()

if gnatprove:
  GPS.Menu.create(menu_prefix, ref = "Window", add_before = True)
  GPS.Menu.create(menu_prefix + "/" + prove_root_project, on_prove_root_project)
  GPS.Menu.create(menu_prefix + "/" + prove_file, on_prove_file)
  GPS.Menu.create(menu_prefix + "/" + show_unprovable_code, on_show_unprovable_code)
  GPS.Contextual (prefix + "/" + prove_file).create(on_activate = on_prove_file)
  GPS.Contextual (prefix + "/" + prove_line).create(on_activate = on_prove_line)
  GPS.Contextual (prefix + "/" + prove_subp).create(
          on_activate = on_prove_subp, filter = is_subp_decl_context)
  GPS.parse_xml(xml_gnatprove)
