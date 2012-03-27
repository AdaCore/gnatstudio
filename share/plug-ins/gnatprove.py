"""This file provides support for using the GNATprove tool.

   GNATprove is a tool to apply automatic program proof to Ada programs.
   This plugin allows the user to perform a proof run and integrate its output
   into GPS.
   See menu Tools->GNATprove.
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
       <switches command="%(tool_name)s" columns="1" lines="1">
         <title column="1" line="1" >Compilation</title>
         <check label="Force Recompilation" switch="-f" column="1"
                tip="All actions are redone entirely, including compilation and proof" />
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

  </GNATPROVE>
"""

prefix             = "Prove"
menu_prefix        = "/" + prefix
prove_root_project = "Prove Root Project"
prove_file         = "Prove File"
prove_line         = "Prove Line"

# Check for GNAT toolchain: gnatprove

gnatprove = os_utils.locate_exec_on_path("gnatprove")

def on_prove_root_project(self):
    GPS.BuildTarget(prove_root_project).execute()

def on_prove_file(self):
    GPS.BuildTarget(prove_file).execute()

def on_prove_line(self):
    target = GPS.BuildTarget(prove_root_project)
    loc = self.location()
    locstring = os.path.basename(loc.file().name()) + ":" + str(loc.line())
    target.execute (extra_args="--limit-line="+locstring)

if gnatprove:
  GPS.Menu.create(menu_prefix, ref = "Window", add_before = True)
  GPS.Menu.create(menu_prefix + "/" + prove_root_project, on_prove_root_project)
  GPS.Menu.create(menu_prefix + "/" + prove_file, on_prove_file)
  GPS.Contextual (prefix + "/" + prove_file).create(on_activate = on_prove_file)
  GPS.Contextual (prefix + "/" + prove_line).create(on_activate = on_prove_line)
  GPS.parse_xml(xml_gnatprove)
