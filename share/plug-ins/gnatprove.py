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

    <target-model name="gnatprove" >
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
       <in-menu>TRUE</in-menu>
       <icon>gps-build-all</icon>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>gnatprove</arg>
          <arg>-P%PP</arg>
          <arg>--mode=prove</arg>
       </command-line>
    </target>

  </GNATPROVE>
"""

# Check for GNAT toolchain: gnatprove

gnatprove = os_utils.locate_exec_on_path("gnatprove")

if gnatprove:
  GPS.parse_xml(xml_gnatprove)
