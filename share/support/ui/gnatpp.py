"""
Adds support for GNAT's gnatpp tool

The gnatpp tool is a command line pretty printer for Ada code,
provided with the GNAT compiler.
This package integrates it into GPS:
  - menu /Code/Pretty Print
  - switches edition through the project editor
"""

import GPS
import gps_utils
import workflows
from workflows.promises import ProcessWrapper


@workflows.run_as_workflow
def gnatpp(file):
    """
    Run gnatpp on a specific file.
    Nothing is done if the file is not an Ada file.
    """
    if file.language().lower() != 'ada':
        GPS.Logger("GNATPP").log("Not an Ada file: %s" % file.path)
        return

    sv = GPS.Project.scenario_variables()
    x_args = ['-X%s=%s' % (k, v) for k, v in sv.items()] if sv else []

    cmd = [gps_utils.get_gnat_driver_cmd(),
           'pretty',
           '-rnb',
           '-P%s' % GPS.Project.root().file().path] + x_args + [file.path]

    p = ProcessWrapper(cmd, spawn_console='')
    status, output = yield p.wait_until_terminate()

    if status != 0:
        GPS.Locations.parse(output, category='gnat pretty')
    else:
        GPS.EditorBuffer.get(file, force=True, open=True)


XML = u"""<?xml version="1.0" ?>
<GPS>
   <action name="pretty print" output="none" category="Editor" >
      <description>Reformat the current Ada source file, and reload the
      reformated version. Specific formating options can be set in the project
      file</description>
      <filter language="ada" error="Pretty printing requires an Ada file" />
      <shell lang="python">GPS.MDI.save_all()</shell>
      <shell lang="python">p = gnatpp.gnatpp(GPS.File("%F"))</shell>
   </action>

   <tool name="Pretty Printer" package="Pretty_Printer" index="Ada"
         attribute="Default_Switches">
      <language>Ada</language>
      <switches>
         <title line="1" column="1">Spacing</title>
         <title line="2" column="1">Casing</title>
         <title line="3" column="1">Layout</title>
         <title line="1" column="2">Alignment</title>
         <title line="2" column="2">General</title>
         <title line="3" column="2">Extra</title>

         <spin label="Default indentation" switch="-i" min="1" max="9"
         default="3" />
         <spin label="Continuation lines" switch="-cl" min="1" max="9"
         default="2" />
         <spin label="Maximum line length" switch="-M" min="32" max="256"
         default="79" />
         <check label="No tabulation in comments" switch="-N" />

         <combo label="Keyword" switch="-k" line="2" noswitch="L" nodigit="L" >
            <combo-entry label="Lower case" value="L" />
            <combo-entry label="Upper case" value="U" />
         </combo>
         <combo label="Attribute" switch="-a" line="2" noswitch="M" nodigit="M"
         >
            <combo-entry label="Mixed case" value="M" />
            <combo-entry label="Lower case" value="L" />
            <combo-entry label="Upper case" value="U" />
         </combo>
         <combo label="Name" switch="-n" line="2" noswitch="D" nodigit="D" >
            <combo-entry label="As declared" value="D" />
            <combo-entry label="Mixed case" value="M" />
            <combo-entry label="Lower case" value="L" />
            <combo-entry label="Upper case" value="U" />
         </combo>
         <combo label="Pragma" switch="-p" line="2" noswitch="M" nodigit="M" >
            <combo-entry label="Mixed case" value="M" />
            <combo-entry label="Lower case" value="L" />
            <combo-entry label="Upper case" value="U" />
         </combo>

         <combo label="Construct" switch="-l" line="3" noswitch="1"
         nodigit="1">
            <combo-entry label="GNAT style" value="1" />
            <combo-entry label="Compact" value="2" />
            <combo-entry label="Uncompact" value="3" />
         </combo>
         <combo label="Comment" switch="-c" line="3" noswitch="1" nodigit="1"
         separator="">
            <combo-entry label="Do not format comments" value="0" />
            <combo-entry label="GNAT style line indentation" value="1" />
            <combo-entry label="Standard line indentation" value="2" />
         </combo>
         <check label="GNAT style beginning" switch="-c3" line="3"/>
         <check label="Reformat blocks" switch="-c4" line="3"/>
         <check label="Do not change annotated comments" switch="-c5" line="3"
    tip="Do not change comments with a special character just after '--'"/>

         <check label="Set the default for all alignments OFF" switch="-A0"
         line="1" column="2" before="true" />

         <check label="Colons in declarations" switch="-A1" line="1" column="2"
         default="on"/>
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="-A1" master-status="off"
                     slave-switch="-A0" slave-status="on" />
         <default-value-dependency master-switch="-A0" slave-switch="-A1"
         slave-status="off" />
         <check label="Assignments in declarations" switch="-A2" line="1"
         column="2" default="on"/>
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="-A2" master-status="off"
                     slave-switch="-A0" slave-status="on" />
         <default-value-dependency master-switch="-A0" slave-switch="-A2"
         slave-status="off" />
         <check label="Assignments in statements" switch="-A3" line="1"
         column="2" default="on"/>
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="-A3" master-status="off"
                     slave-switch="-A0" slave-status="on" />
         <default-value-dependency master-switch="-A0" slave-switch="-A3"
         slave-status="off" />
         <check label="Arrow delimiters in associations" switch="-A4" line="1"
         column="2" default="on"/>
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="-A4" master-status="off"
                     slave-switch="-A0" slave-status="on" />
         <default-value-dependency master-switch="-A0" slave-switch="-A4"
         slave-status="off" />
         <check label="'AT' keywords in component clauses" switch="-A5"
         line="1" column="2" default="on"/>
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="-A5" master-status="off"
                     slave-switch="-A0" slave-status="on" />
         <default-value-dependency master-switch="-A0" slave-switch="-A5"
         slave-status="off" />

         <check label="Do not set missing end/exit labels" switch="-e" line="2"
         column="2" />
         <combo label="Wide characters" switch="-W" line="2" column="2"
         noswitch="b" nodigit="b" separator="">
            <combo-entry label="Hex ESC encoding" value="h" />
            <combo-entry label="Upper half encoding" value="u" />
            <combo-entry label="Shift-JIS encoding" value="s" />
            <combo-entry label="EUC encoding" value="e" />
            <combo-entry label="UTF-8 encoding" value="8" />
            <combo-entry label="Brackets encoding" value="b" />
         </combo>

         <check label="Try to place 'IS' on the same line"
         switch="--no-separate-is" line="3" column="2"
                tip="Try not to place 'IS' on a separate line in a subprogram
                body" />
         <check label="Separate line for LOOP and THEN"
         switch="--separate-loop-then" line="3" column="2"
                tip="Use a separate line for LOOP and THEN keywords" />
         <check label="No separate line for LOOP and THEN"
         switch="--no-separate-loop-then" line="3" column="2"
                tip="Do not use a separate line for LOOP and THEN keywords" />
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
                     master-switch="--no-separate-loop-then" master-status="on"
                     slave-switch="--separate-loop-then" slave-status="off" />
         <dependency master-page="Pretty Printer" slave-page="Pretty Printer"
          master-switch="--separate-loop-then" master-status="on"
          slave-switch="--no-separate-loop-then" slave-status="off" />
         <check label="Separate lines for USE clauses"
         switch="--use-on-new-line" line="3" column="2"
                tip="Use separate lines for USE clauses in a context clause" />
         <check label="Separate lines for statement names"
         switch="--separate-stmt-name" line="3" column="2"
tip="Use separate lines for statement name with
no extra indentation for statement itself" />
      </switches>
   </tool>
</GPS>
"""

GPS.parse_xml(XML)
