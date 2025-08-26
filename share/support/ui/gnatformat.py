"""
Adds support for GNAT's gnatformat tool

The gnatformat tool is a command line pretty printer for Ada code,
provided with the GNAT compiler.
This package integrates it into GS:
  - Action to run GNATformat on a project + a project and its subprojects
  - switches edition through the project editor
"""

import GPS
import gs_utils


@gs_utils.interactive(
    name="run gnatformat on project", filter="Project only", category="Project"
)
def run_gnatformat_on_project_only():
    """
    Run gnatformat on the root project, without pretty printing the subprojects.
    """
    GPS.BuildTarget("Format current project").execute(synchronous=False)


@gs_utils.interactive(
    name="run gnatformat on project and subprojects",
    filter="Project only",
    category="Project",
)
def run_gnatformat_on_project_and_subprojects():
    """
    Run gnatformat on the root project and the subprojects.
    """
    GPS.BuildTarget("Format current project and subprojects").execute(synchronous=False)


XML = """<?xml version="1.0" ?>
<GNAT_Studio>
   <target-model name="gnatformat">
        <description>Generic launch of gnatformat</description>
        <command-line>
            <arg>gnatformat</arg>
            <arg>-P%PP</arg>
            <arg>%X</arg>
        </command-line>
        <output-parsers>
            output_chopper
            utf8_converter
            progress_parser
            console_writer
            end_of_build
        </output-parsers>
        <iconname>gps-semantic-check-symbolic</iconname>

          <switches>
            <title line="1" column="1">General</title>
            <title line="2" column="1">Spacing</title>
            <title line="3" column="1">Encoding</title>

            <check label="Verbose output" switch="--verbose" line="1"/>
            <check label="No subprojects" switch="--no-subprojects" line="1"/>
            <check label="Keep going after errors" switch="--keep-going" line="1"/>

            <spin label="Indentation size" switch="--indentation" separator="=" min="1" max="9" default="3" line="2"/>
            <spin label="Continuation line indentation" switch="--indentation-continuation" separator="=" min="1" max="9" default="2" line="2"/>
            <spin label="Maximum line width" switch="--width" separator="=" min="32" max="256" default="79" line="2"/>

            <combo label="End of line" switch="--end-of-line" separator="=" default="lf" line="3">
                <combo-entry label="LF (Unix)" value="lf" />
                <combo-entry label="CRLF (Windows)" value="crlf" />
            </combo>
            <combo label="Charset" switch="--charset" separator="=" default="iso-8859-1" line="3">
                <combo-entry label="ISO-8859-1" value="iso-8859-1" />
                <combo-entry label="UTF-8" value="utf-8" />
            </combo>
        </switches>
   </target-model>

   <target model="gnatformat" category="_Project"
    name="Format current project">
        <in-toolbar>FALSE</in-toolbar>
        <in-menu>FALSE</in-menu>
        <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
        <read-only>TRUE</read-only>
        <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
        <command-line>
            <arg>gnatformat</arg>
            <arg>-P%PP</arg>
            <arg>%X</arg>
            <arg>-U</arg>
            <arg>--no-subprojects</arg>
        </command-line>
        <output-parsers>
            output_chopper
            utf8_converter
            progress_parser
            console_writer
            end_of_build
        </output-parsers>
    </target>

    <target model="gnatformat" category="_Project"
    name="Format project and subprojects">
        <in-toolbar>FALSE</in-toolbar>
        <in-menu>FALSE</in-menu>
        <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
        <read-only>TRUE</read-only>
        <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
        <command-line>
            <arg>gnatformat</arg>
            <arg>-P%PP</arg>
            <arg>%X</arg>
            <arg>-U</arg>
        </command-line>
        <output-parsers>
            output_chopper
            utf8_converter
            progress_parser
            console_writer
            end_of_build
        </output-parsers>
    </target>

    <contextual action="run gnatformat on project" after="Coverage">
        <title>Format/Format %p</title>
    </contextual>

    <contextual action="run gnatformat on project and subprojects"
                after="Coverage">
        <title>Format/Format %p and subprojects</title>
    </contextual>
</GNAT_Studio>
"""

GPS.parse_xml(XML)
