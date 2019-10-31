"""
This file provides support for gnathub.
"""

import GPS
import gs_utils
import os_utils
import tool_output

PASSED = "[PASSED]"
NEED_INIT = True


class GNAThub_Parser(tool_output.OutputParser):

    def __init__(self, child=None):
        global NEED_INIT
        tool_output.OutputParser.__init__(self, child)
        self.open_report = False
        if NEED_INIT:
            GPS.Console().create_link(
                "\[PASSED\]",
                lambda x: None,
                foreground="green",
                background="",
                underline=False)
            GPS.Console().create_link(
                "\[FAILED\]",
                lambda x: None,
                foreground="red",
                background="",
                underline=False)
            NEED_INIT = False

    def print_output(self, text):
        if text:
            GPS.Console().write_with_links(text + "\n")

    def on_stdout(self, text, command):
        lines = text.splitlines()
        for line in lines:
            if PASSED in line:
                # At least a plugin worked => open the report
                self.open_report = True
            self.print_output(line)

    def on_exit(self, status, command):
        if self.open_report:
            GPS.execute_action("gnathub display analysis")
        else:
            self.print_output(
                "No GNAThub plugin has succeeded: " +
                "don't open the analysis report")
        if self.child is not None:
            self.child.on_exit(status, command)


gnathub_menu = "/Analyze/GNAThub/"
tools = {'codepeer':     'codepeer',
         'gcov':         'gcov',
         'gnatcoverage': 'gnatcov',
         'gnatcheck':    'gnatcheck',
         'gnatstack':    'gnatstack',
         'gnatmetric':   'gnatmetric',
         'spark2014':    'gnatprove'
         }


XML = r"""<?xml version="1.0" ?>
<GNAT_Studio>
  <target-model name="gnathub">
    <iconname>gps-build-all-symbolic</iconname>
    <description>Run gnathub executable</description>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
    </command-line>
    <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
    </output-parsers>
    <switches command="%(tool_name)s" columns="2">
    <title line="1" column="1">Available plugins</title>
    {}
    <title line="1" column="2">Other options</title>
    <field line="1" column="2" label="Execute" switch="--exec="
      tip="Python script to execute (implies --incremental)"/>
    <spin line="1" column="2" label="Parallel" switch="-j"
      tip="Number of jobs to run in parallel"
      max="99" min="0" default="0"/>
    <check line="1" column="2" label="Incremental" switch="-i"
      tip="Do not remove database if exists"/>
    <check line="1" column="2" label="Quiet" switch="-q"
      tip="Toggle quiet mode on"/>
    <check line="1" column="2" label="Verbose" switch="-v"
      tip="Toggle verbose mode on"/>
    </switches>
  </target-model>

  <target name="gnathub" category="_Project_" model="gnathub">
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <in-menu>FALSE</in-menu>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
    </command-line>
    <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
    </output-parsers>
  </target>

</GNAT_Studio>
"""

# Template to insert into target-model for each gnathub plugin
template = r"""<check line="1" column="1"
 label="{}" switch="--plugins={}" tip="Run {} plugin" active="{}"/>
"""

# Check for GNAThub module active status:

logger = GPS.Logger("GPS.INTERNAL.MODULE_GNAThub")

if logger.active:
    checkboxes = ""
    for name, tool in tools.iteritems():
        if os_utils.locate_exec_on_path(tool):
            checkboxes += template.format(name, name, name, "on")
        else:
            checkboxes += template.format(name, name, name, "off")

    GPS.parse_xml(XML.format(checkboxes))

    @gs_utils.interactive(category="Gnathub",
                          menu=gnathub_menu+"Run...",
                          name="Run gnathub...")
    def show_dialog_and_run_gnathub():
        target = GPS.BuildTarget("gnathub")
        target.execute(synchronous=False)
