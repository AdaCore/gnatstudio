"""
This test create a build target running a python command which returns
a string.
Check that the output parsers defined in the build target properly runs
on the string and create a message about it.
"""

import GPS
from gs_utils.internal.utils import *

XML = """
<target-model name="my_model" category="my__category">
   <uses-python>True</uses-python>
   <command-line>
      <arg>hum.do_something("</arg>
      <arg>%f</arg>
      <arg>")</arg>
   </command-line>
   <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      location_parser
      console_writer
      end_of_build
   </output-parsers>
</target-model>

<target model="my_model" name="my__command" category="my__category">
   <in-toolbar>True</in-toolbar>
   <command-line>
      <arg>do_something("</arg>
      <arg>%f</arg>
      <arg>")</arg>
   </command-line>
   <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      location_parser
      console_writer
      end_of_build
   </output-parsers>
</target>
"""


def do_something(path):
    return "%s:1:1: warning: hello world\n" % path


@gs_utils.interactive(name="do_something", filter="File")
def my_action():
    target = GPS.BuildTarget("my_command")
    target.execute(synchronous=False)


@run_test_driver
def test_driver():
    GPS.parse_xml(XML)
    GPS.EditorBuffer.get(GPS.File("a.adb"))
    GPS.execute_action("do_something")
    yield wait_idle()
    gps_assert(str(GPS.Message.list()[0].get_file()).endswith("a.adb"),
               True,
               "Should be a message about a.adb")
    # Rerunning the target will remove the previous message and generate a
    # new one
    GPS.EditorBuffer.get(GPS.File("b.adb"))
    GPS.execute_action("do_something")
    yield wait_idle()
    gps_assert(str(GPS.Message.list()[0].get_file()).endswith("b.adb"),
               True,
               "Should be a message about b.adb")
