"""
This file adds a new menu: Build->View types layout

Selecting an opened source editor, and then clicking on this menu, will
output the layout of all the records in the current file, by recompiling
the file with -gnatR.
"""

import GPS
import gps_utils
from tool_output import OutputParser


class OutputStore(OutputParser):

    def __init__(self, child):
        super(OutputStore, self).__init__(child)
        self.output = ""

    def on_stdout(self, text, command):
        self.output += text
        if self.child:
            self.child.on_stdout(text, command)

    def on_exit(self, status, command):
        if self.child:
            self.child.on_exit(status, command)

        buffer = GPS.EditorBuffer.get_new()
        buffer.insert(buffer.beginning_of_buffer(), self.output)
        buffer.set_read_only(False)

        # ??? How to get access to the file name ?
        GPS.MDI.get_by_child(buffer.current_view()).rename('Type layout')

XML = r"""<?xml version="1.0" ?>
<GPS>
   <target-model name='gnatR' category='File'>
      <description>
Launches GNAT to generate information on the layout of types (using -gnatR)
      </description>
      <iconname>gps-custom-build-symbolic</iconname>
      <server>Tools_Server</server>
      <switches></switches>
   </target-model>
   <target name='Compute Types Layout' category='File' model='gnatR'>
      <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
      <in-menu>FALSE</in-menu>
      <in-toolbar>FALSE</in-toolbar>
      <command-line>
         <arg>%gnatmake</arg>
         <arg>-q</arg>
         <arg>-f</arg>
         <arg>-c</arg>
         <arg>-gnatR</arg>
         <arg>-u</arg>
         <arg>-P%PP</arg>
         <arg>%eL</arg>
         <arg>%X</arg>
         <arg>%fp</arg>
      </command-line>
      <output-parsers>outputstore</output-parsers>
   </target>
</GPS>
"""
GPS.parse_xml(XML)


@gps_utils.interactive(
    name='view types layout in file',
    filter='Source editor',
    category='Builder')
def view_types_layout():
    context = GPS.current_context()
    f = context.file()
    GPS.BuildTarget('Compute Types Layout').execute(file=f)
