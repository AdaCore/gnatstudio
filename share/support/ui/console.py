"""
This file adds contextual menu to GPS Messages window.
"""


import GPS

XML = r"""<?xml version="1.0"?>
<console>
  <action name="Clear messages" output="none">
    <filter module="GPS.Kernel.Console"/>
    <shell>Console</shell>
    <shell>Console.clear %1</shell>
  </action>

  <action name="Select all in console" output="none">
    <filter module="GPS.Kernel.Console"/>
    <shell>Console</shell>
    <shell>Console.select_all %1</shell>
  </action>

  <action name="Copy in console" output="none">
    <filter module="GPS.Kernel.Console"/>
    <shell>Console</shell>
    <shell>Console.copy_clipboard %1</shell>
  </action>
</console>
"""

GPS.parse_xml(XML)
