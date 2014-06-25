"""
Adds support for rsh when using GPS remote.

The GPS remote facility (/Tools/View/Remote) allows you to easily
work (eg build and compile) on remote hosts. The communication with
these remote hosts can be done through a number of protocols. This package
adds support for the rsh protocol.

Please note that use of rsh is now deprecated on most systems, and may cause
connection issues or unterminated remote executions with the GPS remote mode.

Consider such use as obsolete and not recommended.
"""


import GPS

XML = r"""<?xml version="1.0"?>
<GPS>

  <!-- RSH -->

  <!-- rlogin ignores the start command. We use rlogin instead of rsh to make
       sure under Windows that the Microsoft rsh version is not used. The GNU
       rsh provides directly the rlogin tool that we want to use anyway.
    -->
  <remote_connection_config name="rsh">
    <start_command use_pipes="false">rlogin</start_command>
    <start_command_common_args>%U %h</start_command_common_args>
    <start_command_user_args>-l %u</start_command_user_args>
  </remote_connection_config>

</GPS>
"""

GPS.parse_xml(XML)
