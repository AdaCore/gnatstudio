"""
This file provides support for using the GNATFormat tool.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import os_utils
import os.path

# This is creating the action "display documentation GNATFormat User's Guide"
xml_gnatformat = """<?xml version="1.0"?>
  <GNATFORMAT>
    <doc_path>{root}/share/user_manual/gnatformat/html/user-guide</doc_path>

    <documentation_file>
      <name>introduction.html</name>
      <descr>GNATFormat User's Guide</descr>
      <category>GNATformat</category>
      <menu before="About">/Help/Tools/GNATformat User's Guide</menu>
    </documentation_file>
  </GNATFORMAT>
"""

gnatformat = os_utils.locate_exec_on_path("gnatformat")

if gnatformat:
    root = os.path.dirname(os.path.dirname(gnatformat)).replace("\\", "/")
    GPS.parse_xml(xml_gnatformat.format(root=root))
