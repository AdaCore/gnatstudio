"""Enter Unicode characters in editors

   This module provides a GPS action that, when executed, opens a
   small command window. In this window, the user can either type the
   numeric value or the name of a unicode character to insert at the
   cursor position in the current editor.

   For instance, select the menu /Edit/Insert Unicode, then type the
   following decimal value:
      928
   This will insert a PI character. You could also insert it by typing
      greek capital letter pi
   or (in hexadecimal):
      x3A0
"""


############################################################################
## No user customization below this line
############################################################################

from GPS import *
import unicodedata

def on_gps_started (hook_name):
   parse_xml ("""
   <action name="Enter unicode" output="none">
      <filter id="Source editor"/>
      <shell lang="python">unicode.Unicode()</shell>
   </action>
   <menu action="Enter unicode" before="Insert File...">
     <title>/Edit/Insert Unicode</title>
   </menu>""")

class Unicode (CommandWindow):
   def __init__ (self):
      CommandWindow.__init__ (self, prompt="Character code:",
                              on_activate = self.on_activate)

   def on_activate (self, input):
      if input != "":
         buffer = EditorBuffer.get()
         if input[0] in "0123456789":
            chr = unichr (int (input))
         elif input[0] == 'x' and input[1] in "0123456789":
            chr = unichr (int (input[1:], 16))
         else:
            chr = unicodedata.lookup (input)
         buffer.insert (buffer.current_view().cursor(), chr.encode('utf-8'))
                          
Hook ("gps_started").add (on_gps_started)
