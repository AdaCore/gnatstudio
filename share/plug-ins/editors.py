"""Add a menu to close all editors

This script adds a new menu /File/Close All Editors that close all open editors
and only editors upon activation.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS

def close_editors (menu):
   GPS.execute_action ("/File/Save More/All")
   for ed in GPS.EditorBuffer.list():
      ed.close (True)

def close_editors_except_current (menu):
   buffer = GPS.EditorBuffer.get (open=False)
   GPS.execute_action ("/File/Save More/All")
   for ed in GPS.EditorBuffer.list():
      if ed != buffer:
         ed.close (True)

GPS.Menu.create ("/File/Close All Editors",
                 on_activate=close_editors,
                 ref="Close All",
                 add_before=False);
GPS.Menu.create ("/File/Close All Editors Except Current",
                 on_activate=close_editors_except_current,
                 ref="Close All Editors",
                 add_before=False);
