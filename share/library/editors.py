"""Add a menu to close all editors

This script adds a new menu /File/Close All Editors that close all open editors
and only editors upon activation.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS


def close_editors(menu):
   for ed in GPS.EditorBuffer.list():
      ed.close()

GPS.Menu.create ("/File/Close All Editors",
                 on_activate=close_editors,
                 ref="Close All",
                 add_before=False);
