"""Add a menu to close all editors

This script adds a new menu /File/Close All Editors that close all open editors
(and only editors) upon activation.
"""


############################################################################
## No user customization below this line
############################################################################

import GPS
import gps_utils

@gps_utils.interactive(
    name='close all editors',
    menu='/File/Close All Editors',
    after='Close All')
def close_editors():
   GPS.execute_action("/File/Save More/All")
   for ed in GPS.EditorBuffer.list():
      ed.close(True)


@gps_utils.interactive(
    name='close all editors except current',
    menu='/File/Close All Editors Except Current',
    after='Close All Editors')
def close_editors_except_current():
   buffer = GPS.EditorBuffer.get(open=False)
   GPS.execute_action("/File/Save More/All")
   for ed in GPS.EditorBuffer.list():
      if ed != buffer:
         ed.close(True)
