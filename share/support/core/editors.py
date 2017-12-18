"""Add a menu to close all editors

This script adds a new menu /File/Close All Editors that close all open editors
(and only editors) upon activation.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils


@gps_utils.interactive(name='close all editors',
                       category="MDI",
                       for_learning=True)
def close_editors():
    """
    Save and close all source editors.
    """
    GPS.execute_action("save files and projects")
    for ed in GPS.EditorBuffer.list():
        ed.close(True)


@gps_utils.interactive(name='close all editors except current',
                       category="MDI",
                       for_learning=True)
def close_editors_except_current():
    """
    Save and close all source editors, except the curret one.
    """
    buffer = GPS.EditorBuffer.get(open=False)
    GPS.execute_action("save files and projects")
    for ed in GPS.EditorBuffer.list():
        if ed != buffer:
            ed.close(True)
