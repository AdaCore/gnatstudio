"""
This plugin is used to automatically locate in the Project view the file
associated to an editor when the focused editor changes.
"""

import GPS
from gps_utils import hook


def __on_child_selected(hook, child):
    try:
        GPS.execute_action("Locate file in explorer")
    except Exception:
        GPS.Logger("AUTO_LOCATE_FILE").log(
            "Could not automatically locate: %s" % (str(file)))


@hook('gps_started')
def __on_gps_started():
    GPS.Hook('mdi_child_selected').add_debounce(__on_child_selected)
