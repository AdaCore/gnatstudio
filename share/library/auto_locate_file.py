"""
This plugin is used to automatically locate in the Projec view the file
associated to an editor that has just been opened.
"""

import GPS
from gps_utils import hook


def __on_file_edited(hook, file):
    editor = GPS.EditorBuffer.get(file, force=False, open=False)
    if editor:
        try:
            GPS.execute_action("Locate file in explorer")
        except Exception:
            GPS.Logger("AUTO_LOCATE_FILE").log(
                "Could not automatically locate: %s" % (str(file)))


@hook('gps_started')
def __on_gps_started():
    GPS.Hook('file_edited').add(__on_file_edited)
