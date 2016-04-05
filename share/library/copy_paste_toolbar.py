"""This plugin provides Cut, Copy and Paste buttons in the toolbar
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
import gps_utils


@gps_utils.hook('gps_started')
def __gps_started():
    GPS.Action('cut to clipboard').button(
        toolbar='main', section='editor', label='Cut')
    GPS.Action('copy to clipboard').button(
        toolbar='main', section='editor', label='Copy')
    GPS.Action('paste from clipboard').button(
        toolbar='main', section='editor', label='Paste')
