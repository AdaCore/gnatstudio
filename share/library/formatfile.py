"""
This plugin provides a new menu to reformat the whole buffer.
Select /Edit/Format Whole File
or create a keybinding to the Editor/Format Whole File action via
the menu /Edit/Key Shortcuts.

This is basically the same thing as "Select All", followed by
"Format Selection", but this preserves the current location of the
cursor, which Select All cannot do.
"""


import GPS
import gps_utils


@gps_utils.interactive(category="Editor",
                       name="Format Whole File",
                       menu="/Edit/Format Whole File",
                       after="/Edit/Format Selection")
@gps_utils.with_save_excursion
def format_whole_file():
    buf = GPS.EditorBuffer.get()
    buf.select(buf.beginning_of_buffer(), buf.end_of_buffer())
    GPS.execute_action("Format Selection")
