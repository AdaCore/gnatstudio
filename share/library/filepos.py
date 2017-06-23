"""
This script saves the cursor location when an editor is closed, and
restore it when the editor is reopened later on.
"""

############################################################################
# No user customization below this line
############################################################################

from GPS import EditorBuffer
import gps_utils


@gps_utils.hook('file_closed')
def on_file_closed(file):
    buffer = EditorBuffer.get(file, open=False)
    if buffer:
        line = buffer.current_view().cursor().line()
        column = buffer.current_view().cursor().column()
        file.set_property("lastloc_line", repr(line), persistent=True)
        file.set_property("lastloc_column", repr(column), persistent=True)


@gps_utils.hook('file_edited')
def on_file_edited(file):
    try:
        # If the file was opened inside an editor (as opposed to a
        # QGen browser for instance)
        buffer = EditorBuffer.get(file, open=False)
        if not buffer:
            return

        # Do not change the line if the editor was already scrolled for
        # any reason
        cursor = buffer.current_view().cursor()
        if cursor.line() == 1 and cursor.column() == 1:
            line = file.get_property("lastloc_line")
            column = file.get_property("lastloc_column")
            buffer.current_view().goto(
                buffer.at(int(line), int(column)))
    except:
        pass
