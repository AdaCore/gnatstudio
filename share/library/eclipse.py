""" Enables a selection of Eclipse-like shortcuts in GPS.

The "delete selected lines" (Ctrl-D or Cmd-D) action was contributed
by Robert ter Vehn.
"""


import GPS
import gps_utils


name = 'delete line from selection'


@gps_utils.interactive(name=name, key='primary-d')
def delete_line():
    """
    Remove the lines that include the current selection. If there is
    no selection, remove the line at the cursor position.
    """

    buffer = GPS.EditorBuffer.get()

    append = GPS.last_command() == name
    if append:
        GPS.set_last_command(name)

    start = buffer.selection_start().beginning_of_line()
    end = buffer.selection_end().end_of_line()
    buffer.delete(start, end)
