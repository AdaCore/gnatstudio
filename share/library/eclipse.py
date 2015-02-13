""" Enables a selection of Eclipse-like shortcuts in GPS.

The "delete selected lines" (Ctrl-D or Cmd-D) action was contributed
by Robert ter Vehn.
"""


import GPS
import gps_utils


@gps_utils.interactive(name='delete selected lines',
                       category='Editor',
                       filter='Source editor',
                       key='primary-d')
def delete_selected_lines():
    """
    Remove all the lines that are at least partially selected.
    If there is no selection, remove the current line.
    """

    buffer = GPS.EditorBuffer.get()
    start = buffer.selection_start().beginning_of_line()
    end = buffer.selection_end().end_of_line()
    buffer.delete(start, end)
