"""
New GPS action: insert newline, but don't auto-indent

This file binds the Shift-Return key so that it just
inserts a newline character, but not do auto-reindent the
current line first, as opposed to what Return does.

One usage is if you want some parts of your code to have some
special indentation that doesn't match what the automatic
indentation engine tries to do.
"""


import GPS
import gps_utils


@gps_utils.interactive(
    name='newline no auto-indent',
    category='Editor',
    filter='Source editor',
    key='shift-Return')
def newline_no_auto_indent():
    """
    Insert a newline character, but do not auto-reindent the current
    line first (as opposed to what <return> does).
    """
    ctx = GPS.current_context()
    f = ctx.file().path
    GPS.Editor.replace_text(
        f, GPS.Editor.cursor_get_line(f), GPS.Editor.cursor_get_column(f),
        "\n", 0, 0)
    GPS.Editor.indent()
