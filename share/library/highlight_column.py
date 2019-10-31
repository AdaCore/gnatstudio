"""
This plug-in provides separate functions to move the column
highlight vertical line to each of the following positions:

   margin column (user preference)
   column 80
   column before or after the current cursor
   column before or after the current line end

Each of these may be assigned to a separate key shortcut.

They are useful for checking margins and/or the vertical
alignment of text.
"""

import GPS
import gs_utils

PREF_NAME = "Plugins/highlight_column/right-margin"
CATEGORY = "Highlight Column Plugin"

GPS.Preference(PREF_NAME).create(
    "Margin Column",
    "integer",
    "Column for right margin highlight line (range 1 .. 255).",
    80, 1, 255)


def _set_column(value):
    return GPS.Preference("Src-Editor-Highlight-Column").set(value)


def _get_cursor():
    return GPS.EditorBuffer.get().current_view().cursor()


@gs_utils.interactive(name="Highlight before current column",
                      category=CATEGORY)
def before_current_column():
    _set_column(_get_cursor().column() - 1)


@gs_utils.interactive(name="Highlight after current column",
                      category=CATEGORY)
def after_current_column():
    _set_column(_get_cursor().column())


@gs_utils.interactive(name="Highlight before end-of-line column",
                      category=CATEGORY)
def before_end_of_line_column():
    _set_column(_get_cursor().end_of_line().column() - 1)


@gs_utils.interactive(name="Highlight after end-of-line column",
                      category=CATEGORY)
def after_end_of_line_column():
    _set_column(_get_cursor().end_of_line().column())


@gs_utils.interactive(name="Highlight right margin",
                      category=CATEGORY)
def right_margin():
    _set_column(GPS.Preference(PREF_NAME).get())


@gs_utils.interactive(name="Highlight column 80",
                      category=CATEGORY)
def column_80():
    _set_column(80)
