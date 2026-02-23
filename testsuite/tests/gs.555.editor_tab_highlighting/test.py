"""
Test that we have source editor tab highlighted on errors and warnings.
"""

from GPS import *
from gs_utils.internal.utils import *
from gi.repository import Gtk

ERROR_CSS_STYLE = "mdi-errors-tab"
WARNING_CSS_STYLE = "mdi-warnings-tab"


@run_test_driver
def run_test():
    def check_notebook(name, style, idx):
        notebook = GPS.MDI.get(name).get_child().pywidget()
        while not isinstance(notebook, Gtk.Notebook):
            notebook = notebook.get_parent()

        tab_label = get_widgets_by_type(
            Gtk.Label, notebook.get_tab_label(notebook.get_nth_page(idx))
        )[0]

        return tab_label.get_style_context().has_class(style)

    yield wait_tasks()
    foo = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    gps_assert(
        check_notebook("foo.adb", ERROR_CSS_STYLE, 0), False, "Shoud not have styles"
    )
    gps_assert(
        check_notebook("foo.adb", WARNING_CSS_STYLE, 0), False, "Shoud not have styles"
    )

    GPS.execute_action("Compile File")
    yield wait_tasks()

    gps_assert(
        check_notebook("foo.adb", ERROR_CSS_STYLE, 0), True, "Shoud have errors style"
    )
    gps_assert(
        check_notebook("foo.adb", WARNING_CSS_STYLE, 0),
        False,
        "Shoud not have warnings style",
    )

    bar = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    yield wait_idle()
    gps_assert(
        check_notebook("bar.adb", ERROR_CSS_STYLE, 1), False, "Shoud not have styles"
    )
    gps_assert(
        check_notebook("bar.adb", WARNING_CSS_STYLE, 1), False, "Shoud not have styles"
    )

    GPS.execute_action("Compile File")
    yield wait_tasks()

    gps_assert(
        check_notebook("bar.adb", ERROR_CSS_STYLE, 1),
        False,
        "Shoud not have errors style",
    )
    gps_assert(
        check_notebook("bar.adb", WARNING_CSS_STYLE, 1),
        True,
        "Shoud have warnings style",
    )

    GPS.execute_action("locations clear")
    gps_assert(
        check_notebook("foo.adb", ERROR_CSS_STYLE, 0), False, "Shoud not have styles"
    )
    gps_assert(
        check_notebook("foo.adb", WARNING_CSS_STYLE, 0), False, "Shoud not have styles"
    )
    gps_assert(
        check_notebook("bar.adb", ERROR_CSS_STYLE, 1), False, "Shoud not have styles"
    )
    gps_assert(
        check_notebook("bar.adb", WARNING_CSS_STYLE, 1), False, "Shoud not have styles"
    )
