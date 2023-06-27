"""
This test checks that editors for files that do not belong
to the project have their tab labels displayed in italic.
"""
import GPS
from gs_utils.internal.utils import run_test_driver, gps_assert, get_widgets_by_type
from gi.repository import Gtk


@run_test_driver
def driver():
    def get_parent_notebook(child):
        parent = child.get_parent()
        while not isinstance(parent, Gtk.Notebook):
            parent = parent.get_parent()

        return parent

    # Open two editors: one for a file that belongs to the project, another
    # one for a file that does not belong to it
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.EditorBuffer.get(GPS.File("test.ads"))

    # Check that test.ads editor is considered as not belonging to the project
    # via its style classes
    test_buf_child = GPS.MDI.get("test.ads").get_child().pywidget()
    notebook = get_parent_notebook(test_buf_child)
    tab_label = get_widgets_by_type(
        Gtk.Label, notebook.get_tab_label(notebook.get_nth_page(1)))[0]
    gps_assert(
        tab_label.get_style_context().has_class("not-from-project"),
        True,
        "test.ads editor should have the 'not-from-project' style class",
    )

    # Add test.ads directory to the project's source dirs and check that
    # the 'not-from-project' style class gets removed
    GPS.Project.root().add_source_dir(".")
    GPS.Project.root().recompute()
    gps_assert(
        tab_label.get_style_context().has_class("not-from-project"),
        False,
        "test.ads editor belongs to the project but the"
        + " 'not-from-project' style class is still present'",
    )
