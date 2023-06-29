"""
Test the Project View VCS filter: changing the VCS status button should filter
the Project View to only display files matching the status.
"""

from gs_utils.internal.utils import *

default_expect = ["p",
                  ["src1", ["foo.adb"],
                   "src2", ["bar.adb"],
                   "."]]

modified_expect = ["p",
                   ["src1", ["foo.adb"],
                    "src2",
                    "."]]

uptodate_expect = ["p",
                   ["src1",
                    "src2", ["bar.adb"],
                    "."]]


def select_vcs_filter(vcs_button, value):
    windows = Gtk.Window.list_toplevels()
    click_in_widget(vcs_button.get_child().get_event_window())
    contextual = get_contextual(windows)
    for (menu, menu_label, accel, level) in MenuTree(contextual):
        if menu_label == value:
            menu.activate()


@run_test_driver
def driver():
    # Open and expand the Project view
    prj_view = Project_View()
    yield prj_view.open_and_yield()
    explorer = prj_view.dialog
    explorer.expand_all()
    vcs_button = get_widget_by_name("project_view_vcs_filter")

    # Modified a file monitored by VCS
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    buf.insert(buf.at(1, 1), "-- This is a comment\n")
    buf.save()

    # Wait for the git command to finish and update the status
    yield wait_tasks(other_than=known_tasks)

    gps_assert(dump_tree_model(explorer.get_model(), 1),
               default_expect,
               "Wrong Project view for no VCS filtering")

    select_vcs_filter(vcs_button, "<gps>/Show Modified files")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               modified_expect,
               "Issue when filtering modified files")

    select_vcs_filter(vcs_button, "<gps>/Show Up to date files")
    yield wait_tasks(other_than=known_tasks)
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               uptodate_expect,
               "Issue when filtering up to date files")
