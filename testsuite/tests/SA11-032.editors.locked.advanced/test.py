"""
This test checks the good behavior of locked editors on several situations:

. Check if the 'Lock or unlock current editor (split)' action correctly
  splits the MDI in two.

. Check that navigating from a splitted and locked editor will
  put any newly opened view in the other notebook.

. Clicking in the Call Trees view

. Clicking in the debug Call Stack
"""
import GPS
from gs_utils.internal.utils import *


def get_parent_notebook(child):
    parent = child.get_parent()
    while not isinstance(parent, Gtk.Notebook):
        parent = parent.get_parent()

    return parent

@run_test_driver
def run_test():
    # Open test_2.ads
    GPS.EditorBuffer.get(GPS.File("test_2.ads"))
    init_nb_of_notebooks = len(get_widgets_by_type(Gtk.Notebook))

    # Open main.adb and lock it in split mode. Check that it creates
    # a separate notebook.
    main_buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("Lock or unlock current editor (split)")

    gps_assert(len(get_widgets_by_type(Gtk.Notebook)), init_nb_of_notebooks + 1,
               "Locking an editor in split mode should create a separate "
               + "notebook")

    # Open test.ads via a ctrl-click in main.adb: it should open a new buffer
    # on the other editor
    main_buf.current_view().goto(main_buf.at(6, 10))
    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration", "Ada")

    test_buf_child = GPS.MDI.get('test.ads').get_child().pywidget()
    main_buf_child = GPS.MDI.get('main.adb').get_child().pywidget()

    test_buf_notebook = get_parent_notebook(test_buf_child)
    main_buf_notebook = get_parent_notebook(main_buf_child)

    gps_assert(
        test_buf_notebook == main_buf_notebook, False,
        "The editor for 'test.ads' should be opened in the other notebook")

    # Check that the Call Trees opens a new view for main.adb since
    # the first one is locked
    main_buf.current_view().goto(main_buf.at(6, 10))
    GPS.execute_action("Entity called by")
    yield wait_language_server('callHierarchy/incomingCalls', 'Ada')

    call_tree = get_widget_by_name("Call Graph Tree")
    selection = call_tree.get_selection()
    selection.unselect_all()
    model = call_tree.get_model()
    selection.select_iter(model.iter_nth_child(model.get_iter_first(), 0))
    yield wait_idle()

    call_locs = get_widget_by_name("Call Graph Location Tree")
    click_in_tree(call_locs, path=Gtk.TreePath("0"))

    gps_assert(len(main_buf.views()), 2,
               "Opened view for main.adb is locked: another view should have "
               + "been opened after clicking in Call Trees location")

    main_buf.views()[0].destroy()

    # Verify that locked editors works when debugging (Call Stack)
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    d = GPS.Debugger.get()
    d.send("b main.adb:6")
    yield wait_until_not_busy(d)
    d.send("run")
    yield wait_until_not_busy(d)

    win = GPS.MDI.get("Call Stack").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]

    click_in_tree(tree, Gtk.TreePath(0), column=1, button=1)
    yield wait_idle()

    gps_assert(len(main_buf.views()), 2,
               "Opened view for main.adb is locked: another view should have "
               + "been opened after clicking in the Call Stack")
