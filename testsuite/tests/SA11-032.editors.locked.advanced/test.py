"""
This test checks the good behavior of locked editors on several situations:

. Check if the 'Lock or unlock current editor (split)' action correctly
  splits the MDI in two.

. Clicking in the Call Trees view

. Clicking in the debug Call Stack
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    init_nb_of_notebooks = len(get_widgets_by_type(Gtk.Notebook))
    main_buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("Lock or unlock current editor (split)")
    main_buf.current_view().goto(main_buf.at(5, 10))

    gps_assert(len(get_widgets_by_type(Gtk.Notebook)), init_nb_of_notebooks + 1,
               "Locking an editor in split mode should create a separate "
               + "notebook")

    GPS.execute_action("Entity called by")
    yield hook('language_server_response_processed')
    yield wait_idle()

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

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    d = GPS.Debugger.get()
    d.send("b main.adb:5")
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
