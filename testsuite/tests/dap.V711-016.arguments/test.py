""" Test how local variables/arguments are displayed """

from GPS import *
from gs_utils.internal.utils import *

expected_values = ["", ["0", "false"]]
expected_names = ["<b>Arguments</b>", ["<b>i</b>", "<b>b</b>"]]

expected_values_1 = ["", ["0", "false"], "5"]
expected_names_1 = ["<b>Arguments</b>", ["<b>i</b>", "<b>b</b>"], "<b>arguments</b>"]


@run_test_driver
def run_test():
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_for_mdi_child("main.adb")

    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_idle()

    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)

    GPS.MDI.get("main.adb").raise_window()
    yield wait_tasks()
    buf.current_view().goto(buf.at(6, 1))
    GPS.process_all_events()
    yield wait_idle()
    yield wait_until_true(
        lambda: GPS.Action("debug set line breakpoint").can_execute() == False
    )
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks()

    debug.send("run")
    yield wait_DAP_server("stackTrace")

    GPS.execute_action("debug tree display arguments")
    yield wait_DAP_server("variables")
    yield wait_until_not_busy(debug)
    yield wait_idle()
    tree = get_widget_by_name("Variables Tree")
    dump = dump_tree_model(tree.get_model(), 1)
    gps_assert(dump, expected_values)
    dump = dump_tree_model(tree.get_model(), 0)
    gps_assert(dump, expected_names)

    yield idle_modal_dialog(lambda: GPS.execute_action("debug tree display expression"))
    dialog = get_window_by_title("Display the value of an expression")
    box = get_widgets_by_type(Gtk.ComboBoxText, dialog)[0]
    box.prepend_text("arguments")
    box.set_active(0)
    get_stock_button(dialog, STOCK_OK).clicked()
    yield wait_idle()

    dump = dump_tree_model(tree.get_model(), 1)
    gps_assert(dump, expected_values_1)
    dump = dump_tree_model(tree.get_model(), 0)
    gps_assert(dump, expected_names_1)

    # test that we restore the variable view selection
    debug.send("graph display R")
    yield wait_until_not_busy(debug)
    tree = get_widget_by_name("Variables Tree")
    tree.expand_row(Gtk.TreePath((0)), True)
    tree.expand_row(Gtk.TreePath((2)), True)
    tree.get_selection().select_path(Gtk.TreePath("1"))

    debug.send("step")
    yield wait_DAP_server("variables")
    yield wait_until_not_busy(debug)

    gps_assert(
        tree.get_selection().iter_is_selected(tree.get_model().get_iter("1")),
        True,
        "Wrong variable is selected",
    )
    gps_assert(tree.row_expanded(Gtk.TreePath((2))), True, "Expansion not restored")

    # check that expansion restoring works with a variable
    # that already loaded before expanding
    debug.send("graph display NR")
    yield wait_until_not_busy(debug)
    tree.expand_row(Gtk.TreePath((3)), False)
    yield wait_until_not_busy(debug)
    tree.expand_row(Gtk.TreePath((3, 0)), False)
    yield wait_until_not_busy(debug)
    debug.send("graph display NR.r1")
    yield wait_until_not_busy(debug)
    tree.expand_row(Gtk.TreePath((4)), False)
    yield wait_until_not_busy(debug)
    tree.get_selection().unselect_all()
    tree.get_selection().select_path(Gtk.TreePath((4, 0)))
    yield wait_idle()
    debug.send("step")
    yield wait_DAP_server("variables")
    yield wait_until_not_busy(debug)

    gps_assert(
        tree.get_selection().iter_is_selected(tree.get_model().get_iter((4, 0))),
        True,
        "Wrong variable is selected",
    )
    gps_assert(tree.row_expanded(Gtk.TreePath((4))), True, "Expansion not restored")
