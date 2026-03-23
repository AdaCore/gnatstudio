"""
Check that we can set breakpoints when debugger is not started
and have them when it is started
"""

import GPS
from gs_utils.internal.utils import *

FILE_COLUMN = 4
EXCEPTION_COLUMN = 6
ADDRESS_COLUMN = 8


@run_test_driver
def test_driver():
    GPS.Preference("Debugger-Break-On-Exception").set(True)
    yield wait_tasks()
    # Open the editor and add a breakpoint
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    # create exception BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(4)
    yield wait_idle()
    yield ed.ok()
    yield wait_idle()

    GPS.execute_action("open breakpoints editor")
    yield wait_idle()
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()

    gps_assert(
        dump_tree_model(model, EXCEPTION_COLUMN),
        ["all"],
        "Wrong breakpoint after creating",
    )

    # check we have proper editor information
    yield view.select(0)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    gps_assert(bp_type.get_active(), 4, "Check the breakpoint type")
    yield ed.cancel()

    # create address BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(2)
    yield wait_idle()
    bp_address = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[2]
    bp_address.prepend_text("0x55af")
    bp_address.set_active(0)
    yield ed.ok()
    yield wait_idle()

    gps_assert(
        dump_tree_model(model, ADDRESS_COLUMN),
        ["0x55af"],
        "Wrong breakpoint after creating",
    )
    # check we have proper editor information
    yield view.select(1)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    gps_assert(bp_type.get_active(), 2, "Check the breakpoint type")
    yield ed.cancel()

    # create expression BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(3)
    yield wait_idle()
    bp_expression = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[3]
    bp_expression.prepend_text("hello")
    bp_expression.set_active(0)
    yield ed.ok()
    yield wait_idle()

    gps_assert(
        dump_tree_model(model, FILE_COLUMN),
        ["hello"],
        "Wrong breakpoint after creating",
    )

    # create assertion BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(4)
    yield wait_idle()
    exception_name = get_widget_by_name("breakpoint-exception-name")
    exception_name.set_active(1)
    yield ed.ok()
    yield wait_idle()

    yield view.select(3)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    gps_assert(bp_type.get_active(), 4, "Check the breakpoint type")
    yield ed.cancel()

    # create "constraint_error" BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(4)
    yield wait_idle()
    exception_name = get_widget_by_name("breakpoint-exception-name")
    exception_name.append_text("constraint_error")
    exception_name.set_active(2)
    yield ed.ok()
    yield wait_idle()
    # check we have proper editor information
    yield view.select(4)
    ed = view.edit()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    gps_assert(bp_type.get_active(), 4, "Check the breakpoint type")
    yield ed.cancel()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()
    debug = GPS.Debugger.get()
    yield wait_until_not_busy(debug)

    # Check that we have 4 breakpoints in the bp view
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        5,
        "Wrong number of breakpoints when starting the debugger",
    )

    GPS.execute_action("debug clear breakpoints")
    debug.send("b main.adb:11")
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_until_not_busy(debug)

    # create variable BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(4)
    yield wait_idle()
    entry = get_widget_by_name("breakpoint-watchpoint-name")
    entry.set_text("X")
    yield ed.ok()
    yield wait_idle()

    # Check that we have breakpoint in the bp view
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        2,
        "Wrong number of breakpoints when starting the debugger",
    )
    GPS.execute_action("debug clear breakpoints")

    # create exception BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(4)
    yield wait_idle()
    yield ed.ok()
    yield wait_idle()

    # create address BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(2)
    yield wait_idle()
    bp_address = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[2]
    bp_address.prepend_text("0x55af")
    bp_address.set_active(0)
    yield ed.ok()
    yield wait_idle()

    # create assertion BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(5)
    yield wait_idle()
    exception_name = get_widget_by_name("breakpoint-exception-name")
    exception_name.set_active(1)
    yield ed.ok()
    yield wait_idle()

    # create expression BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(3)
    yield wait_idle()
    bp_expression = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[3]
    bp_expression.prepend_text("hello")
    bp_expression.set_active(0)
    yield ed.ok()
    yield wait_idle()

    # create "constraint_error" BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(5)
    yield wait_idle()
    exception_name = get_widget_by_name("breakpoint-exception-name")
    exception_name.append_text("constraint_error")
    exception_name.set_active(2)
    yield ed.ok()
    yield wait_idle()

    # create condition BP
    view = Breakpoints_View()
    ed = view.create()
    yield ed.open_and_yield()
    bp_type = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[0]
    bp_type.set_active(3)
    yield wait_idle()
    bp_expression = get_widgets_by_type(Gtk.ComboBoxText, ed.dialogs)[3]
    bp_expression.prepend_text("hello")
    bp_expression.set_active(0)
    condition = get_widget_by_name("breakpoint-condition")
    condition.prepend_text("True")
    condition.set_active(0)
    ignore = get_widget_by_name("breakpoint-ignore")
    ignore.set_value(1.0)
    yield ed.ok()
    yield wait_idle()

    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        5,
        "Wrong number of breakpoints",
    )
    GPS.execute_action("debug clear breakpoints")
    yield wait_idle()

    debug.close()
    yield wait_idle()
    GPS.execute_action("/Debug/Initialize/main")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_until_not_busy(debug)
    yield wait_idle()
    bp = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, bp.pywidget())[0]
    model = tree.get_model()
    gps_assert(
        len(dump_tree_model(model)),
        1,
        "Should have exception breakpoint",
    )
