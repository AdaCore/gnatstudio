"""
This test verifies that breaking on a subprogram updates the
Breakpoints view accordingly. It also verifies that the
breakpoint is reached when running the debugger.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    main = GPS.File("main.adb")
    editor = GPS.EditorBuffer.get(main)

    editor.current_view().goto(editor.at(6, 15))

    # Place a Breakpoint on the Print_Result subprogram

    GPS.execute_action("debug set subprogram breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Open the Breakpoints view and check that the breakpoint has been
    # set

    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')

    view = GPS.MDI.get("Breakpoints")

    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    iter = model.get_iter_first()

    gps_assert(dump_tree_model(model, 7), ["Print_Result"],
               "the bp on Print_Result should be in the Breakpoints view")

    # Launch the debugger and verify that we stop in the Print_Result
    # subprogram's body

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)

    gps_assert(debug.current_line, 8,
               "The debugger should be stopped at line 8")
