"""
This test verifies that removing a breakpoint from the Breakpoints
view while a debugger is running works correctly.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # Open main.adb and set a breakpoint line 25

    main = GPS.File("main.adb")
    editor = GPS.EditorBuffer.get(main)

    editor.current_view().goto(editor.at(25, 1))

    GPS.execute_action("debug set line breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Launch the debugger

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    # Open the Breakpoints view and remove the breakpoint
    # from it

    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child('Breakpoints')

    view = GPS.MDI.get("Breakpoints")

    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    iter = model.get_iter_first()

    tree.get_selection().select_iter(iter)

    GPS.execute_action("debug delete breakpoint")
    yield wait_tasks(other_than=known_tasks)

    # Verify that the breakpoint has been removed

    gps_assert(
        dump_tree_model(model, 0), [],
        "The breakpoint should be removed from the Breakpoints view")
    

