"""
Test the lal_view action used to generate standalone
Libadalang reproducers.
"""
from GPS import *
from gs_utils.internal.utils import *

ACTION_NAME = "create lal python reproducer"
LAL_REPRO_BASENAME = "lal_repro.py"
EXPECTED_TEXT_AFTER_EXEC= 'Completion results for <Id "Foo" foo.adb:1:11-1:14>'


@run_test_driver
def run_test():
    GPS.execute_action("open Libadalang")
    tree = get_widget_by_name("lal_view tree")
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    loc = buf.at(1, 12)

    # Click in the buffer to see the tree
    buf.current_view().goto(loc)
    yield hook("location_changed", debounced=True)

    # Select the 'Foo' identifier node in the LAL tree view
    windows = Gtk.Window.list_toplevels()
    select_in_tree(tree, 0, '<b>Identifier</b> Foo')
    yield wait_idle()

    # Execute the action to create a LAL python repro. Check that
    # a new editor has been opened for it
    GPS.execute_action(ACTION_NAME)
    yield wait_tasks(other_than=known_tasks)
    buf = GPS.EditorBuffer.get()
    gps_assert(
        buf.file().base_name(),
        LAL_REPRO_BASENAME,
        "A Libadalang reproducer python file should be opened",
    )

    # Execute the LAL repro in the GS Python console
    GPS.execute_action("open Python")
    python_console = GPS.Console("Python")
    python_console.add_input(
        f"exec(open('{LAL_REPRO_BASENAME}').read())"
    )
    pygps.send_key_event(pygps.GDK_RETURN)
    yield timeout(1000)

    # Check that the script has ran without errors, with the expected
    # result
    python_console_text = python_console.get_text()
    gps_assert(
        EXPECTED_TEXT_AFTER_EXEC in python_console_text,
        True,
        "Wrong text in Python console after executing script. "
        + f"Python console text is:\n {python_console_text}",
    )
