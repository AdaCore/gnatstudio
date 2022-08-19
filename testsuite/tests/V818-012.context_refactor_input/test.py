"""Test the behavior of the dialog that appears when a refactoring
   operation needs user input.
"""
import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_language_server,
    wait_until_true,
    gps_assert,
    get_widget_by_name,
    timeout,
    send_key_event,
    GDK_BACKSPACE,
    GDK_DELETE,
)


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("a.ads"))
    buf.current_view().goto(buf.at(2, 15))

    initial_text = buf.get_chars(buf.at(2, 1), buf.at(3, 1))

    yield wait_language_server("textDocument/codeAction")

    # We should have one message saying 'Add Parameter'
    m = GPS.Message.list(category="_internal_code_actions")
    m = list(filter(lambda x: x.get_text() == "Add Parameter", m))
    gps_assert(len(m), 1, "there should be exactly 1 message saying 'Add Parameter'")

    # Execute the action
    m[0].execute_action()
    yield timeout(1000)

    #
    # Test the behavior of the "Add parameter" button: a dialog should appear
    #

    # We should now have the window with the refactoring_input entry
    inp = get_widget_by_name("refactoring_input")

    # The following 2 lines are needed on Xvfb to support giving the focus
    # to the new window - on normal displays this is done automatically.
    inp.get_toplevel().present()
    yield timeout(100)

    gps_assert(inp is not None, True, "the refactoring input box should have appeared")

    send_key_event(GDK_BACKSPACE)

    # Deliberately move the cursor in the editor, to trigger the generation of a
    # new context.
    buf.current_view().goto(buf.at(2, 10))

    send_key_event(GDK_DELETE)
    yield timeout(100)

    # Verify that the delete action was not sent to the editor
    new_text = buf.get_chars(buf.at(2, 1), buf.at(3, 1))
    gps_assert(initial_text, new_text, "the delete was sent to the editor")
