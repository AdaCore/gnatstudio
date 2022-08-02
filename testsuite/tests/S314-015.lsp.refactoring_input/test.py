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
    GDK_ESCAPE,
    GDK_RETURN,
)


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("a.ads"))
    buf.current_view().goto(buf.at(2, 15))
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

    #
    # Test the behavior of the workflow that validates the syntax
    #
    for char in "a :":
        send_key_event(ord(char))
        yield timeout(50)

    response_label = get_widget_by_name("refactoring_response_label")

    # Wait until the response_label shows that the syntax is invalid
    wait_until_true(
        lambda: str(response_label.get_text()) == "Invalid Syntax", timeout=5000
    )

    for char in " String":
        send_key_event(ord(char))
        yield timeout(50)

    # Wait until the response_label is cleared
    wait_until_true(lambda: response_label.get_text() == "", timeout=5000)

    #
    # Test the behaviour of the ENTER key
    #

    send_key_event(GDK_RETURN)

    # Note: we can't use "wait_language_server" here: the response might have
    # arrived before the end of send_key_event! So we use a timeout.
    # Wait until the new text has arrived.
    wait_until_true(
        lambda: buf.get_chars(buf.at(2, 1), buf.at(2, 30))
        == "   procedure Foo (a : String);",
        timeout=2000,
    )

    #
    # Test the behavior of the ESCAPE key
    #

    # Execute the action
    m[0].execute_action()
    yield timeout(1000)

    send_key_event(GDK_ESCAPE)
    yield timeout(100)
    inp = get_widget_by_name("refactoring_input")
    gps_assert(
        inp,
        None,
        "the refactoring input box should have disappeared when pressing ESC",
    )
