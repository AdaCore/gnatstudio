# Check the behavior of the UI for code actions.

from gs_utils.internal.utils import run_test_driver, wait_language_server, \
    gps_assert, get_widget_by_name, timeout


@run_test_driver
def driver():
    # Open an editor and go to a line where there's a code action
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()
    v.goto(b.at(5, 9))

    # Wait until the language server has responded to the codeAction request
    yield wait_language_server("textDocument/codeAction")

    # Verify that one codeAction message has been created
    m = GPS.Message.list()
    gps_assert(len(m), 1, "there should be one message at this point")
    gps_assert(m[0].get_category(), "_internal_code_actions",
               "we have a message, but not in the expected category")

    # Click on the side action
    b.click_on_side_icon(5, 1, "gps-light-bulb")

    # Wait for the language server
    yield wait_language_server("workspace/executeCommand")

    # Check that the edits have been received
    gps_assert(b.get_chars(b.at(5, 1), b.at(6, 1)).strip(),
               'Put_Line (Item => "hello");',
               "edits not received")

    yield wait_language_server("textDocument/codeAction")
    m = GPS.Message.list(category="_internal_code_actions")
    gps_assert(len(m), 0, "there should be no code action message")
