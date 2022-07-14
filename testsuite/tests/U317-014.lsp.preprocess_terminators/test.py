# Check the suppression of CR characters when applying edits
# received from the LSP

from gs_utils.internal.utils import run_test_driver, wait_language_server, \
    gps_assert, get_widget_by_name, timeout


@run_test_driver
def driver():
    # Open an editor and go to a line where there's a code action
    b = GPS.EditorBuffer.get(GPS.File("p.adb"))
    v = b.current_view()
    v.goto(b.at(2, 22))

    # Wait until the language server has responded to the codeAction request
    yield wait_language_server("textDocument/codeAction", "Ada")

    # Verify that one codeAction message has been created
    m = GPS.Message.list()
    gps_assert(len(m), 1, "there should be one message at this point")

    # Execute the codeaction
    m[0].execute_action()
    yield wait_language_server("workspace/executeCommand", "Ada")

    # Check that the edits have been received
    gps_assert("\r" in b.get_chars(),
               False,
               "There are stray CR characters in the editor")
