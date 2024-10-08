# Check the behavior of the UI for code actions when using clangd

from gs_utils.internal.utils import (
    run_test_driver,
    wait_language_server,
    gps_assert,
    get_widget_by_name,
    timeout,
)


@run_test_driver
def driver():
    # Open an editor and go to a line where there's a code action
    b = GPS.EditorBuffer.get(GPS.File("main.c"))
    v = b.current_view()
    v.goto(b.at(5, 8))

    # Wait until the language server has responded to the codeAction request
    yield wait_language_server("textDocument/codeAction", "C")

    # Verify that one codeAction message has been created
    m = GPS.Message.list()
    gps_assert(len(m), 1, "there should be one message at this point")
    gps_assert(
        m[0].get_category(),
        "_internal_code_actions",
        "we have a message, but not in the expected category",
    )

    # Click on the side action
    b.click_on_side_column(5, 1, "gps-codefix")
    # Allow a timeout for the asynchronous popup of the menu
    yield timeout(1000)

    menu = get_widget_by_name("gnatstudio_code_actions_menu")
    gps_assert(menu is not None, True, "no menu found")

    # Check that the menu contains the "Name parameters" action
    item = menu.get_children()[0]
    gps_assert(
        item.get_label(),
        "Expand macro &apos;FOO&apos;",
        "the menu item doesn' have the right title",
    )

    # Now activate the menu item and wait for the application of the edits
    item.activate()
    yield wait_language_server("workspace/executeCommand", "C")

    # Check that the edits have been received
    gps_assert(
        b.get_chars(b.at(5, 1), b.at(6, 1)).strip(), "if (0 > 2)", "edits not received"
    )

    yield wait_language_server("textDocument/codeAction", "C")
    m = GPS.Message.list(category="_internal_code_actions")
    gps_assert(len(m), 0, "there should be no code action message")
