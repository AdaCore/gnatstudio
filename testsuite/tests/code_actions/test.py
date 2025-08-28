# Check the behavior of the UI for code actions.

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_language_server,
    gps_assert,
    wait_idle,
    get_widget_by_name,
    get_widgets_by_type,
)
from workflows.promises import idle_modal_dialog
from gi.repository import Gtk


@run_test_driver
def driver():
    # Open an editor and go to a line where there's two code actions
    # ('Name parameters' and 'Delete Entity')
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()
    v.goto(b.at(5, 9))

    # Wait until the language server has responded to the codeAction request
    yield wait_language_server("textDocument/codeAction")

    # Verify that two codeAction messages have been created
    m = GPS.Message.list()
    gps_assert(len(m), 2, "there should be two messages at this point")
    gps_assert(
        [m[0].get_text(), m[1].get_text()],
        ["Name parameters in the call", "Delete Entity and all references"],
        "we have two codeActions, but not the expected ones",
    )

    # Click on the side area
    yield idle_modal_dialog(lambda: b.click_on_side_column(5, 1, ""))

    # Verify that the multiactions menu is correclty displayed
    multi_actions_menu = get_widget_by_name("gnatstudio_multiple_actions_menu")
    gps_assert(
        multi_actions_menu is not None, True, "The multi actions menu should be shown"
    )

    # Click on the 'Introduce Parameter' codeAction menu item and verify
    # that it executed the action
    items = get_widgets_by_type(Gtk.MenuItem, multi_actions_menu)
    items[2].activate()
    items[2].destroy()

    # Wait for the language server
    yield wait_language_server("workspace/executeCommand")

    # Check that the edits have been received
    gps_assert(
        b.get_chars(b.at(5, 1), b.at(6, 1)).strip(),
        'Put_Line (Item => "hello");',
        "edits not received",
    )

    # Check that we only have the 'Introduce Parameter' code action now
    v.goto(b.at(5, 22))
    yield wait_idle()
    yield wait_language_server("textDocument/codeAction")
    m = GPS.Message.list(category="_internal_code_actions")[0].get_text()
    gps_assert(
        m,
        "Introduce Parameter",
        "there should be only 'Introduce Parameter' code action",
    )
