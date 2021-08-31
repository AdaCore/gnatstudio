"""
Test the capability to reuse a pure_buffer when trying to open a new editor
for the same file => it allows to preserve the undo/redo queue for actions
such as renaming even if the file is not opened.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Retrieve the buffer text and close it
    b1 = GPS.EditorBuffer.get(GPS.File("refactor.adb"))
    expected = b1.get_chars()
    b1.close()

    b2 = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    b2.current_view().goto(b2.at(5, 14))

    # Execute a rename touching multiple files
    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Bye")
    dialog = get_window_by_title("Renaming entity")
    # Use the auto save preference to avoid manually saving each buffers and
    # prevent opening them.
    check = get_button_from_label("Automatically save modified files", dialog)
    check.set_active(True)
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('language_server_response_processed')
    yield timeout(500)  # Wait for the messages to be created and sorted
    gps_assert(GPS.MDI.get("refactor.adb"),
               None,
               "Refactor.adb should not be opened")

    # At this point, refactor.adb is not be present in the editors area
    # and it was modified via the "rename" => we should be able to undo the
    # local rename when opening it.
    # Invalidate the global undo/redo by modifying the buffer manually
    b3 = GPS.EditorBuffer.get(GPS.File("refactor.adb"))
    b3.insert("!")
    GPS.execute_action("undo")  # Undo the insert
    yield wait_idle()
    GPS.execute_action("undo")  # Undo the rename local
    yield wait_idle()
    b3.save(force=True)
    gps_assert(b3.get_chars(),
               expected,
               "The undo/redo queue was not preserved")
