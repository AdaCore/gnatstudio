"""
Test the undo option when an error happens during renaming.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Set an editor read only
    b1 = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    b1.set_read_only(True)
    expected1 = b1.get_chars()

    # Do a renaming on a second editor
    b2 = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    b2.current_view().goto(b2.at(7, 16))
    expected2 = b2.get_chars()

    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Bye")
    dialog = get_window_by_title("Renaming entity")
    # Don't allow the permission to change
    check = get_button_from_label("Make files writable", dialog)
    check.set_active(False)
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield timeout(500)

    # A dialog reporting an error should be opened, reply no to it
    error_dialog = get_window_by_title(
        "Refactoring - rename Hello to Bye raises errors")
    GPS.Console().write(str(error_dialog))
    get_button_from_label("Undo", error_dialog).clicked()
    yield wait_idle()

    gps_assert(b1.get_chars(),
               expected1,
               "The read only buffer should not be modified")
    gps_assert(b2.get_chars(),
               expected2,
               "The writable buffer should be restored")
