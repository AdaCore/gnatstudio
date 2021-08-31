"""
Test the undo/redo capacity of workspaceEdit via rename across multiple files
"""

import GPS
from gs_utils.internal.utils import *

FIRST_NAME = "Hello"
SECOND_NAME = "Bye"
CATEGORY_NAME = "Refactoring - rename %s to %s" % (FIRST_NAME, SECOND_NAME)


@run_test_driver
def run_test():
    b = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    b.current_view().goto(b.at(5, 14))

    def verify(nb_first, nb_second, nb_msg, comment):
        def __count(s):
            """
            Count the occurence of "s" in the project sources.
            Only works on saved files.
            """
            res = 0
            GPS.Search.lookup(GPS.Search.SOURCES).set_pattern(s)
            for m in GPS.Search.lookup(GPS.Search.SOURCES):
                res = res + 1
            return res

        gps_assert(__count(FIRST_NAME),
                   nb_first,
                   "Wrong number of ref to " + FIRST_NAME + " for " + comment)
        gps_assert(__count(SECOND_NAME),
                   nb_second,
                   "Wrong number of ref to " + SECOND_NAME + " for " + comment)
        gps_assert(len(GPS.Message.list(CATEGORY_NAME)),
                   nb_msg,
                   "Wrong number of messages for " + comment)

    # Execute a rename affecting multiple files
    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text(SECOND_NAME)
    dialog = get_window_by_title("Renaming entity")
    # Use the auto save preference to avoid manually saving each buffers and
    # prevent opening them.
    check = get_button_from_label("Automatically save modified files", dialog)
    check.set_active(True)
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('language_server_response_processed')
    yield timeout(500)  # Wait for the messages to be created and sorted

    verify(41, 14, 14, "Rename")
    GPS.execute_action("undo")  # Undo global 1
    yield wait_idle()
    verify(55, 0, 0, "First undo")
    GPS.execute_action("redo")  # Redo global 1
    yield wait_idle()
    verify(41, 14, 14, "First redo")
    GPS.execute_action("undo")  # Undo global 2
    yield wait_idle()
    verify(55, 0, 0, "Second undo")
    GPS.execute_action("redo")  # Redo global 2
    yield wait_idle()
    verify(41, 14, 14, "Second redo")
    # Opening a file affected by the rename should not reset its undo/redo
    # queue
    GPS.EditorBuffer.get(GPS.File("refactor.adb"))
    GPS.execute_action("undo")  # Undo global 3
    yield wait_idle()
    verify(55, 0, 0, "Third undo")
    GPS.execute_action("redo")  # Redo global 3
    yield wait_idle()
    verify(41, 14, 14, "Third redo")

    # Invalidate the global undo/redo by modifying the buffer manually
    # First raise the window to give load its undo/redo queue
    GPS.MDI.get("foo.adb").raise_window()
    b.insert("!")
    # We should still be able to undo the rename locally
    GPS.execute_action("undo")  # Undo the insert
    yield wait_idle()
    GPS.execute_action("undo")  # Undo the rename local
    yield wait_idle()
    b.save(force=True)
    # At this point we have only undone one renaming
    verify(42, 13, 14, "Local undo")
