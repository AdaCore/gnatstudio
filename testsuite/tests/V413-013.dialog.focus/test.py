"""
When a dialog modal is opened no actions should be executed in the editor.
"""
import GPS
from gs_utils.internal.utils import *
from gs_utils.internal.dialogs import *


@run_test_driver
def test_driver():
    GPS.Preference("General-Use-Native-Dialogs").set(False)
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    expected = b.get_chars()
    b.current_view().goto(b.at(1, 18))
    d = Gtk_File_Chooser_Dialog()
    yield d.open_and_yield("save as")
    # The first BACKSPACE will be sent to the "new name entry" which will
    # trigger the completion
    entry = get_widgets_by_type(Gtk.Entry, d.dialogs[0])[0]
    gps_assert(entry.get_text(), "main.adb", "Wrong text in entry")
    entry.get_toplevel().grab_focus()
    entry.grab_focus()
    send_key_event(GDK_BACKSPACE)
    yield timeout(200)
    # During the completion the focus is whacky => the second and third
    # BACKSPACE should do nothing
    send_key_event(GDK_BACKSPACE)
    yield timeout(200)
    send_key_event(GDK_BACKSPACE)
    yield timeout(200)
    gps_assert(entry.get_text(),
               ".adb",
               "Backspace should have affected the entry")
    yield d.cancel()
    gps_assert(len(b.get_chars()),
               len(expected),
               "The buffer has been modified while a modal dialog was shown")
