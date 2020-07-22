"""
Verify exporting from the Locations view
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    buf.current_view().goto(buf.at(7, 21))
    GPS.execute_action("Find All References")
    yield hook("language_server_response_processed")
    yield wait_idle()

    entry = get_widget_by_name("Locations View Filter")
    entry.set_text("")
    entry.grab_focus()
    yield wait_idle()

    # export all messages and check them
    GPS.execute_action("export locations to editor")
    yield wait_idle()

    focus_window = MDI.current().pywidget()

    view = get_widgets_by_type(Gtk.TextView, focus_window)[0]
    buffer = view.get_buffer()
    text = (buffer.get_text(buffer.get_start_iter(),
                            buffer.get_end_iter(), False))

    lines = text.splitlines()
    gps_assert(len(lines), 9)
    gps_assert(lines[0], "References for Put_Line (main.adb:7)")

    if lines[1].endswith("a.adb"):
        gps_assert(lines[2], '        7:19 [call] Ada.Text_IO.Put_Line ("P");')
        gps_assert(lines[3].endswith("main.adb"), True)
        gps_assert(lines[4], '        7:16 [call] Ada.Text_IO.Put_Line ("Hello");')
        gps_assert(lines[5], '        8:16 [call] Ada.Text_IO.Put_Line ("!");')
        gps_assert(lines[6].endswith("a-textio.ads"), True)
        gps_assert(lines[7].endswith("[reference] procedure Put_Line"), True)
    else:
        gps_assert(lines[1].endswith("a-textio.ads"), True)
        gps_assert(lines[2].endswith("[reference] procedure Put_Line"), True)
        gps_assert(lines[3].endswith("a.adb"), True)
        gps_assert(lines[4], '        7:19 [call] Ada.Text_IO.Put_Line ("P");')
        gps_assert(lines[5].endswith("main.adb"), True)
        gps_assert(lines[6], '        7:16 [call] Ada.Text_IO.Put_Line ("Hello");')
        gps_assert(lines[7], '        8:16 [call] Ada.Text_IO.Put_Line ("!");')

    # filter messages belong to main.adb file and check that only
    # these messages are exported

    entry = get_widget_by_name("Locations View Filter")
    entry.set_text("main")
    entry.grab_focus()
    yield wait_idle()
    send_key_event(GDK_RETURN)
    yield wait_idle()

    GPS.execute_action("export filtered locations to editor")
    yield wait_idle()

    focus_window = MDI.current().pywidget()

    view = get_widgets_by_type(Gtk.TextView, focus_window)[0]
    buffer = view.get_buffer()
    text = (buffer.get_text(buffer.get_start_iter(),
                            buffer.get_end_iter(), False))

    lines = text.splitlines()
    gps_assert(len(lines), 4)
    gps_assert(lines[0], "References for Put_Line (main.adb:7)")
    gps_assert(lines[1].endswith("main.adb"), True)
    gps_assert(lines[2], '    7:16 [call] Ada.Text_IO.Put_Line ("Hello");')
    gps_assert(lines[3], '    8:16 [call] Ada.Text_IO.Put_Line ("!");')
