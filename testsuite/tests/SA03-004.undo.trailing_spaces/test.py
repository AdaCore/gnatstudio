from gs_utils.internal.utils import run_test_driver, GDK_RETURN,\
    send_key_event, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    orig = b.get_chars()
    v = b.current_view()
    v.goto(b.at(2, 14))
    send_key_event(GDK_RETURN)

    # Check contents after entern
    gps_assert(b.get_chars(), orig.replace(" Foo", "\n   Foo"),
               "enter character was not received")

    # Undo and check for contents
    b.undo()
    gps_assert(b.get_chars(), orig, "contents corrupted after undo")
