"""
This test checks that pressing Shift-Enter while the Search
view has the focus performs a backward search.
"""
import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    main = GPS.File("main.adb")
    buf = GPS.EditorBuffer.get(main)
    buf.current_view().goto(buf.at(11, 1))

    s = dialogs.Search()
    yield s.open_and_yield()
    s.pattern.set_text("Result")

    send_key_event(GDK_RETURN, shift=1)
    yield wait_idle()

    gps_assert(
        buf.current_view().cursor(),
        buf.at(9, 14),
        "Shift-Enter did not search backwards",
    )
