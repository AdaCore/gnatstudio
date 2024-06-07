"""
This test checks that we can handle sources with tabs.
"""
import GPS
from gs_utils.internal.utils import *

expected = """
int main()
{
  demo ();
  
}
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.c"))
    yield wait_tasks()
    buf.current_view().goto(buf.at(4, 16))
    yield wait_idle()

    send_key_event(GDK_RETURN)
    yield wait_language_server("textDocument/onTypeFormatting", "C")
    yield wait_idle()

    b = buf.gtk_text_buffer()
    text = b.get_text(b.get_start_iter(), b.get_end_iter(), False)

    gps_assert(text, expected, "Incorrect")
