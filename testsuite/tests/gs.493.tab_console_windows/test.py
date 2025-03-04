"""
This test checks that <tab> is displayed properly
in the OS console in Windows, until (or when) we
find a way to process it for completion in
Windows too.
In Linux we process <tab> for completion properly.

"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    console_name = os.getenv("COMSPEC")
    GPS.execute_action("open os shell")
    yield wait_for_mdi_child(console_name)

    for ch in "cd cor":
        send_key_event(ord(ch))
        yield timeout(100)
    send_key_event(GDK_TAB)
    yield wait_idle()

    gps_assert(
        ">cd cor	" in GPS.Console(console_name).get_text(),
        True,
        "Wrong content in OS console:" + GPS.Console(console_name).get_text(),
    )
