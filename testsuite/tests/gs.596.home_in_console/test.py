"""
This test checks that <home> is dispatched properly
in the OS console.

"""

import GPS
import os.path
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    console_name = os.getenv("COMSPEC", "") + os.getenv("SHELL", "")
    GPS.execute_action("open os shell")
    yield wait_for_mdi_child(console_name)

    for ch in "kdir test":
        send_key_event(ord(ch))
        yield timeout(100)
    send_key_event(GDK_HOME)
    yield timeout(100)
    send_key_event(ord("m"))
    yield timeout(100)
    send_key_event(GDK_RETURN)
    yield timeout(100)

    # Check if "mkdir test" was executed:
    gps_assert(
        os.path.exists("test"),
        True,
        "HOME key is not working in console:" + GPS.Console(console_name).get_text(),
    )
