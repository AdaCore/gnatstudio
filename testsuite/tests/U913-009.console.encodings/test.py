"""
Write some characters in OS Shell => no error regarding the encoding should
be shown in the Messages view.
"""

from GPS import *
from gs_utils.internal.utils import *
import os


@run_test_driver
def run_test():
    if os.name == "nt" and os.getenv("COMSPEC"):
        console_name = os.getenv("COMSPEC")
    elif os.getenv("SHELL") and os.getenv("TERM"):
        console_name = os.getenv("SHELL")

    # Spawn the console
    GPS.execute_action("open os shell")
    yield wait_for_mdi_child(console_name)

    # Clear the Message view => at this point the view should be empty if no
    # error
    GPS.Console().clear()
    for c in "hello":
        send_key_event(ord(c))
        yield timeout(100)

    gps_assert(
        GPS.Console().get_text(), "", "The Messages view should not contains any error"
    )
