"""
Execute a command which doesn't exist in the OS Shell => no exception should
be raised.
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

    for c in "helloworld":
        send_key_event(ord(c))
        yield timeout(100)

    send_key_event(GDK_RETURN)
