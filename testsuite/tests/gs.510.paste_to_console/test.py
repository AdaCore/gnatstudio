"""Test that Paste to interactive console works as expected"""
from GPS import *
from gs_utils.internal.utils import *
import sys


@run_test_driver
def run_test():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("accented.adb"))
    buf.select(buf.at(5, 4), buf.at(5, 12))
    send_key_event(ord("c"), control=True)
    yield wait_idle()

    GPS.execute_action("open Python")
    yield wait_idle()

    send_key_event(ord("v"), control=True)
    yield wait_idle()

    gps_assert(
        GPS.Console("Python").get_text(),
        ">>> Put_Line",
        "Wrong Paste",
    )
