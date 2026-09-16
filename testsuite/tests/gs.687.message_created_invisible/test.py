"""
Regression test: the public 'message_created' hook must run for every
message, including a message which is visible nowhere (neither on the
editor's side area nor in the Locations view).
"""

from GPS import *
from gs_utils.internal.utils import *

CREATED = []


def on_message_created(hook, message):
    CREATED.append(message.get_text())


@run_test_driver
def run_test():
    yield wait_tasks()

    GPS.Hook("message_created").add(on_message_created)

    GPS.Message(
        "invisible category",
        GPS.File("foo.adb"),
        5,
        4,
        "invisible message",
        show_on_editor_side=False,
        show_in_locations=False,
    )
    yield wait_idle()

    gps_assert(
        CREATED,
        ["invisible message"],
        "message_created should run for an invisible message",
    )

    GPS.Message(
        "visible category",
        GPS.File("foo.adb"),
        5,
        4,
        "visible message",
    )
    yield wait_idle()

    gps_assert(
        CREATED,
        ["invisible message", "visible message"],
        "message_created should still run for a visible message",
    )
