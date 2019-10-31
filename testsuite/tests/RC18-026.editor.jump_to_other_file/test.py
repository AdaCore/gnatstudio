"""
Test the Goto Body<->Decl action when the second file is closed or
already opened.
"""

from GPS import *
from gs_utils.internal.utils import *
from os.path import basename

GOTO_ACTION = "Goto other file"
BODY = "bar.adb"
SPEC = "bar.ads"


def compare_location(name, line, column, message):
    context = current_context()
    gps_assert(basename(context.file().name()),
               name,
               "Wrong file having the focus " + message)
    location = context.location()
    gps_assert(location.line(),
               line,
               "Wrong line location " + message)
    gps_assert(location.column(),
               column,
               "Wrong column location " + message)


@run_test_driver
def run_test():
    body = GPS.EditorBuffer.get(GPS.File(BODY))
    compare_location(BODY, 1, 1, "when opening the body")

    # Open bar.ads at (1, 1)
    GPS.execute_action(GOTO_ACTION)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(GPS.MDI.get("bar.ads") is not None,
               True,
               "The spec should have been opened")
    spec = GPS.EditorBuffer.get(GPS.File(SPEC), open=False)
    compare_location(SPEC, 1, 1, "when opening the spec")

    # Set the location in the body ...
    body.current_view().goto(body.at(3, 15))
    GPS.execute_action(GOTO_ACTION)
    yield wait_tasks(other_than=known_tasks)
    # ... the location should be unchanged after the action
    compare_location(BODY, 3, 15, "when going back to the body")

    # Go back to the spec
    GPS.execute_action(GOTO_ACTION)
    yield wait_tasks(other_than=known_tasks)
    compare_location(SPEC, 1, 1, "when going back to the spec")
