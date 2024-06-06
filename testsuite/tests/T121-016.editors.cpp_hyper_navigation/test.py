"""
This test checks that goto declaration of the undeclared C++ entity
don't open empty editor.
"""

from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    b = GPS.EditorBuffer.get(GPS.File("test.cpp"))
    yield (wait_tasks())
    b.current_view().goto(b.at(4, 5))
    yield (wait_tasks())
    old_children = GPS.MDI.children()

    GPS.execute_action("goto declaration")
    yield (wait_tasks())
    new_children = GPS.MDI.children()

    gps_assert(
        False,
        bool(set(new_children).symmetric_difference(set(old_children))),
        "Set of opened windows are different",
    )
