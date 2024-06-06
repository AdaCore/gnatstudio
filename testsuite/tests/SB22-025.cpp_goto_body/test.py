"""Test that goto body action does not open Untitled (new) file when the body
is not found"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("f.hh"))
    yield wait_tasks(other_than=known_tasks)
    loc = b.at(47, 5)
    b.insert(loc, "void my_third_public_func ();\n")

    v = b.current_view()
    v.goto(b.at(47, 20))
    GPS.execute_action("goto body")
    yield wait_tasks()

    view = GPS.EditorBuffer.get().current_view()
    gps_assert(view.title(True), "f.hh")

    b.undo()
    yield wait_tasks()
