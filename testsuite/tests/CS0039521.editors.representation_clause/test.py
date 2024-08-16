"""
Test rep clauses plugin in case of a generic package.
"""

from GPS import *
from gs_utils.internal.utils import *

SHOW = "Show representation clauses"


def play_scenario(name, msg):
    buf = GPS.EditorBuffer.get(GPS.File(name))
    init_chars = buf.characters_count()
    # The call should add special lines like this:
    # for Unit.Access_Type'Alignment use ??;
    GPS.execute_action(SHOW)
    yield wait_tasks(other_than=known_tasks)
    show_chars = buf.characters_count()
    gps_assert(show_chars > init_chars, True, "Missing representation clause in " + msg)


@run_test_driver
def run_test():
    yield play_scenario("unit.ads", "package decl")
