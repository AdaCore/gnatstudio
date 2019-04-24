"""
Test the show_representation plugin on a main/package decl/package body.
"""

from GPS import *
from gps_utils.internal.utils import *

SHOW = "Show representation clauses"
HIDE = "Hide representation clauses"


def play_scenario(name, msg):
    buf = GPS.EditorBuffer.get(GPS.File(name))
    init_chars = buf.characters_count()
    # The first call should add special lines
    GPS.execute_action(SHOW)
    yield wait_tasks(other_than=known_tasks)
    show_chars = buf.characters_count()
    gps_assert(show_chars > init_chars,
               True,
               "Missing representation clause in " + msg)
    # The second call to SHOW should not duplicate the number of special lines
    GPS.execute_action(SHOW)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.characters_count(),
               show_chars,
               "Issue with the second show for " + msg)
    expected = GPS.Console().get_text()
    # Should remove all the added lines
    GPS.execute_action(HIDE)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.characters_count(),
               init_chars,
               "Issue with the first hide for " + msg)
    gps_assert(GPS.Console().get_text(),
               expected,
               "No message should be added by hide for " + msg)
    # The second call to HIDE should have no effect
    GPS.execute_action(HIDE)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.characters_count(),
               init_chars,
               "Issue with the second hide for " + msg)
    gps_assert(GPS.Console().get_text(),
               expected,
               "No message should be added by hide for " + msg)
    # No error for a SHOW after a HIDE
    GPS.execute_action(SHOW)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.characters_count(),
               show_chars,
               "Issue with the last show for " + msg)


@run_test_driver
def run_test():
    yield play_scenario("foo.adb", "main file")
    yield play_scenario("a.ads", "package decl")
    yield play_scenario("a.adb", "package body")
