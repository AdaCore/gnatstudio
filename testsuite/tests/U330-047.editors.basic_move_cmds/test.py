"""
Verify that basic move commands (i.e: move to next line) work fine
on normal code.
"""

from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    # wait LSP responses has been processed
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    v.goto(b.at(2, 1))
    GPS.execute_action("move to next line")
    gps_assert(v.cursor().line(), 3, "'move to next line' does not work")

    GPS.execute_action("move to previous line")
    gps_assert(v.cursor().line(), 2, "'move to previous line' does not work")

    GPS.execute_action("move to next paragraph")
    gps_assert(v.cursor().line(), 7, "'move to next paragraph' does not work")

    GPS.execute_action("move to previous paragraph")
    gps_assert(v.cursor().line(), 1, "'move to previous paragraph' does not work")
