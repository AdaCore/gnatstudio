"""
Verify that basic move commands (i.e: move to next line) work fine with
folded blocks.
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

    yield timeout(1000)
    b.blocks_fold()

    v.goto(b.at(2, 1))
    GPS.execute_action("move to next line")
    gps_assert(
        v.cursor().line(), 6, "'move to next line' does not work with folded blocks"
    )

    GPS.execute_action("move to previous line")
    gps_assert(
        v.cursor().line(), 2, "'move to previous line' does not work with folded blocks"
    )

    GPS.execute_action("move to next paragraph")
    gps_assert(
        v.cursor().line(),
        10,
        "'move to next paragraph' does not work with folded blocks",
    )

    GPS.execute_action("move to previous paragraph")
    gps_assert(
        v.cursor().line(),
        1,
        "'move to previous paragraph' does not work with folded blocks",
    )
