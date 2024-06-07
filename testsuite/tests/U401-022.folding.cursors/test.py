"""
Verify that we correctly unfold the cursor's enclosing block
when changing it's location programatically.
"""

from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    # Set a brakpoint at line 5
    v.goto(b.at(5, 1))
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    # wait LSP responses has been processed
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    # Fold all the blocks. The breakpoint is now folded
    yield timeout(500)
    b.blocks_fold()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Run it until we reach the breakpoint
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)

    # Verify that we can't unfold the block, since it should
    # already be unfolded after reaching the breakpoint
    icon_found = True
    try:
        b.click_on_side_icon(5, 1, "gps-unfold-block-symbolic")
    except Exception:
        icon_found = False

    gps_assert(icon_found, False, "The block should already be unfolded")
