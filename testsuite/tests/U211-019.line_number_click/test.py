# This test verifies the behavior of clicking on line numbers:
#   - clicking shouldn't move the cursor
#
#   - clicking should add breakpoints both when the debugger is off and on
#   - clicking should remove breakpoints
#   - the two clicking operations above should work even if the cursor is not
#     on the line being clicked

from gs_utils.internal.utils import run_test_driver, gps_assert, \
   wait_until_not_busy, timeout


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("bla.adb"))
    view = buf.current_view()

    # Place the cursor on line 3
    view.goto(buf.at(3, 1))

    # Verify the cursor location
    gps_assert(view.cursor().line(), 3, "cursor not moved initially")

    # click on line 5 and verify that the cursor didn't move
    buf.click_on_line_number(5)

    gps_assert(view.cursor().line(), 3, "cursor moved after clicking on line")

    # Launch the debugger
    d = GPS.Debugger.spawn(GPS.File("bla.adb"))
    yield wait_until_not_busy(d)

    # Re-grab the editor focus after the debugger has launched
    GPS.MDI.get("bla.adb").raise_window()

    # There should be one breakpoint, on line 5
    gps_assert(len(d.breakpoints), 1, "wrong number of breakpoints")
    gps_assert(d.breakpoints[0].line, 5, "wrong loc for breakpoint")

    # Click on line 7, verify the location of the cursor
    # and of the new breakpoint
    view.pywidget().grab_focus()
    buf.click_on_line_number(7)
    yield wait_until_not_busy(d)
    gps_assert(view.cursor().line(), 3, "cursor moved after adding breakpoint")
    gps_assert(d.breakpoints[1].line, 7, "wrong loc for new breakpoint")

    # Click on line 5, this should cancel the first breakpoint
    buf.click_on_line_number(5)
    yield wait_until_not_busy(d)
    gps_assert(len(d.breakpoints), 1, "wrong number of breakpoints")
    gps_assert(d.breakpoints[0].line, 7, "wrong loc for breakpoint")

    # One last check that the cursor didn't move
    gps_assert(view.cursor().line(), 3, "cursor moved after clicking on line")

