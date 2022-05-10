"""Tests the behavior of the GNATfuzz integration, on the
   "fuzz" parts of the workflow.
"""

from distutils.log import debug
from gs_utils.internal.utils import (
    run_test_driver,
    timeout,
    get_button_from_label,
    get_widget_by_name,
    get_window_by_title,
    gps_assert,
    idle_modal_dialog,
    wait_for_mdi_child,
)
from pygps import double_click_events
from pygps.tree import click_in_tree

INCREMENTS_MS = 1000     # Timeout increments
MAX_TIME_MS = 20 * 1000  # Max timeout wait

@run_test_driver
def driver():
    # Launch a fuzzing session
    yield idle_modal_dialog(lambda: GPS.execute_action("gnatfuzz fuzz workflow"))
    yield wait_for_mdi_child("gnatfuzz fuzz")
    dialog = get_window_by_title("gnatfuzz fuzz")
    get_button_from_label("Execute", dialog).clicked()

    yield wait_for_mdi_child("Fuzz Crashes")

    view = get_widget_by_name("fuzz_crash_list_view")
    model = view.get_model()

    # Wait 20 seconds at most, until messages appear in the "Fuzz crashes" view

    time_waited = 0

    while time_waited < MAX_TIME_MS:
        if len(model) > 0:
            break
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS

    # Test the contents of the model: presence of the crash...
    gps_assert(model[0][0], "1 (Crash)", "wrong contents in the first row")

    # ... and the fact that the faulty parameter (causing integer overflow)
    # is properly found by the fuzzer and displayed in the view.
    gps_assert(
        int(model[0, 0][1]),
        2**31 - 1,
        "wrong value for the parameter which causes the crash",
    )

    # We can stop fuzzing now that we've had one crash
    GPS.execute_action("gnatfuzz fuzz workflow")

    # Click in the view to launch a debug workflow
    click_in_tree(view, path="0", events=double_click_events)

    # Wait 20 seconds at most, until we have the right data in the debugger view

    debugger_text = None

    time_waited = 0

    expected_text = "Y := X + 1"

    while time_waited < MAX_TIME_MS:
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS
        d = None
        try:
            d = GPS.Debugger.get()
        except GPS.Exception:
            pass
        if d is None:
            continue
        debugger_text = d.get_console().get_text()

        # The debugger console should contain this
        if expected_text in debugger_text:
            break


    gps_assert(expected_text in debugger_text, True,
               f"{expected_text} didn't appear in output:\n{debugger_text}")