"""Simple integration test for gnatfuzz.

This tests the "analyze" and "generate" features.
"""

from gs_utils.internal.utils import (
    run_test_driver,
    timeout,
    get_button_from_label,
    get_window_by_title,
    idle_modal_dialog,
    gps_assert,
    wait_tasks,
)


@run_test_driver
def driver():
    GPS.EditorBuffer.get(GPS.File("p.ads"))
    GPS.execute_action("gnatfuzz analyze file workflow")

    messages = []

    # Wait at most 2 seconds for messages to appear in the locations view
    time_waited = 0
    while time_waited < 2000:
        yield timeout(100)
        time_waited += 100
        messages = GPS.Message.list("Fuzzable Subprograms")
        if len(messages) > 0:
            break

    gps_assert(len(messages) > 0, True, "No messages found after analyze")
    m = messages[0]

    # We have one "Fuzzable program" message: click on the action
    yield idle_modal_dialog(lambda: m.execute_action())
    dialog = get_window_by_title("gnatfuzz analyze")

    # The confirmation dialog to switch to the harness project should pop up
    yield idle_modal_dialog(lambda: get_button_from_label("Execute", dialog).clicked())
    dialog = get_window_by_title("Confirmation")
    get_button_from_label("Yes", dialog).clicked()
    yield wait_tasks()

    # Check that we've switched to the harness project
    gps_assert(GPS.Project.root().name(), "Fuzz_Test", "wrong project name after generation")