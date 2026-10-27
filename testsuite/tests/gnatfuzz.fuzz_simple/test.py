"""Tests the behavior of the GNATfuzz integration, on the
   "fuzz" parts of the workflow.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    timeout,
    get_button_from_label,
    get_widget_by_name,
    get_widgets_by_type,
    get_window_by_title,
    gps_assert,
    idle_modal_dialog,
    wait_for_mdi_child,
    wait_idle,
    wait_tasks,
)
from gi.repository import Gtk
from pygps import double_click_events
from pygps.tree import click_in_tree

INCREMENTS_MS = 1000  # Timeout increments
MAX_TIME_MS = 60 * 1000  # Max timeout wait


def wait_for_dialog(title):
    """Poll until a window titled title appears, and return it."""
    time_waited = 0
    dialog = get_window_by_title(title)
    while dialog is None and time_waited < MAX_TIME_MS:
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS
        dialog = get_window_by_title(title)
    gps_assert(dialog is not None, True, f"The '{title}' dialog never appeared")
    return dialog


def wait_for_fuzz_task():
    """Poll until the "gnatfuzz fuzz" task is running.

    The Start/Stop action stops the session only once this task exists:
    before that, it starts a second session instead.
    """
    time_waited = 0
    while time_waited < MAX_TIME_MS:
        if any(t.name() == "gnatfuzz fuzz" for t in GPS.Task.list()):
            return
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS
    gps_assert(False, True, "The 'gnatfuzz fuzz' task never started")


def open_fuzzing_session():
    """Start the fuzzing workflow and return its "gnatfuzz build" dialog.

    The workflow empties the GNATfuzz views before opening it.
    """
    yield idle_modal_dialog(lambda: GPS.execute_action("gnatfuzz fuzz workflow"))
    return (yield from wait_for_dialog("gnatfuzz build"))


def run_fuzzing_session(build_dialog):
    """Run the workflow with the default options, from its build dialog.

    gnatfuzz build, generate-corpus and minimize-corpus run before
    gnatfuzz fuzz, which opens a dialog of its own.
    """
    get_button_from_label("Execute", build_dialog).clicked()

    dialog = yield from wait_for_dialog("gnatfuzz fuzz")
    # CMPLOG, enabled by default, needs a core of its own
    cl_entry = [
        ent
        for ent in get_widgets_by_type(Gtk.Entry, dialog)
        if ent.get_text().startswith("gnatfuzz ")
    ][0]
    cl_entry.set_text(cl_entry.get_text() + " --cores=2")
    get_button_from_label("Execute", dialog).clicked()


@run_test_driver
def driver():
    build_dialog = yield from open_fuzzing_session()
    yield from run_fuzzing_session(build_dialog)

    yield wait_for_mdi_child("Fuzz crashes")

    view = get_widget_by_name("fuzz_crash_list_view")
    model = view.get_model()

    # Wait until messages appear in the "Fuzz crashes" view

    time_waited = 0

    while time_waited < MAX_TIME_MS:
        if len(model) > 0:
            break
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS

    gps_assert(len(model) > 0, True, "no crash in the 'Fuzz crashes' view")

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
    yield wait_tasks()

    # Click in the view to launch a debug workflow
    GPS.MDI.get("Fuzz crashes").raise_window()
    yield wait_idle()
    click_in_tree(view, path="0", events=double_click_events)

    # Wait until we have the right data in the debugger view

    debugger_text = None

    time_waited = 0

    # The debugger stops at the overflow in the fuzzed subprogram
    expected_text = "simple.adb:5"

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

    gps_assert(
        expected_text in debugger_text,
        True,
        f"{expected_text} didn't appear in output:\n{debugger_text}",
    )

    # Quit the debugger
    GPS.execute_action("terminate all debuggers")
    yield wait_idle()

    # Restart a fuzz session: the Fuzz crashes view should be empty. Check
    # it before fuzzing starts, which finds this crash again within seconds.
    build_dialog = yield from open_fuzzing_session()
    model = get_widget_by_name("fuzz_crash_list_view").get_model()
    gps_assert(
        len(model), 0, "The Fuzz crashes view should clear when starting a session"
    )
    yield from run_fuzzing_session(build_dialog)

    # Stop the session as soon as it runs
    yield from wait_for_fuzz_task()
    GPS.execute_action("gnatfuzz fuzz workflow")
    yield wait_tasks()

    gps_assert(
        any(t.name() == "gnatfuzz fuzz" for t in GPS.Task.list()),
        False,
        "The 'gnatfuzz fuzz' task should be gone after stopping the session",
    )
