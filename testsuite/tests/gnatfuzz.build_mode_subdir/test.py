"""Tests the GNATfuzz workflow in a build mode that puts the objects in a
   subdirectory: analyze, harness generation and a fuzzing session, all
   launched from GNAT Studio.
"""

import os

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
    wait_tasks,
)
from gi.repository import Gtk

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


@run_test_driver
def driver():
    # The "debug" build mode puts the objects in the "debug" subdirectory
    GPS.set_build_mode("debug")
    yield wait_tasks()
    obj_dir = GPS.Project.root().object_dirs()[0]
    gps_assert(
        os.path.basename(os.path.normpath(obj_dir)),
        "debug",
        "the build mode should select an object subdirectory",
    )

    # Analyze: the report must be where the plugin reads it
    GPS.execute_action("gnatfuzz analyze project workflow")
    yield wait_tasks()

    analyze_json = os.path.join(obj_dir, "gnatfuzz", "analyze.json")
    gps_assert(os.path.exists(analyze_json), True, f"{analyze_json} not written")
    messages = GPS.Message.list("Fuzzable Subprograms")
    gps_assert(len(messages), 1, "wrong number of fuzzable subprograms")

    # Generate the harness from the message, and switch to it
    yield idle_modal_dialog(lambda: messages[0].execute_action())
    dialog = yield from wait_for_dialog("gnatfuzz generate")
    yield idle_modal_dialog(lambda: get_button_from_label("Execute", dialog).clicked())
    dialog = yield from wait_for_dialog("Confirmation")
    get_button_from_label("Yes", dialog).clicked()
    yield wait_tasks()

    gps_assert(
        os.path.basename(GPS.Project.root().file().name()),
        "fuzz_test.gpr",
        "not switched to the harness project",
    )

    # Fuzz. The workflow opens the gnatfuzz fuzz dialog only once
    # gnatfuzz build, generate-corpus and minimize-corpus have succeeded.
    yield idle_modal_dialog(lambda: GPS.execute_action("gnatfuzz fuzz workflow"))
    dialog = yield from wait_for_dialog("gnatfuzz build")
    get_button_from_label("Execute", dialog).clicked()

    dialog = yield from wait_for_dialog("gnatfuzz fuzz")
    # CMPLOG, enabled by default, needs a core of its own
    cl_entry = [
        ent
        for ent in get_widgets_by_type(Gtk.Entry, dialog)
        if ent.get_text().startswith("gnatfuzz ")
    ][0]
    cl_entry.set_text(cl_entry.get_text() + " --cores=2")
    get_button_from_label("Execute", dialog).clicked()

    yield wait_for_mdi_child("Fuzz crashes")
    model = get_widget_by_name("fuzz_crash_list_view").get_model()

    time_waited = 0
    while len(model) == 0 and time_waited < MAX_TIME_MS:
        yield timeout(INCREMENTS_MS)
        time_waited += INCREMENTS_MS

    gps_assert(len(model) > 0, True, "no crash in the 'Fuzz crashes' view")

    GPS.execute_action("gnatfuzz fuzz workflow")
    yield wait_tasks()
