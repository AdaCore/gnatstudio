"""
This test checks that we correctly display the documentation
for all overloaded suprograms, even when they are gathered under
the same completion proposal.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Disable completion snippets and restart the ALS
    GPS.Preference("LSP-Completion-Use-Snippets").set(False)
    GPS.execute_action("Restart ada language server")
    yield hook('language_server_started')
    yield wait_tasks()
    
    # Enable dynamic completion and insert text to get completion
    # proposals
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()
    for ch in "Not":
        send_key_event(ord(ch))
        yield timeout(100)

    # Select the first entry ('Do_Nothing')
    yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=single_click_events)
    yield wait_until_true(
        lambda: get_widget_by_name("completion-notes-title") != None)

    # Get the documentation labels displayed after selecting Do_Nothing
    notes_box = get_widget_by_name("notes-doc-box")
    doc_labels = get_widgets_by_type(Gtk.Label, notes_box)
    label_text = ""
    for doc_label in doc_labels:
        label_text += doc_label.get_label()

    # Verify that documentation for both Do_Nothing subprograms is there
    gps_assert("bar.ads (7:4)" in label_text, True,
        "Documentation for first Do_Nothing should be listed")
    gps_assert("bar.ads (8:4)" in label_text, True,
        "Documentation for second Do_Nothing should be listed")