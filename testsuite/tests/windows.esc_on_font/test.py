"""
This test checks that pressing ESC in a font entry does
not crash GPS.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    p = dialogs.Preferences()
    yield p.open_and_yield()

    # Open the "General" notebook page
    tree = get_widgets_by_type(Gtk.TreeView, p.dialog)[0]
    model = tree.get_model()
    path = find_in_tree(tree, 0, "Custom Styles")
    click_in_tree(tree, path)

    # Find the entry for the fixed width font
    entry = [e for e in get_widgets_by_type(Gtk.Entry, p.dialog)
             if e.get_text() == "Consolas 9" or
             e.get_text() == "Lucida Console 9" or
             e.get_text() == "Menlo 11" or
             e.get_text() == "Monaco 11" or
             e.get_text() == "DejaVu Sans Mono 8" or
             e.get_text() == "DejaVu LGC Sans Mono 8" or
             e.get_text() == "MiscFixed 10" or
             e.get_text() == "Courier 10"][0]

    # Grab the focus on the entry. This is important, since we want a
    # "focus_out" signal to be emitted upon exit.
    entry.grab_focus()

    # Press ESC and check that GPS does not crash
    send_key_event(GDK_ESCAPE)
    yield wait_idle()
