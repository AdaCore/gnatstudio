"""
This test checks that GPS.Action.destroy_ui() properly removes the
contextual menus associated with the action.
"""

import GS
from gi.repository import Gtk
from gs_utils.internal.utils import (
    run_test_driver,
    click_in_text,
    dump_contextual,
    gps_assert,
    close_contextual,
)


@run_test_driver
def run_test():
    # Create an action with two contextual menus, one at toplevel and one nested
    a = GS.Action("Test")
    a.contextual("hi")
    a.contextual("/nested/bye")
    a.create(lambda: GS.MDI.dialog("foo"))

    # Check that the contextual menus are present
    b = GS.EditorBuffer.get(GS.File("test.py"))
    b.current_view().goto(b.at(1, 1))
    w = Gtk.Window.list_toplevels()
    click_in_text(GS.EditorBuffer.get().current_view().cursor(), button=3)
    d = dump_contextual(w)
    close_contextual(w)

    # Check that we've created both contextual menus
    gps_assert("hi" in d, True, "Contextual menu 'hi' not found")
    gps_assert(["bye"] in d, True, "Contextual menu 'nested/bye' not found")

    # Now destroy the UI
    a.destroy_ui()

    # Check that the contextual menus are gone
    w = Gtk.Window.list_toplevels()
    click_in_text(GS.EditorBuffer.get().current_view().cursor(), button=3)
    d = dump_contextual(w)
    close_contextual(w)

    gps_assert("hi" in d, False, "Contextual menu 'hi' remains")
    gps_assert(["bye"] in d, False, "Contextual menu 'nested/bye' remains")
