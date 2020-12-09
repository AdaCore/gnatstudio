"""
Outline behavior when clicking in a line with multiple entities defined.
"""

from GPS import *
from gs_utils.internal.utils import *

NAME_COLUMN = 1


@run_test_driver
def run_test():
    # Show the highlevel objects: we are looking at multiple variables
    # defined together
    GPS.Preference("outline-show-objects").set(True)

    # Retrieve the Outline Tree and fill it
    GPS.execute_action("open Outline")
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    outline_view = GPS.MDI.get("Outline")
    tree = get_widgets_by_type(Gtk.TreeView, outline_view.pywidget())[0]
    selection = tree.get_selection()

    # Click on the variables and look at the selected element in the Outline
    def verify_loc(line, column, name):
        buf.current_view().goto(buf.at(line, column))
        # This wait for the debounce on_location_changed
        yield hook("location_changed", debounced=True)
        model, iter = selection.get_selected()
        gps_assert(model.get_value(iter, NAME_COLUMN),
                   name,
                   "Wrong entity selected: " + name)
    yield verify_loc(2, 5,  "VarA")
    yield verify_loc(2, 11, "VarB")
    yield verify_loc(2, 18, "VarC")
    yield verify_loc(3, 1,  "VarD")
    yield verify_loc(4, 8,  "VarE")
    yield verify_loc(4, 21, "VarF")
    yield verify_loc(5, 8,  "VarE")
