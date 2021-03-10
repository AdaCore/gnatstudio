"""
Verify that the SHA1 is correctly displaid in the Windows View.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def on_gps_started():
    GPS.execute_action("open Windows")
    view = GPS.MDI.get("Windows").pywidget()
    gps_assert(view is not None, True, "can't open the windows view")
    yield wait_tasks(other_than=known_tasks)
    tree = get_widgets_by_type(Gtk.TreeView, view)[0]
    model = tree.get_model()

    # Open a file and check the SHA1
    f = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(f)
    sha1 = model.get_value(model.get_iter(Gtk.TreePath("0")), 4)
    gps_assert(sha1,
               "8890418ccad7e96e7917abc5ca6c6dc9a33d99c5",
               "Wrong sha1 after opening the file")
    # Modify it => the value should only change after saving
    buf.insert(buf.at(1, 1), "a")
    gps_assert(sha1,
               model.get_value(model.get_iter(Gtk.TreePath("0")), 4),
               "The sha1 should have been modified after the save")
    buf.save()
    gps_assert(sha1 != model.get_value(model.get_iter(Gtk.TreePath("0")), 4),
               True,
               "The sha1 should have been modified after the save")
    # Set the previous content
    buf.undo()
    buf.save()
    gps_assert(sha1,
               model.get_value(model.get_iter(Gtk.TreePath("0")), 4),
               "Revert the state and save: should be the starting value")
