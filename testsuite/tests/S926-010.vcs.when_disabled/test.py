"""
This test verifies that disabling VCS2 completely does not result
in exceptions in the Project and Files views. Same thing when
opening and writing in editors.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Open the Project view
    GPS.execute_action("open Project")
    yield wait_for_mdi_child("Project")

    # Open the Files view
    GPS.execute_action("open Files")
    yield wait_for_mdi_child("Files")

    # Open an editor, modify the file, save it, undo, and save again
    # to restore it to its original state
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    buf.insert(buf.at(1, 1), "blabla")
    buf.save()
    buf.undo()
    buf.save()
