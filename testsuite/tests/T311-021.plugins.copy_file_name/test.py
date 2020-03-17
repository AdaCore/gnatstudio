"""
This test checks the good behavior of the copy_file_name.py plugin.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Open main.adb and check that 'copy_base_file_name' works
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("copy_base_file_name")
    clipboard_contents = GPS.Clipboard().contents()
    gps_assert(clipboard_contents[GPS.Clipboard().current()],
               "main.adb",
               "copy_base_file_name not working on focused editors")

    # Close the editor and select main.adb in the Project view
    # and check that 'copy_file_name' works
    explorer = get_widget_by_name("Project Explorer Tree")
    windows = Gtk.Window.list_toplevels()

    explorer.grab_focus()
    select_in_tree(explorer, column=1, key="main.adb")
    GPS.execute_action("copy_file_name")
    clipboard_contents = GPS.Clipboard().contents()
    gps_assert(clipboard_contents[GPS.Clipboard().current()],
               GPS.File(os.path.join(GPS.pwd(), "main.adb")).path,
               "copy_file_name not working on files in the Project view")

    # Check that 'copy_file_name' works on directories from the
    # Project view
    select_in_tree(explorer, column=1, key=".")
    GPS.execute_action("copy_file_name")
    clipboard_contents = GPS.Clipboard().contents()
    gps_assert(clipboard_contents[GPS.Clipboard().current()],
               GPS.File(os.path.join(GPS.pwd())).path,
               "copy_file_name not working on dirs in the Project view")
