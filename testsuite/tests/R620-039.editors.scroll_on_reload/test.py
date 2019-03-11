import GPS
from gps_utils.internal.utils import *
import shutil
import os


@run_test_driver
def test_driver():
    GPS.Preference("Auto-Reload-Files").set(True)

    # Open the editor
    f = GPS.File("gps-main.adb")
    b = GPS.EditorBuffer.get(f)
    v = b.current_view()

    # Place the cursor somewhere and scroll to it
    v.goto(b.at(300, 1))
    v.center()

    yield wait_idle()

    # Get the scrollbar's ajdustment value
    win = pygps.get_widgets_by_type(Gtk.ScrolledWindow, v.pywidget())[0]
    vadj = win.get_vadjustment()
    value_at_beginning = vadj.get_value()

    # Delete some text and save the buffer
    b.delete(b.at(300, 1), b.at(750, 1))
    b.save()

    yield wait_idle()

    # Reset the buffer to its original state outside of GPS
    current_dir = os.getcwd()
    shutil.copyfile(os.path.join(current_dir,"gps-main.adb.saved"),
                    os.path.join(current_dir, "gps-main.adb"))

    # Move the focus away of GPS and set it back to the editor after
    # so that GPS can detect the changes on disk and reload the file
    new_w = Gtk.Window()
    new_w.present()
    new_w.close()
    GPS.MDI.get("gps-main.adb").raise_window()

    yield wait_idle()

    gps_assert(vadj.get_value(), value_at_beginning,
               "The editor should be scrolled back to cursor's location " +
               "before reloading the file")
