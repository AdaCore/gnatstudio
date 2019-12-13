"""
This test checks the good behavior of the 'rename entity'
action when the 'Make files writable' option is disabled.
"""
from gs_utils.internal.utils import *
from GPS import *
from pygps import *


@run_test_driver
def on_gps_started():
    # DIsable Jump to first location because it will open bar.adb
    GPS.Preference("locations-auto-jump-to-first").set(False)
    buffer = GPS.EditorBuffer.get(File("foo.adb"))
    view = buffer.current_view()
    view.goto(buffer.at(1, 7))

    @gs_utils.hook("file_edited")
    def on_file_edited(file):
        simple_error("No new editor should have been opened")

    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("FooBar")
    dialog = get_window_by_title("Renaming entity")

    get_button_from_label(
        "Automatically save modified files", dialog).set_active(True)

    yield idle_modal_dialog(
        lambda: get_stock_button(dialog, Gtk.STOCK_OK).clicked())
