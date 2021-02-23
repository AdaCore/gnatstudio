"""
Check renaming in files including closed files using diff on files after renaming
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.EditorBuffer.get(GPS.File("a.ads"))
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(4, 14))
    yield wait_idle()

    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Object_Type_Aa")
    dialog = get_window_by_title("Renaming entity")
    yield idle_modal_dialog(
        lambda: get_stock_button(dialog, Gtk.STOCK_OK).clicked())

    yield hook("language_server_response_processed")
    # yield wait_language_server("textDocument/rename", "ada")
