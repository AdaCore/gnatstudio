"""
Check that renaming works correctly in folded blocks, even
when there are special lines too.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf_1 = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    # Add a special line
    buf_1.add_special_line(3, "Special line")

    # Fold the main procedure in main.adb
    buf_1.at(3, 2).block_fold()

    # Rename the Print method
    buf_2 = GPS.EditorBuffer.get(GPS.File("a.ads"))
    buf_2.current_view().goto(buf_2.at(7, 16))
    yield wait_idle()

    yield idle_modal_dialog(lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Pri")
    dialog = get_window_by_title("Renaming entity")
    yield idle_modal_dialog(lambda: get_stock_button(dialog, STOCK_OK).clicked())
    yield wait_language_server("textDocument/rename")

    gps_assert(
        buf_1.get_chars(buf_1.at(6, 1), buf_1.at(6, 1).end_of_line()).strip(),
        "Pri (ObjA);",
        "Renaming dit not work correctly in folded block",
    )
