"""
This test checks that renaming an entity across multiple fine
works fine through LSP and clangd.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("clangd-BasedOnStyle").set("LLVM")
    yield wait_tasks()
    b1 = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    buf = GPS.EditorBuffer.get(GPS.File("my_class.hh"))
    yield wait_tasks()
    # timeout to let clangd indexing the files
    yield timeout(200)
    view = buf.current_view()
    view.goto(buf.at(1, 7))
    yield wait_idle()

    # Use an alphabetical sort to have deterministic results (clangd seems to
    # process files via threads, which can lead in different orders)
    GPS.Locations.set_sort_order_hint(
        category="Refactoring - rename My_Class to Dummy",
        hint="Alphabetical")

    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Dummy")
    dialog = get_window_by_title("Renaming entity")
    check = get_button_from_label("Automatically save modified files", dialog)
    check.set_active(False)
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('language_server_response_processed')
    yield wait_idle()

    # Wait for the sort of the Locations view
    yield timeout(500)

    gps_assert(dump_locations_tree(),
               ['Refactoring - rename My_Class to Dummy (3 items in 2 files)',
                ['main.cpp (1 item)',
                 ['<b>5:3</b>       <b>My_Class</b> renamed to <b>Dummy</b>'],
                    'my_class.hh (2 items)',
                    ['<b>1:7</b>       <b>My_Class</b> renamed to <b>Dummy</b>',
                     '<b>9:3</b>       <b>My_Class</b> renamed to <b>Dummy</b>']]],
               "wrong location tree")
