"""
This test checks GPS.MDI.file_selector with except_filter=*.*
"""

from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield idle_modal_dialog(lambda:
                            GPS.MDI.file_selector('.', except_filter='*.*'))
    dialog = get_window_by_title("Select a file", Gtk.Window.list_toplevels())
    tree = get_widget_by_name("file_selector_window.file_tree")
    tm = dump_tree_model(tree.get_model(), 0)
    gps_assert(tm, ['noext'])
    button = get_widget_by_name("file_selector_window.ok_button")
    button.clicked()

    yield idle_modal_dialog(lambda:
                            GPS.MDI.file_selector('*.xml', except_filter=''))
    dialog = get_window_by_title("Select a file", Gtk.Window.list_toplevels())
    get_button_from_label("gtk-cancel", dialog).clicked()
