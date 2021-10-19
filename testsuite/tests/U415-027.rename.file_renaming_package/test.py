"""
Check that we correctly rename the specification and body files
when renaming a package via the LSP.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED_AFTER_RENAME =  ['Test',
                          ['.',
                           ['barz.ads',
                            'barz.adb'],
                           '.']]

EXPECTED_AFTER_UNDO =  ['Test',
                        ['.',
                         ['bar.ads',
                          'bar.adb'],
                         '.']]


@run_test_driver
def run_test():
    b1 = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    b1.current_view().goto(b1.at(1, 15))

    # Rename Bar to Barz
    yield idle_modal_dialog(
        lambda: GPS.execute_action("rename entity"))
    new_name_ent = get_widget_by_name("new_name")
    new_name_ent.set_text("Barz")
    dialog = get_window_by_title("Renaming entity")
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('project_view_changed')
    yield timeout(500)

    # Check that the files have been renamed accordingly
    prj_view = Project_View()
    yield prj_view.open_and_yield()
    prj_view.compare_contents(
        EXPECTED_AFTER_RENAME,
        msg='The files should have been renamed too')

    # Verify that navigation works corretly after renaming
    GPS.EditorBuffer.get().close()
    b2 = GPS.EditorBuffer.get(GPS.File("barz.adb"))
    b2.current_view().goto(b2.at(7, 17))
    GPS.execute_action("goto declaration")
    yield wait_language_server('textDocument/declaration')

    current_buffer = GPS.EditorBuffer.get()
    current_loc = current_buffer.current_view().cursor()

    gps_assert(
        current_buffer.file(), GPS.File("barz.ads"),
        "goto declaration did not open the right file")
    gps_assert(
        current_loc.line(), 3, "Wrong line after Go To Declaration")
    gps_assert(
        current_loc.column(), 19, "Wrong column after Go To Declaration")


    # Check that 'undo' reverts to the old file names too
    GPS.execute_action("undo")
    yield timeout(500)
    prj_view.compare_contents(
        EXPECTED_AFTER_UNDO,
        msg='We should revert to the original file names when undoing')
