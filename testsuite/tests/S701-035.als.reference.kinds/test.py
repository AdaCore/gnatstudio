from gps_utils.internal.utils import *

@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("m.adb"))
    b.select(GPS.EditorLocation(b, 5, 6), GPS.EditorLocation(b, 5, 7))

    yield idle_modal_dialog(lambda: GPS.execute_action(
        "find references..."))
    dialog = get_window_by_title("Find References Options")
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('language_server_response_processed')

    gps_assert(GPS.Locations.list_locations(
                   "Entities imported into m.adb", "m.adb"),
               [GPS.FileLocation(GPS.File('m.adb'), 5, 6), '[call] P.S;'],
               "There is no 'call' reference for P.S in m.adb")
    gps_assert(GPS.Locations.list_locations(
                   "Entities imported into m.adb", "p.adb"),
               [GPS.FileLocation(GPS.File('p.adb'), 4, 14), 'procedure S is',
                GPS.FileLocation(GPS.File('p.adb'), 7, 8), 'end S;'],
                "Wrong references to P.S in p.adb")

    yield idle_modal_dialog(lambda: GPS.execute_action(
        "find references..."))
    dialog = get_window_by_title("Find References Options")
    get_button_from_label('call', dialog).set_active(False)
    get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    yield hook('language_server_response_processed')

    gps_assert(GPS.Locations.list_locations(
                   "Entities imported into m.adb", "m.adb"),
               [],
               "There are reference for P.S in m.adb")
    gps_assert(GPS.Locations.list_locations(
                   "Entities imported into m.adb", "p.adb"),
               [GPS.FileLocation(GPS.File('p.adb'), 4, 14), 'procedure S is',
                GPS.FileLocation(GPS.File('p.adb'), 7, 8), 'end S;'],
                "Wrong references to P.S in p.adb")
