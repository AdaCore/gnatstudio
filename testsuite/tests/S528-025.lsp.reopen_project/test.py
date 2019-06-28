"""
Check that GPS can execute 'references' request after reopen of the
currently opened project.
"""
import GPS
from gps_utils.internal.utils import *

@run_test_driver
def test_driver():
    f = GPS.File('to_be_called.adb')
    sf = GPS.File('second.adb')
    b = GPS.EditorBuffer.get(f)

    b.find_all_refs(b.at(1, 17), True)
    yield hook("language_server_response_processed")
    yield wait_tasks()

    gps_assert(
        GPS.Locations.list_locations(
            "References for To_Be_Called (to_be_called.adb:1)", sf.path),
        [GPS.FileLocation(sf, 1, 6), 'To_Be_Called',
         GPS.FileLocation(sf, 4, 4), 'To_Be_Called'],
        "wrong list of locations (1)")
    GPS.Locations.remove_category(
        "References for To_Be_Called (to_be_called.adb:1)")
    yield wait_tasks()

    #  Reopen project

    GPS.execute_action('/File/Open Recent Projects/test.gpr')
    yield wait_tasks()

    b = GPS.EditorBuffer.get(f)

    b.find_all_refs(b.at(1, 17), True)
    yield hook("language_server_response_processed")
    yield wait_tasks()

    gps_assert(
        GPS.Locations.list_locations(
            "References for To_Be_Called (to_be_called.adb:1)", sf.path),
        [GPS.FileLocation(sf, 1, 6), 'To_Be_Called',
         GPS.FileLocation(sf, 4, 4), 'To_Be_Called'],
        "wrong list of locations (2)")

    l = GPS.EditorLocation(b, 1, 16)
    b.insert (l, " ")
    yield timeout(1000)

    gps_assert(GPS.Locations.list_locations("Diagnostics", f.path),
               [GPS.FileLocation(f, 1, 17), 'Invalid token, ignored',
                GPS.FileLocation(f, 1, 18), "Missing ';'"],
               "Unexpected diagnostics")
