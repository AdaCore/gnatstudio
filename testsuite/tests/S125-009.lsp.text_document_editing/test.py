"""
Check basic text document operations and editing.
"""
import GPS
from gps_utils.internal.utils import *

diagnosticTimeout = 1000
diagnosticOnDidOpen = False

@run_test_driver
def test_driver():
    adb_file = GPS.File("main.adb")
    ads_file = GPS.File("main.ads")

    # Open ADB file, do modifications, check for diagnostics.

    adb_buffer = GPS.EditorBuffer.get(adb_file)

    location = GPS.EditorLocation(adb_buffer, 3, 16)
    adb_buffer.insert (location, " ")
    yield timeout(diagnosticTimeout)
    gps_assert(GPS.Locations.list_locations("Diagnostics", adb_file.path),
               [GPS.FileLocation(adb_file, 3, 17), "Missing ';'"],
               "Unexpected diagnostics (assertion 1)")

    # Save and close ADB file.

    adb_buffer.save()
    adb_buffer.close()

    # Reopen ADB file and check for diagnostics

    adb_buffer = GPS.EditorBuffer.get(adb_file)

    yield timeout(diagnosticTimeout)
    if diagnosticOnDidOpen:
        gps_assert(GPS.Locations.list_locations("Diagnostics", adb_file.path),
                   [GPS.FileLocation(adb_file, 3, 17), "Missing ';'"],
                   "Unexpected diagnostics (assertion 2)")

    # Undo changes, check for diagnostics

    location = GPS.EditorLocation(adb_buffer, 3, 16)
    adb_buffer.delete (location, location)
    yield timeout(diagnosticTimeout)
    gps_assert(GPS.Locations.list_locations("Diagnostics", adb_file.path),
               [],
               "Unexpected diagnostics (assertion 3)")

    # Create new file, fill it, save as ADS and check for diagnostics

    ads_buffer = GPS.EditorBuffer.get_new()
    location = ads_buffer.at(1, 1)
    ads_buffer.insert (location, "procedure Main")
    ads_buffer.save(False, ads_file)
    yield timeout(diagnosticTimeout)
    if diagnosticOnDidOpen:
        gps_assert(GPS.Locations.list_locations("Diagnostics", ads_file.path),
                   [GPS.FileLocation(adb_file, 1, 16), "Missing ';'"],
                   "Unexpected diagnostics (assertion 4)")

    # Fix error in ADS file and check for diagnostics

    location = ads_buffer.at(1, 16)
    ads_buffer.insert (location, ";")
    yield timeout(diagnosticOnDidOpen)
    gps_assert(GPS.Locations.list_locations("Diagnostics", adb_file.path),
               [],
               "Unexpected diagnostics (assertion 5)")
