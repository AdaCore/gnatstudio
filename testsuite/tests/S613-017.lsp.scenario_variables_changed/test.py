"""
Check that GPS send didConfigurationChanged notification of change of the
scenario variables.
"""
import GPS
from gps_utils.internal.utils import *

@run_test_driver
def test_driver():
    f = GPS.File('to_be_called.adb')
    sf = GPS.File('second.adb')
    tf = GPS.File('third.adb')
    b = GPS.EditorBuffer.get(f)

    yield ('language_server_started')

    b.find_all_refs(b.at(1, 17), True)

    yield timeout(1000)

    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", sf.path),
               [GPS.FileLocation(sf, 1, 6), 'To_Be_Called',
                GPS.FileLocation(sf, 4, 4), 'To_Be_Called'],
               "wrong list of locations (1/1)")
    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", tf.path),
               [],
               "wrong list of locations (1/2)")
    GPS.Project.set_scenario_variable('VALUE', 'third')
    GPS.Project.recompute()

    yield timeout(1000)

    b.find_all_refs(b.at(1, 17), True)

    yield timeout(1000)

    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", sf.path),
               [],
               "wrong list of locations (2/1)")
    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", tf.path),
               [GPS.FileLocation(tf, 1, 6), 'To_Be_Called',
                GPS.FileLocation(tf, 4, 4), 'To_Be_Called'],
               "wrong list of locations (2/2)")
