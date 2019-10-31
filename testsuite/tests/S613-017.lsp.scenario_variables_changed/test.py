"""
Check that GPS send didConfigurationChanged notification of change of the
scenario variables.
"""
import GPS
from gs_utils.internal.utils import *

@run_test_driver
def test_driver():
    f = GPS.File('to_be_called.adb')
    sf = GPS.File('second.adb')
    tf = GPS.File('third.adb')
    b = GPS.EditorBuffer.get(f)

    b.find_all_refs(b.at(1, 17), True)
    yield hook("language_server_response_processed")
    yield wait_tasks()

    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", sf.path),
               [GPS.FileLocation(sf, 1, 6), 'with To_Be_Called;',
                GPS.FileLocation(sf, 4, 4), '[call] To_Be_Called;'],
               "wrong list of locations (1/1)")
    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", tf.path),
               [],
               "wrong list of locations (1/2)")
    GPS.Project.set_scenario_variable('VALUE', 'third')
    GPS.Project.recompute()

    GPS.EditorBuffer.get(sf).close()
    yield wait_tasks()

    b.find_all_refs(b.at(1, 17), True)
    yield hook("language_server_response_processed")
    yield wait_tasks()

    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", tf.path),
               [GPS.FileLocation(tf, 1, 6), 'with To_Be_Called;',
                GPS.FileLocation(tf, 4, 4), '[call] To_Be_Called;'],
               "wrong list of locations (2/1)")
    gps_assert(GPS.Locations.list_locations("References for To_Be_Called (to_be_called.adb:1)", sf.path),
               [],
               "wrong list of locations (2/2)")
