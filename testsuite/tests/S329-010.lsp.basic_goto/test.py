"""
This test checks that the LSP-based 'goto declaration' and 'goto body' actions
work correctly when the declaration is not located in the same file as the body.
"""
import GPS
from gps_utils.internal.utils import \
    run_test_driver, timeout, gps_assert


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File('main.adb'))
    buf.current_view().goto(buf.at(5, 10))

    GPS.execute_action('goto declaration')
    yield timeout(1000)

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('hello_world.ads'),
               "'goto declaration' did not open the right file")

    current_loc = current_buf.main_cursor().location()
    gps_assert(current_loc, current_buf.at(3,33),
               "'goto declaration' did not jump to right location")

    GPS.execute_action('goto body')
    yield timeout(1000)

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('hello_world.adb'),
               "'goto body' did not open the right file")

    current_loc = current_buf.main_cursor().location()
    gps_assert(current_loc, current_buf.at(5,33),
               "'goto body' did not jump to right location")
