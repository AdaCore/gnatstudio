"""
This test checks that the LSP-based 'goto declaration' and 'goto body' actions
work correctly when the declaration is not located in the same file as the body.
"""
import GPS
from gs_utils.internal.utils import \
    gps_assert, run_test_driver, timeout, wait_tasks, wait_language_server
from workflows.promises import known_tasks


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File('main.adb'))
    buf.current_view().goto(buf.at(5, 10))

    # wait LSP responses has been processed to have folding information
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    GPS.execute_action('goto declaration')
    yield wait_language_server('textDocument/declaration')

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('hello_world.ads'),
               "'goto declaration' did not open the right file")

    current_loc = current_buf.main_cursor().location()
    gps_assert(current_loc, current_buf.at(3,33),
               "'goto declaration' did not jump to right location")

    # wait LSP responses has been processed to have folding information
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    GPS.execute_action('goto body')
    yield wait_language_server('textDocument/implementation')

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('hello_world.adb'),
               "'goto body' did not open the right file")

    current_loc = current_buf.main_cursor().location()
    gps_assert(current_loc, current_buf.at(5,33),
               "'goto body' did not jump to right location")
