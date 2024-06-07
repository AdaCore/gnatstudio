"""
Verify that goto type of entity works fine with the LSP.
"""

from GPS import *
from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def run_test():
    file = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(file)
    buf.current_view().goto(buf.at(6, 14))

    # wait LSP responses has been processed to have folding information
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    GPS.execute_action("goto type of entity")
    yield hook("language_server_response_processed")

    current_buffer = GPS.EditorBuffer.get()
    current_loc = current_buffer.current_view().cursor()

    gps_assert(
        current_buffer.file(),
        GPS.File("foo.ads"),
        "goto type of entity  did not open the right file",
    )
    gps_assert(current_loc.line(), 3, "Wrong line after Go To Declaration")
    gps_assert(current_loc.column(), 16, "Wrong column after Go To Declaration")
