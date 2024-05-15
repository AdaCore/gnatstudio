"""
Basic integration test for the GPR language server.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    # Test the communication with the Outline
    buf = GPS.EditorBuffer.get(GPS.File("default.gpr"))
    yield wait_outline("default.gpr")

    outline = get_widget_by_name("Outline View Tree")

    gps_assert(
        dump_tree_model(outline.get_model(), 1),
        ["Default", ["Builder"]],
        "Wrong outline view for default.gpr",
    )

    # Test basic navigation
    buf.current_view().goto(buf.at(1, 9))
    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration", "Project File")
    gps_assert(
        GPS.EditorBuffer.get().file(),
        GPS.File("config.gpr"),
        "'goto declaration' failed: we should have opened config.gpr",
    )
