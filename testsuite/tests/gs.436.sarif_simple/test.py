"""
This test loads a simple sarif file in the Analysis Report via the
action "Load Sarif File".
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load Sarif File")
    load.select_file("gnatprove.sarif")
    yield load.ok()
    yield wait_idle()

    report = dialogs.AnalysisReport()
    yield report.open_and_yield()

    expected = [
        "GNATProve (7 items in 1 file)",
        [
            "foo.adb (7 items)",
            [
                "<b>8:19</b>      &quot;X&quot; is not modified, parameter type could be rewritten as &apos;access constant Integer&apos;",
                "<b>8:19</b>      unused variable &quot;X&quot;",
                "<b>16:7</b>      statement has no effect",
                "<b>17:21</b>     this statement is never reached",
                "<b>17:21</b>     unreachable code (CVC5: 1 VC)",
                "<b>21:4</b>      resource or memory leak might occur at end of scope",
                "<b>23:9</b>      null exclusion check proved (Z3: 1 VC)",
            ],
        ],
    ]

    gps_assert(dump_locations_tree(), expected, "wrong messages")

    gps_assert(
        report.dump_filters(dialogs.AnalysisReport.FilterKind.TOOL),
        [["GNATProve", "7", True]],
        "Wrong list of Tools",
    )
