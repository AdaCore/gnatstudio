"""
Test loading multiple sarif files:
- One after the other => they should clear the previous report
- Together => they should merge the results
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


GNATPROVE_EXPECTED = [
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

GNATSAS_EXPECTED = [
    "Infer (1 item in 1 file)",
    [
        "foo.adb (1 item)",
        [
            "<b>23:4</b>      Memory dynamically allocated by `new` on line 21 is not freed after the last access at line 23, column 4",
            [
                "          allocation part of the trace starts here",
                "             allocated by `new` here",
                "          memory becomes unreachable here",
            ],
        ],
    ],
]


def load_sarif(action, filename, expected, msg):
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield(action)
    load.select_file(filename)
    yield load.ok()
    yield wait_idle()

    report = dialogs.AnalysisReport()
    yield report.open_and_yield()
    gps_assert(dump_locations_tree(), expected, "wrong messages for %s" % msg)


@run_test_driver
def run_test():
    yield load_sarif(
        "Load Sarif File", "gnatprove.sarif", GNATPROVE_EXPECTED, "first load"
    )
    yield load_sarif(
        "Load Sarif File", "gnatsas.sarif", GNATSAS_EXPECTED, "second load"
    )
    yield load_sarif(
        "Add Sarif File",
        "gnatprove.sarif",
        GNATSAS_EXPECTED + GNATPROVE_EXPECTED,
        "third load",
    )
