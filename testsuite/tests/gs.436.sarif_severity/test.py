"""
This test loads a simple SARIF file in the Analysis Report via the
action "Load Sarif File".

It ensure that the severity level is of loaded messages is correctly handled
and that overrides are correctly resolved.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def run_test():
    # Clean the locations view before loading the SARIF file for more stability
    GPS.execute_action("locations clear")

    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load Sarif File")
    load.select_file("report.sarif")
    yield load.ok()
    yield wait_idle()

    report = dialogs.AnalysisReport()
    yield report.open_and_yield()

    expected = [
        "Example tool (6 items in 1 file)",
        [
            "foo.adb (6 items)",
            [
                "<b>3:4</b>       Message with default severity",
                "<b>3:4</b>       Message with severity in instance",
                "<b>3:4</b>       Message with severity in instance and referencing the rule by index",
                "<b>3:4</b>       Message with severity in rule descriptor",
                "<b>3:4</b>       Message with severity in rule descriptor and in the instance",
                "<b>3:4</b>       Message with severity in configuration override",
            ],
        ],
    ]

    gps_assert(
        report.dump_filters(dialogs.AnalysisReport.FilterKind.SEVERITY),
        [["High", "1", True], ["Medium", "1", True], ["Informational", "4", True]],
        "Wrong list of severities",
    )

    gps_assert(dump_locations_tree(), expected, "wrong messages")
