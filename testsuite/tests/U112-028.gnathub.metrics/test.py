"""
This test checks that metrics are correctly displayed in
the Analysis Report, and that they can be filtered
when needed.
"""

from GPS import *
from gs_utils.internal.utils import *


EXPECTED_ALL_LINES = ["39", [["39", ["18", "35"]]]]


@run_test_driver
def run_test():
    report = AnalysisReport()
    yield report.open_and_yield()

    # Dump the 'all_lines' metrics column and check
    # that it's corect.
    dump = report.dump_messages_report(17)
    gps_assert(dump, EXPECTED_ALL_LINES, "metrics are not correctly displayed")

    # Filter out the 'all_lines' row in the Metrics filter panel
    # and check that the corresponding column has been hidden
    column = [
        x for x in report.messages_report.get_columns() if x.get_title() == "all_lines"
    ][0]
    gps_assert(column.get_visible(), True, "The column should be visible")
    yield report.yield_toggle_filter("all_lines", AnalysisReport.FilterKind.METRIC)
    gps_assert(column.get_visible(), False, "The column should be hidden")
