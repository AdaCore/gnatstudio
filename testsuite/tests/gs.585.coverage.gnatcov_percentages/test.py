"""
The Coverage Report must show the percentages GNATcoverage itself reports,
for the criterion it was asked to measure, instead of always recomputing a
line-based figure of its own.

pkg.adb is the telling row: GNAT Studio's own line-based figure is 0 %, while
GNATcoverage reports 25 % MC/DC, which is what --level=stmt+mcdc asked for.
"""

import os
import GPS
from gs_utils.internal.utils import (
    dump_tree_model,
    get_widget_by_name,
    gps_assert,
    run_test_driver,
    wait_tasks,
)

# Model columns of the coverage report, see code_analysis_gui.ads
COV_COL = 5
COV_BAR_TXT = 7
COV_BAR_LABEL = 9

SOURCES = ("main.adb", "pkg.adb")

# main.adb reports statement coverage only, so the MC/DC level it announces
# cannot drive its percentage and the strictest criterion it does report is
# used instead. pkg.adb reports all three, so MC/DC drives it. The project
# row sums per criterion, and only statement is reported by both files.
EXPECTED_COV = [
    "5 statement obligations (1 not covered)",
    [
        "3 statement obligations (0 not covered)",
        ["3 lines (0 not covered)"],
        "4 MC/DC obligations (3 not covered)",
        ["1 line (1 not covered)", "1 line (1 not covered)"],
    ],
]

EXPECTED_PCT = [
    "   80 %",
    ["  100 %", ["  100 %"], "   25 %", ["    0 %", "    0 %"]],
]

# Every percentage announces the criterion it was computed from: subprogram
# rows stay line-based, so a 25 % file above a 0 % subprogram is plainly not
# an arithmetic blunder.
EXPECTED_LABEL = [
    "statement",
    ["statement", ["lines"], "MC/DC", ["lines", "lines"]],
]


def dump(tree):
    return (
        dump_tree_model(tree.get_model(), COV_COL),
        dump_tree_model(tree.get_model(), COV_BAR_TXT),
        dump_tree_model(tree.get_model(), COV_BAR_LABEL),
    )


@run_test_driver
def run_test():
    directory = GPS.Project.root().file().directory()
    analysis = GPS.CodeAnalysis.get("Coverage")

    for source in SOURCES:
        # The artifact must not look older than the source it describes
        os.utime(os.path.join(directory, source + ".xcov"), None)
        analysis.add_gcov_file_info(
            src=GPS.File(source), cov=GPS.File(source + ".xcov")
        )

    analysis.show_analysis_report()
    yield wait_tasks()

    cov, pct, label = dump(get_widget_by_name("Coverage"))
    gps_assert(cov, EXPECTED_COV, "Wrong coverage column")
    gps_assert(pct, EXPECTED_PCT, "Wrong percentage column")
    gps_assert(label, EXPECTED_LABEL, "Wrong criterion labels")

    # The figures must survive a trip through a saved report

    saved = GPS.File(os.path.join(directory, "saved.xml"))
    analysis.dump_to_file(xml=saved)
    GPS.CodeAnalysis.get("Coverage").clear()

    reloaded = GPS.CodeAnalysis.get("Reloaded")
    reloaded.load_from_file(xml=saved)
    reloaded.show_analysis_report()
    yield wait_tasks()

    cov, pct, label = dump(get_widget_by_name("Reloaded"))
    gps_assert(cov, EXPECTED_COV, "Wrong coverage column after reload")
    gps_assert(pct, EXPECTED_PCT, "Wrong percentage column after reload")
    gps_assert(label, EXPECTED_LABEL, "Wrong criterion labels after reload")
