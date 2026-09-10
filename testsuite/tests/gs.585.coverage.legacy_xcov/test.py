"""
An .xcov artifact from before GNATcoverage reported its own figures must
still be displayed exactly as it was: our own line-based computation, plainly
labelled as such.
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


@run_test_driver
def run_test():
    directory = GPS.Project.root().file().directory()
    analysis = GPS.CodeAnalysis.get("Coverage")

    # The artifact must not look older than the source it describes
    os.utime(os.path.join(directory, "pkg.adb.xcov"), None)
    analysis.add_gcov_file_info(src=GPS.File("pkg.adb"), cov=GPS.File("pkg.adb.xcov"))
    analysis.show_analysis_report()
    yield wait_tasks()

    tree = get_widget_by_name("Coverage")
    gps_assert(
        dump_tree_model(tree.get_model(), COV_COL),
        [
            "2 lines (1 not covered)",
            [
                "2 lines (1 not covered)",
                ["1 line (0 not covered)", "1 line (1 not covered)"],
            ],
        ],
        "Wrong coverage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_TXT),
        ["   50 %", ["   50 %", ["  100 %", "    0 %"]]],
        "Wrong percentage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_LABEL),
        ["lines", ["lines", ["lines", "lines"]]],
        "Wrong criterion labels",
    )
