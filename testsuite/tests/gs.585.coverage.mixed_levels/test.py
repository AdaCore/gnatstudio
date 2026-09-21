"""
Each file's percentage follows the level its own artifact announces, so one
report can mix criteria. A project whose files disagree on the level has no
level of its own to inherit: it must then show the strictest criterion every
one of its files reports, so that its total never silently omits a file.

Here main.adb was analyzed at stmt and pkg.adb at stmt+mcdc, and statement
coverage is the only criterion both of them report.
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

    tree = get_widget_by_name("Coverage")
    gps_assert(
        dump_tree_model(tree.get_model(), COV_COL),
        [
            "5 statement obligations (1 not covered)",
            [
                "3 statement obligations (0 not covered)",
                ["3 lines (0 not covered)"],
                "4 MC/DC obligations (3 not covered)",
                ["1 line (1 not covered)", "1 line (1 not covered)"],
            ],
        ],
        "Wrong coverage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_TXT),
        ["   80 %", ["  100 %", ["  100 %"], "   25 %", ["    0 %", "    0 %"]]],
        "Wrong percentage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_LABEL),
        ["statement", ["statement", ["lines"], "MC/DC", ["lines", "lines"]]],
        "Wrong criterion labels",
    )
