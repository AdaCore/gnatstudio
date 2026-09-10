"""
A criterion may only drive the project's percentage when every one of its
files reported that criterion, so a project total never silently leaves a
file out.

Here pkg.adb reports statement, decision and MC/DC while main.adb's artifact
predates GNATcoverage reporting any figure of its own. No criterion covers
both files, so the project row falls back on the line-based figure -- which
they do both have -- and the tooltip names the file missing from each of the
others, both as displayed and after a save/reload round trip.
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
COV_TOOLTIP = 10

SOURCES = ("main.adb", "pkg.adb")

# Only pkg.adb reports a criterion of its own, so every one of the project's
# figures leaves main.adb out and must say so by name.
EXPECTED_PARTIALS = (
    (
        "statement",
        "statement: 1 of 2 obligations covered (50 %), from 1 of 2 files"
        " (not reported by main.adb)",
    ),
    (
        "decision",
        "decision: 1 of 2 obligations covered (50 %), from 1 of 2 files"
        " (not reported by main.adb)",
    ),
    (
        "MC/DC",
        "MC/DC: 1 of 4 obligations covered (25 %), from 1 of 2 files"
        " (not reported by main.adb)",
    ),
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

    tree = get_widget_by_name("Coverage")
    gps_assert(
        dump_tree_model(tree.get_model(), COV_COL),
        [
            "5 lines (2 not covered)",
            [
                "3 lines (0 not covered)",
                ["3 lines (0 not covered)"],
                "4 MC/DC obligations (3 not covered)",
                ["1 line (1 not covered)", "1 line (1 not covered)"],
            ],
        ],
        "Wrong coverage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_TXT),
        ["   60 %", ["  100 %", ["  100 %"], "   25 %", ["    0 %", "    0 %"]]],
        "Wrong percentage column",
    )
    gps_assert(
        dump_tree_model(tree.get_model(), COV_BAR_LABEL),
        ["lines", ["lines", ["lines"], "MC/DC", ["lines", "lines"]]],
        "Wrong criterion labels",
    )

    # The project's tooltip must own up to the file each criterion omits, and
    # name it: a count alone does not say which file is missing
    project_tooltip = dump_tree_model(tree.get_model(), COV_TOOLTIP)[0]
    for criterion, expected in EXPECTED_PARTIALS:
        gps_assert(
            expected in project_tooltip,
            True,
            "The project tooltip does not name the file %s omits: %s"
            % (criterion, project_tooltip),
        )

    # The omitted names must survive a trip through a saved report, or the
    # tooltip would quietly lose them on reload

    saved = GPS.File(os.path.join(directory, "saved.xml"))
    analysis.dump_to_file(xml=saved)
    GPS.CodeAnalysis.get("Coverage").clear()

    reloaded = GPS.CodeAnalysis.get("Reloaded")
    reloaded.load_from_file(xml=saved)
    reloaded.show_analysis_report()
    yield wait_tasks()

    tree = get_widget_by_name("Reloaded")
    project_tooltip = dump_tree_model(tree.get_model(), COV_TOOLTIP)[0]
    for criterion, expected in EXPECTED_PARTIALS:
        gps_assert(
            expected in project_tooltip,
            True,
            "After reload, the tooltip does not name the file %s omits: %s"
            % (criterion, project_tooltip),
        )
