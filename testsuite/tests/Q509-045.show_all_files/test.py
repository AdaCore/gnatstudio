"""
This test verifies that exec and library directories
are tacken into account when
"Show all files in any project directory" option is set
for the files view.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("explorers-file-dirs-from-project").set(True)
    GPS.Project.load("exec/default.gpr")
    GPS.execute_action("open Files")
    yield wait_tasks()

    explorer = get_widget_by_name("File Explorer Tree")

    gps_assert(
        dump_tree_model(explorer.get_model(), 1),
        ['exec',
         ['exec',
          'obj',
          'src',
          'default.gpr']],
        "Wrong contents for exec dir")

    GPS.Project.load("../lib/lib_prj.gpr")
    yield wait_tasks()

    explorer = get_widget_by_name("File Explorer Tree")
    gps_assert(
        dump_tree_model(explorer.get_model(), 1),
        ['lib',
         ['lib',
          'obj',
          'src',
          'lib_prj.gpr']],
        "Wrong contents for lib dir")
