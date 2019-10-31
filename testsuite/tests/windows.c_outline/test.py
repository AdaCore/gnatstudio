"""
Verify that the outline works fine for complex C files.
"""

from GPS import *
from gs_utils.internal.utils import *


expected = [
    'matrixAdd <span foreground="#A0A0A0">(const int *, const int *)</span>',
    'matrixAlloc <span foreground="#A0A0A0">(short, short)</span>',
    'matrixAllocAll <span foreground="#A0A0A0">(int, int, int)</span>',
    'matrixAllocId <span foreground="#A0A0A0">(int, int)</span>',
    'matrixCopy <span foreground="#A0A0A0">(const int *)</span>',
    'matrixFree <span foreground="#A0A0A0">(int *)</span>',
    'matrixGet <span foreground="#A0A0A0">(const int *, int, int)</span>',
    'matrixMul <span foreground="#A0A0A0">(const int *, const int *)</span>',
    'matrixSet <span foreground="#A0A0A0">(int *, int, int, int)</span>']


@run_test_driver
def run_test():
    GPS.execute_action("open Outline")
    buf = GPS.EditorBuffer.get(GPS.File("matrix.c"))

    explorer = get_widget_by_name("Outline View Tree")
    GPS.Console().write(str(dump_tree_model(explorer.get_model(), 1)))
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected,
               "Wrong outline view for matrix.c")
