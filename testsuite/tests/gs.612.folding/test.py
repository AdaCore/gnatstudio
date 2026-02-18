"""
Test that a.adb file has folding information
"""

from GPS import *
from gs_utils.internal.utils import *

expect = """package body A is

   type Bar is new Integer range 3 .. 5;

   procedure C is
end A;
"""


@run_test_driver
def run_test():
    yield wait_tasks(other_than=known_tasks)
    buf = GPS.EditorBuffer.get(GPS.File("a.adb"))
    buf.at(5, 1).block_fold()
    gps_assert(
        buf.get_chars(include_hidden_chars=False),
        expect,
        "wrong code after the folding",
    )
