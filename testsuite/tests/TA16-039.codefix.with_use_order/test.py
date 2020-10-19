"""
Test the redundant codefixes: execute 2 codefix to remove with and use
and then execute the last codefix to properly place the with / use.
Check the end order of the with/use (use after with clause)
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.BuildTarget("Build All").execute()
    yield wait_idle()

    messages = GPS.Message.list()
    for m in messages:
        m.execute_action()
        yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    text = buf.get_chars()
    gps_assert("with Ada.Text_IO;\nuse Ada.Text_IO;" in text,
               True,
               "Codefix failed")
