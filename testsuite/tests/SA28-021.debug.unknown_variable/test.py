"""
This test checks that GS does not hung when a variable is not valid
 while debugging.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    variables = Variables_View()
    yield variables.open_and_yield()
    Variables_View.display("List")
    yield wait_idle()
    variables.expand([0])
