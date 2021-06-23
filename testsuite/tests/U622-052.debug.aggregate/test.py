"""
Launch build & debug on an aggregate project.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    # This will freeze if the debugger is not launched
    yield hook('debugger_started')
