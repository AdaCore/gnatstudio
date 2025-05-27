"""
Test that we have proper prompt in the debugger console.
After the debgger start the console hould have only one
prompt.
"""

import GPS
from gs_utils.internal.utils import *
from workflows import run_as_workflow
import re


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Start the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_idle()

    # Check prompt symbols
    console = GPS.Debugger.get().get_console()
    text = console.get_text()
    count = len(re.findall("^>", text, re.MULTILINE))
    gps_assert(
        count, 1, "Wrong prompts count :\n" + text
    )


