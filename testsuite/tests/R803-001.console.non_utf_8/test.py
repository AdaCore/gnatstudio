# -*- coding: latin-1 -*-
"""Test that a string encoded in Latin-1 is correctly displaid in a Console"""
from GPS import *
from gs_utils.internal.utils import *
import sys

@run_test_driver
def run_test():
    yield execute_action("Build & Run Number 1")
    yield wait_tasks(other_than=known_tasks)
    yield timeout(500)
    name = "Run: accented" + (".exe" if sys.platform == "win32" else "")
    gps_assert("Printing àéè" in GPS.Console(name).get_text(),
               True,
               "The Latin-1 characters should have been displayed")
