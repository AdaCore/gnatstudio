# -*- coding: latin-1 -*-
"""Test that a string encoded in Latin-1 is correctly displaid in a Console"""
from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    yield execute_action("Build & Run Number 1")
    yield wait_tasks(other_than=known_tasks)
    gps_assert("Printing àéè" in GPS.Console("Run: accented").get_text(),
               True,
               "The Latin-1 characters should have been displayed")
