"""
Verify that the active task finishes faster that the normal one
"""

import GPS
from gps_utils.internal.utils import *
import time

X = 100
Y = 1000


def task1(task):
    global X
    X = X - 1
    if X:
        return GPS.Task.EXECUTE_AGAIN
    else:
        simple_error("This task should never finish first")
        return GPS.Task.SUCCESS


def task2(task):
    global Y
    Y = Y - 1
    if Y:
        return GPS.Task.EXECUTE_AGAIN
    else:
        GPS.exit(True)
        return GPS.Task.SUCCESS


@run_test_driver
def test_driver():
    GPS.Task("Task1", task1, active=False)
    GPS.Task("Task2", task2, active=True)
    yield wait_tasks(other_than=known_tasks)
