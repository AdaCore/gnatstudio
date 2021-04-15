"""
Test the feedback when sending command to a dead process.
"""

from gs_utils.internal.utils import run_test_driver, gps_assert

expected = "The process is dead or was not started"


@run_test_driver
def driver():
    p = GS.Process("echo 1")
    p.get_result()

    try:
        p.send("1")
        raise Exception("Should not reach this")
    except Exception as e:
        gps_assert(str(e),
                   expected,
                   "Wrong error message")

    try:
        p.kill()
        raise Exception("Should not reach this")
    except Exception as e:
        gps_assert(str(e),
                   expected,
                   "Wrong error message")
