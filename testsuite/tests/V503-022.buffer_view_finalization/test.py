from gs_utils.internal.utils import run_test_driver


@run_test_driver
def driver():
    # The goal of this test is to exit immediately; this should show a
    # memory corruption under valgrind if failing.
    pass
