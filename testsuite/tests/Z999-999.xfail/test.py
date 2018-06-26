from gps_utils.internal.utils import run_test_driver, XFAIL


@run_test_driver
def driver():
    GPS.Logger('TESTSUITE').log("this is supposed to XFAIL")
    return XFAIL
