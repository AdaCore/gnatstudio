from gps_utils.internal.utils import run_test_driver, simple_error


@run_test_driver
def driver():
    GPS.execute_action("open Scenario")
    msgs = GPS.Console().get_text()
    if "a scenario variable named 'Bla' appears more than once" not in msgs:
        simple_error("we did not find the expected collision"
                     " warning in the messages view:\n{}".format(msgs))
