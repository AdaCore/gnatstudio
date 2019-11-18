"""
This test checks that an additional warning message is displayed
in the Messages view when loading a project with undefined
scenario variable values.
"""

from gs_utils.internal.utils import run_test_driver, simple_error



UNDEFINED_EXTERNALS_MSG =  (
    "Some scenario variables relying on undefined "
    + "externals have been found while loading the "
    + "project: GNAT Studio will use the first available "
    + "values for these scenario variables as a fallback "
    + "(go to the Scenario view to see which values were "
    + " picked).")


@run_test_driver
def driver():
    GPS.execute_action("open Scenario")
    msgs = GPS.Console().get_text()
    if UNDEFINED_EXTERNALS_MSG not in msgs:
        simple_error("No warning message about undefined scenario variables"
                     " found in the messages view:\n{}".format(msgs))
