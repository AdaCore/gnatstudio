"""Verify that the scenario variable doesn't get put in "-X" switches
   if it has the default value.

   Then reset it to default, and verify (in test.cmd) that the
   value is not stored in properties.json"""

from gs_utils.internal.utils import run_test_driver, simple_error


@run_test_driver
def driver():
    p = GPS.Project.root()

    t = GPS.BuildTarget("Build All")

    t.execute()
    if "-Xmy_variable" in GPS.Console().get_text():
        simple_error("-X shouldn't contain 'my_variable' "
                     "when it has the default value")

    p.set_scenario_variable("my_variable", "other_value")

    t.execute()
    if "-Xmy_variable" not in GPS.Console().get_text():
        simple_error("-X should show 'my_variable' now")

    # Reset to default value
    p.set_scenario_variable("my_variable", "default_value")
