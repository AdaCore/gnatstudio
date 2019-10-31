from gs_utils.internal.utils import (run_test_driver, wait_tasks, known_tasks,
                                     simple_error)


@run_test_driver
def driver():
    yield wait_tasks(other_than=known_tasks)

    msgs = GPS.Console().get_text()
    if "undefined external" in msgs:
        simple_error(
            "The console shows a project load issue:\n{}".format(msgs))
