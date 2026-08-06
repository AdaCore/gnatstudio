"""
Test that interrupting the Alire setup from the Task Manager really stops it.

An interrupted task is never resumed, so the sequence cannot notice by itself:
nothing would stop the 'alr' in progress, and the crate's environment would stay
installed for a crate that is not set up.

The last part checks that the session is none the worse for it: loading the
crate's project again sets it up.
"""

import os
import GPS
import alire
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # GNAT Studio's current directory follows the project being loaded, so
    # derive the test's directory from the project loaded at startup rather
    # than from the current one.
    plain_project = GPS.Project.root().file().path
    test_dir = os.path.dirname(os.path.dirname(plain_project))
    crate_root = os.path.join(test_dir, "crate")

    def in_crate(name):
        return os.path.join(crate_root, name)

    def contents(name):
        if not os.path.exists(in_crate(name)):
            return ""

        with open(in_crate(name)) as f:
            return f.read().strip()

    def setup_tasks():
        return [
            task
            for task in GPS.Task.list()
            if task.name().startswith(alire.ALIRE_SETUP_TASK_NAME)
        ]

    GPS.Project.load(in_crate("hello.gpr"))

    yield wait_until_true(
        lambda: os.path.exists(in_crate("sync_started")),
        timeout=10000,
        error_msg="'alr build' has not been launched",
    )
    gps_assert(
        contents("bad_command_line"),
        "",
        "The Alire setup targets should use command lines 'alr' accepts",
    )

    # 'alr build' is waiting for us: interrupt the setup as the user would.
    tasks = setup_tasks()
    gps_assert(
        len(tasks),
        1,
        "The Alire setup should be monitored by a task while it runs",
    )
    tasks[0].interrupt()

    yield wait_until_true(
        lambda: alire.alire_state is None,
        timeout=10000,
        error_msg="The interrupted Alire setup has not been cleaned up",
    )

    # 'alr' should have been terminated rather than left to its own devices.
    yield wait_until_true(
        lambda: os.path.exists(in_crate("sync_interrupted")),
        timeout=10000,
        error_msg="'alr build' was left running by the interrupted setup",
    )
    gps_assert(
        os.path.exists(in_crate("sync_finished")),
        False,
        "'alr build' should not have run to completion",
    )
    gps_assert(
        contents("alr_runs").split(),
        ["build"],
        "The interrupted sequence should not have gone any further",
    )

    # The environment should be back to what it was, and nothing should be left
    # of the crate we have given up on.
    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored after the interruption",
    )
    gps_assert(
        alire.saved_env,
        {},
        "No environment should be left to restore",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "A crate that is not set up should have no known project files",
    )
    gps_assert(
        setup_tasks(),
        [],
        "The interrupted task should be gone",
    )

    # Loading the crate's project again should set it up, this time without
    # anything holding 'alr build' back.
    open(in_crate("sync_may_finish"), "w").close()
    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        contents("alr_runs").split(),
        ["build", "build", "show", "printenv"],
        "Alire should be run again after an interrupted setup",
    )
    gps_assert(
        GPS.Project.root().name(),
        "Hello",
        "The project reported by 'alr show' should have been reloaded",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment reported by 'alr printenv' should have been set",
    )
