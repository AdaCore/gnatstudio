"""
Test that setting up an Alire crate does not block GNAT Studio's main loop.

The fake 'alr build' used here does not return until this driver creates the
'sync_may_finish' file: reaching that point at all proves that the main loop
kept running while Alire was synchronizing the crate.

The second half of the test checks that the environment entered for the crate
(including the ALIRE variable) is restored once a project outside the crate is
loaded.
"""

import os
import GPS
import alire
from gs_utils.internal.utils import *

PLAIN_PROJECT = """project Plain is
   for Source_Dirs use ();
end Plain;
"""


@run_test_driver
def test_driver():
    crate_root = os.path.dirname(GPS.Project.root().file().path)

    def in_crate(name):
        return os.path.join(crate_root, name)

    def rejected_command_line():
        """
        Return the command line the fake 'alr' has refused to run, if any.
        """
        if not os.path.exists(in_crate("bad_command_line")):
            return ""

        with open(in_crate("bad_command_line")) as f:
            return f.read().strip()

    # The setup sequence should have been started right after the initial
    # project load, without waiting for 'alr' to finish.
    yield wait_until_true(
        lambda: os.path.exists(in_crate("sync_started")) or rejected_command_line(),
        timeout=10000,
        error_msg="'alr build' has not been launched",
    )
    gps_assert(
        rejected_command_line(),
        "",
        "The Alire setup targets should use command lines 'alr' accepts",
    )

    # 'alr build' is still running, so the sequence is under way: it should be
    # monitored by a task of its own, which is what makes waiting for GNAT
    # Studio's tasks a way of waiting for the setup as a whole.
    gps_assert(
        alire.ALIRE_SETUP_TASK_NAME in [task.name() for task in GPS.Task.list()],
        True,
        "The Alire setup should be monitored by a task while it runs",
    )

    # Let the fake 'alr build' terminate
    open(in_crate("sync_may_finish"), "w").close()

    yield wait_until_true(
        lambda: os.path.exists(in_crate("sync_finished"))
        or os.path.exists(in_crate("sync_gave_up")),
        timeout=40000,
        error_msg="'alr build' neither finished nor gave up",
    )
    gps_assert(
        os.path.exists(in_crate("sync_gave_up")),
        False,
        "GNAT Studio's main loop was blocked while Alire was setting up the crate",
    )

    yield wait_until_true(
        lambda: alire.alire_state is None,
        timeout=30000,
        error_msg="The Alire setup sequence did not finish",
    )

    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project reported by 'alr show' should have been reloaded",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment reported by 'alr printenv' should have been set",
    )
    gps_assert(
        GPS.getenv("ALIRE"),
        "True",
        "ALIRE should be set while an Alire crate is loaded",
    )

    # Loading a project that has nothing to do with the crate should give the
    # environment back the values it had before we entered the crate.
    plain_dir = os.path.join(crate_root, os.pardir, "plain")
    os.makedirs(plain_dir, exist_ok=True)
    plain_project = os.path.join(plain_dir, "plain.gpr")

    with open(plain_project, "w") as f:
        f.write(PLAIN_PROJECT)

    GPS.Project.load(plain_project)
    yield wait_tasks()

    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored when leaving the Alire crate",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "",
        "The crate's environment should have been restored",
    )
