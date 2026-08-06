"""
Test that waiting for GNAT Studio's tasks waits for the whole Alire crate setup.

This is how the integration testsuite loads a crate: load its project, wait for
the tasks, expect the crate to be set up. A sequence leaving the task manager
empty between two 'alr' runs would be taken for finished while Alire works on.

The second half loads the other project of the same manifest, which must neither
re-run Alire nor fail to resolve: its path is relative to the crate's root, where
the setup has left GNAT Studio's current directory.
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

    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        contents("bad_command_line"),
        "",
        "The Alire setup targets should use command lines 'alr' accepts",
    )

    # Waiting for the tasks should have been enough: the whole sequence has run
    # and the project 'alr show' reported has been reloaded.
    gps_assert(
        contents("alr_runs").split(),
        ["build", "show", "printenv"],
        "Waiting for the tasks should wait for the whole Alire setup",
    )
    gps_assert(
        alire.alire_state,
        None,
        "The Alire setup sequence should be over",
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

    # Load the other project of the same manifest, by a path relative to the
    # crate's root.
    GPS.Project.load("bye.gpr")
    yield wait_tasks()

    gps_assert(
        GPS.Project.root().name(),
        "Bye",
        "The other project of the manifest should have been loaded",
    )
    gps_assert(
        contents("alr_runs").split(),
        ["build", "show", "printenv"],
        "Alire should not be run again for a project of a known manifest",
    )
