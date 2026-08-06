"""
Test what becomes of an Alire crate whose setup fails.

The setup must be given up on rather than half applied: the environment goes back
to its former values, and Alire runs again the next time one of the crate's
projects is loaded rather than the crate being taken for set up.

The last part checks the same after simply leaving the crate: the shortcut that
skips Alire holds only while the crate's environment is in place.
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

    def alr_runs():
        return contents("alr_runs").split()

    # Make 'alr printenv', the last step of the sequence, fail.
    open(in_crate("fail_printenv"), "w").close()

    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        contents("bad_command_line"),
        "",
        "The Alire setup targets should use command lines 'alr' accepts",
    )
    gps_assert(
        alr_runs(),
        ["build", "show", "printenv"],
        "The whole sequence should have been run",
    )
    gps_assert(
        alire.alire_state,
        None,
        "The failed setup sequence should be over",
    )

    # The intermediate project stays loaded: nothing is retried behind the
    # user's back.
    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project loaded before Alire was run should still be loaded",
    )

    # Both the variable we set ourselves and the one 'alr printenv' had already
    # reported before failing should be back to their former values.
    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored after the failed setup",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "",
        "The environment applied before the failure should have been undone",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "A crate that is not set up should have no known project files",
    )

    # Loading a project 'alr show' has reported must not take the shortcut meant
    # for a crate that is set up: the setup has to be retried.
    os.remove(in_crate("fail_printenv"))
    GPS.Project.load(in_crate("bye.gpr"))
    yield wait_tasks()

    gps_assert(
        alr_runs(),
        ["build", "show", "printenv"] * 2,
        "Alire should be run again for a crate whose setup has failed",
    )
    gps_assert(
        alire.alire_state,
        None,
        "The second setup sequence should be over",
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
    gps_assert(
        sorted(os.path.basename(f) for f in alire.alire_project_files),
        ["bye.gpr", "hello.gpr"],
        "The project files of the manifest should be known now",
    )

    # Leaving the crate restores the environment, so its project files are not
    # 'already set up' anymore either.
    GPS.Project.load(plain_project)
    yield wait_tasks()

    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored when leaving the Alire crate",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "Leaving the crate should invalidate its known project files",
    )

    GPS.Project.load(in_crate("bye.gpr"))
    yield wait_tasks()

    gps_assert(
        alr_runs(),
        ["build", "show", "printenv"] * 3,
        "Alire should be run again for a crate we have left",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The crate's environment should have been set again",
    )
