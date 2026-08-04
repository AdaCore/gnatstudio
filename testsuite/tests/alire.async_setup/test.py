"""
Test that setting up an Alire crate does not block GNAT Studio's main loop.

The fake 'alr build' used here does not return until this driver creates the
'sync_may_finish' file: reaching that point at all proves that the main loop
kept running while Alire was synchronizing the crate.
"""

import os
import GPS
import alire
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    crate_root = os.path.dirname(alire.alire_manifest)

    # The setup sequence should have been started right after the initial
    # project load, without waiting for 'alr' to finish.
    yield wait_until_true(
        lambda: os.path.exists(os.path.join(crate_root, "sync_started")),
        timeout=10000,
        error_msg="'alr build' has not been launched",
    )
    gps_assert(
        alire.alire_state,
        "Alire Sync",
        "The Alire Sync target should still be running",
    )

    # Let the fake 'alr build' terminate
    open(os.path.join(crate_root, "sync_may_finish"), "w").close()

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
