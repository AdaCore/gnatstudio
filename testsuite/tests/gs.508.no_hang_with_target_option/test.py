"""
Test that GS does not get stuck when specifying the --debug option
combined with the --target one on a non-editable project.
"""

import GPS
from gs_utils.internal.asserts import gps_assert
from gs_utils.internal.utils import run_test_driver


@run_test_driver
def test_driver():
    gps_assert(
        GPS.MDI.current_perspective(), "Debug", "We should have started a debug session"
    )
