"""
Test for ALS source information diagnostics, making sure they
are properly displayed/cleared in the Locations view according to
its related preference.
"""

import GPS
from gs_utils.internal.utils import *


SOURCE_INFO_DIAGS_CATEGORY = "Diagnostics: ada.sourceInformation"


@run_test_driver
def run_test():
    # Make sure to enable source info diagnostics first
    GPS.Preference("LSP-Ada-Source-Info-Diagnostics").set(True)

    # Open a file that does not belong to the project
    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_until_true(
        lambda: len(GPS.Message.list(category=SOURCE_INFO_DIAGS_CATEGORY)) == 1,
        timeout=3000,
        error_msg=(
            "Timeout while waiting for source information diagnostic "
            "to appear when opening a file that does not belong to the project."
        ),
    )

    # Disable source info diagnostics
    GPS.Preference("LSP-Ada-Source-Info-Diagnostics").set(False)
    yield wait_until_true(
        lambda: len(GPS.Message.list(category=SOURCE_INFO_DIAGS_CATEGORY)) == 0,
        timeout=3000,
        error_msg=(
            "Timeout while waiting for source information diagnostics to disappear "
            "after disabling the 'LSP-Ada-Source-Info-Diagnostics' preference"
        ),
    )
