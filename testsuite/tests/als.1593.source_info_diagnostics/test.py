"""
Test for ALS source information diagnostics, making sure they
are properly displayed/cleard in the Locations view according to
its related preference.
"""

import GPS
from gs_utils.internal.utils import *


SOURCE_INFO_DIAGS_CATEGORY = "Diagnostics: ada.sourceInformation"

@run_test_driver
def run_test():
    # Make sure to enable source info diagnostics first
    GPS.Preference('LSP-Ada-Source-Info-Diagnostics').set(True)

    # Open a file that does not belong to the project
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield timeout(1000)

    # Check that we get the expected source info diagnostic
    msgs = GPS.Message.list(category=SOURCE_INFO_DIAGS_CATEGORY)
    gps_assert(
        len(msgs),
        1,
        "Wrong number of messages when source info diagnostics are enabled.",
    )
    diag_msg = msgs[0]
    gps_assert(
        diag_msg.get_category(),
        SOURCE_INFO_DIAGS_CATEGORY,
        "Wrong message category for expected diagnostic",
    )

    # Disable source info diagnostics
    GPS.Preference('LSP-Ada-Source-Info-Diagnostics').set(False)
    yield timeout(1000)
    msgs = GPS.Message.list(category=SOURCE_INFO_DIAGS_CATEGORY)
    gps_assert(
        msgs, [], "The source info diagnostic should have disappeared"
    )
