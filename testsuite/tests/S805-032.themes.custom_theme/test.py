"""
This test verifies that color preference changes that are
done without selecting a color theme are not overridden
by the colorschemes.py theme manager.
"""

import GPS
from gs_utils.internal.utils import *


SRC_EDITOR_REF_STYLE_PREF = "Src-Editor-Reference-Style"


@run_test_driver
def run_test():
    # Modify the Src-Editor-Reference-Style pref
    GPS.Preference(SRC_EDITOR_REF_STYLE_PREF).set(
        "DejaVu Sans Mono 11@rgb(255,255,255)@rgb(39,40,34)")

    # Get its new value
    src_ref_style_val = GPS.Preference(SRC_EDITOR_REF_STYLE_PREF).get()

    # Open the Preferences Editor and select the Color Themee page
    p = Preferences()
    yield p.open_and_yield()
    p.select_page("Color Theme")
    yield wait_tasks(other_than=known_tasks)

    # Close the Preferences Editor
    p.dialog.close()
    yield wait_tasks(other_than=known_tasks)

    # Verify that the Src-Editor-Reference-Style still has our custom value
    # set at the beginning
    gps_assert(
        src_ref_style_val == GPS.Preference(SRC_EDITOR_REF_STYLE_PREF).get(),
        True,
        "Changes to Src-Editor-Reference-Style pref have been overridden")
