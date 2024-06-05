"""
This test checks that we don't override .clang-format files at startup when
already present.
It also verifies that a confirmation dialog is correctky spawned when the user
wants to override settings after changing formatting preferences.
"""
import os.path
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    first_line = ""
    with open(os.path.join(GPS.pwd(), "src", ".clang-format")) as f:
        lines = f.readlines()

    try:
        Idx = lines.index("BasedOnStyle: GNU\n")
    except ValueError:
        simple_error("GS should not override .clang-format files at startup")

    # Change the clang formatting style: we should have a confirmation dialog
    # displayed
    yield idle_modal_dialog(lambda: GPS.Preference("clangd-BasedOnStyle").set("LLVM"))
    dialog = get_window_by_title("C/C++ formatting settings")
    get_button_from_label("Yes", dialog).clicked()
    yield timeout(300)

    last_line = ""
    # Verify that we have overridden the corresponding setting (and only this
    #  one) in the .clang-format file
    with open(os.path.join(GPS.pwd(), "src", ".clang-format")) as f:
        lines = f.readlines()

    try:
        Idx = lines.index("BasedOnStyle: LLVM\n")
    except ValueError:
        simple_error("GS should override .clang-format corresponding setting")
    try:
        Idx = lines.index("AllowShortBlocksOnASingleLine: Always\n")
    except ValueError:
        simple_error("GS should override .clang-format corresponding setting")

    # Revert to the original state
    yield idle_modal_dialog(lambda: GPS.Preference("clangd-BasedOnStyle").set("GNU"))
    dialog = get_window_by_title("C/C++ formatting settings")
    get_button_from_label("Yes", dialog).clicked()
    yield timeout(300)
