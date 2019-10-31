from gs_utils.internal.utils import run_test_driver


@run_test_driver
def driver():
    GPS.Preference("Editor/C & C++:Clang/Show diagnostics").set("editor_only")
