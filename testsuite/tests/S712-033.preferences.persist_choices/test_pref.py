from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    gps_assert(GPS.Preference("Editor/C & C++:Clang/Show diagnostics").get(),
               "editor_only",
               "The preference wasn't preserved")
