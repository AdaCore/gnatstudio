"""Verify that exotic-gdb does not overriding the "native" toolchain set by
gnatls."""

from gs_utils.internal.utils import run_test_driver, gps_assert, get_widget_by_name
from gs_utils.internal.dialogs import Project_Properties_Editor


@run_test_driver
def driver():
    e = Project_Properties_Editor()
    yield e.open_and_yield(wait_scan=True)

    page = e.get_page("Build/Toolchain")
    compiler_entry = get_widget_by_name("gnat_driver_tool", page)

    gps_assert(compiler_entry.get_text(), "gnat", "native target is overriding")
    yield e.cancel()
