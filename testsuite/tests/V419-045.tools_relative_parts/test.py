"""
Test that we use relative project paths.
"""

from GPS import *
from gs_utils.internal.utils import *

expected = """project Default is

   for Main use ("foo.adb");

   package Check is
      for Default_Switches ("ada") use ("-rules", "-from=test.yaml");
   end Check;

end Default;

"""


@run_test_driver
def run_test():
    e = Project_Properties_Editor()
    yield e.open_and_yield(wait_scan=False)

    page = e.get_page("General")
    toggle = get_widgets_by_type(Gtk.ToggleButton, page)[0]
    toggle.set_active(True)

    page = e.get_page("Build/Switches/GNATcheck")
    entry = get_widgets_by_type(Gtk.Entry, page)[0]
    entry.set_text("-rules -from=" + GPS.pwd() + "test.yaml")
    yield e.save()

    f = open("default.gpr", "r")
    gps_assert(f.read(), expected)
