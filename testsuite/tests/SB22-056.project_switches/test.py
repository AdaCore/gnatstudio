"""
This test verifies that switches with default values represented in combo
boxes work properly. Like Optimization level which can be set as -O[1,2,3]
where 1 is the default value which can be omitted and a command line
will contain `-O`
"""
import GPS
from gs_utils.internal.utils import *

tip = "Controls the optimization level"


@run_test_driver
def run_test():
    e = Project_Properties_Editor()
    yield e.open_and_yield()

    page = e.get_page('Build/Switches/Ada')
    ent = get_widgets_by_type(Gtk.Entry, page)[-1]
    optimization = get_widgets_by_type(Gtk.ComboBox, page)[0]
    gps_assert(optimization is None, False,
               "Optimization level combobox is not found")

    optimization.set_active(1)
    gps_assert(ent.get_text(), "-O",
               "Command line is incorrect for `Some optimization` level")

    optimization.set_active(2)
    gps_assert(ent.get_text(), "-O2",
               "Command line is incorrect for `Full optimization` level")

    optimization.set_active(0)
    gps_assert(ent.get_text(), "",
               "Command line is incorrect for `No optimization` level")

    yield e.cancel()
