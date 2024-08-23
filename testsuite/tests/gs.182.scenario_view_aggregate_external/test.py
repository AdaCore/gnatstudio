"""
Aggregate external attribute should lock scenario variable in read-only mode
"""
from GPS import *
from gs_utils.internal.utils import *


def check_variable(view, name, value, readonly, msg):
    var = get_widget_by_name(name, view)
    gps_assert(var is not None, True, "Can't Find " + name + " variable")
    combo = get_widgets_by_type(Gtk.ComboBox, var)[0]
    gps_assert(combo.get_active_text(), value, "Wrong default value for " + name)
    gps_assert(combo.is_sensitive(), readonly, msg)
    return combo


@run_test_driver
def gps_started():
    # Open and retrieve the scenario view
    execute_action("open scenario")
    view = GPS.MDI.get("Scenario").pywidget()
    gps_assert(view is not None, True, "Can't get the Scenario View")

    # Check that Ext3 is not shown
    gps_assert(get_widget_by_name("Ext3", view), None, "Ext3 is not used in subproject")

    # Check the default value
    combo = check_variable(view, "Ext1", "Val2", False, "at startup")
    check_variable(view, "Ext2", "Val1", True, "at startup")

    # Try to update the value of Ext1
    combo.set_active(1)
    check_variable(view, "Ext1", "Val2", False, "Ext1 should not be updated")
