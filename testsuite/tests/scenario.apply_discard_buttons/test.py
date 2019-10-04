"""
Test the apply/discard buttons in the scenario view.
"""
from GPS import *
from gps_utils.internal.utils import *


def check_value(view, name, value, cmd_value, msg):
    var = get_widget_by_name(name, view)
    gps_assert(var is not None, True, "Can't Find " + name + " variable")
    combo = get_widgets_by_type(Gtk.ComboBox, var)[0]
    gps_assert(combo.get_active_text(),
               value,
               "Wrong default value for " + name + " " + msg)
    cmd = "-X" + name + "=" + cmd_value
    gps_assert(cmd in GPS.Project.scenario_variables_cmd_line(prefix="-X"),
               True,
               "Wrong value for " + name + " in the command line " + msg)
    return combo


@run_test_driver
def gps_started():
    # Open and retrieve the scenario view
    execute_action("open scenario")
    view = GPS.MDI.get("Scenario").pywidget()
    gps_assert(view is not None, True, "Can't get the Scenario View")

    # Check the default value
    combo = check_value(view, "Ext1", "one", "one", "at startup")
    check_value(view, "Ext2", "hello", "hello", "at startup")

    # Update the value of Ext1
    combo.set_active(1)
    check_value(view, "Ext1", "two", "one", "after updating Ext1")
    check_value(view, "Ext2", "hello", "hello", "after updating Ext1")

    # Give the focus to a variable (it should have no impact: however it can
    # detect case like when the editing callback was linked to the discard
    # button
    var = get_widget_by_name("Ext1", view)
    var.get_parent().select_child(var)

    # Discard the change
    yield idle_modal_dialog(
        lambda: get_button_from_icon_name(
            view, "gps-stop-symbolic").clicked())
    check_value(view, "Ext1", "one", "one", "after discard")
    combo = check_value(view, "Ext2", "hello", "hello", "after discard")

    # Update the value of Ext2
    combo.set_active(1)
    check_value(view, "Ext1", "one", "one", "after updating Ext2")
    check_value(view, "Ext2", "world!", "hello", "after updating Ext2")

    # Validate the change
    yield idle_modal_dialog(
        lambda: get_button_from_icon_name(
            view, "gps-syntax-check-symbolic").clicked())
    check_value(view, "Ext1", "one", "one", "after apply")
    check_value(view, "Ext2", "world!", "world!", "after apply")
