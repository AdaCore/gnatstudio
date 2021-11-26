"""
Verify that we display the multiactions menu when several actions
are available for the same line in the editor's side area.
Also check that clicking on an action item works fine.
"""

from gs_utils import interactive
from gs_utils.internal.utils import *


@interactive(name="my_action", category="")
def my_action():
    GPS.Console().write("My action\n")


@run_test_driver
def driver():
    # Open an editor and go to a line that can be folded
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()
    v.goto(b.at(3, 1))

    # Create a message with an associated action at the same line
    m = GPS.Message('custom category', GPS.File("hello.adb"),
                    3, 1, "Zboob", show_in_locations=False)
    m.set_action('my_action', image='gps-light-bulb', tooltip="My Action")
    yield wait_tasks()

    # Click on the side area
    yield idle_modal_dialog(lambda: b.click_on_side_column(3, 1, icon_name=""))

    # Verify that the multiactions menu is correclty displayed, since we
    # have several actions available
    multi_actions_menu = get_widget_by_name('gnatstudio_multiple_actions_menu')
    gps_assert(multi_actions_menu != None, True,
               "The multi actions menu should be shown")

    # Click on the action menu item and verify that it executed the action
    items = get_widgets_by_type(Gtk.MenuItem, multi_actions_menu)
    items[0].activate()
    yield wait_idle()

    gps_assert('My action' in GPS.Console().get_text(), True,
               "Action was not executed")
