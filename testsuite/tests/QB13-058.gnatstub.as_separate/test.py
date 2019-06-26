"""Test generation procedure stub and corresponding separate file"""
from GPS import *
from gps_utils.internal.utils import *

expected_out_1 = \
['Default',
 ['.',
 ['aaa-ccc.adb',
  'aaa.adb',
  'aaa.ads'],
  '.']]

@run_test_driver
def run_test():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("aaa.ads"))
    buf.current_view().goto(buf.at(2, 14))
    yield idle_modal_dialog(lambda: GPS.execute_action(
        "generate body"))
    dialog = get_window_by_title("Confirmation")
    get_stock_button(dialog, Gtk.STOCK_YES).clicked()
    yield hook("project_view_changed")
    yield wait_tasks()

    # Close all editors and reopen the .ads so that
    # it gets the focus back
    GPS.execute_action("close all editors")
    buf = GPS.EditorBuffer.get(GPS.File("aaa.ads"))
    buf.current_view().goto(buf.at(3, 14))

    yield idle_modal_dialog(lambda:GPS.execute_action(
        "generate body as separate"))
    dialog = get_window_by_title("Confirmation")
    get_stock_button(dialog, Gtk.STOCK_YES).clicked()
    yield hook("project_view_changed")

    explorer = get_widget_by_name("Project Explorer Tree")
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected_out_1,
               "Wrong project view output")
    buf = GPS.EditorBuffer.get(GPS.File("aaa.adb"))
    body_text = buf.get_chars()
    buf = GPS.EditorBuffer.get(GPS.File("aaa.adb.expect"))
    expect_text = buf.get_chars()
    gps_assert(body_text, expect_text, "Wrong body text")
