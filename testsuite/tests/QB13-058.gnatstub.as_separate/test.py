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
    buf = GPS.EditorBuffer.get(GPS.File("aaa.ads"))
    buf.current_view().goto(buf.at(2, 14))
    select_editor_contextual("Generate Body of Bbb")
    yield GPS.Hook('file_edited')
    GPS.MDI.get_by_child(buf.current_view()).raise_window()
    buf.current_view().goto(buf.at(3, 14))
    select_editor_contextual("Generate Body of Ccc as separate")
    yield wait_tasks(other_than=known_tasks)
    explorer = get_widget_by_name("Project Explorer Tree")
    gps_assert(dump_tree_model(explorer.get_model(), 1),
               expected_out_1,
               "Wrong project view output")
    buf = GPS.EditorBuffer.get(GPS.File("aaa.adb"))
    body_text = buf.get_chars()
    buf = GPS.EditorBuffer.get(GPS.File("aaa.adb.expect"))
    expect_text = buf.get_chars()
    gps_assert(body_text, expect_text, "Wrong body text")
