"""
This test checks that find all references works fine through LSP and clangd.
"""

import GPS
from gs_utils.internal.utils import *

expected = [
 'References for ch (main.cpp:11) (3 items in 2 files)',
  ['main.cpp (1 item)',
  ['<b>11:16</b>     auto a = obj.<b>ch</b>;'],
  'my_class.hh (2 items)',
  ['<b>7:16</b>      int num;char <b>ch</b>;My_Record_Of_Record rec;',
  '<b>12:24</b>     My_Class(){num = 100;<b>ch</b> = &apos;A&apos;;}']]]


@run_test_driver
def run_test():
    b1  = GPS.EditorBuffer.get(GPS.File("my_class.cpp"))
    b1  = GPS.EditorBuffer.get(GPS.File("my_class.hh"))
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    yield wait_tasks()
    # timeout to let clangd indexing the files
    yield timeout(200)
    buf.current_view().goto(buf.at(11, 17))
    yield wait_idle()

    GPS.execute_action("find all references")
    yield hook("language_server_response_processed")
    yield wait_idle()

    gps_assert(dump_locations_tree(), expected)
