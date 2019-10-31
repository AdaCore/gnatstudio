"""
This test verifies that the variables view works properly with
gdb pretty printer on.
"""
import GPS
from gs_utils.internal.utils import *

data = ['<b>Array_Of_Integers</b> = main.a_log_array ',
  ['<b>(0)</b> = main.a_log ',
  ['<b>int1</b> = integer 0'],
  '<b>(1)</b> = main.a_log ',
  ['<b>int1</b> = integer 1'],
  '<b>(2)</b> = main.a_log ',
  ['<b>int1</b> = integer 2'],
  '<b>(3)</b> = main.a_log ',
  ['<b>int1</b> = integer 3'],
  '<b>(4)</b> = main.a_log ',
  ['<b>int1</b> = integer 4'],
  '<b>(5)</b> = main.a_log ',
  ['<b>int1</b> = integer 5'],
  '<b>(6)</b> = main.a_log ',
  ['<b>int1</b> = integer 6'],
  '<b>(7)</b> = main.a_log ',
  ['<b>int1</b> = integer 7'],
  '<b>(8)</b> = main.a_log ',
  ['<b>int1</b> = integer 8'],
  '<b>(9)</b> = main.a_log ',
  ['<b>int1</b> = integer 9']],
  '<b>S</b> = string (1 .. 24) &quot;First line.[&quot;0a&quot;]Second line.&quot;']


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    debug = GPS.Debugger.get()
    debug.send("break main.adb:20")
    debug.send("run")
    yield wait_until_not_busy(debug)

    debug.send("set print pretty on")
    yield wait_until_not_busy(debug)

    GPS.MDI.get_by_child(b.current_view()).raise_window()
    view = Variables_View()
    yield view.open_and_yield()

    Variables_View.display("Array_Of_Integers")
    view.expand([0])
    Variables_View.display("S")

    gps_assert(
        data,
        view.dump(),
        "Invalid contents in the Variables view")
