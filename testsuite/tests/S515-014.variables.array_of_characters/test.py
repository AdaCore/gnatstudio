"""
Verify that the Variables view can properly compute simple type.
"""

from gps_utils.internal.utils import *

expect = [
   '<b>Foo</b> = string (1 .. 12) &quot;Hello World!&quot;',
   '<b>Bar</b> = string (1 .. 10) &quot;aaaaaaaaaa&quot;',
   '<b>Sym</b> = gnatcoll.symbols.symbol 0xfff',
   ['None'],
   '<b>Norm</b> = String &quot;&quot;']


def display(debug, buf, line, col, name):
    GPS.MDI.get_by_child(buf.current_view()).raise_window()
    buf.current_view().goto(buf.at(line, col))
    yield hook('context_changed')
    GPS.execute_action("debug tree display variable")
    yield wait_tasks(other_than=known_tasks)


@run_test_driver
def driver():
    mode = GPS.Preference("GPS6-Debugger-Debugger-Kind").get()

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    debug = GPS.Debugger.get()
    debug.send("break main.adb:17")
    debug.send("run")
    yield wait_until_not_busy(debug)


    yield display(debug, b, 6, 5, "Foo")
    yield display(debug, b, 8, 5, "Bar")
    yield display(debug, b, 10, 5, "Sym")
    yield display(debug, b, 14, 5, "Norm")

    view = Variables_View()
    yield view.open_and_yield()

    d = view.dump()
    gps_assert(d is not None, True, "Assembly view is empty in " + mode)
    # The Windows output is more verbose, control its size
    d[2] = d[2][:42]
    gps_assert(expect, d, "Invalid contents of the Variables view in " + mode)
