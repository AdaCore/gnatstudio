"""
Verify that the Variables view can properly compute simple type.
"""

from gs_utils.internal.utils import *
from workflows import promises

expect = [
   '<b>foo</b> = array (1 .. 12) of character &quot;Hello World!&quot;',
   '<b>bar</b> = array (1 .. 10) of character &quot;aaaaaaaaaa&quot;',
   '<b>sym</b> = gnatcoll.symbols.symbol &quot',
   '<b>norm</b> = string &quot;&quot;']


def display(debug, buf, line, col, name):
    GPS.MDI.get_by_child(buf.current_view()).raise_window()
    buf.current_view().goto(buf.at(line, col))
    yield wait_until_true(
        lambda: GPS.Action('debug tree display variable').can_execute())
    GPS.execute_action("debug tree display variable")


@run_test_driver
def driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    
    p = promises.DebuggerWrapper(GPS.File("foo"))
    debug = p.get()

    yield p.send_promise("break main.adb:17")
    yield p.send_promise("run")
    yield wait_until_not_busy(debug)


    yield display(debug, b, 6, 5, "Foo")
    yield wait_DAP_server("variables")
    yield display(debug, b, 8, 5, "Bar")
    yield display(debug, b, 10, 5, "Sym")
    yield display(debug, b, 14, 5, "Norm")

    view = Variables_View()
    yield view.open_and_yield()

    d = view.dump()
    gps_assert(d is not None, True, "Assembly view is empty")
    # The Windows output is more verbose, control its size
    d[2] = d[2][:42]
    gps_assert(expect, d, "Invalid contents of the Variables view")
