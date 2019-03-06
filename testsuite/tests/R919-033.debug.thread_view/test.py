import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    for s in ["b main.adb:31",
              "run"]:
        yield wait_until_not_busy(d)
        d.send(s)

    yield wait_until_not_busy(d)
    GPS.execute_action("open threads debugger window")

    win = GPS.MDI.get("Threads").pywidget()
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    tm = dump_tree_model(tree.get_model(), 0)
    # Expect 6 threads in Threads View
    gps_assert(len(tm), 6, "Threads incorrect")
    d.send('q')

    yield wait_tasks()
