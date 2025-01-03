"""
Test that exec_dir is taken into account by gdb

We start with project where exec_dir is commented.
After, we uncomment it in the project file itself,
save file and reload project and ensure that the
attribute is used by GS to start GDB
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("../default.gpr"))

    buf.current_view().goto(buf.at(5, 3))
    yield wait_idle()
    send_key_event(GDK_BACKSPACE)
    send_key_event(GDK_BACKSPACE)
    yield wait_idle()
    buf.save(False, GPS.File("../default.gpr"))
    yield wait_idle()
    GPS.execute_action("reload project")
    yield wait_idle()

    GPS.execute_action("/Debug/Initialize/main")
    yield wait_for_mdi_child("Debugger Console")
    yield wait_idle()

    console = GPS.Debugger.get().get_console()
    text = console.get_text()
    found = (
        text.find(os.path.join("exec", "main") + ": No such file or directory.") != -1
    )

    gps_assert(found, True, "Incorrect debugger's executable used" + text)
