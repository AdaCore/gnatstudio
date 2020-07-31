"""Contextual menu for Astyle

This script provides a contextual menu to run Astyle on a file if file is
C/CPP writable file and Astyle is accessible.

Astyle configuration can be set in:
    o the file named .astylerc or _ astylerc.
    o the file named .astylerc in the HOME directory (for Linux).
    o the file name astylerc in the APPDATA directory (for Windows).

"""

import GPS
import os_utils
from gs_utils import interactive, in_c_file, is_writable
import workflows.promises as promises


def in_proper_file(context):
    return in_c_file(context) and is_writable(context) and \
      os_utils.locate_exec_on_path('astyle') != ""


@interactive(name="Astyle",
             contextual="Format with Astyle",
             contextual_group=GPS.Contextual.Group.EDITING,
             filter=in_proper_file)
def on_activate():
    f = GPS.current_context().file()
    cmd = ["astyle", f.path]
    try:
        con = promises.ProcessWrapper(cmd)
    except Exception:
        GPS.Console().write("Could not launch executable %s" % (cmd[0]))
        return

    status, output = yield con.wait_until_terminate()
    if status != 0:
        #  Show output in the Messages view on error
        GPS.Console().write(output)
        GPS.Console().write("%s returned an error." % (cmd[0]))
    else:
        GPS.EditorBuffer.get(f, force=True, open=True)
