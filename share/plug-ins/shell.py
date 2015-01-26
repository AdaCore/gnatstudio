"""
This plug-in will spawn a Unix/Windows shell console in your GPS.
The shell that is spawned is read from your environment variables
SHELL or COMSPEC. When the latter is used, the terminal will not
do special handling for ANSI escape sequences (the ones used to color
text, move the cursor to specific locations,...).
However, when a Unix-like shell is spawned (ie when SHELL is set), the
terminal is fully capable of handling these escape sequences. In
particular, you can also run commands like "vi" in the terminal.

This terminal behaves a lot like a standard Unix terminal. In particular,
you need to make sure that your shell will output all the information.
In some cases, the configuration of your shell (.bashrc if you are
running bash for instance) will deactivate the echo of what you type to
the terminal. Since GPS is not outputing anything on its own, just showing
what the shell is outputing, you need to somehow ensure that your shell
always echos what you type. This is done by running the command
    stty echo
in such cases. In general, this can be safely done in your .bashrc
"""

import os.path
import GPS
import re
import traceback
import os
from gps_utils import interactive, save_dir
from gps_utils.console_process import *


class Unix_Shell(ANSI_Console_Process):

    def __init__(self, process, args=""):
        oldterm = os.environ["TERM"]
        os.environ["TERM"] = "xterm"
        ANSI_Console_Process.__init__(self, process, args)
        os.environ["TERM"] = oldterm


class Win32_Shell(Console_Process):

    def __init__(self, process, args=""):
        Console_Process.__init__(self, process, args)


def on_filter(context):
    if not isinstance(context, GPS.FileContext):
        return False
    try:
        #  Check if context has directory information
        dir = context.directory()
        return GPS.Preference("Plugins/shell/contextual").get()
    except:
        return False


@interactive(name="open os shell", filter=on_filter)
@save_dir
def create_default_shell():
    """Spawns the user's shell as read from the environment variable SHELL"""
    try:
        context = GPS.current_context()
        GPS.cd(context.directory())
    except:
        pass

    if os.getenv("SHELL") and os.getenv("TERM"):
        Unix_Shell(os.getenv("SHELL"), "-i")
    elif os.getenv("COMSPEC"):
        Win32_Shell(os.getenv("COMSPEC"), "/Q")

GPS.Preference("Plugins/shell/contextual").create(
    "Contextual menu", "boolean",
    "Add contextual menu to start OS shell from project view",
    True)
