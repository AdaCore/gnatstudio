"""
This plugin will spawn a Unix/Windows shell console in your GPS.
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
in such cases. In general, this can be safely done in your .bashrc.
A preference is provided to automatically emit this command.

An environment variable GPSSHELL is set in the new shell. This can be used
in your various configuration files to enable/disable behaviors when
running inside GPS.

"""

import os.path
import GPS
import os
from gps_utils import save_dir, make_interactive, interactive
from gps_utils.console_process import ANSI_Console_Process, Console_Process


class Unix_Shell(ANSI_Console_Process):

    def __init__(self, command):
        oldterm = os.environ["TERM"]
        os.environ["TERM"] = "xterm"
        os.environ["GPSSHELL"] = "1"
        ANSI_Console_Process.__init__(self, command)
        if GPS.Preference("External Commands:Shell/stty").get():
            self.send('stty echo; PS1="\[\e[1G\]$PS1"; clear')
        os.environ["TERM"] = oldterm


class Win32_Shell(Console_Process):

    def __init__(self, command):
        os.environ["GPSSHELL"] = "1"
        Console_Process.__init__(self, command)


def on_label(context):
    # If the context has directory information, use that
    try:
        return "Run OS shell in <b>%s</b>" % (
            os.path.basename(os.path.dirname("%s/" % context.directory())))
    except:
        # Otherwise open in the current directory
        return "Run OS shell in <b>%s</b>" % (
            os.path.basename(os.path.dirname("%s/" % os.getcwd())))


@interactive(name='open os shell')
@save_dir
def create_default_shell():
    """Spawns the user's shell as read from the environment variable SHELL"""
    try:
        context = GPS.current_context()
        GPS.cd(context.directory())
    except:
        pass

    if os.getenv("SHELL") and os.getenv("TERM"):
        Unix_Shell([os.getenv("SHELL"), "-i"])
    elif os.getenv("COMSPEC"):
        Win32_Shell([os.getenv("COMSPEC"), "/Q"])


def if_has_directory(context):
    try:
        return context.directory() is not None
    except:
        return False


GPS.Preference("External Commands:Shell/contextual").create(
    "Contextual menu", "boolean",
    "Add contextual menu to start OS shell from project view "
    "(needs to restart GPS)",
    True)
GPS.Preference("External Commands:Shell/stty").create(
    "Send stty setup", "boolean",
    "Send the 'stty echo' command automatically. This command "
    "is needed in some shells to see the keys typed on the keyboard. "
    "This is only applicable to Unix shells.",
    True)

if GPS.Preference("External Commands:Shell/contextual").get():
    make_interactive(
        create_default_shell, name='open os shell for contextual menu',
        contextual=on_label, filter=if_has_directory)
