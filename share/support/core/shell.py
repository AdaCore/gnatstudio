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
import os_utils
from gs_utils import save_dir, make_interactive, interactive
from gs_utils.console_process import ANSI_Console_Process, Console_Process


class Unix_Shell(ANSI_Console_Process):
    def __init__(self, command):
        oldterm = os.environ["TERM"]
        os.environ["TERM"] = "xterm"
        os.environ["GPSSHELL"] = "1"
        ANSI_Console_Process.__init__(self, command)
        if GPS.Preference("External Commands:Shell/stty").get():
            self.send(
                'stty echo; PS1="\[\e[1G\]$PS1";'
                ' echo -en "\\e[1;"$(stty size | cut -d" " -f1)'
                '";1;"$(stty size | cut -d" " -f2)"r";'
                " clear"
            )
        os.environ["TERM"] = oldterm


class Win32_Shell(Console_Process):
    def __init__(self, command):
        os.environ["GPSSHELL"] = "1"
        Console_Process.__init__(self, command)

    def on_completion(self, input):
        self.add_input("\t")

def on_label(context):
    # If the context has directory information, use that
    dir = get_directory(GPS.current_context())

    if dir is not None:
        return "Run OS shell in <b>%s</b>" % (
            os.path.basename(os.path.dirname("%s/" % dir))
        )
    else:
        # Otherwise open in the current directory
        return "Run OS shell in <b>%s</b>" % (
            os.path.basename(os.path.dirname("%s/" % os.getcwd()))
        )


@interactive(name="open os shell")
@save_dir
def create_default_shell():
    """Spawns the user's shell as read from the environment variable SHELL"""

    def __error_msg(variables):
        """
        :type variables: [name]
        """
        msg = (
            "Can't start OS shell, the following environment variables"
            + " need to be set: "
        )
        msg += ", ".join(
            '%s (currently "%s")' % (var, str(os.getenv(var))) for var in variables
        )
        msg += "\n"
        GPS.Console("Messages").write(msg, mode="error")

    dir = get_directory(GPS.current_context())

    if dir is not None:
        GPS.cd(dir)

    if os.name == "nt":
        if os.getenv("COMSPEC"):
            Win32_Shell([os.getenv("COMSPEC"), "/Q"])
        else:
            __error_msg(["COMSPEC"])

    else:
        if os.getenv("TERM"):
            if os.getenv("SHELL"):
                Unix_Shell([os.getenv("SHELL"), "-i"])
            else:
                __error_msg(["SHELL"])
                # For Linux, try to start the bash in the path as a fallback
                bash_exe = os_utils.locate_exec_on_path("bash")
                if bash_exe:
                    GPS.Console("Messages").write(
                        "Retrying with SHELL=%s\n" % str(bash_exe)
                    )
                    Unix_Shell([str(bash_exe), "-i"])
        else:
            __error_msg(["TERM", "SHELL"])


def get_directory(context):
    """Get directory from the context or from project file"""
    dir = None

    try:
        dir = context.directory()
    except Exception:
        pass

    if dir is None:
        try:
            dir = context.project().file().directory()
        except Exception:
            pass

    return dir


def if_has_directory(context):
    return get_directory(context) is not None


GPS.Preference("External Commands:Shell/contextual").create(
    "Contextual menu",
    "boolean",
    "Add contextual menu to start OS shell from project view " "(needs to restart GPS)",
    False,
)
GPS.Preference("External Commands:Shell/stty").create(
    "Send stty setup",
    "boolean",
    "Send the 'stty echo' command automatically. This command "
    "is needed in some shells to see the keys typed on the keyboard. "
    "This is only applicable to Unix shells.",
    True,
)

if GPS.Preference("External Commands:Shell/contextual").get():
    make_interactive(
        create_default_shell,
        name="open os shell for contextual menu",
        contextual=on_label,
        filter=if_has_directory,
    )
