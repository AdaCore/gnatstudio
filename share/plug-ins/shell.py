"""This plug-in will spawn a Unix/Windows shell console in your GPS

   When spawning zsh, it is recommended that your .zshrc file contains
   the following:
      [[ $EMACS = t ]] && unsetopt zle
   This prevents zsh from duplicating its input to the output, as well
   as some missynchronization between GPS and zsh after a while

   This plugin will filter out ANSI sequences that your shell might be
   writting (for instance colors). These are currently not supported by
   GPS)

"""


import GPS, re, traceback, os
from gps_utils.console_process import *

class Unix_Shell (ANSI_Console_Process):
  def __init__ (self, process, args=""):
    oldterm = os.environ["TERM"]
    os.environ["TERM"] = "xterm"
    ANSI_Console_Process.__init__ (self, process, args)
    os.environ["TERM"] = oldterm

def create_default_shell (menu):
  """Spawns the user's shell as read from the environment variable SHELL"""
  if os.getenv ("SHELL"):
    Unix_Shell (os.getenv ("SHELL"), "-i")
  elif os.getenv ("COMSPEC"):
    Unix_Shell (os.getenv ("COMSPEC"))

GPS.Menu.create ("/Tools/Consoles/_OS Shell", create_default_shell, ref="Auxiliary Builds", add_before=1)

