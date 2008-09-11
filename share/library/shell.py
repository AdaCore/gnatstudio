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

class Ansi_Console (GPS.Console):
  """A console that hides ANSI escape sequences"""

  ansi_re = re.compile (r'\033\[\d*(;\d+)*[HfABCDRsuJKm]')

  def write (self, txt):
     txt = re.sub (Ansi_Console.ansi_re, "", txt)
     GPS.Console.write (self, txt)

class Unix_Shell (Ansi_Console, Console_Process):
  def on_output (self, matched, unmatched):
    Console_Process.on_output (self, matched, unmatched)

  def __init__ (self, process, args=""):
    os.putenv ("EMACS", "t") # Emulate whas emacs does, since some shells
                             # rely on this (zsh)
    Console_Process.__init__ (self, process, args)

def create_default_shell (menu):
  """Spawns the user's shell as read from the environment variable SHELL"""
  if os.getenv ("SHELL"):
    Unix_Shell (os.getenv ("SHELL"), "-i")
  elif os.getenv ("COMSPEC"):
    Unix_Shell (os.getenv ("COMSPEC"))

GPS.Menu.create ("/Tools/Consoles/OS Shell", create_default_shell)

