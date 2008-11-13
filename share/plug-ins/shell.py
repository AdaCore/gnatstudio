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

class Unix_Shell (Console_Process):
  def on_input (self, input):
    # Do nothing, this was already handled when each key was pressed
    pass

  def on_completion (self, input):
    # Do nothing, this was already handled when each key was pressed
    pass

  def on_key (self, keycode, key, modifier):
    if keycode == Console_Process.Key_Return:
       self.send ("\r", add_lf=False)
    elif keycode == Console_Process.Key_Tab:
       self.send ("\t", add_lf=False)
    elif keycode == Console_Process.Key_Backspace:
       self.send (chr(8), add_lf=False)
    elif key != 0:
       self.send (unichr (key).encode ("utf8"), add_lf=False)
    elif keycode == Console_Process.Key_Left:
       self.send ("\033[D", add_lf=False)
    elif keycode == Console_Process.Key_Right:
       self.send ("\033[C", add_lf=False)
    elif keycode == Console_Process.Key_Up:
       self.send ("\033[A", add_lf=False)
    elif keycode == Console_Process.Key_Down:
       self.send ("\033[B", add_lf=False)
    else:
       GPS.Logger ("UNIX").log (`keycode` + " " + `modifier` + " " + `key`)
       return False
    return True

  def __init__ (self, process, args=""):
    os.putenv ("EMACS", "t") # Emulate whas emacs does, since some shells
                             # rely on this (zsh)
    os.environ["TERM"] = "xterm"
    Console_Process.__init__ (self, process, args, force = True,
                              ansi = True, manage_prompt = False)

def create_default_shell (menu):
  """Spawns the user's shell as read from the environment variable SHELL"""
  if os.getenv ("SHELL"):
    Unix_Shell (os.getenv ("SHELL"), "-i")
  elif os.getenv ("COMSPEC"):
    Unix_Shell (os.getenv ("COMSPEC"))

GPS.Menu.create ("/Tools/Consoles/_OS Shell", create_default_shell, ref="Auxiliary Builds", add_before=1)

