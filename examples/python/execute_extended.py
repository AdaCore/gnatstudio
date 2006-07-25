## This file provides an action that allows you to execute
## any other GPS action by typing its name. This is similar to
## Emacs' mini-buffer behavior (along with the M-x shortcut)
##
## If you press <tab> while in the command window, GPS will complete
## the command name with the longuest possible string. Completion is
## case insensitive

from GPS import *

parse_xml ("""
   <action name="execute extended command" output="none">
      <description>This action asks interactively for the name of an action to execute, and execute it. This allows you to execute any GPS action even if no menu and no key shortcut exists for it</description>
      <shell lang="python">execute_extended.Extended_Command ()</shell>
   </action>

   <key action="execute extended command">alt-x</key>
  """)

def smaller_than (x, y):
  """Compare the length of two strings. This is used as a sorting criteria"""
  return len (x) < len (y)

class Extended_Command (CommandWindow):
  """This class provides a command window in which the user can type the name
     of any GPS command to execute in the current context"""

  def __init__ (self):
    try:
       CommandWindow.__init__ (self,
                               global_window = True,
                               prompt  = "Action:",
                               on_key  = self.on_key,
                               on_activate = self.on_activate)
       self.actions = lookup_actions()
    except:
       pass

  def on_activate (self, input):
    if input != "":
       execute_action (input)

  def on_key (self, input, key, cursor_pos):
    if key.lower() == "tab":
       input = input.lower()
       match = filter (lambda x: x.startswith (input), self.actions)
       if match != []:
          match.sort (smaller_than)
          if match[0] != input:
             self.write (match [0])
       return True

        
        
     
