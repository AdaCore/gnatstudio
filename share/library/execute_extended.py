"""Provides an action that allows you to interactively execute another action

   This is similar to Emacs' mini-buffer behavior (along with the M-x shortcut)

   If you press <tab> while in the command window, GPS will complete
   the command name with the longuest possible string. Completion is
   case insensitive
"""


############################################################################
## No user customization below this line
############################################################################

action_name = "execute extended command"

from GPS import *

Preference ("Plugins/execute_extended/bgcolor").create (
  "Background color","color",
  """Background color of the command window""",
  "yellow")

Preference ("Plugins/execute_extended/completions").create (
  "Show completions", "boolean",
  """If enabled, GPS will display the list of possible completions in the command window when you press <tab>. If disabled, it will only complete the current command as much as possible, but give no hint of valid completions.""",
  True)

def on_gps_started (hook_name):
   parse_xml ("""
   <action name='""" + action_name + """' output="none">
      <description>This action asks interactively for the name of an action to execute, and execute it. This allows you to execute any GPS action even if no menu and no key shortcut exists for it</description>
      <shell lang="python">if $repeat == 1: execute_extended.Extended_Command ($remaining + 1)</shell>
   </action>
  """)

def findcommonstart(strlist):
  return strlist[0][:([min([x[0]==elem for elem in x]) \
                 for x in zip(*strlist)]+[0]).index(0)]

def remove_completion (input):
  brace = input.find ('{')
  if brace == -1:
     return input
  else:
     brace2 = input.find ('}')
     return input[:brace] + input [brace2 + 1:]

class Extended_Command (CommandWindow):
  """This class provides a command window in which the user can type the name
     of any GPS command to execute in the current context"""

  def __init__ (self, repeat_count=1):
    try:
       CommandWindow.__init__ (self,
                               global_window = True,
                               prompt  = "Action:",
                               on_key  = self.on_key,
                               on_activate = self.on_activate)
       self.set_background (Preference ("Plugins/execute_extended/bgcolor").get())
       self.actions = lookup_actions()
       self.locked  = False
       self.repeat_count = repeat_count
    except:
       pass

  def on_activate (self, input):
    if input != "":
       input = remove_completion (input)
       for r in range (1, self.repeat_count + 1):
          execute_action (input)

  def on_key (self, input, key, cursor_pos):
    if key.lower() == "tab":
       input = remove_completion (input.lower())
       match = filter (lambda x: x.startswith (input), self.actions)
       if match != []:
          match.sort ()
          completions = ""
          prefix = findcommonstart (match)
          if Preference ("Plugins/execute_extended/completions").get():
             for m in match:
                if completions != "": completions = completions + ","
                completions = completions + m[len(prefix):]
             if completions != "": completions = "{" + completions + "}"

          self.write (prefix + completions, cursor=len(prefix))
       return True
    else:
       self.write (remove_completion (input))

Hook ("gps_started").add (on_gps_started)
