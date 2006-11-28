"""Processes a text selection through an external shell command, and
   substitute it with the output of that command.

   This is similar to vi's ! command. For instance, you can use this
   script to run a select chunk of text through the following shell
   commands:
      - "fmt"  => Reformat each paragraph of the selection, using
                  advanced algorithms that try not to break after the first
                  word of a sentence, or before the last. Also try to
                  balance line lengths.
                  See the function fmt_selection() below, which automatically
                  sets a number of parameters when calling this function.

      - "sort" => Sort the selected lines
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

background_color = "yellow"


############################################################################
## No user customization below this line
############################################################################

from GPS import *

def process_through_shell (command, buffer=None):
   """Process the current selection in BUFFER through COMMAND,
      and replace that selection with the output of the command"""
   if not buffer:
      buffer = EditorBuffer.get()
   start  = buffer.selection_start()
   end    = buffer.selection_end()

   # Ignore white spaces and newlines at end, to preserve the rest
   # of the text
   while end.get_char() == ' ' or end.get_char() == '\n':
     end = end - 1

   text = buffer.get_chars (start, end)
   proc = Process (command)
   proc.send (text)
   proc.send (chr (4))  # Close input
   output = proc.get_result()
   buffer.start_undo_group()
   buffer.delete (start, end)
   buffer.insert (start, output.rstrip())
   buffer.finish_undo_group()

def fmt_selection ():
  """Format the current selection through fmt"""
  width  = Preference ("Src-Editor-Highlight-Column").get()
  buffer = EditorBuffer.get()
  prefix = None

  if buffer.file().language() == "ada":
     prefix = "--"

  loc = buffer.selection_start().beginning_of_line()
  while loc.get_char() == ' ':
     loc = loc + 1
    
  prefix = '-p """' + (' ' * (loc.column() - 1)) + prefix + '"""' 
  process_through_shell ("fmt " + prefix + " -w " + `width`, buffer)

class ShellProcess (CommandWindow):
   """Send the current selection to an external process,
      and replace it with the output of that process"""
    
   def __init__ (self):
      CommandWindow.__init__ (self, global_window = True,
                              prompt = "Shell command:",
                              on_activate = self.on_activate)
      self.set_background (background_color)

   def on_activate (self, shell_command):
      process_through_shell (shell_command)

def interactive_process_through_shell (menu):
   ShellProcess()

def on_gps_started (hook):
   Menu.create ("/Edit/Process through shell",
                ref = "Create Bookmark",
                on_activate=interactive_process_through_shell)

parse_xml ("""
  <action name="Process through shell">
     <description>Process the current selection through a shell command,
        and replace it with the output of that command.</description>
     <filter id="Source editor" />
     <shell lang="python">external.ShellProcess()</shell>
  </action>

  <action name="Fmt selection">
     <description>Process the current selection through the "fmt" command
        to reformat paragraphs</description>
     <filter id="Source editor" />
     <shell lang="python">external.fmt_selection()</shell>
  </action>
""")

Hook ("gps_started").add (on_gps_started)

