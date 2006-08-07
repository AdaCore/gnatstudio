"""Spell-checking support

This script demonstrates how to add a spell-checking contextual menu.
This menu should only be visible when the user has clicked on a word
inside the source editor.
It requires that the "ispell" executable be visible in the PATH.
Two types of menus are provided in this example:
  - Static menu: the menu will be a simple menu entry, which, when
    clicked, starts ispell and displays, in the console, the possible
    replacements for a word
  - Dynamic menu: a submenu is created, with one entry per possible
    replacement. When the user selects one of these entries, the current
    word is replaced

The menus are implemented as new python classes, since this is the
cleanest way to encapsulate data in python. We could have used global
function calls instead.
This example also demonstrates how to start and monitor an external
executable.
It also shows how to get the word under the cursor in GPS.
"""

############################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

ispell_command = "ispell -S -a"
# Command to use to start a process to which words can be sent on standard
# input. The process is expected to return a list of words that could
# replace the current one

use_static_menu = False
# Whether we should use the static menu, as described above, or the
# dynamic menu


###########################################################################
## No user customization below this line
############################################################################

import GPS, re

ispell = None

class Ispell:
   """Interface to ispell. This takes care of properly starting a process,
      monitoring it for unexpected termination, and generally interactive with
      it"""

   def __init__ (self):
      self.proc = None

   def on_exit (self, proc, exit_status, output):
      """Called the ispell process has terminated"""
      self.proc = None
 
   def read (self, word):
      """Run ispell to find out the possible completions for the word stored in
         the context. Ispell runs forever, waiting for words to check on its
         standard input.
         Note the use of a timeout in the call to expect(). This is so that if
         for some reason ispell answers something unexpected, we don't keep
         waiting for ever"""

      if self.proc == None:
         ## Do not display the process in the task manager, since it will run
         ## forever in any case.
         self.proc = GPS.Process (ispell_command, on_exit=self.on_exit, task_manager=False)
         result = self.proc.expect ("^.*", timeout=2000)

      self.proc.send (word + "\n")
      result = self.proc.expect ("^[&#\*\+\?\-].*", timeout=2000)
      try:
         return re.compile ("^([&#*+?-].*)", re.M).search (result).group(1)
      except:
         GPS.Console ("Messages").write ("Error while parsing ispell=" + result + "\n")

   def parse (self, word):
      """Parse the output from ispell, and returns a list of all possible
         replacements"""

      result = self.read (word)
      if result[0] == '&':
         colon = result.find (":")
         result = result[colon + 2 :].replace (" ","")
         return result.split (",") 
      else:
         return []

   def kill (self):
      if self.proc != None:
         self.proc.kill ()
         self.proc = None

def before_exit (hook):
   """Kill the ispell process when GPS exits.
      We wouldn't get a dialog asking whether the process should be killed,
      since the call to GPS.Process specified that it shouldn't appear in the
      task manager. However, it is still cleaner to properly kill it"""
   global ispell
   if ispell != None:
      ispell.kill()
   return True

def find_current_word (context):
   """Stores in context the current word start, end and text.
      This is called only when computing whether the menu should be
      displayed, and stored in the contextual menu for efficiency, since
      that means we won't have to recompute the info if the user selects
      the menu"""
 
   view = GPS.EditorBuffer.get().current_view()
   cursor = view.cursor()

   start = cursor
   while not start.starts_word():
      start = start.forward_char(-1)

   while not cursor.ends_word():
      cursor = cursor.forward_char(1)

   context.ispell_module_start = start
   context.ispell_module_end   = cursor 
   context.ispell_module_word  = view.buffer().get_chars(start, cursor)

###################################
### Static contextual menu
### The following subprograms deal with the static contextual menu, as
### described in the header of this file.
### It shows how the title of the menu entry can be changed dynamically
### based on the context 
###################################

class Static_Contextual (GPS.Contextual):
   def __init__ (self):
      """Create a new static contextual menu for spell checking"""
      GPS.Contextual.__init__ (self, "Spell Check Static") 
      self.create (on_activate = self.on_activate,
                   filter      = self.filter,
                   label       = self.label)

   def on_activate (self, context): 
      """Display in the console the message read from ispell"""
      GPS.Console ("Messages").write \
         ("Ispell: " + ispell.read (context.ispell_module_word) + "\n")

   def filter (self, context):
      """Decide whether the contextual menu should be made visible"""
      if isinstance (context, GPS.EntityContext):
         find_current_word (context)
         return context.ispell_module_word != ""
      else:
         return False

   def label (self, context):
      """Return the label to use for the contextual menu entry"""
      return "Spell Check " + context.ispell_module_word

###################################
### Dynamic contextual menu
### The following functions create a dynamic contextual submenu, which lists
### all the possible replacement texts. Clicking on any of them will replace
### the current word
###################################

class Dynamic_Contextual (GPS.Contextual):
   def __init__ (self):
      """Create a new dynamic contextual menu for spell checking"""
      GPS.Contextual.__init__ (self, "Spell Check")
      self.create_dynamic (on_activate = self.on_activate,
                           filter      = self.filter,
                           factory     = self.factory)

   def filter (self, context):
      """Decide whether the contextual menu should be made visible"""
      if isinstance (context, GPS.EntityContext):
         find_current_word (context)
         return context.ispell_module_word != ""
      else:
         return False

   def factory (self, context):
      """Return a list of strings, each of which is the title to use for an
         entry in the dynamic contextual menu"""
      return ispell.parse (context.ispell_module_word)

   def on_activate (self, context, choice, choice_index):
      context.ispell_module_start.buffer().delete (context.ispell_module_start, context.ispell_module_end)
      context.ispell_module_start.buffer().insert (context.ispell_module_start, choice)



def on_gps_started (hook_name):
   ispell = Ispell()
   if use_static_menu:
      static = Static_Contextual ()
   else:
      dynamic = Dynamic_Contextual() 

GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("gps_started").add (on_gps_started)


