### This script demonstrates how to add a spell-checking contextual menu.
### This menu should only be visible when the user has clicked on a word
### inside the source editor.
### It requires that the "ispell" executable be visible in the PATH.
### Two types of menus are provided in this example:
###   - Static menu: the menu will be a simple menu entry, which, when
###     clicked, starts ispell and displays, in the console, the possible
###     replacements for a word
###   - Dynamic menu: a submenu is created, with one entry per possible
###     replacement. When the user selects one of these entries, the current
###     word is replaced

import GPS, re

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
   context.ispell_module_end   = cursor - 1
   context.ispell_module_word  = view.buffer().get_chars(start, cursor - 1)

def read_ispell (context):
   """Run ispell to find out the possible completions for the word stored in
      the context. Ispell runs forever, waiting for words to check on its
      standard input. Therefore, we explicitly kill it when done with it.
      A different implementation would be to start ispell once the first time
      it is needed, and then reuse it every time.
      Note the use of a timeout in the call to expect(). This is so that if
      for some reason ispell answers something unexpected, we don't keep
      waiting for ever"""

   proc = GPS.Process ("ispell -S -a")
   proc.send (context.ispell_module_word + "\n")
   result = proc.expect ("^[&#\*\+\?\-].*", timeout=2000)
   proc.kill()
   # GPS.Console ("Messages").write ("result=" + result + "\n")
   return re.compile ("^([&#*+?-].*)", re.M).search (result).group(1)

def parse_ispell (context):
   """Parse the output from ispell, and returns a list of all possible
      replacements"""

   result = read_ispell (context)
   if result[0] == '&':
      colon = result.find (":")
      result = result[colon + 2 :].replace (" ","")
      return result.split (",") 
   else:
      return []

###################################
### Static contextual menu
### The following subprograms deal with the static contextual menu, as
### described in the header of this file.
### It shows how the title of the menu entry can be changed dynamically
### based on the context 
###################################

def on_contextual (contextual):
   """Display in the console the message read from ispell"""
   context = GPS.contextual_context()
   GPS.Console ("Messages").write ("Ispell: " + read_ispell (context) + "\n")

def contextual_filter (contextual):
   """Decide whether the contextual menu should be made visible"""
   context = GPS.contextual_context()
   if isinstance (context, GPS.EntityContext):
      find_current_word (context)
      return context.ispell_module_word != ""
   else:
      return False

def contextual_label (contextual):
   """Return the label to use for the contextual menu entry"""
   context = GPS.contextual_context()
   return "Spell Check " + context.ispell_module_word

#GPS.Contextual  ("Spell Check").create \
#   (on_activate=on_contextual,
#    filter=contextual_filter,
#    label=contextual_label)

###################################
### Dynamic contextual menu
### The following functions create a dynamic contextual submenu, which lists
### all the possible replacement texts. Clicking on any of them will replace
### the current word
###################################

def build_contextual (contextual):
   """Return a list of strings, each of which is the title to use for an
      entry in the dynamic contextual menu"""
   return parse_ispell (GPS.contextual_context())

def replace_with (contextual, choice):
   context = GPS.contextual_context()
   context.ispell_module_start.buffer().delete (context.ispell_module_start, context.ispell_module_end)
   context.ispell_module_start.buffer().insert (context.ispell_module_start, choice)
  
GPS.Contextual ("Spell Check Dyn").create_dynamic \
   (on_activate =replace_with,
    factory     =build_contextual,
    filter      =contextual_filter)
