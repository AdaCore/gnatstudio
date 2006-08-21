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

This script also adds a new menu /Edit/Spell Check, which runs spell
checking on the whole buffer, or only the comments. In this mode, the
keys are recognized:
  - "i": Accept the current word in your personal dictionnary, to be
         remembered across sessions
  - "a": Accept this word during this session. This setting will be
         remembered until you either kill the ispell process in the
         task manager, or exit GPS
  - "space": Ignore this word, and go to the next word
  - "Escape": Cancel current spell checking
  - "0-9" or "A-Z": Replace the current word with this replacement

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

#ispell_command = "ispell -S -a"
ispell_command = "aspell -a --lang=en "
# Command to use to start a process to which words can be sent on standard
# input. The process is expected to return a list of words that could
# replace the current one.
#   -S => Sort the list of guesses by probable correctness
#   -a => read from stdin, until pipe is closed


contextual_menu_type = "dynamic"
# What type of contextual menu we should use. Value can be:
#   "dynamic", "static", or "none"
# See above for a description of the various types

background_color = "yellow"
# Background color for the command window when checking a whole buffer


###########################################################################
## No user customization below this line
############################################################################

import GPS, re
from text_utils import *

ispell = None

class Ispell:
   """Interface to ispell. This takes care of properly starting a process,
      monitoring it for unexpected termination, and generally interactive with
      it"""

   def __init__ (self):
      self.proc = None
      self.personal_dict_modified=False

   def on_exit (self, proc, exit_status, output):
      """Called when the ispell process has terminated"""
      self.save_personal_dict ()
      self.proc = None

   def save_personal_dict (self):
      if self.personal_dict_modified and self.proc:
        if GPS.MDI.yes_no_dialog ("Spell-checking personal dictionary modified. Save ?"):
          self.proc.send ("#")

          # Make sure the dict is saved: since ispell doesn't show any output,
          # we generate some
          self.read ("GPS")

          self.personal_dict_modified = False

   def restart_if_needed (self):
      if self.proc == None:
        ## Do not display the process in the task manager, since it will run
        ## forever in any case.
        self.proc = GPS.Process \
           (ispell_command, on_exit=self.on_exit, task_manager=False)
        result = self.proc.expect ("^.*", timeout=2000)

   def read (self, word):
      """Run ispell to find out the possible completions for the word stored in
         the context. Ispell runs forever, waiting for words to check on its
         standard input.
         Note the use of a timeout in the call to expect(). This is so that if
         for some reason ispell answers something unexpected, we don't keep
         waiting for ever"""

      attempt = 0
      while attempt < 2:
         self.restart_if_needed()
         self.proc.send (word)
         result = self.proc.expect ("^[&#\*\+\?\-].*", timeout=2000)
         if result: break
         attempt = attempt + 1
         self.proc.kill()
         self.proc = None

      try:
         return re.compile ("^([&#*+?-].*)", re.M).search (result).group(1)
      except:
         GPS.Console ("Messages").write ("Error while parsing ispell=" + result + "\n")

   def parse (self, word):
      """Parse the output from ispell, and returns a list of all possible
         replacements"""

      result = self.read (word)
      if result and result[0] == '&':
         colon = result.find (":")
         result = result[colon + 2 :].replace (" ","")
         return result.split (",") 
      else:
         return []

   def ignore (self, word):
      """Should ignore word from now on, but not add it to personal dict"""
      self.restart_if_needed()
      self.proc.send ("@" + word)

   def add_to_dict (self, word):
      """Add word to the user's personal dictionary"""
      self.restart_if_needed()
      self.proc.send ("*" + word)
      self.personal_dict_modified=True

   def kill (self):
      if self.proc != None:
         self.save_personal_dict ()
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


####################################
### Spell check paragraphs and buffers
####################################

class SpellCheckBuffer (GPS.CommandWindow):
   """Spell check all the comments in a buffer.
      The user is asked interactively for possible replacements"""

   def __init__ (self, category, buffer = None):
      GPS.CommandWindow.__init__ (self, prompt="(i,a,space)",
                                  on_key=self.on_key)
      self.set_background (background_color)
      if not buffer: buffer = GPS.EditorBuffer.get()
      self.word_iter = None
      self.comment = BlockIterator (buffer, category)
      try:
        self.next_with_error()
      except StopIteration:
        self.destroy()

   def next (self):
      """Move to next word to analyze. Raise StopIteration at the end.
         Returns a tuple (word_start, word_end) on success"""
      while True:
         try:
           if not self.word_iter: raise StopIteration
           return self.word_iter.next()
         except StopIteration:
           block          = self.comment.next()
           self.word_iter = WordIterator (block[0], block[1])

   def next_with_error (self):
      """Store the location of the next word with spelling errors.
         Raise StopIteration when there are no more words"""
      global ispell
      while True:
         word = self.next()
         text = word[0].buffer().get_chars (word[0], word[1])
         replace = ispell.parse (text)
         if replace:
           suggest = ""
           for p in range (len(replace)):
              if p <= 9: key=`p`
              else:      key=chr (ord('A') + p - 10)
              suggest=suggest + "[" + key + "]" + replace[p] + " "
           word[0].buffer().select (word[0], word[1] + 1)
           self.write (suggest, 0)
           self.word    = word
           self.replace = replace
           return

   def on_key (self, input, key, cursor_pos):
      global ispell
      # Do the replacement
      buffer = self.word[0].buffer()
      replace = None
      key = key.replace ("shift-", "")
      if key == "i":
         ispell.add_to_dict (buffer.get_chars (self.word[0], self.word[1]))
      elif key == "a":
         ispell.ignore (buffer.get_chars (self.word[0], self.word[1]))
      elif key.isdigit():
         try: replace = self.replace[int (key)]
         except IndexError: return True
      elif key.isupper():
         try: replace = self.replace [ord(key) - ord('A') + 10]
         except IndexError: return True
      elif key == "Escape":
         self.destroy()
         return True 
      elif key == "space":
         pass
      else:
         return True

      if replace:
         buffer.delete (self.word[0], self.word[1])
         buffer.insert (self.word[0], replace)

      # Move to next word
      try:
        self.next_with_error()
      except StopIteration:
        self.destroy()
      return True

####################################

def on_gps_started (hook_name):
   global ispell
   ispell = Ispell()

   if contextual_menu_type == "static":
      static = Static_Contextual ()
   elif contextual_menu_type == "dynamic":
      dynamic = Dynamic_Contextual() 

   GPS.parse_xml ("""
     <action name="spell check comments" category="Editor" output="none">
      <description>Check the spelling for all comments in the current editor</description>
      <filter id="Source editor"/>
      <shell lang="python">ispell.SpellCheckBuffer ("comment")</shell>
    </action>
    <action name="spell check editor" category="Editor" output="none">
      <description>Check the spelling for the whole contents of the editor</description>
      <filter id="Source editor" />
      <shell lang="python">ispell.SpellCheckBuffer ("")</shell>
    </action>
    <action name="spell check selection" category="Editor" output="none">
      <description>Check the spelling in the current selection</description>
      <filter id="Source editor" />
      <shell lang="python">ispell.SpellCheckBuffer ("selection")</shell>
    </action>
    <action name="spell check word" category="Editor" output="none">
      <description>Check the spelling of the current word</description>
      <filter id="Source editor" />
      <shell lang="python">ispell.SpellCheckBuffer ("word")</shell>
    </action>

    <submenu before="Comment Lines">
      <title>/Edit/Spell Check</title>
      <menu action="spell check comments">
         <title>Comments</title>
      </menu>
      <menu action="spell check editor">
         <title>Editor</title>
      </menu>
      <menu action="spell check selection">
         <title>Selection</title>
      </menu>
      <menu action="spell check word">
         <title>Word</title>
      </menu>
     </submenu>""")

GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("gps_started").add (on_gps_started)


