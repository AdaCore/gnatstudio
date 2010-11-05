"""Spell-checking support

This plug-in adds spell-checking capabilities.
In particular, a contextual menu is added, which should only be visible
when the user has clicked on a word inside the source editor.
It will be enabled when the "aspell" executable is visible in the PATH.
Two types of menus are provided in this plug-in:
  - Static menu: the menu will be a simple menu entry, which, when
    clicked, starts aspell and displays, in the console, the possible
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
         remembered until you either kill the aspell process in the
         task manager, or exit GPS
  - "r": Replace current word with typed-in value. The replacement
         is rechecked after insertion
  - "space": Ignore this word, and go to the next word
  - "Escape": Cancel current spell checking
  - "0-9" or "A-Z": Replace the current word with this replacement

The menus are implemented as new python classes, since this is the
cleanest way to encapsulate data in python. We could have used global
function calls instead.
This plug-in also demonstrates how to start and monitor an external
executable.
It also shows how to get the word under the cursor in GPS.
"""


###########################################################################
## No user customization below this line
###########################################################################

import re, os_utils
from text_utils import *
import traceback
import GPS

GPS.Preference ("Plugins/ispell/cmd").create (
    "Command", "string", """External command to use to spell check words.
This command should return a list of words that could replace the current
one. Recommended values are "aspell" or "ispell". Input to this command
is sent to its stdin.""", "aspell -a --lang=en")

GPS.Preference ("Plugins/ispell/bgcolor").create (
    "Background color", "color", """Background color for the command window that contains the suggested replacements""", "yellow")

GPS.Preference ("Plugins/ispell/menutype").create (
    "Menu type", "enum", """The type of contextual menu we should use. "dynamic" only shows the possible replacements for the current word. "static" displays a single entry that spawns the spell checked for the current word.""",
    1, "static", "dynamic", "none");

ispell = None
static = None
dynamic = None

def on_pref_changed (h):
   global ispell_command, background_color, contextual_menu_type
   global ispell, static, dynamic

   ispell_command       = GPS.Preference ("Plugins/ispell/cmd").get()
   background_color     = GPS.Preference ("Plugins/ispell/bgcolor").get()
   contextual_menu_type = GPS.Preference ("Plugins/ispell/menutype").get()

   # Activate the module if the ispell command is available. If the user
   # changes it to something invalid, we cannot hide everything, so we keep
   # the menus

   if not ispell \
     and os_utils.locate_exec_on_path (ispell_command.split()[0]) != "":
      GPS.Logger ("ISPELL").log ("initialize ispell module " + ispell_command)
      GPS.Hook ("before_exit_action_hook").add (before_exit)

      ispell = Ispell()
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

    <submenu after="Selection">
      <title>/Edit/Spe_ll Check</title>
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

   # Update the command

   if ispell and ispell_command != ispell.cmd:
      GPS.Logger ("ISPELL").log ("command changed, restart process")
      ispell.kill()
      ispell = Ispell()

   # Activate the right kind of contextual menu

   if ispell:
      if contextual_menu_type == "static":
         GPS.Logger ("ISPELL").log ("Activate static contextual menu")
         if dynamic: dynamic.hide()
         if not static: static = Static_Contextual()
         else: static.show()

      elif contextual_menu_type == "dynamic":
         GPS.Logger ("ISPELL").log ("Activate dynamic contextual menu")
         if static: static.hide()
         if not dynamic: dynamic = Dynamic_Contextual()
         else: dynamic.show()

      else:
         if static: static.hide()
         if dynamic: dynamic.hide()

class Ispell:
   """Interface to ispell. This takes care of properly starting a process,
      monitoring it for unexpected termination, and generally interactive with
      it"""

   def __init__ (self):
      global ispell_command
      self.proc = None
      self.cmd = ispell_command
      self.personal_dict_modified=False

   def before_kill (self, proc, output):
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
        try:
           ## Do not display the process in the task manager, since it will run
           ## forever in any case.
           self.proc = GPS.Process \
              (self.cmd, before_kill=self.before_kill, task_manager=False)
           result = self.proc.expect ("^.*\\n", timeout=2000)
        except:
           GPS.Console ("Messages").write ("Could not start external command: " + self.cmd + "\n")

   def read (self, words):
      """Run ispell to find out the possible correction for the words.
         Ispell runs forever, waiting for words to check on its
         standard input.
         Note the use of a timeout in the call to expect(). This is so that if
         for some reason ispell answers something unexpected, we don't keep
         waiting for ever"""

      attempt = 0
      while attempt < 2:
         self.restart_if_needed()
         if not self.proc:
            GPS.Logger ("ISPELL").log ("process not started " + self.cmd)
            return ""

         # Always prepend a space, to protect special characters at the
         # beginning of words that might be interpreted by aspell. This
         # means we'll have to adjust indexes in parse()

         self.proc.send (" " + words)

         # output of aspell ends with an empty line, but includes
         # multiple blank lines
         result = self.proc.expect ("^[\\r\\n]+", timeout=2000)
         if result != None: break
         attempt = attempt + 1
         self.proc.kill()
         self.proc = None

      try:
         return result.strip(" \r\n")
      except:
         GPS.Console ("Messages").write ("Error while parsing ispell=" + result + "\n")

   def parse (self, words):
      """Parse the output from ispell, and returns a list of all possible
         replacements. This returns a list of tuples, where each element has
         the following contents ["mispelled", index, ["word1", "word2"]],
         where index is the index in words of the mispelled word"""

      result = []
      try:
        output = self.read (words)
        for line in output.splitlines():
          if line and line[0] == '&':
              colon = line.find (":")
              pos   = line [:colon].split()
              # Adjust index with -1, since we added a space at the beginning
              result.append ((pos[1], int (pos[-1]) - 1 ,
                             line[colon + 2:].replace (" ", "").split(",")))
      except:
         GPS.Logger ("ISPELL").log (traceback.format_exc())
      return result

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

   buffer = GPS.EditorBuffer.get ()
   view = buffer.current_view()
   cursor = view.cursor()

   start  = goto_word_start (cursor, underscore_is_word=False)
   cursor = goto_word_end (cursor, underscore_is_word=False)

   context.ispell_module_start = start
   context.ispell_module_end   = cursor
   context.ispell_module_word  = buffer.get_chars(start, cursor)

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
      # Needed to initialize ispell external process, we do not want to
      # initialize it while creating the contextual menu as it would take
      # too much time. This initialization time badly interact with the
      # contextual menu.
      ispell.restart_if_needed()

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
      list = ispell.parse (context.ispell_module_word)

      if list:
         return list[0][2]
      else:
         return []

   @with_save_excursion
   def on_activate (self, context, choice, choice_index):
      context.ispell_module_start.buffer().delete (
         context.ispell_module_start, context.ispell_module_end)
      context.ispell_module_start.buffer().insert (
         context.ispell_module_start, choice)

####################################
### Spell check paragraphs and buffers
####################################

class SpellCheckBuffer (GPS.CommandWindow):
   """Spell check all the words in a buffer that belong to a specific category.
      The user is asked interactively for possible replacements"""

   def __init__ (self, category, buffer = None):
      if not buffer: buffer = GPS.EditorBuffer.get()
      self.block = BlockIterator (buffer, category)
      self.replace_mode = False
      self.line_iter = None
      self.has_window = False
      self.replacement = None
      self.current_line_start = 0
      self.current_replace = 0
      self.index_adjust = 0
      self.local_dict = dict()
      self.next_with_error_or_destroy()

   @with_save_excursion
   def do_replace (self, buffer, start, stop, old, new):
      self.index_adjust = len (new) - len (old) + self.index_adjust
      buffer.delete (start, stop)
      buffer.insert (start, new)

   def create_window (self):
      """Create the command window if necessary. This is only created when
         needed, to avoid a flicker if the buffer contains no mispelled word.
         Also that gives the impression that ispell starts faster, since we
         wait before the window is displayed, not after it has been displayed"""
      if not self.has_window:
         GPS.CommandWindow.__init__ \
           (self, prompt="(i,a,r,space)", on_key=self.on_key,
            on_activate=self.on_activate, close_on_activate=False)
         self.set_background (background_color)
         self.has_window = True
      else:
         self.set_prompt ("(i,a,r,space")

   def next (self):
      """Move to the next line to analyze"""

      # First try to move to next mispelled word on line
      # We also need to handle a local dictionary, since the user might
      # have added some words in the dictionary since the beginning of the
      # block, and since we have computed the replacement
      self.current_replace = self.current_replace + 1
      while self.replacement and self.current_replace < len (self.replacement):
         if self.replacement[self.current_replace][0] not in self.local_dict:
           return
         self.current_replace = self.current_replace + 1

      while True:
        try:
          # Else try to move to next line of current block
          if not self.line_iter : raise StopIteration
          line = self.line_iter.next ()
          text = line[0].buffer().get_chars (line[0], line[1])
          self.current_line_start = line[0]
          self.local_dict         = dict() # Dict already known by aspell
          self.replacement        = ispell.parse (text)
          self.current_replace    = 0
          self.index_adjust       = 0

          # If there is a mispelled word on this line, process it
          if self.replacement:
            return
        except StopIteration:
          # Finally move to next block (and loop for first line of this block)
          block = self.block.next ()
          self.line_iter = LineIterator (block[0], block[1])

   def next_with_error_or_destroy (self):
      """Store the location of the next word with spelling errors.
         Raise StopIteration when there are no more words"""
      global ispell

      try:
         self.next ()
         (word, index, replace) = self.replacement [self.current_replace]
         suggest = ""
         for p in range (len(replace)):
           if p <= 9:
              key=`p`
           else:
              key=chr (ord('A') + p - 10)
           suggest=suggest + "[" + key + "]" + replace[p] + " "

         start = self.current_line_start + index + self.index_adjust
         stop  = start + len (word) - 1
         self.current_line_start.buffer().select (start, stop + 1)
         self.create_window()
         self.write (suggest, 0)

      except StopIteration:
         if self.has_window:
            self.destroy()
            self.has_window = False
      except:
        GPS.Logger ("ISPELL").log (traceback.format_exc())
        try:
           if self.has_window:
             self.destroy()
             self.has_window = False
        except:
           pass  # already destroyed

   def on_activate (self, input):
      if self.replace_mode:
         (word, index, replace) = self.replacement [self.current_replace]
         buffer = self.current_line_start.buffer()
         start  = self.current_line_start + index + self.index_adjust
         stop   = start + len (word) - 1

         self.do_replace (buffer, start, stop, word, input)

         self.replace_mode = False
         self.next_with_error_or_destroy()

   def on_key (self, input, key, cursor_pos):
      global ispell
      if self.replace_mode:
         return False
      else:
         (word, index, replace) = self.replacement [self.current_replace]
         buffer = self.current_line_start.buffer()
         start  = self.current_line_start + index + self.index_adjust
         stop   = start + len (word) - 1

         repl = None
         key = key.replace ("shift-", "")
         if key == "i":
            ispell.add_to_dict (word)
            self.local_dict[word] = True
         elif key == "a":
            ispell.ignore (buffer.get_chars (start, stop))
            self.local_dict[word] = True
         elif key == "r":
            self.write ("")
            self.replace_mode = True
            return True
         elif key.isdigit():
            try: repl = replace[int (key)]
            except IndexError: return True
         elif key.isupper():
            try: repl = replace [ord(key) - ord('A') + 10]
            except IndexError: return True
         elif key == "Escape":
            self.destroy()
            return True
         elif key == "space":
            pass
         else:
            # Invalid key, wait for valid one
            return True

         if repl:
            self.do_replace (buffer, start, stop, word, repl)

         # Move to next word
         self.next_with_error_or_destroy()
         return True

GPS.Hook ("preferences_changed").add (on_pref_changed)
