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
checking on the whole buffer, or only the comments.

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


use_static_menu = False
# Whether we should use the static menu, as described above, or the
# dynamic menu

background_color = "yellow"
# Background color for the command window when checking a whole buffer


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


####################################
### Spell check paragraphs and buffers
####################################

class BlockIterator:
   """An iterator for all comment blocks. Each iteration returns a
      tuple (start, end) of EditorLocation"""
   def __init__ (self, buffer, overlay_name):
      self.mark    = buffer.beginning_of_buffer ().create_mark()
      if overlay_name != "" and overlay_name != "selection":
         self.overlay = buffer.create_overlay (overlay_name)
         self.in_comment = buffer.beginning_of_buffer().has_overlay (self.overlay)
      else:
         self.overlay = None
         self.overlay_name = overlay_name
   def __iter__ (self):
      return self
   def next (self):
      loc = self.mark.location ()
      if not self.overlay:
        if loc < loc.buffer().end_of_buffer():
           self.mark.move (loc.buffer().end_of_buffer())
           if self.overlay_name == "selection":
              return (loc.buffer().selection_start(),
                      loc.buffer().selection_end())
           else:
              return (loc.buffer().beginning_of_buffer(),
                      loc.buffer().end_of_buffer())
      else:
        while loc < loc.buffer().end_of_buffer():
           loc2 = loc.forward_overlay (self.overlay)
           self.in_comment = not self.in_comment
           if not self.in_comment:
             # Use a mark, in case the buffer is modified between iterations
             self.mark.move (loc2 + 1)
             return (loc, loc2 - 1)
           else:
             loc = loc2
      raise StopIteration

class WordIterator:
   """An iterator for all words in a block. Each iteration returns a
      tuple (start, end) of EditorLocation"""
   def __init__ (self, start, end):
      self.mark = start.create_mark()
      self.end  = end
   def __iter__ (self):
      return self
   def next (self):
      loc = self.mark.location () 
      while loc < self.end:
         loc2 = loc.forward_word ()
         if loc.get_char().isalpha():
            # Use a mark, in case the buffer is modified
            self.mark.move (loc2 + 1)
            return (loc, loc2 - 1)
         else:
            loc = loc + 1
      raise StopIteration

class SpellCheckBuffer (GPS.CommandWindow):
   """Spell check all the comments in a buffer.
      The user is asked interactively for possible replacements"""

   def __init__ (self, category, buffer = None):
      GPS.CommandWindow.__init__ (self, on_key=self.on_key)
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
           for p in range (min (9, len (replace))):
              suggest=suggest + "[" + `p` + "]" + replace[p] + " "
           word[0].buffer().select (word[0], word[1] + 1)
           self.write (suggest, 0)
           self.word    = word
           self.replace = replace
           return

   def on_key (self, input, key, cursor_pos):
      # Do the replacement
      if key.isdigit():
         buffer = self.word[0].buffer()
         buffer.delete (self.word[0], self.word[1])
         buffer.insert (self.word[0], self.replace[int (key)])

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
   if use_static_menu:
      static = Static_Contextual ()
   else:
      dynamic = Dynamic_Contextual() 

   GPS.parse_xml ("""
     <action name="spell check comments" category="Editor" output="none">
      <description>Check the spelling for all comments in the current editor</description>
      <filter id="Source editor"/>
      <shell lang="python">ispell.SpellCheckBuffer ("comments")</shell>
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
    </submenu>""")

GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("gps_started").add (on_gps_started)


