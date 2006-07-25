## This script adds an incremental-search capability to GPS, similar to
## what Emacs does.
## When you select the menu /Navigate/Find Incremental (or bind a key
## shortcut to it through the editor at /Edit/Key shortcuts), a temporary
## window is open. From then on, any character you type is displayed in
## this new window, and makes a search pattern. Whenever this pattern
## is modified, GPS will search, starting at the current location, for its
## next occurrence in the current file.
## While you are editing the pattern, there are a number of special key
## shortcuts that can be used:
##    - control-w
##      will copy the current word into the pattern, and moves the cursor
##      to the next word, so that pressing control-w multiple times in
##      a row allows you to easily copy part of a line into the pattern
##
##    - control-y
##      is similar to control-w but copies the end of the current line into
##      the pattern. If the cursor is at the end of the current line, the
##      whole next line is copied
##
##    - Key that activates the isearch
##      If you press the same key that was used to activate the incremental
##      search, GPS will jump to the next occurrence. If you press the key
##      to activate the backward incremental search, GPS will jump to the
##      stack occurrence 
##
##    - Backspace
##      Goes back to the stack location or search pattern. If you have just
##      added a character to the pattern, this character is removed. Otherwise
##      the pattern is preserved and the editor is moved back to the stack
##      location.
##
##    - Esc, movement keys, keys with control or alt
##      cancels the current search, and unselect the last occurrence found
##
## If you press <enter> while there is no search string, this module will
## automatically open the advanced, non-incremental search dialog of GPS, to
## match Emacs' behavior

from GPS import *

## Changing the name of menus should be reflected in emacs.xml
isearch_action_name = 'isearch'
isearch_menu = '/Navigate/Find Incremental'
isearch_backward_action_name = 'isearch backward'
isearch_backward_menu = '/Navigate/Find Previous Incremental'

parse_xml ("""
  <action name='""" + isearch_action_name + """' category="Editor" output="none">
     <description>This action provides an incremental search facility: once activated, each character you type is added to the search pattern, and GPS jumps to the next occurrence of the pattern</description> 
     <filter id="Source editor" />
     <shell lang="python">isearch.Isearch()</shell>
  </action>
  <action name='""" + isearch_backward_action_name + """' category="Editor"
          output="none">
     <description>This action provides a backward incremental search facility: once activated, each character you type is added to the search pattern, and GPS jumps to the stack occurrence of the pattern</description>
     <filter id="Source editor" />
     <shell lang="python">isearch.Isearch (backward=1)</shell>
  </action>

 <menu action='""" + isearch_backward_action_name + """' after="Find Or Replace...">
    <title>""" + isearch_backward_menu + """</title>
 </menu>
 <menu action='""" + isearch_action_name + """' after="Find Or Replace...">
    <title>""" + isearch_menu + """</title>
 </menu>
""")


class Isearch (CommandWindow):
   """This class provides an incremental search facility in GPS.
      When instanciated, it immediately starts executing"""

   def __init__ (self, case_sensitive=0, backward=0, regexp=0):
     try:
       CommandWindow.__init__ (self,
                               prompt      = "Pattern:",
                               on_changed  = self.on_changed,
                               on_cancel   = self.on_cancel,
                               on_key      = self.on_key,
                               on_activate = self.on_activate)
       self.editor   = EditorBuffer.get ()
       self.loc      = self.editor.current_view ().cursor ()
       self.end_loc  = self.loc
       self.regexp   = regexp
       self.case_sensitive = case_sensitive
       self.backward = backward
       self.stack = [(self.loc, self.end_loc, "")]
       self.locked = False
     except:
       pass

   def highlight_match (self, save_in_stack=1):
     """Highlight the match at self.loc"""
     self.editor.select (self.loc, self.end_loc)
     if save_in_stack:
        self.stack.append ((self.loc, self.end_loc, self.read ()))

   def on_key (self, input, key, cursor_pos):
     """The user has typed a new key.
        Return True if you have handled the key yourself, or if you want
        to prevent its insertion in the command line.
        Return False if the key should be processed as usual"""

     # ctrl-w copies the current word
     # ctrl-y copies the end of the current line
     if key == "control-w" or key == "control-y":
       start = self.editor.current_view().cursor()
       if key == "control-w":
          end = start.forward_word () - 1  ## Go to end of current word
       elif self.editor.get_chars (start, start) == "\n":
          end = (start + 1).forward_line () - 2  ## Go to end of next line
       else:
          end = start.forward_line () - 2  ## Go to end of this line
    
       self.locked = True
       self.write (input[:cursor_pos + 1] + self.editor.get_chars (start, end) + \
             input[cursor_pos + 1 :])
       self.locked = False
       self.editor.select (self.loc, end + 1)
       return True

     # backspace goes back to stack location and pattern
     if key.lower() == "backspace" and self.stack != []:
        if self.stack != []:
           self.stack.pop ()
        if self.stack != []:
           self.locked = True
           (self.loc, self.end_loc, pattern) = self.stack [-1]
           self.write (pattern)
           self.highlight_match (save_in_stack = 0)
           self.locked = False
           return True

     # doing another isearch just searches for the next occurrence
     # Since we do not know which key binding is bound to this action, we test for
     # the name of the action directly. Note that if the user has defined another
     # action wrapping this function, this will fail... Not too bad
     actions = lookup_actions_from_key (key)
     if actions.__contains__ (isearch_action_name) \
        or actions.__contains__ (isearch_menu):
        self.backward = False
        self.loc = self.loc + 1
        self.on_changed (input, len (input))
        return True

     if actions.__contains__ (isearch_backward_action_name) \
        or actions.__contains__ (isearch_backward_menu):
        self.backward = True
        self.loc = self.end_loc
        self.on_changed (input, len (input))
        return True

     # Cancel the search on any special key. Currently, the key is lost, not
     # sent to the parent window
     try:
        key.index ("control-")
        key.index ("alt-")
        self.destroy ()
        return True
     except:
        pass

     if key.lower() == "left" \
        or key.lower() == "right" \
        or key.lower() == "up" \
        or key.lower() == "down":

        self.destroy ()
        return True

     return False

   def on_changed (self, input, cursor_pos):
     """The user has modified the command line.
        cursor_pos can be used to find where on the line the cursor is located,
        in case we need to change the command line.
        input [:cursor_pos + 1]  is before the cursor
        input [cursor_pos + 1:]  is after the cursor"""

     if not self.locked and input != "":
        # Special case for backward search: if the current location matches,
        # no need to do anything else
        if self.backward:
           (match_from, match_to) = self.loc.search \
              (input, regexp = self.regexp,
               case_sensitive = self.case_sensitive,
               backward = False)
           if match_from == self.loc:
              self.end_log = match_to
              self.highlight_match ()
              return 
           
        result = self.loc.search \
            (input, regexp = self.regexp,
                    case_sensitive = self.case_sensitive,
                    backward = self.backward)
        if result:
           (self.loc, self.end_loc) = result
           self.highlight_match ()

   def on_activate (self, input):
     """The user has pressed enter"""
     if input == "":
        execute_action ("/Navigate/Find or Replace...")

   def on_cancel (self, input):
     """The user has cancelled the search"""
     self.editor.unselect ()
