from GPS import *

## Changing the name of menus should be reflected in emacs.xml
isearch_action_name = 'isearch'
isearch_menu = '/Navigate/Find Incremental'
isearch_backward_action_name = 'isearch backward'
isearch_backward_menu = '/Navigate/Find Previous Incremental'

parse_xml ("""
  <action name='""" + isearch_action_name + """' category="Editor" output="none">
     <filter id="Source editor" />
     <shell lang="python">isearch.Isearch()</shell>
  </action>
  <action name='""" + isearch_backward_action_name + """' category="Editor"
          output="none">
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
     except:
       pass

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
    
       self.write (input[:cursor_pos + 1] + self.editor.get_chars (start, end) + \
             input[cursor_pos + 1 :])
       self.editor.current_view().goto (end + 1)
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

     return False

   def on_changed (self, input, cursor_pos):
     """The user has modified the command line.
        cursor_pos can be used to find where on the line the cursor is located,
        in case we need to change the command line.
        input [:cursor_pos + 1]  is before the cursor
        input [cursor_pos + 1:]  is after the cursor"""

     if input != "":
        # Special case for backward search: if the current location matches,
        # no need to do anything else
        if self.backward:
           (match_from, match_to) = self.loc.search \
              (input, regexp = self.regexp,
               case_sensitive = self.case_sensitive,
               backward = False)
           if match_from == self.loc:
              self.editor.select (match_from, match_to)
              return 
           
        (match_from, match_to) = self.loc.search (input, regexp = self.regexp,
                                                  case_sensitive = self.case_sensitive,
                                                  backward = self.backward)
        self.loc = match_from
        self.end_loc = match_to
        self.editor.select (match_from, match_to)

   def on_activate (self, input):
     """The user has pressed enter"""
     pass  ## Nothing to be done for us

   def on_cancel (self, input):
     """The user has cancelled the search"""
     self.editor.unselect ()
