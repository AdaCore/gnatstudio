"""
 adds an incremental-search capability to GPS

This is similar to what Emacs does
When you select the menu /Navigate/Find Incremental (or bind a key
shortcut to it through the editor at /Edit/Key shortcuts), a temporary
window is open. From then on, any character you type is displayed in
this new window, and makes a search pattern. Whenever this pattern
is modified, GPS will search, starting at the current location, for its
next occurrence in the current file.
While you are editing the pattern, there are a number of special key
shortcuts that can be used:
   - control-w
     will copy the current word into the pattern, and moves the cursor
     to the next word, so that pressing control-w multiple times in
     a row allows you to easily copy part of a line into the pattern

   - control-y
     is similar to control-w but copies the end of the current line into
     the pattern. If the cursor is at the end of the current line, the
     whole next line is copied

   - Key that activates the isearch
     If you press the same key that was used to activate the incremental
     search, GPS will jump to the next occurrence. If you press the key
     to activate the backward incremental search, GPS will jump to the
     stack occurrence.
     If you press that key twice in a row when the pattern is empty, it
     will restart a search for the previous pattern

   - Backspace
     Goes back to the stack location or search pattern. If you have just
     added a character to the pattern, this character is removed. Otherwise
     the pattern is preserved and the editor is moved back to the stack
     location.

   - alt-c
     Toggles the case sensitivity of the search
     The prompt will include the status "[CS]" if the search is currently
     case sensitive. By default, searches are case insensitive, unless your
     pattern includes upper cased letters

   - Esc, movement keys, keys with control or alt
     cancels the current search, and unselect the last occurrence found

If you press <enter> while there is no search string, this module will
automatically open the advanced, non-incremental search dialog of GPS, to
match Emacs' behavior

If the variable highlight_next_matches is set to True, then whenever you
modify the current pattern, GPS will also highlight the next matches of
this pattern in the buffer. Such higlights will stay even when you cancel
the current search. To hide them, start a new search, and cancel it
immediately. The highlighting of the next matches is done in the background
if pygtk was installed along with GPS. Otherwise, it is done every time the
pattern is modified, and will slow things down a little
"""

from GPS import CommandWindow, EditorBuffer, Hook, Preference, \
    execute_action, lookup_actions_from_key
from gps_utils import interactive

Preference('Plugins/isearch/highlightnext').create(
    'Highlight next matches',
    'boolean',
    "Highlight the next matches in the editor." +
    " This highlighting will be visible until the next isearch command." +
    " To cancel, start an isearch and press Esc immediately",
    True)

bg_next_match_pref = Preference('Search-Src-Highlight-Color')
bg_color_pref = Preference('Command-Windows-Background-Color')
bg_error_pref = Preference('High-Importance-Messages-Highlight')

isearch_action_name = 'isearch'
isearch_backward_action_name = 'isearch backward'
# Changing the name of menus should be reflected in emacs.xml

try:
    # If we have PyGTK installed, we'll do the highlighting of the next
    # matches in the background, which makes the interface more responsive
    from gi.repository import GLib
    has_pygtk = 1
except Exception:
    has_pygtk = 0


class Isearch(CommandWindow):

    """This class provides an incremental search facility in GPS.
      When instanciated, it immediately starts executing"""

    last_search = ''
    last_case_sensitive = False

    def __init__(self, case_sensitive=0, backward=0, regexp=0):
        try:
            self.editor = EditorBuffer.get()
            self.loc = self.editor.current_view().cursor()
            self.end_loc = self.loc
            self.regexp = regexp
            self.case_sensitive = case_sensitive
            self.explicit_case_sensitive = False  # # Automatic or from alt-c ?
            self.backward = backward
            self.stack = [(self.loc, self.end_loc, '', 0)]
            self.locked = False
            self.overlay = self.editor.create_overlay('isearch')
            self.overlay.set_property(
                'background',
                bg_next_match_pref.get())
            self.insert_overlays_id = 0
            self.remove_overlays()
            CommandWindow.__init__(
                self,
                prompt=self.prompt(),
                on_changed=self.on_changed,
                on_cancel=self.on_cancel,
                on_key=self.on_key,
                on_activate=self.on_activate)

        except Exception:
            pass

    def prompt(self):
        """Return the prompt to use for the command window"""

        prompt = ''
        if self.case_sensitive:
            prompt = prompt + '[CS] '
        return prompt + 'Pattern:'

    def cancel_idle_overlays(self):
        """Cancel the background loop that computes the next matches"""

        if self.insert_overlays_id != 0:
            GLib.source_remove(self.insert_overlays_id)
            self.insert_overlays_id = 0

    def remove_overlays(self):
        """Remove all isearch overlays in the current editor"""

        highlight_next_matches = Preference(
            'Plugins/isearch/highlightnext').get()

        self.cancel_idle_overlays()

        if highlight_next_matches:
            loc = self.editor.beginning_of_buffer()
            is_on = loc.has_overlay(self.overlay)
            end = self.editor.end_of_buffer()
            while loc < end:
                loc2 = loc.forward_overlay(self.overlay)
                if is_on:
                    self.editor.remove_overlay(self.overlay, loc, loc2)
                is_on = not is_on
                loc = loc2

    def insert_next_overlay(self, input):
        result = self.overlay_loc.search(
            input, regexp=self.regexp,
            case_sensitive=self.case_sensitive, dialog_on_failure=False,
            backward=self.backward)
        if result:
            (self.overlay_loc, end_loc) = result

            self.editor.apply_overlay(
                self.overlay, self.overlay_loc, end_loc - 1)

            self.overlay_loc += 1
            return True
        else:
            self.insert_overlays_id = 0
            return False

    def insert_overlays(self):
        highlight_next_matches = Preference(
            'Plugins/isearch/highlightnext').get()

        if highlight_next_matches:
            input = self.read()
            self.overlay_loc = self.loc
            if input != '':
                if has_pygtk:
                    self.insert_overlays_id = GLib.idle_add(
                        self.insert_next_overlay, input)
                elif len(input) > 2:
                    while self.insert_next_overlay(input):
                        pass

    def highlight_match(self, save_in_stack=1):
        """Highlight the match at self.loc"""

        view = self.editor.current_view()
        view.goto(self.loc)
        view.center(self.loc)
        self.editor.select(self.loc, self.end_loc)

        if save_in_stack:
            self.stack.append((self.loc, self.end_loc, self.read(), 1))

    def on_key(self, input, key, cursor_pos):
        """The user has typed a new key.
           Return True if you have handled the key yourself, or if you want
           to prevent its insertion in the command line.
           Return False if the key should be processed as usual.
        """

        # ctrl-w copies the current word (do not change case sensitivity
        # though)
        # ctrl-y copies the end of the current line

        if key in ('control-w', 'control-y'):
            start = self.editor.current_view().cursor()
            if key == 'control-w':
                end = start.forward_word() - 1  # # Go to end of current word
            elif self.editor.get_chars(start, start) == '\n':
                end = (start + 1).forward_line() - 2  # # end of next line
            else:
                end = start.forward_line() - 2  # # Go to end of this line

            self.locked = True
            case_sensitive = self.case_sensitive
            Isearch.last_search = input[:cursor_pos + 1] \
                + self.editor.get_chars(start, end) + input[cursor_pos + 1:]
            self.write(Isearch.last_search)
            self.locked = False
            self.editor.select(self.loc, end + 1)
            self.case_sensitive = case_sensitive
            return True

        # backspace goes back to stack location and pattern
        if key.lower() == 'backspace' and self.stack != []:
            if self.stack != []:
                self.stack.pop()
            if self.stack != []:
                self.locked = True
                (self.loc, self.end_loc, pattern, matched) = self.stack[-1]
                changed = pattern != input
                if changed:
                    self.remove_overlays()
                self.write(pattern)
                self.highlight_match(save_in_stack=0)
                self.set_background(bg_color_pref.get())
                if changed:
                    self.insert_overlays()
                self.locked = False
                return True

        # Toggle case sensitivity
        if key.lower() == 'alt-c':
            self.case_sensitive = not self.case_sensitive
            self.explicit_case_sensitive = True
            self.set_prompt(self.prompt())
            self.on_changed(input, len(input), redo_overlays=1)
            return True

        # doing another isearch just searches for the next occurrence Since we
        # do not know which key binding is bound to this action, we test for
        # the name of the action directly. Note that if the user has defined
        # another action wrapping this function, this will fail when he starts
        # using the other action

        actions = lookup_actions_from_key(key)
        if isearch_action_name in actions:
            self.backward = False
            if input == '':
                self.case_sensitive = Isearch.last_case_sensitive
                self.explicit_case_sensitive = True
                self.write(Isearch.last_search)
            else:
                self.loc = self.loc + 1
                self.search_next(input, len(input), redo_overlays=0)
            return True

        if isearch_backward_action_name in actions:
            self.backward = True
            if input == '':
                self.case_sensitive = Isearch.last_case_sensitive
                self.explicit_case_sensitive = True
                self.write(Isearch.last_search)
            else:
                self.loc = self.loc - 1
                self.search_next(input, len(input), redo_overlays=0)
            return True

        # Cancel the search on any special key. Currently, the key is lost, not
        # sent to the parent window
        if 'control-' in key or 'alt-' in key:
            self.destroy()
            return True

        if key.lower() in ('left', 'right', 'up', 'down'):
            self.destroy()
            return True

        return False

    def on_changed(self, input, cursor_pos, redo_overlays=1):
        """The user has modified the command line.
           cursor_pos can be used to find where on the line the cursor is
           located, in case we need to change the command line.
              input [:cursor_pos + 1]  is before the cursor
              input [cursor_pos + 1:]  is after the cursor"""

        if not self.locked and input != '':
            # Automatic case sensitivity: when we have an upper case, switch to
            # case sensitive
            if (not self.explicit_case_sensitive and not
                    not self.case_sensitive and
                    input.lower() != input):
                self.case_sensitive = True
                self.set_prompt(self.prompt())
            Isearch.last_case_sensitive = self.case_sensitive
            self.search_next(input, cursor_pos, redo_overlays)

    def search_next(self, input, cursor_pos, redo_overlays):
        """Same as a on_changed, but doesn't change case sensitivity"""

        if redo_overlays:
            self.remove_overlays()

        Isearch.last_search = input

        # Special case for backward search: if the current location matches,
        # no need to do anything else. This is so that when the user keeps
        # adding characters to the pattern, we correctly highlight them at
        # the current location

        if self.backward:
            result = self.loc.search(input, regexp=self.regexp,
                                     case_sensitive=self.case_sensitive,
                                     dialog_on_failure=False, backward=False)
            if result and result[0] == self.loc:
                self.set_background(bg_color_pref.get())
                (match_from, match_to) = result
                self.end_loc = match_to
                self.highlight_match()
                self.insert_overlays()
                return

        result = self.loc.search(input, regexp=self.regexp,
                                 case_sensitive=self.case_sensitive,
                                 dialog_on_failure=False,
                                 backward=self.backward)
        if result:
            self.set_background(bg_color_pref.get())
            (self.loc, self.end_loc) = result
            self.highlight_match()
            if redo_overlays:
                self.insert_overlays()

        else:
            # If the last entry in the stack was a match, add a new one
            if self.stack != [] and self.stack[-1][3]:
                self.stack.append((self.loc, self.end_loc, self.read(), 0))

            # Loop around, so that next search matches
            if self.backward:
                self.loc = self.loc.buffer().end_of_buffer()
            else:
                self.loc = self.loc.buffer().beginning_of_buffer()
            self.end_loc = self.loc
            self.set_background(bg_error_pref.get())
            Hook('stop_macro_action_hook').run()

    def on_activate(self, input):
        """The user has pressed enter"""

        if input == '':
            execute_action('search')

    def on_cancel(self, input):
        """The user has cancelled the search"""

        self.remove_overlays()
        self.editor.unselect()


@interactive(name=isearch_action_name,
             category='Editor',
             filter='Source editor')
def interactive_search():
    """
This action provides an incremental search facility: once activated,
each character you type is added to the search pattern, and GPS jumps
to the next occurrence of the pattern.
    """
    Isearch()


@interactive(name=isearch_backward_action_name,
             category="Editor",
             filter="Source editor")
def interactive_search_backward():
    """
This action provides a backward incremental search facility:
once activated, each character you type is added to the search pattern, and
GPS jumps to the stack occurrence of the pattern.
    """
    Isearch(backward=True)
