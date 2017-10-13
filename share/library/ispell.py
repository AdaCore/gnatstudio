"""Spell-checking support

This plugin adds spell-checking capabilities.
In particular, a contextual menu is added, which should only be visible
when the user has clicked on a word inside the source editor.
It will be enabled when the "aspell" executable is visible in the PATH.
Two types of menus are provided in this plugin:
  - Static menu: the menu will be a simple menu entry, which, when
    clicked, starts aspell and displays, in the console, the possible
    replacements for a word
  - Dynamic menu: a submenu is created, with one entry per possible
    replacement. When the user selects one of these entries, the current
    word is replaced

This script also adds a new menu /Edit/Spell Check, which runs spell
checking on the whole buffer, or only the comments. In this mode, the
keys are recognized:
  - "i": Accept the current word in your personal dictionary, to be
         remembered across sessions
  - "a": Accept this word during this session. This setting will be
         remembered until you either kill the aspell process in the
         Tasks view, or exit GPS
  - "r": Replace current word with typed-in value. The replacement
         is rechecked after insertion
  - "space": Ignore this word, and go to the next word
  - "Escape": Cancel current spell checking
  - "0-9" or "A-Z": Replace the current word with this replacement

The menus are implemented as new python classes, since this is the
cleanest way to encapsulate data in python. We could have used global
function calls instead.
This plugin also demonstrates how to start and monitor an external
executable.
It also shows how to get the word under the cursor in GPS.
"""

###########################################################################
# No user customization below this line
###########################################################################

import os_utils
from text_utils import goto_word_start, goto_word_end, BlockIterator, \
    with_save_excursion
import GPS
import modules   # from GPS
from gps_utils import make_interactive


def find_current_word(context):
    """
    Stores in context the current word start, end and text.
    This is called only when computing whether the menu should be
    displayed, and stored in the contextual menu for efficiency, since
    that means we won't have to recompute the info if the user selects
    the menu.
    """

    buffer = GPS.EditorBuffer.get()
    view = buffer.current_view()
    cursor = view.cursor()

    start = goto_word_start(cursor, underscore_is_word=False)
    cursor = goto_word_end(cursor, underscore_is_word=False)

    context.ispell_module_start = start
    context.ispell_module_end = cursor
    context.ispell_module_word = buffer.get_chars(start, cursor)


class Spell_Check_Module(modules.Module):

    def setup(self):
        """Initialize the module"""
        self.pref_cmd = GPS.Preference("Plugins/ispell/cmd")
        self.pref_cmd.create(
            "Command",
            "string",
            """External command to use to spell check words.
This command should return a list of words that could replace the current
one. Recommended values are "aspell" or "ispell". Input to this command
is sent to its stdin.""",
            "aspell -a --lang=en")

        self.pref_bgcolor = GPS.Preference("Command-Windows-Background-Color")

        self.pref_type = GPS.Preference("Plugins/ispell/menutype")
        self.pref_type.create(
            "Menu type",
            "enum",
            """The type of contextual menu we should use:
- "dynamic" only shows the possible replacements for the current word.
- "static" displays a single entry that spawns the spell checked for the
  current word.""",
            0, "static", "dynamic", "none")

        self.ispell = None          # The ispell process
        self.ispell_command = None  # The command used to start ispell
        self.static = None          # context menu
        self.dynamic = None         # context menu
        self.personal_dict_modified = False
        self.window = None          # The command window for user interaction
        self.local_dict = set()     # Temporary saves user overrides

        make_interactive(
            callback=self.spell_check_comments,
            name='spell check comments',
            filter='Source editor',
            category='Editor')
        make_interactive(
            callback=self.spell_check_editor,
            name='spell check editor',
            filter='Source editor',
            category='Editor')
        make_interactive(
            callback=self.spell_check_selection,
            name='spell check selection',
            filter='Source editor',
            category='Editor')
        make_interactive(
            callback=self.spell_check_word,
            name='spell check word',
            filter=self._filter_has_word,
            category='Editor')

        self.preferences_changed()

    def teardown(self):
        """Terminates the module"""
        self.kill()
        super(Spell_Check_Module, self).teardown()

    def _filter_has_word(self, context):
        """
        Whether ispell is available, and the cursor on a word
        """
        if context.module_name == 'Source_Editor' and context.entity_name():
            find_current_word(context)
            return context.ispell_module_word != ""
        else:
            return False

    def spell_check_comments(self):
        """Check the spelling for all comments in the current editor"""
        self.start_window(category='comment')

    def spell_check_editor(self):
        """Check the spelling for the whole contents of the editor"""
        self.start_window(category='')

    def spell_check_selection(self):
        """Check the spelling in the current selection"""
        self.start_window(category='selection')

    def spell_check_word(self):
        """Check the spelling in the current word"""
        self.start_window(category='word')

    def preferences_changed(self):
        """Called when the preferences are changed"""

        cmd = self.pref_cmd.get()

        if self.ispell_command != cmd:
            if self.ispell:
                GPS.Logger('ISPELL').log('command changed, restart process')
                self.kill()

            self.ispell_command = ''
            if os_utils.locate_exec_on_path(cmd.split()[0]):
                GPS.Logger('ISPELL').log('initialize ispell module: %s' % cmd)
                self.ispell_command = cmd

        if self.ispell_command and self.pref_type.get() == 'static':
            GPS.Logger("ISPELL").log('Activate static contextual menu')
            if self.dynamic:
                self.dynamic.hide()
            Static_Contextual(ispell=self)

        elif self.ispell_command and self.pref_type.get() == 'dynamic':
            GPS.Logger("ISPELL").log("Activate dynamic contextual menu")
            GPS.Contextual('spell check word').hide()  # disable static menu
            if not self.dynamic:
                self.dynamic = Dynamic_Contextual(ispell=self)
            else:
                self.dynamic.show()

        else:
            if self.dynamic:
                self.dynamic.hide()

    def _save_personal_dict(self):
        """Save the user's personal dictionary if modified"""
        if self.personal_dict_modified and self.ispell:
            if GPS.MDI.yes_no_dialog(
                    "Spell-checking personal dictionary modified. Save ?"):
                self.ispell.send("#")

                # Make sure the dict is saved: since ispell doesn't show any
                # output, we generate some
                self.generate_fix("word").next()

                self.personal_dict_modified = False

    def ignore_word(self, word):
        """Should ignore word from now on, but not add it to personal dict"""
        self._restart_if_needed()
        self.local_dict.add(word)
        self.ispell.send("@%s\n" % word)

    def add_word_to_dict(self, word):
        """Add word to the user's personal dictionary"""
        self._restart_if_needed()
        self.ispell.send("*%s\n" % word)
        self.local_dict.add(word)
        self.personal_dict_modified = True

    def _before_killing_ispell(self, proc, output):
        """Called just before killing ispell"""
        self._save_personal_dict()
        self.ispell = None

    def _restart_if_needed(self):
        """Start the ispell process if not started already"""
        if not self.ispell and self.ispell_command:
            try:
                self.ispell = GPS.Process(
                    self.ispell_command,
                    before_kill=self._before_killing_ispell,
                    task_manager=False)
                self.ispell.expect("^.*\\n", timeout=2000)
            except:
                GPS.Console().write(
                    "Could not start external command: %s\n" % self.cmd)

    def kill(self):
        """Kill ispell if it is running.
           Always reset self.ispell to None.
        """
        if self.ispell:
            # Will run _before_killing_ispell and save dict
            self.ispell.kill()
            self.ispell = None

    ##############################
    # Finding mispellings
    ##############################

    def generate_fix(self, category):
        """
        A generator that runs ispell to find out the possible mispelling in
        the text. It yields for every mispelling (and sets self.current to
        the current value, so that replace() can be called).
        For efficiency, it passes whole lines at a time to ispell (which does
        not accept multi-line input).

        Ispell runs forever, waiting for words to check on its standard input.
        Note the use of a timeout in the call to expect(). This is so that if
        for some reason ispell answers something unexpected, we don't keep
        waiting for ever.
        """

        self.buffer = GPS.EditorBuffer.get()

        for start, end in BlockIterator(self.buffer, category):
            while start < end:
                end_line = start.forward_line()
                # need mark, since we modify buffer
                next_line = end_line.create_mark()
                line = self.buffer.get_chars(start, end_line - 1)
                offset_adjust = 0
                attempt = 0
                result = None

                # When a user choses to ignore a word, we need to take
                # into account for all mispelling suggested in the current
                # block, since ispell had not been updated yet.

                self.local_dict = set()

                while attempt < 2:
                    self._restart_if_needed()
                    if not self.ispell:
                        raise StopIteration

                    # Always prepend a space, to protect special characters at
                    # the beginning of words that might be interpreted by
                    # aspell.

                    self.ispell.send(" %s" % (line, ))

                    # output of aspell ends with an empty line, but includes
                    # multiple blank lines

                    result = self.ispell.expect("^[\\r\\n]+", timeout=2000)
                    if result:
                        break

                    attempt += 1
                    self.kill()

                if not result:
                    raise StopIteration

                for proposal in result.splitlines():
                    if proposal and proposal[0] == '&':
                        colon = proposal.find(":")
                        meta = proposal[:colon].split()

                        if meta[1] not in self.local_dict:
                            s = start - 1 + offset_adjust + int(meta[3])
                            e = s + len(meta[1])
                            e_off = e.offset()

                            # need to take a mark one character away, since
                            # otherwise the mark would end up at the beginning
                            # of the replacement
                            e_mark = (e + 1).create_mark()

                            self.current = (
                                meta[1],   # mispelled
                                s,
                                e,
                                proposal[colon + 2:].replace(' ', '')
                                .split(','))
                            yield self.current

                            # Take into account changes in the length of words
                            offset_adjust += (e_mark.location().offset() -
                                              e_off - 1)

                start = next_line.location()

        raise StopIteration

    ##############################
    # Command window
    ##############################

    def start_window(self, category):
        """
        Start spell checking a specific category of word in the current
        editor. This displays a command window to interactive with the
        user. The processing is asynchronous.
        """

        self.replace_mode = False   # User is typing his own replacement
        self.mispellings = self.generate_fix(category)  # init generator
        self._next_with_error_or_destroy()

    @with_save_excursion
    def replace(self, input):
        """
        Replace the current mispelling with input.
        """
        start = self.current[1]
        self.buffer.delete(start, self.current[2] - 1)
        self.buffer.insert(start, input)

    def _next_with_error_or_destroy(self):
        """Analyzes one block"""

        try:
            self.mispellings.next()
        except StopIteration:
            if self.window:
                self.window.destroy()
                self.window = None
            return

        (mispelled, location, endloc, replace) = self.current
        suggest = ""

        for index, p in enumerate(replace):
            # Otherwise we end up with non-ASCII chars (26 letters + 10 digits)
            if index <= 36:
                if index <= 9:
                    key = "%s" % index
                else:
                    key = chr(ord('a') + index - 10)

                try:
                    p.decode('utf-8')
                    suggest += "[%s]%s " % (key, p)
                except UnicodeDecodeError:
                    pass

        if not self.window:
            self.window = GPS.CommandWindow(
                on_key=self._on_key,
                on_activate=self._on_activate,
                on_cancel=self._on_cancel,
                close_on_activate=False)
            self.window.set_background(self.pref_bgcolor.get())

        self.replace_mode = False
        self.window.set_prompt("(i,a,r,space)")
        self.window.write(text=suggest, cursor=0)
        self.buffer.select(self.current[1], self.current[2])
        self.buffer.current_view().center()

    def _on_cancel(self, input):
        self.window = None

    def _on_activate(self, input):
        """Called when the user presses <enter> in the command window"""
        if self.replace_mode:
            self.replace(input)
            self._next_with_error_or_destroy()

    def _on_key(self, input, key, cursor_pos):
        """Handles key events in the command window"""

        if self.replace_mode:
            return False

        key = key.replace("shift-", "")
        if key == "i":
            self.ignore_word(self.current[0])
            self.local_dict.add(self.current[0])
            self._next_with_error_or_destroy()
        elif key == "a":
            self.add_word_to_dict(self.current[0])
            self.local_dict.add(self.current[0])
            self._next_with_error_or_destroy()
        elif key == "r":
            self.window.write("")
            self.replace_mode = True
        elif key.isdigit():
            try:
                self.replace(self.current[3][int(key)])
                self._next_with_error_or_destroy()
            except IndexError:
                pass
        elif key == "Escape":
            self.window.destroy()
            self.window = None
        elif key == "space":
            self._next_with_error_or_destroy()
        elif key.islower():
            try:
                self.replace(self.current[3][ord(key) - ord('a') + 10])
                self._next_with_error_or_destroy()
            except IndexError:
                pass

        return True


class Static_Contextual(object):

    def __init__(self, ispell):
        """Create a new static contextual menu for spell checking"""
        GPS.Action('spell check word').contextual(self._label)

    def _label(self, context):
        """Return the label to use for the contextual menu entry"""
        return "Spell Check %s" % (context.ispell_module_word, )


class Dynamic_Contextual(GPS.Contextual):

    def __init__(self, ispell):
        """Create a new dynamic contextual menu for spell checking"""
        GPS.Contextual.__init__(self, "Spell Check")
        self.ispell = ispell
        self.create_dynamic(
            on_activate=self._on_activate,
            filter=self._filter,
            factory=self._factory)

    def _filter(self, context):
        """Decide whether the contextual menu should be made visible"""
        if context.entity_name():
            find_current_word(context)
            return context.ispell_module_word != ""
        else:
            return False

    def _factory(self, context):
        """
        Return a list of strings, each of which is the title to use for an
        entry in the dynamic contextual menu.
        """
        try:
            current = self.ispell.generate_fix('word').next()
        except StopIteration:
            return []

        if current:
            return current[3]
        else:
            return []

    @with_save_excursion
    def _on_activate(self, context, choice, choice_index):
        self.ispell.replace(choice)
        # b = context.ispell_module_start.buffer()
        # b.delete(context.ispell_module_start, context.ispell_module_end)
        # b.insert (context.ispell_module_start, choice)
