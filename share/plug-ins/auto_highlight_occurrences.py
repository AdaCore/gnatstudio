""" When the cursor stops on an entity in an editor, this plugin highlights
    all local references to this entity.

"""


############################################################################
## no user customization below this line
############################################################################

import GPS
from gps_utils import *

# Constants

ENTITIES_IN_ONE_BATCH=2
# Number of lines to process in one batch when processing entities

WORDS_IN_ONE_BATCH=5
# Number of lines to process in one batch when processing words

TIMEOUT=50           # Interval (in milliseconds) between two batches

default_colors =  {
  "label"      : "lightblue",
  "literal"    : "lightblue",
  "object"     : "#ffbeee",
  "subprogram" : "#ffcf90",
  "package/namespace" : "lightgreen",
  "type"              : "lightgreen",
  "unknown"    : "#D7D7D7" }

editor_location_styles = {}

# Register some preferences

# Whether to display occurrences in the speed bar
GPS.Preference ("Plugins/auto_highlight_occurrences/speedbar").create (
      "Show in speedbar", "boolean",
      "Whether to display the matches in the speed bar in editors."
      " You must restart gps to take changes into account.",
      False)

GPS.Preference ("Plugins/auto_highlight_occurrences/highlight_entities"
     ).create (
    "Highlight entities", "boolean",
    "Whether this plugin should highlight occurrences of the current entity.",
    True)

GPS.Preference ("Plugins/auto_highlight_occurrences/highlight_selection"
    ).create (
    "Highlight selection", "boolean",
    "Whether to attempt highlighting of the current selection.",
    True)

GPS.Preference ("Plugins/auto_highlight_occurrences/highlight_word"
    ).create (
    "Highlight current word", "boolean",
    "Whether to attempt highlighting of the word under the cursor.",
    True)

highlight_entities = None
highlight_selection = None
highlight_word = None

# The default colors
for k in default_colors:
    pref_name = ("Plugins/auto_highlight_occurrences/color_" +
                  k.replace("/", "_"))
    GPS.Preference (pref_name).create (
          "Highlight color for " + k, "color",
          "color used to highlight matching occurrences."
          " You must restart gps to take changes into account",
          default_colors[k])

# The main class
class LocationHighlighter:
    """ Class to handle the highlighting of local occurrences. """

    def process_one_line (self, line):
        """ Look at one line, and highlight results if necessary
        """

        # Get the string on the line
        beg_loc = GPS.EditorLocation (self.buffer, line, 1)
        end_loc = beg_loc.end_of_line()

        s = self.buffer.get_chars (beg_loc, end_loc).decode("utf8")
        s_len = len(s)

        if self.word:
            the_word = self.word
        else:
            the_word = self.entity_name

        l = len(the_word)

        # Find name and expand tabs at the same time

        index = 0
        tab_expanded_index = 0

        while index + l < s_len:
            if s[index:index+l] == the_word:
                # Cet the entity at this match if we are trying to highlight
                # entities

                if self.entity:
                    # GPS.Entity might raise an exception: catch it
                    try:
                        e=GPS.Entity("", self.file, line, tab_expanded_index+1)
                    except:
                        e=None

                    if e:
                        # We have found an entity: verify whether it is the
                        # same as the one we are interested in.

                        if e.declaration()==self.declaration:
                            # It is the same entity: create a message
                            msg = GPS.Message (
                                "dynamic occurrences",
                                self.file,
                                line,
                                tab_expanded_index+1,
                                "",
                                2)
                            msg.set_style(self.style, l)

                            self.messages += [msg]
                else:
                    # we are highlighting a word
                    msg = GPS.Message (
                        "dynamic occurrences",
                        self.file,
                        line,
                        tab_expanded_index+1,
                        "",
                        2)
                    msg.set_style(self.style, l)

                    self.messages += [msg]

                index += l
                tab_expanded_index += l - 1

            else:
                index += 1

            if s[index-1]=='\t':
                # snap to the next multiple of 8
                prev=tab_expanded_index
                tab_expanded_index += 8 - (tab_expanded_index) % 8

            else:
                tab_expanded_index += 1

    def process_one_batch(self):
        """ Process a batch of search locations.
        """

        if self.entity:
            counter = ENTITIES_IN_ONE_BATCH
        else:
            counter = WORDS_IN_ONE_BATCH

        found = True

        while counter > 0 and found:
            # Look at one pair of lines
            found = False

            if self.current_line_going_up > 0:
                self.process_one_line(self.current_line_going_up)
                self.current_line_going_up -= 1
                found = True

            if self.current_line_going_down < self.last_line:
                self.process_one_line(self.current_line_going_down)
                self.current_line_going_down += 1
                found = True

            counter -= 1

        # If we have found something during the last pass, return True
        return counter == 0 and found

    def on_timeout (self, timeout):
        try:
            if not self.process_one_batch():
                self.timeout.remove()
                self.timeout=None
        except:
            timeout.remove()
            self.timeout=None

    def __init__ (self, context, buffer, entity=None, word=None):
        # Get the current buffer

        self.buffer = buffer
        self.entity = entity  # The original entity
        self.messages = []    # The registered messages
        self.declaration = None
        self.entity_name = None
        self.timeout = None
        self.file=self.buffer.file()
        self.word = word  # The original word

        if self.entity:
            self.entity_name = entity.name().decode("utf8")
            self.declaration=self.entity.declaration()

            cat = self.entity.category()
            if cat in editor_location_styles:
                self.style = editor_location_styles[cat]
            else:
                self.style = editor_location_styles["unknown"]

        else:
            self.style = editor_location_styles["unknown"]

        line=context.location().line()

        self.current_line_going_up = line - 1
        self.current_line_going_down = line

        self.last_line = self.buffer.lines_count()

        # Process the immediate surroundings immediately
        # If we still have text matches after that, register a timeout to
        # process them

        if self.process_one_batch():
            self.timeout = GPS.Timeout(TIMEOUT, self.on_timeout)

        # We have registered a timeout which depends on the buffer: kill this
        # when the buffer is destroyed

        self.buffer.current_view().pywidget().connect (
            "destroy", self.cb_destroy)

    def cb_destroy(self, event):
        """ Callback on the destroy event on the view. """

        # Unregister the idle callback

        if self.timeout:
            self.timeout.remove()
            self.timeout=None

    def destroy(self):
        """ Destroy self """

        # Stop highlighting new messages

        if self.timeout:
            self.timeout.remove()
            self.timeout=None

        # Remove all messages

        if self.messages:
            for m in self.messages:
                m.remove()

            self.messages=[]


current_highlighter=None
exiting=False # Whether GPS is about to exit

def destroy_current_highlighter():
    global current_highlighter

    if current_highlighter:
        current_highlighter.destroy()
        current_highlighter = None

    # For safety, clear all dynamic occurrences styles from the editor
    buf=GPS.EditorBuffer.get(open=False)

    if buf:
        for k in default_colors:
            overlay=buf.create_overlay("dynamic occurrences " + k)
            buf.remove_overlay(overlay)

def get_buffer(context):
    """ Return the buffer contained in the context, if any. """

    try:
        location = context.location()
        buffer = GPS.EditorBuffer.get(location.file(), open=False)
        return buffer
    except:
        return None

def re_highlight():
    global current_highlighter

    if exiting:
        return

    if highlight_entities is None:
        # This means that the callback to preferences_changed has not yet
        # been called: exit now
        return

    entity  = None
    word    = None
    context = GPS.current_context()
    buffer  = get_buffer(context)

    start_loc = None
    end_loc   = None

    # If we want to highlight based on the selection, look for it first

    if highlight_selection:
        if buffer:
            start_loc = buffer.selection_start()
            end_loc = buffer.selection_end()

            if (start_loc != end_loc):
                end_loc = end_loc.forward_char(-1)
                word = buffer.get_chars(start_loc, end_loc)

    # If we have not found a word, and are attempting to highlight entities,
    # look for an entity now

    if (not word) and (highlight_entities) and (
        context.__class__ == GPS.EntityContext):

        try:
            entity = context.entity()
        except:
            entity = None

    # If we have not found an entity and a selection, and we are trying to
    # highlight the currently selected word, look for it now.

    if (not entity) and (not word) and (highlight_word):
        try:
            location = context.location()
        except:
            location = None

        buffer  = get_buffer(context)
        if location and buffer:
            location = GPS.EditorLocation (
                buffer, location.line(), location.column())

            if location.inside_word():
                start_loc = location

                while not start_loc.starts_word():
                    start_loc = start_loc.forward_char(-1)

                end_loc = location

                while not end_loc.ends_word():
                    end_loc = end_loc.forward_char()

                word = buffer.get_chars(start_loc, end_loc)

    if entity:
        # Destroy the current highlighter unless it is already highlighting
        # the same entity
        if (current_highlighter
            and (not entity.declaration() == current_highlighter.declaration)):
            destroy_current_highlighter()

        # Highlight the current entity
        if not current_highlighter:
            current_highlighter=LocationHighlighter(
                context, buffer, entity)

        return

    if word:
        if word.strip() == "":
            return

        # Destroy the current highlighter unless it is already highlighting
        # the same word
        if (current_highlighter and (word != current_highlighter.word)):
            destroy_current_highlighter()

        # Highlight the current word
        if not current_highlighter:
            current_highlighter=LocationHighlighter(
                context, buffer, word=word)

        return

    # If we reach this point, there is no entity in the context: remove
    # highlighting

    destroy_current_highlighter()

def on_location_changed(hook, file, line, column):
    """ Called when the current location changes """
    global current_highlighter
    try:
        re_highlight()
    except:
        destroy_current_highlighter()

def remove_all_messages():
    global current_highlighter

    destroy_current_highlighter()

    for m in GPS.Message.list("dynamic occurrences"):
        m.remove()

def on_preferences_changed (hook_name):
    global highlight_selection
    global highlight_word
    global highlight_entities

    remove_all_messages()

    # Create preferences
    for k in default_colors:
        pref_name = "Plugins/auto_highlight_occurrences/color_" + k.replace("/", "_")

        # Create styles

        editor_location_styles[k]=GPS.Style("dynamic occurrences " + k)
        editor_location_styles[k].set_background(
            GPS.Preference (pref_name).get())
        editor_location_styles[k].set_in_speedbar(
          GPS.Preference ("Plugins/auto_highlight_occurrences/speedbar").get())

        GPS.Hook ("location_changed").add(on_location_changed)

    # Read word and selection preferences
    highlight_entities = GPS.Preference (
        "Plugins/auto_highlight_occurrences/highlight_entities").get()
    highlight_selection = GPS.Preference (
        "Plugins/auto_highlight_occurrences/highlight_selection").get()
    highlight_word = GPS.Preference (
        "Plugins/auto_highlight_occurrences/highlight_word").get()

    # Re-highlight after preferences changed

    re_highlight()

def before_exit(hook_name):
    global exiting
    try:
        remove_all_messages()
        exiting=True
    except:
        pass
    return 1

GPS.Hook ("before_exit_action_hook").add (before_exit)
GPS.Hook ("preferences_changed").add (on_preferences_changed)
