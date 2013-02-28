""" When the cursor stops on an entity in an editor, this plugin highlights
    all local references to this entity.

"""


############################################################################
## no user customization below this line
############################################################################

import GPS
import gtk, pygtk
from gps_utils import *

LINES_IN_ONE_BATCH = 5 # Number of lines to process in one batch
TIMEOUT=50             # Interval (in milliseconds) between two batches

default_colors = {
  "object"            : "#ffcefe",
  "subprogram"        : "#ffcf90",
  "package/namespace" : "lightgreen",
  "type"              : "lightgreen",
  "unknown"           : "#d7d7d7"}

# Whether to display occurrences in the speed bar
GPS.Preference("Plugins/auto_highlight_occurrences/speedbar").create(
      "Show in speedbar", "boolean",
      "Whether to display the matches in the speed bar in editors."
      " You must restart gps to take changes into account.",
      False)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_entities").create(
    "Highlight entities", "boolean",
    "Whether this plugin should highlight occurrences of the current entity.",
    True)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_selection").create(
    "Highlight selection", "boolean",
    "Whether to attempt highlighting of the current selection.",
    True)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_word").create(
    "Highlight current word", "boolean",
    "Whether to attempt highlighting of the word under the cursor.",
    False)

# The default colors
for k in default_colors:
    pref_name = ("Plugins/auto_highlight_occurrences/color_" +
                  k.replace("/", "_"))
    GPS.Preference (pref_name).create(
          "Highlight color for " + k, "color",
          "color used to highlight matching occurrences."
          " You must restart gps to take changes into account",
          default_colors[k])


class LocationHighlighter(object):
    """Class to handle the highlighting of local occurrences."""

    def __init__(self):
        """
        Initialize a new highlighter. It monitors changes in the current
        context to highlight the entity under the cursor.
        It is intended that a single entity of this lass is created in GPS.
        """
        self.messages = []    # The registered messages

        # Cache for some of the preferences
        self.highlight_entities = None
        self.highlight_selection = None
        self.highlight_word = None

        self.terminated = False   # True if we should not highlight anymore

        self.synchronous = False  # If true, highlighting is done on a whole
                             # file at once. This is intended for testsuites

        self.entity = None
        self.word = None
        self.timeout = None   # The timeout for the highlighting

        self.styles = {}

        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        GPS.Hook("location_changed").add(self.highlight)
        GPS.Hook("before_exit_action_hook").add(self.__before_exit)

    def remove_all_messages(self):
        """
        Remove all highlighting performed by self.
        """
        if self.messages:
            for m in self.messages:
                m.remove()
            self.messages = []

    def stop(self, *args, **kwargs):
        """
        Stop highlighting the current file. Highlighting will restart when
        the current location is changed.
        :param args: ignored, so that this function can be used in callbacks
        :param kwargs: ignored, so that this function can be used in callbacks
        """

        if self.timeout:
            self.timeout.remove()
            self.timeout = None

    def __on_preferences_changed(self, hook_name):
        """
        Called whenever one of the preferences has changed.
        """

        for k in default_colors:
            pref_name = "Plugins/auto_highlight_occurrences/color_" + k.replace("/", "_")
            self.styles[k] = GPS.Style("dynamic occurrences " + k)
            self.styles[k].set_background(
                GPS.Preference(pref_name).get())
            self.styles[k].set_in_speedbar(
              GPS.Preference("Plugins/auto_highlight_occurrences/speedbar").get())

        self.highlight_entities = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_entities").get()
        self.highlight_selection = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_selection").get()
        self.highlight_word = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_word").get()

        self.remove_all_messages()
        self.highlight()

    def __before_exit(self, hook_name):
        """
        Called when GPS is about to exit.
        """
        self.terminated = True
        self.stop()
        self.remove_all_messages()
        return True

    def __process_one_line(self, line):
        """Perform highlighting on a single line"""

        # Get the string on the line
        beg_loc = GPS.EditorLocation(self.buffer, line, 1)
        end_loc = beg_loc.end_of_line()
        s = self.buffer.get_chars(beg_loc, end_loc).decode("utf8")
        s_len = len(s)

        # Find the tokens on the current line
        tokens = []
        index = 0
        tab_expanded_index = 0

        while index < s_len:
            if s[index].isalpha():
                end_index = index + 1
                while end_index < s_len and (
                    s[end_index].isalnum() or s[end_index] == '_'):
                    end_index += 1

                if self.entity:
                    loc = GPS.FileLocation(
                        self.file, line, tab_expanded_index + 1)

                    if loc in self.entity_refs:
                        tokens.append((tab_expanded_index, end_index - index))
                else:
                    if s[index:end_index] == self.word:
                        tokens.append((tab_expanded_index, end_index - index))

                tab_expanded_index += end_index - index
                index = end_index

            else:
                if s[index] == '\t':
                    # snap to the next multiple of 8
                    tab_expanded_index += 8 - (tab_expanded_index) % 8
                else:
                    tab_expanded_index += 1

                index += 1

        for t in tokens:
            msg = GPS.Message(
                category="dynamic occurrences",
                file=self.file,
                line=line,
                column=t[0] + 1,   # index in python starts at 0
                text="",
                flags=2)
            msg.set_style(self.style, t[1])
            self.messages.append(msg)

    def __process_one_batch(self):
        """Process a batch of search locations"""
        counter = self.batch_size
        found = True

        while counter > 0 and found:
            found = False

            if self.current_line_going_up > 0:
                self.__process_one_line(self.current_line_going_up)
                self.current_line_going_up -= 1
                found = True

            if self.current_line_going_down < self.last_line:
                self.__process_one_line(self.current_line_going_down)
                self.current_line_going_down += 1
                found = True

            counter -= 1

        return counter == 0 and found  # Whether we should try highligh again

    def on_timeout(self, timeout):
        try:
            if not self.__process_one_batch():
                self.stop()
                return False
            return True
        except:
            self.stop()

    def highlight(self, *args, **kwargs):
        """
        Compute the current context, and perform the highlighting.
        :param args: ignored, so that this function can be used in callbacks
        :param kwargs: ignored, so that this function can be used in callbacks
        """

        if self.terminated:
            return

        if self.highlight_entities is None:
            # preferences_changed has not yet been called
            return

        entity  = None
        word    = None
        context = GPS.current_context()
        start_loc = None
        end_loc   = None

        try:
            location = context.location()
            self.buffer = GPS.EditorBuffer.get(location.file(), open=False)
        except:
            self.buffer = None

        # If we want to highlight based on the selection, look for it first

        if self.highlight_selection:
            if self.buffer:
                start_loc = self.buffer.selection_start()
                end_loc = self.buffer.selection_end()

                if start_loc != end_loc:
                    end_loc = end_loc.forward_char(-1)
                    word = self.buffer.get_chars(start_loc, end_loc).strip()

        # Attempt entity highlighting if no word was found.

        if not word and isinstance(context, GPS.EntityContext):
            try:
                entity = context.entity()
            except:
                entity = None

        # No entity found, highlight the current text

        if not entity and not word:
            try:
                location = context.location()
            except:
                location = None

            if location and self.buffer:
                location = GPS.EditorLocation(
                    self.buffer, location.line(), location.column())

                if location.inside_word():
                    start_loc = location
                    while not start_loc.starts_word():
                        start_loc = start_loc.forward_char(-1)

                    end_loc = location
                    while not end_loc.ends_word():
                        end_loc = end_loc.forward_char()

                    word = self.buffer.get_chars(start_loc, end_loc).strip()

        if not entity and not word:
            self.stop()
            self.remove_all_messages()
            return

        if (entity and self.entity == entity) \
           or (word and self.word == word):
            return

        self.stop()
        self.remove_all_messages()

        self.batch_size = 100000 if self.synchronous else LINES_IN_ONE_BATCH
        self.entity = entity
        self.word = word
        self.file = self.buffer.file()
        self.current_line_going_down = context.location().line()
        self.current_line_going_up = self.current_line_going_down - 1
        self.last_line = self.buffer.lines_count()

        if self.entity:
            # Compute all refs to the entity immediately, so that we do not
            # have to do any xref query later on when doing the highlighting
            # This query is fast since it only involves a single source file.

            self.entity_refs = self.entity.references(
                include_implicit=False,
                synchronous=True,
                in_file=self.file)

            if self.entity.is_subprogram():
                self.style = self.styles["subprogram"]
            elif self.entity.is_container():
                self.style = self.styles["package/namespace"]
            elif self.entity.is_type():
                self.style = self.styles["type"]
            else:
                self.style = self.styles["object"]
        else:
            self.style = self.styles["unknown"]

        # Process the immediate surroundings immediately
        # If we still have text matches after that, register a timeout to
        # process them

        if self.__process_one_batch():
            self.timeout = GPS.Timeout(TIMEOUT, self.on_timeout)

            try:
                # Destroy the timeout when the buffer is destroyed
                self.buffer.current_view().pywidget().connect(
                    "destroy", self.stop)
            except:
                # This can happen if pywidget() is not found: rather than leave
                # GPS open to crashing, deactivate highlighting
                self.destroy()


highlighter = LocationHighlighter()
