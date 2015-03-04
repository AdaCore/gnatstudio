"""
This plug-in highlights all occurrences of the entity under the cursor.

Whenever your cursor rests in a new location, GPS will search for all
other places where this entity is referenced using either the cross-reference
engine in GPS, if this information is up-to-date, or a simple textual
search. Each of these occurrences will then be highlighted in a color
depending on the kind of the entity.

This plug-in does its work in the background, whenever GPS is not busy
responding to your actions, so it should have limited impact on the
performances and responsiveness of GPS.

If you are interested in doing something similar in your own plugins,
we recommend you look at the
:py:class:`gps_utils.highlighter.Background_Highlighter` class instead,
which provides the underlying framework.

A similar plug-in which you might find useful is in the
:py:class:`gps_utils.highlighter.Regexp_Highlighter` class. By creating
a simple python file in your gps directory, you are able to highlight any
regular expression in the editor, which is useful for highlighting text
like "TODO", or special comments for instance.

"""

############################################################################
# no user customization below this line
############################################################################

import GPS
from gps_utils import *
from gps_utils.highlighter import Location_Highlighter, OverlayStyle
import traceback

default_colors = {
    "object":            "rgba(255, 190, 238, 0.7)",
    "subprogram":        "rgba(252, 175, 62, 0.5)",
    "package/namespace": "rgba(144, 238, 144, 0.5)",
    "type":              "rgba(144, 238, 144, 0.5)",
    "unknown":           "#d7d7d7"}

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
    pref_name = \
        ("Plugins/auto_highlight_occurrences/color_" + k.replace("/", "_"))

    GPS.Preference(pref_name).create(
        "Highlight color for " + k, "color",
        "color used to highlight matching occurrences."
        " You must restart gps to take changes into account",
        default_colors[k])


class Current_Entity_Highlighter(Location_Highlighter):

    """Class to handle the highlighting of local occurrences."""

    def __init__(self):
        """
        Initialize a new highlighter. It monitors changes in the current
        context to highlight the entity under the cursor.
        It is intended that a single entity of this lass is created in GPS.
        """
        Location_Highlighter.__init__(self, style=None, context=0)

        # Cache for some of the preferences
        self.highlight_entities = None
        self.highlight_selection = None
        self.highlight_word = None
        self.entity = None
        self.word = None
        self.styles = {}  # dict of OverlayStyle
        self.speedbar = None
        self.pref_cache = {}

        self.current_buffer = None

        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        GPS.Hook("location_changed").add(self.highlight)
        GPS.Hook("file_closed").add(self.__on_file_closed)

    def __on_file_closed(self, hook, file):
        if self.current_buffer:
            if self.current_buffer.file() == file:
                self.current_buffer = None

    def __on_preferences_changed(self, hook_name):
        """
        Called whenever one of the preferences has changed.
        """

        changed = False

        v = GPS.Preference("Plugins/auto_highlight_occurrences/speedbar").get()
        if v != self.speedbar:
            self.speedbar = v
            changed = True

        for k in default_colors:
            pref_name = "Plugins/auto_highlight_occurrences/color_" + \
                k.replace("/", "_")
            v = GPS.Preference(pref_name).get()
            if (pref_name not in self.pref_cache
                    or self.pref_cache[pref_name] != v
                    or changed):   # take speedbar changes into account

                self.pref_cache[pref_name] = v
                changed = True
                self.styles[k] = OverlayStyle(
                    name="dynamic occurrences " + k,
                    background=v,
                    speedbar=self.speedbar)

        v = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_entities").get()
        if v != self.highlight_entities:
            self.highlight_entities = v
            changed = True

        v = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_selection").get()
        if v != self.highlight_selection:
            self.highlight_selection = v
            changed = True

        v = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_word").get()
        if v != self.highlight_word:
            self.highlight_word = v
            changed = True

        if changed:
            self.remove_highlight()
            self.highlight()

    def recompute_refs(self, buffer):
        if self.entity:
            # Compute all refs to the entity immediately, so that we do not
            # have to do any xref query later on when doing the highlighting
            # This query is fast since it only involves a single source file.

            n = self.entity.name()

            return [(n, r) for r in self.entity.references(
                include_implicit=False,
                synchronous=True,
                in_file=buffer.file())]

        else:
            return []   # irrelevant

    def process(self, start, end):
        """Called by Background_Highlighter"""

        if self.entity:
            Location_Highlighter.process(self, start, end)
        else:
            buffer = start.buffer()
            while start < end:

                # Get the string on the line
                end_loc = start.end_of_line()
                s = buffer.get_chars(start, end_loc)  # byte-sequence
                s = s.decode("utf8")  # make unicode-string
                s_len = len(s)

                # Find the tokens on the current line
                index = 0
                visible_column = 0

                while index < s_len:
                    if s[index].isalpha():
                        end_index = index + 1

                        while end_index < s_len and (
                                s[end_index].isalnum() or s[end_index] == '_'):
                            end_index += 1
                        if s[index:end_index] == self.word:
                            self.style.apply(
                                start=GPS.EditorLocation(
                                    buffer,
                                    start.line(),
                                    start.column() + visible_column),
                                end=GPS.EditorLocation(
                                    buffer,
                                    start.line(),
                                    start.column() +
                                    visible_column + len(self.word) - 1))

                        visible_column += end_index - index
                        index = end_index

                    else:
                        visible_column += 1

                        if s[index] == '\t':
                            visible_column = (visible_column
                                              + 8 - (visible_column % 8))

                        index += 1

                start = end_loc + 1

    def highlight(self, *args, **kwargs):
        """
        Compute the current context, and perform the highlighting.
        :param args: ignored, so that this function can be used in callbacks
        :param kwargs: ignored, so that this function can be used in callbacks
        """

        if self.highlight_entities is None:
            # preferences_changed has not yet been called
            return

        entity = None
        word = None
        context = GPS.current_context()
        start_loc = None
        end_loc = None

        try:
            location = context.location()
            buffer = GPS.EditorBuffer.get(location.file(), open=False)
        except:
            buffer = None

        # If we want to highlight based on the selection, look for it first

        if self.highlight_selection:
            if buffer:
                start_loc = buffer.selection_start()
                end_loc = buffer.selection_end()

                if start_loc != end_loc:
                    end_loc = end_loc.forward_char(-1)
                    word = buffer.get_chars(start_loc, end_loc).strip()
                    word = word.decode("utf8")  # make unicode-string

        # Attempt entity highlighting if no word was found.

        if not word and isinstance(context, GPS.EntityContext):
            try:
                entity = context.entity(approximate_search_fallback=False)
            except:
                entity = None

        # No entity found, highlight the current text

        if not entity and not word:
            try:
                location = context.location()
            except:
                location = None

            if location and buffer:
                location = GPS.EditorLocation(
                    buffer, location.line(), location.column())

                if location.inside_word():
                    start_loc = location
                    while not start_loc.starts_word():
                        start_loc = start_loc.forward_char(-1)

                    end_loc = location
                    while not end_loc.ends_word():
                        end_loc = end_loc.forward_char()

                    word = buffer.get_chars(start_loc, end_loc).strip()
                    word = word.decode("utf8")  # make unicode-string

        # Exit if we are highlighting the word or the entity that we were
        # already highlighting.
        if (entity and self.entity and self.entity == entity) \
           or (word and self.word and self.word == word):
            return

        self.stop_highlight()
        self.remove_highlight()

        if not entity and not word:
            self.entity = None
            self.word = None
            return

        self.entity = entity
        self.word = word

        if self.current_buffer and (self.current_buffer != buffer):
            # We have just switched buffers: clear the highlighting on the
            # previous buffer
            self.stop_highlight(buffer=self.current_buffer)
            self.remove_highlight(buffer=self.current_buffer)

        if self.entity:
            if self.entity.is_subprogram():
                self.set_style(self.styles["subprogram"])
            elif self.entity.is_container():
                self.set_style(self.styles["package/namespace"])
            elif self.entity.is_type():
                self.set_style(self.styles["type"])
            else:
                self.set_style(self.styles["object"])
        else:
            self.set_style(self.styles["unknown"])

        self.current_buffer = buffer
        self.start_highlight(buffer=buffer)


highlighter = Current_Entity_Highlighter()
