""" When the cursor stops on an entity in an editor, this plugin highlights
    all local references to this entity.

"""


############################################################################
## no user customization below this line
############################################################################

import GPS
from gps_utils import *
from gps_utils.highlighter import Background_Highlighter, OverlayStyle
import traceback

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


class LocationHighlighter(Background_Highlighter):
    """Class to handle the highlighting of local occurrences."""

    def __init__(self):
        """
        Initialize a new highlighter. It monitors changes in the current
        context to highlight the entity under the cursor.
        It is intended that a single entity of this lass is created in GPS.
        """
        Background_Highlighter.__init__(self, style=None)

        # Cache for some of the preferences
        self.highlight_entities = None
        self.highlight_selection = None
        self.highlight_word = None
        self.entity = None
        self.word = None
        self.styles = {} # dict of OverlayStyle

        GPS.Hook("preferences_changed").add(self.__on_preferences_changed)
        GPS.Hook("location_changed").add(self.highlight)

    def __on_preferences_changed(self, hook_name):
        """
        Called whenever one of the preferences has changed.
        """

        speedbar = GPS.Preference(
            "Plugins/auto_highlight_occurrences/speedbar").get()

        for k in default_colors:
            pref_name = "Plugins/auto_highlight_occurrences/color_" + k.replace("/", "_")
            self.styles[k] = OverlayStyle(
                name="dynamic occurrences " + k,
                background=GPS.Preference(pref_name).get(),
                speedbar=speedbar)

        self.highlight_entities = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_entities").get()
        self.highlight_selection = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_selection").get()
        self.highlight_word = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlight_word").get()

        self.remove_highlight()
        self.highlight()

    def process(self, start, end):
        """Called by Background_Highlighter"""

        buffer = start.buffer()

        if self.entity:
            s = GPS.FileLocation(buffer.file(), start.line(), start.column())
            e = GPS.FileLocation(buffer.file(), end.line(), end.column())

            # n is a sequence of bytes, encoded in UTF-8.
            # To compute its length, we need to convert it to a unicode string
            n = self.entity.name()
            u = n.decode("utf-8")

            for r in self.entity_refs:
                if s <= r <= e:
                    s2 = GPS.EditorLocation(buffer, r.line(), r.column())
                    e2 = s2 + len(u) - 1
                    if buffer.get_chars(s2, e2) == n:
                        self.style.apply(s2, e2);

                elif r >= e:
                    break

        else:
            while start < end:

                # Get the string on the line
                end_loc = start.end_of_line()
                s = buffer.get_chars(start, end_loc).decode("utf8")
                s_len = len(s)

                # Find the tokens on the current line
                index = 0
                tab_expanded_index = 0

                while index < s_len:
                    if s[index].isalpha():
                        end_index = index + 1
                        while end_index < s_len and (
                            s[end_index].isalnum() or s[end_index] == '_'):
                            end_index += 1
                        if s[index:end_index] == self.word:
                            self.style.apply(
                                start=GPS.EditorLocation(
                                    buffer, start.line(), index + 1),
                                end=GPS.EditorLocation(
                                    buffer, start.line(), end_index))

                        tab_expanded_index += end_index - index
                        index = end_index

                    else:
                        if s[index] == '\t':
                            # snap to the next multiple of 8
                            tab_expanded_index += 8 - (tab_expanded_index) % 8
                        else:
                            tab_expanded_index += 1
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

        entity  = None
        word    = None
        context = GPS.current_context()
        start_loc = None
        end_loc   = None

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

        if (entity and self.entity == entity) \
           or (word and self.word == word):
            return

        self.stop_highlight()
        self.remove_highlight()

        if not entity and not word:
            return

        self.entity = entity
        self.word = word

        if self.entity:
            # Compute all refs to the entity immediately, so that we do not
            # have to do any xref query later on when doing the highlighting
            # This query is fast since it only involves a single source file.

            self.entity_refs = self.entity.references(
                include_implicit=False,
                synchronous=True,
                in_file=buffer.file())

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

        self.start_highlight(buffer=buffer)


highlighter = LocationHighlighter()
