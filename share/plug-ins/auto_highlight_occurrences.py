"""
This plugin highlights all occurrences of the entity under the cursor.

Whenever your cursor rests in a new location, GPS will search for all
other places where this entity is referenced using either the cross-reference
engine in GPS, if this information is up-to-date, or a simple textual
search. Each of these occurrences will then be highlighted in a color
depending on the kind of the entity.

This plugin does its work in the background, whenever GPS is not busy
responding to your actions, so it should have limited impact on the
performances and responsiveness of GPS.

If you are interested in doing something similar in your own plugins,
we recommend you look at the
:py:class:`gps_utils.highlighter.Background_Highlighter` class instead,
which provides the underlying framework.

A similar plugin which you might find useful is in the
:py:class:`gps_utils.highlighter.Regexp_Highlighter` class. By creating
a simple python file in your gps directory, you are able to highlight any
regular expression in the editor, which is useful for highlighting text
like "TODO", or special comments for instance.

"""

############################################################################
# no user customization below this line
############################################################################

import GPS
# from gps_utils import *
from gps_utils import hook
from gps_utils.highlighter import Location_Highlighter, OverlayStyle
import re

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_entities").create(
    "Highlight entities", "boolean",
    "Highlight occurrences of the current entity.",
    False)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_selection").create(
    "Highlight selection", "boolean",
    "Attempt to highlight the current selection.",
    True)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlight_word").create(
    "Highlight current word", "boolean",
    "Attempt to highlight the word under the cursor.",
    True)

GPS.Preference(
    "Plugins/auto_highlight_occurrences/highlighting_limit").create(
    "Highlighting limit", "integer",
    "Maximum number of locations that can be highlighted if is not zero.",
    0, 0)

MSG_PREFIX = 'dynamic occurrences '
# Messages created by this plugin have a category that starts with this


class Current_Entity_Highlighter(Location_Highlighter):

    """Class to handle the highlighting of local occurrences."""

    def __init__(self):
        """
        Initialize a new highlighter. It monitors changes in the current
        context to highlight the entity under the cursor.
        It is intended that a single entity of this class is created in GPS.
        """
        Location_Highlighter.__init__(self, style=None, context=0)

        # Cache for some of the preferences
        self.highlight_entities = None
        self.highlight_selection = None
        self.highlight_word = None
        self.entity = None
        self.word = None
        self.styles = {
            "text": OverlayStyle(
                name="{}simple".format(MSG_PREFIX),
                speedbar=True,
                style=GPS.Style(
                    "Editor ephemeral highlighting simple", False),
                whole_line=False),
            "entity": OverlayStyle(
                name="{}smart".format(MSG_PREFIX),
                speedbar=True,
                style=GPS.Style(
                    "Editor ephemeral highlighting smart", False),
                whole_line=False)}
        self.pref_cache = {}

        self.current_buffer = None

        self.highlighted_limit = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlighting_limit").get()

        # Words that should not be highlighted.
        # ??? This should be based on the language

        GPS.Hook("preferences_changed").add(self._on_preferences_changed)
        GPS.Hook("location_changed").add_debounce(self.highlight)
        GPS.Hook("file_closed").add(self.__on_file_closed)

    def __on_file_closed(self, hook, file):
        if self.current_buffer:
            if self.current_buffer.file() == file:
                self.current_buffer = None

    def _on_preferences_changed(self, hook_name):
        """
        Called whenever one of the preferences has changed.
        """

        changed = False

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

        self.highlighted_limit = GPS.Preference(
            "Plugins/auto_highlight_occurrences/highlighting_limit").get()

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
                            self.highlighted += 1
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
                            visible_column = (visible_column +
                                              8 - (visible_column % 8))

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
        location = None

        try:
            # Get the lcoation from the current editor instead of getting it
            # from the context. This is needed to highlight occurrences when
            # the context is not the current editor (e.g: Search view).

            buffer = GPS.EditorBuffer.get(open=False)
            context = GPS.current_context()
            location = buffer.current_view().cursor()
        except Exception:
            buffer = None

        # If we want to highlight based on the selection, look for it first

        if self.highlight_selection:
            if buffer:
                try:
                    start_loc = buffer.selection_start()
                    end_loc = buffer.selection_end()
                    if start_loc != end_loc:
                        end_loc = end_loc.forward_char(-1)
                        word = buffer.get_chars(start_loc, end_loc)
                        # make unicode-string
                        word = word.strip().decode("utf8")
                except GPS.Exception:
                    return
        # Attempt entity highlighting.
        # Do this only if there is indeed an entity under the cursor,
        # and, then, only if the semantic tree for this file is already
        # available: we do not want to block the UI waiting for this
        # computation.

        if not word and (
            self.highlight_entities and
            GPS.SemanticTree(context.file()).is_ready() and
                context.entity_name()):
            try:
                entity = context.entity(approximate_search_fallback=False)
            except Exception:
                entity = None

        # No entity found, highlight the current text

        if not entity and (not word and location and
                           buffer and self.highlight_word):
            location = GPS.EditorLocation(
                buffer, location.line(), location.column())
            word, _, _ = location.get_word()

        # Exit if we are highlighting the word or the entity that we were
        # already highlighting.
        if (entity and self.entity and self.entity == entity) \
           or (word and self.word and self.word == word):
            return

        self.stop_highlight()
        self.remove_highlight()

        if not buffer or (not entity and not word):
            self.entity = None
            self.word = None
            return

        # Do nothing when we have a reserved key word, since this might be
        # slow and in general does not bring useful information.
        lang_re = buffer.get_lang().keywords
        if lang_re and word and re.match(lang_re, word):
            return

        self.entity = entity
        self.word = word

        if self.current_buffer and (self.current_buffer != buffer):
            # We have just switched buffers: clear the highlighting on the
            # previous buffer
            self.stop_highlight(buffer=self.current_buffer)
            self.remove_highlight(buffer=self.current_buffer)

        if self.entity:
            self.set_style(self.styles["entity"])
        else:
            self.set_style(self.styles["text"])

        self.current_buffer = buffer
        self.start_highlight(buffer=buffer)


def cleanup_autohighlight_messages():
    """Remove all messages generated by this plugin."""

    for m in GPS.Message.list():
        if m.get_category().startswith(MSG_PREFIX):
            m.remove()


@hook('before_exit_action_hook')
def __before_exit():
    """Run before GPS exits"""
    # Use a try/finally to make sure we never prevent exiting GPS
    try:
        # We do not want to persist the autohighlight messages
        cleanup_autohighlight_messages()
    finally:
        return True


highlighter = None


def setup():
    global highlighter
    if highlighter is None:
        # Do the initialization in the hooks, not systematically. This plugin
        # is imported by sphinx for the documentation, and fails to load in
        # that context otherwise.

        # Cleanup any autohighlight messages that could have been persisted by
        # error in a previous run of GPS
        cleanup_autohighlight_messages()

        # Launch the highlighter
        highlighter = Current_Entity_Highlighter()

        # Highlight the current context. If the user has loaded a file from the
        # command line, the file is already the current context by the time we
        # setup the highlighter, so we need to start highlighting immediately.
        highlighter._on_preferences_changed("")
        highlighter.highlight()


@hook('gps_started')
def __on_gps_started():
    setup()
