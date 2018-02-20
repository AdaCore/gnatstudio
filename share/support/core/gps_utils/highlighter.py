"""
This file provides various classes to help highlight patterns in
files.
"""

import GPS
import time
import traceback

try:
    from gi.repository import GLib
    gobject_available = 1
except Exception:
    gobject_available = 0


class OverlayStyle(object):

    """
    Description for a style to apply to a section of an editor. In practice,
    this could be implemented as an editor overlay, or a message, depending
    on whether highlighting should be done on the whole line or not.

    :param string name: name of the overlay so that we can remove it later.
    :param string foreground:  foreground color
    :param string background:  background color
    :param string weight: one of "bold", "normal", "light"
    :param string slant:  one of "normal", "oblique", "italic"
    :param boolean editable: whether the text is editable by the user
       interactively.
    :param boolean whole_line: whether to highlight the whole line, up to the
       right margin
    :param boolean speedbar: whether to show a mark in the speedbar to the left
       of editors.
    :param GPS.Style style: the style to apply to the overlay.
    :param kwargs: other properties supported by EditorOverlay
    """

    def __init__(self, name, foreground="", background="", weight=None,
                 slant=None, editable=None, whole_line=False, speedbar=False,
                 style=None, **kwargs):
        self.name = name
        self.foreground = foreground
        self.background = background
        self.weight = weight
        self.slant = slant
        self.editable = editable
        self.whole_line = whole_line

        self._style = None
        self._style_ts = time.time()  # detect whether we need to set props
        self._messages = []

        self.others = kwargs

        if speedbar or style:
            if style:
                self._style = style
            else:
                self._style = GPS.Style(self.name)
                if self.background:
                    self._style.set_background(self.background)
                if self.foreground:
                    self._style.set_foreground(self.foreground)

            self._style.set_in_speedbar(speedbar)

    def use_messages(self):
        """
        :return: Whether this style will use a `GPS.Message` or \
           a `GPS.EditorOverlay` to highlight.
        :rtype: boolean
        """
        return self._style is not None

    def __create_style(self, buffer):
        """
        Create (or reuse) a buffer overlay.
        :param GPS.EditorBuffer buffer: the buffer in which the style will
           be applied.
        """
        if self.use_messages():
            return self._style
        else:
            # Return existing one or create
            over = buffer.create_overlay(self.name)
            if not hasattr(over, "ts") or over.ts != self._style_ts:
                if self.foreground:
                    over.set_property("foreground", self.foreground)
                if self.background:
                    if self.whole_line:
                        over.set_property(
                            "paragraph-background", self.background)
                    else:
                        over.set_property("background", self.background)

                if self.weight:
                    over.set_property("weight", self.weight)
                if self.slant:
                    over.set_property("style", self.slant)

                for prop, value in self.others.iteritems():
                    over.set_property(prop, value)

                if self.editable:
                    over.set_property("editable", self.editable)

                over.ts = self._style_ts
            return over

    def apply(self, start, end):
        """
        Apply the highlighting to part of the buffer.

        :param GPS.EditorLocation start: start of highlighted region.
        :param GPS.EditorLocation end: end of highlighted region.
        """
        buffer = start.buffer()
        over = self.__create_style(buffer)

        if self.use_messages():
            msg = GPS.Message(
                category=self.name,
                file=buffer.file(),
                line=start.line(),
                column=start.column(),  # index in python starts at 0
                text="",
                show_on_editor_side=True,
                show_in_locations=False)

            if self.whole_line:
                msg.set_style(over)
            else:
                msg.set_style(over, end.column() - start.column() + 1)
            self._messages.append(msg)

        else:
            buffer.apply_overlay(over, start, end)

    def remove(self, start, end=None):
        """
        Remove the highlighting in whole or part of the buffer.
        :param GPS.EditorLocation start: start of region.
        :param GPS.EditorLocation end: end of region.  If unspecified, the \
            highlighting for the whole buffer is removed.
        """
        if isinstance(start, GPS.EditorBuffer):
            buffer = start
            start = buffer.beginning_of_buffer()
        else:
            buffer = start.buffer()

        if self.use_messages():
            # ??? Missing support for removing partial messages. When we do
            # that, we can remove the call to remove() in start_highlight
            # when appending the buffer to the list.
            if end is None:
                tmp = []
                file = buffer.file()
                for m in self._messages:
                    if m.get_file() == file:
                        m.remove()
                    else:
                        tmp.append(m)
                self._messages = tmp

        else:
            over = self.__create_style(buffer)
            if end is None:
                start = buffer.beginning_of_buffer()
                end = buffer.end_of_buffer()
            buffer.remove_overlay(over, start, end)


class Background_Highlighter(object):

    """
    An abstract class that provides facilities for highlighting parts of an
    editor. If possible, this highlighting is done in the background so that it
    doesn't interfer with the user typing.
    Example of use::

        class Example(Background_Highlighter):
           def process(self, start, end):
               ... analyze the given range of lines, and perform highlighting
               ... where necessary.

        e = Example()
        e.start_highlight(buffer1)   # start highlighting a first buffer
        e.start_highlight(buffer2)   # start highlighting a second buffer

    :param OverlayStyle style: style to use for highlighting.
    """
    # Interval in milliseconds between two batches.
    # This is only used when gobject is not available
    timeout_ms = 40

    # Number of lines to process at each iteration
    batch_size = 20

    # If True, highlighting is always done in the
    # foreground. This is for testsuite purposes
    synchronous = False

    def __init__(self, style):
        self.__source_id = None  # The gtk source_id used for background
        # or the GPS.Timeout instance
        self.__buffers = []      # The list of buffers to highlight
        self.terminated = False
        self.highlighted = 0
        self.highlighted_limit = 0

        self.style = style
        GPS.Hook("before_exit_action_hook").add(self.__before_exit)
        GPS.Hook("file_closed").add(self.__on_file_closed)
        GPS.Hook("source_lines_folded").add(
            self.__on_lines_folded_or_unfolded)
        GPS.Hook("source_lines_unfolded").add(
            self.__on_lines_folded_or_unfolded)

    def __del__(self):
        self.__source_id = None  # Don't try to kill the idle, GPS is quitting
        self.stop_highlight()
        GPS.Hook("before_exit_action_hook").remove(self.__before_exit)
        GPS.Hook("file_closed").remove(self.__on_file_closed)
        GPS.Hook("source_lines_folded").remove(
            self.__on_lines_folded_or_unfolded)
        GPS.Hook("source_lines_unfolded").remove(
            self.__on_lines_folded_or_unfolded)

    def __before_exit(self, hook):
        """
        Called when GPS is about to exit
        """
        self.terminated = True
        self.__source_id = None  # Don't try to kill the idle, GPS is quitting
        self.stop_highlight()
        return True

    def set_style(self, style):
        """
        Change the current highlight style.

        :param OverlayStyle style: style to use for highlighting.
        """
        self.remove_highlight()
        self.style = style

    def start_highlight(self, buffer=None, line=None, context=None):
        """
        Start highlighting the buffer, possibly in the background.

        :param GPS.EditorBuffer buffer:
           The buffer to highlight (defaults to the current buffer). This
           buffer is added to the list of buffers, and will be processed
           when other buffers are finished.

        :param integer line:
           The line the highlighting should start from. By default, this
           is the current line in the editor, so that the user sees
           changes immediately. But you could chose to start from the
           top of the file instead.

        :param integer context:
           Number of lines before and after 'line' that should be
           highlighted. By default, the whole buffer is highlighted.
        """
        if buffer is None:
            location = GPS.current_context().location()
            buffer = GPS.EditorBuffer.get(location.file(), open=False)

        if buffer.file().name() == "":
            # Do nothing, since we won't easily be able to detect the
            # closing of that file.
            return

        if buffer is not None and line is None:
            view = buffer.current_view()
            line = view.cursor().line() if view is not None else 1

        if buffer is not None:
            # Is the buffer already in the list ?
            for b in self.__buffers:
                if b[0] == buffer:
                    return

            end_line = buffer.lines_count()
            if context is not None:
                start_line = max(0, line - context)
                end_line = min(end_line, line + context)
            else:
                start_line = 0

            # push at the back, so that we do not change the current buffer,
            # in case the user has computed data for it (see
            # Location_Highlighter)
            self.__buffers.append(
                (buffer, line, line + 1, start_line, end_line, True, 0))

            if self.style and self.style.use_messages():
                self.style.remove(buffer)

            if self.synchronous:
                self.on_start_buffer(buffer)
                while self.__do_highlight():
                    pass

            elif self.__source_id is None:
                if gobject_available:
                    self.__source_id = GLib.idle_add(
                        self.__do_highlight)  # , priority=GLib.PRIORITY_LOW)
                else:
                    self.__source_id = GPS.Timeout(
                        self.timeout_ms, self.__do_highlight)

                self.on_start_buffer(buffer)

    def __on_file_closed(self, hook, file):
        for b in self.__buffers:
            if b[0].file() == file:
                self.stop_highlight(b[0])
                break

    def __on_lines_folded_or_unfolded(self, hook, start_line, end_line):
        """
        Called when some lines are folded or unfolded
        """

        buffer = GPS.EditorBuffer.get(open=False)

        if buffer:
            self.stop_highlight(buffer)
            self.start_highlight(buffer)

    def on_start_buffer(self, buffer):
        """
        Called before we start processing a new buffer.
        """
        pass

    def stop_highlight(self, buffer=None):
        """
        Stop the background highlighting of the buffer, but preserves
        any highlighting that has been done so far.

        :param GPS.EditorBuffer buffer: If specified, highlighting is \
           only stopped for a specific buffer.
        """

        if buffer is not None:
            for b in self.__buffers:
                if b[0] == buffer:
                    self.__buffers.remove(b)
                    return

        elif self.__source_id:
            if gobject_available:
                GLib.source_remove(self.__source_id)
            else:
                self.__source_id.remove()
            self.__source_id = None

            self.__buffers = []

    def remove_highlight(self, buffer=None):
        """
        Remove all highlighting done by self in the buffer.

        :param GPS.EditorBuffer buffer: defaults to the current buffer
        """
        if buffer is None:
            buffer = GPS.EditorBuffer.get(open=False, force=False)

        if buffer is not None and self.style is not None:
            self.style.remove(buffer)

    def process(self, start, end):
        """
        Called to highlight the given range of editor. When this is called,
        previous highlighting has already been removed in that range.

        :param GPS.EditorLocation start: start of region to process.
        :param GPS.EditorLocation end: end of region to process.
        """
        pass

    def __do_highlight(self, *args, **kwargs):
        """
        The function called at regular intervals, and that computes
        the range of lines to highlight.
        """
        if self.terminated:
            return False

        if not self.__buffers:
            self.stop_highlight()
            return False

        try:
            (buffer, min_line, max_line,
             start_line, end_line, backward,
             self.highlighted) = self.__buffers[0]

            changed = False

            if min_line >= start_line and (backward or max_line >= end_line):
                from_line = max(start_line, min_line - self.batch_size)

                f = buffer.at(from_line, 1)

                # Do not process if the line is folded
                if not (from_line > 1 and f.offset() == 0):
                    e = buffer.at(min_line, 1).end_of_line()
                    if self.style:
                        self.style.remove(f, e)

                    self.process(f, e)

                min_line = from_line - 1
                if max_line < end_line:
                    backward = False
                changed = True

            elif max_line < end_line:
                to_line = min(end_line - 1, max_line + self.batch_size)

                # It is possible that the buffer has been changed so that one
                # of the locations is now invalid, so we just protect.
                try:
                    f = buffer.at(max_line, 1)

                    # Do not process if the line is folded
                    if not (max_line > 1 and f.offset() == 0):
                        e = buffer.at(to_line, 1).end_of_line()

                        if self.style:
                            self.style.remove(f, e)
                        self.process(f, e)

                    max_line = to_line + 1
                    if min_line >= start_line:
                        backward = True
                    changed = True
                except Exception:
                    pass

            if changed and (self.highlighted_limit == 0 or
                            self.highlighted < self.highlighted_limit):
                self.__buffers[0] = (
                    buffer, min_line, max_line, start_line, end_line,
                    backward, self.highlighted)
            else:
                self.__buffers.pop(0)
                if self.__buffers:
                    self.on_start_buffer(self.__buffers[0][0])

            return True

        except Exception as e:
            GPS.Logger("HIGHLIGHTER").log("Unexpected exception: %s" % e)
            traceback.print_exc()
            self.stop_highlight()
            return False


class On_The_Fly_Highlighter(Background_Highlighter):

    """
    This abstract class provides a way to easily highlight text in an editor.
    When possible, the highlighting is done in the background, in
    which case it is also done on the fly every time the file is
    modified. If pygobject is not available, the highlighting is only done
    when the file is opened or saved

    As for Background_Highlight, you need to override the process() function
    to perform actual work.

    :param OverlayStyle style: the style to apply.
    :param integer context_lines: The number of lines (plus or minus)
       around the current location that get refreshed when a local
       highlighting is requested.
    """

    def must_highlight(self, buffer):
        """
        :param GPS.EditorBuffer buffer: The buffer to test.

        :return: whether to highlight this buffer.
           The default is to higlight all buffers, but some highlightings
           might apply only to specific languages for instance
        :rtype: boolean
        """
        return True

    def __init__(self, style, context_lines=0):
        Background_Highlighter.__init__(self, style=style)
        self.context_lines = context_lines
        self.start()

    def start(self):
        """
        Start highlighting. This is automatically called from __init__,
        and only needs to be called when you have called stop() first.
        Do not call this function multiple times.
        """

        GPS.Hook("file_edited").add(self.__do_whole_highlight)
        GPS.Hook("file_saved").add(self.__do_whole_highlight)
        GPS.Hook("file_changed_on_disk").add(self.__do_whole_highlight)

        if gobject_available:
            GPS.Hook("character_added").add(self.__do_context_highlight)
            for buf in GPS.EditorBuffer.list():
                if self.must_highlight(buf):
                    self.start_highlight(buf)

    def stop(self):
        """
        Stop highlighting through self
        """
        GPS.Hook("file_edited").remove(self.__do_whole_highlight)
        GPS.Hook("file_saved").remove(self.__do_whole_highlight)
        GPS.Hook("file_changed_on_disk").remove(self.__do_whole_highlight)

        if gobject_available:
            GPS.Hook("character_added").remove(self.__do_context_highlight)
            for buffer in GPS.EditorBuffer.list():
                if self.must_highlight(buffer) and self.style:
                    self.style.remove(buffer)

    def __do_whole_highlight(self, hook_name, file):
        """
        Called from GPS hooks to request the refresh of the whole file.
        """
        buffer = GPS.EditorBuffer.get(file)
        if self.must_highlight(buffer):
            self.start_highlight(buffer)

    def __do_context_highlight(self, hook_name, file):
        """
        Called from GPS hooks to request the refresh of a few lines
        around the cursor. The number of context lines was specified when
        the highlighted was created.
        """
        buffer = GPS.EditorBuffer.get(file)
        if self.must_highlight(buffer):
            self.start_highlight(buffer, context=self.context_lines)


class Location_Highlighter(Background_Highlighter):
    """
    An abstract class that can be used to implement highlighter related to
    the cross-reference engine.
    As usual, such an highlighter does its job in the background.
    To find the places to highlight in the editor, this class relies on
    having a list of entities and their references. This list will in
    general be computed once when we start processing a new buffer::

           class H(Location_Highlighter):
               def recompute_refs(self, buffer):
                   return ...computation of references within file ...

    :param integer context: The number of lines both before and after a
       given reference where we should find for possible approximate
       matches. This is used when the reference returned by the xref
       engine was outdated.
    """

    def __init__(self, style, context=2):
        Background_Highlighter.__init__(self, style)
        self._refs = []  # list of (entity, ref) in the current buffer
        self.context = context

    def recompute_refs(self, buffer):
        """
        Called before we start processing a new buffer.

        :return: a list of tuples, each of which contains
            an (name, GPS.FileLocation).
            The highlighting is only done if the text at the location is
            name. Name should be a byte-sequence that encodes a UTF-8
            strings, not the unicode string itself (the result of
            `GPS.EditorBuffer.get_chars` or `GPS.Entity.name` can be used).
        """
        return []

    def on_start_buffer(self, buffer):  # overriding
        # ??? Should use a more efficient data structure where we can
        # easily find the references within a given range.
        self._refs = self.recompute_refs(buffer=buffer)

    def process(self, start, end):  # overriding
        ed = start.buffer()

        s = GPS.FileLocation(ed.file(), start.line(), start.column())
        e = GPS.FileLocation(ed.file(), end.line(), end.column())

        for entity_name, ref in self._refs:
            if s <= ref <= e:
                u = entity_name.decode("utf-8").lower()
                s2 = ed.at(ref.line(), ref.column())

                try:
                    e2 = s2 + (len(u) - 1)
                except Exception:
                    # An invalid location ?
                    continue

                b = ed.get_chars(s2, e2).decode("utf-8").lower()
                if b == u:
                    self.highlighted += 1
                    self.style.apply(s2, e2)

                elif self.context > 0:
                    for c in range(1, self.context + 1):
                        # Search after original xref line (same column)
                        try:
                            s2 = GPS.EditorLocation(
                                ed, ref.line() + c, ref.column())
                            e2 = s2 + (len(u) - 1)
                            b = ed.get_chars(s2, e2).decode("utf-8").lower()
                            if b == u:
                                self.highlighted += 1
                                self.style.apply(s2, e2)
                                break

                            # Search before original xref line
                            s2 = GPS.EditorLocation(
                                ed, ref.line() - c, ref.column())
                            e2 = s2 + (len(u) - 1)
                            b = ed.get_chars(s2, e2).decode("utf-8").lower()
                            if b == u:
                                self.highlighted += 1
                                self.style.apply(s2, e2)
                                break
                        except Exception:
                            # An invalid location ?
                            continue


class Regexp_Highlighter(On_The_Fly_Highlighter):

    """
    The Regexp_Highlighter is a concrete implementation to highlight
    editors based on regular expressions. One example is for instance
    to highlight tabs or trailing spaces on lines, when this is considered
    improper style::

        Regexp_Highlighter(
            regexp="\t+|\s+$",
            style=OverlayStyle(
               name="tabs style",
               strikethrough=True,
               background="#FF7979"))

    Another example is to highlight TODO lines. Various conventions exist
    to mark these in the sources, but the following should catch some of
    these::

        Regexp_Highlighter(
            regexp="TODO.*|\?\?\?.*",
            style=OverlayStyle(
               name="todo",
               background="#FF7979"))

    Another example is a class to highlight Spark comments. This should
    only be applied when the language is spark::

        class Spark_Highlighter(Regexp_Highlighter):
            def must_highlight(self, buffer):
                return buffer.file().language().lower() == "spark"

        Spark_Highlighter(
            regexp="--#.*$",
            style=OverlayStyle(
                name="spark", foreground="red"))

    :param string regexp: the regular expression to search for.
       It should preferrably apply to a single line, since highlighting
       is done on small sections of the editor at a time, and it might
       not detect cases where the regular expression would match across
       sections.
    :param OverlayStyle style: the style to apply.
    """

    def __init__(self, regexp, style, context_lines=0):
        self.regexp = regexp
        On_The_Fly_Highlighter.__init__(
            self, context_lines=context_lines, style=style)

    def process(self, start, end):
        while True:
            start = start.search(
                self.regexp, regexp=True, dialog_on_failure=False)
            if not start or start[0] > end:
                return
            self.style.apply(start[0], start[1] - 1)
            start = start[1] + 1


class Text_Highlighter(On_The_Fly_Highlighter):

    """
    Similar to Regexp_Highlighter, but highlights constant text instead of
    a regular expression.
    By default, highlighting is done in all buffer, override the function
    must_highlight to reduce the scope.

    :param string text: the text to search for.
       It should preferrably apply to a single line, since highlighting
       is done on small sections of the editor at a time, and it might
       not detect cases where the text would match across sections.
    :param OverlayStyle style: the style to apply.
    """

    def __init__(self, text, style, whole_word=False, context_lines=0):
        self.text = text
        self.whole_word = whole_word
        On_The_Fly_Highlighter.__init__(
            self, context_lines=context_lines, style=style)

    def process(self, start, end):
        while True:
            start = start.search(
                self.text, regexp=False, dialog_on_failure=False,
                whole_word=self.whole_word)
            if not start or start[0] > end:
                return
            self.style.apply(start[0], start[1] - 1)
            start = start[1] + 1
