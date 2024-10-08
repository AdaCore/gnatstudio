import GPS
from modules import Module

try:
    # While building the doc, we might not have gi.repository
    from gi.repository import Gtk, GLib, Gdk, Pango
    from pygps import get_gtk_buffer, is_editor_visible
except ImportError:
    pass

import re


class HighlighterModule(Module):
    highlighters = {}

    # Map of (preference, value) indexed by style id: $(lang)__$(name)
    preferences = {}

    def init_highlighting(self, f):
        highlighter = self.highlighters.get(f.language().lower(), None)
        if isinstance(highlighter, Highlighter):
            ed = GPS.EditorBuffer.get(f, open=False)
            if ed:
                gtk_ed = get_gtk_buffer(ed)
                if gtk_ed and not gtk_ed.highlighting_initialized:
                    highlighter.init_highlighting(ed)
                    highlighter.gtk_highlight(gtk_ed)

    def setup(self):
        for ed in GPS.EditorBuffer.list():
            if is_editor_visible(ed):
                self.init_highlighting(ed.file())

    def preferences_changed(self):
        for key, item in self.preferences.items():
            pref, value = item
            if pref.get() != value and pref.tag:
                propagate_change(pref)
                self.preferences[key] = (pref, pref.get())

    def context_changed(self, ctx):
        if ctx is not None:
            if ctx.file() is not None:
                ed = GPS.EditorBuffer.get(open=False)
                if ed:
                    self.init_highlighting(ed.file())

    def file_edited(self, f):
        """
        This hook is called when a new file editor is being opened
        """
        self.init_highlighting(f)


#############
# Utilities #
#############

null_span = (-1, -1)


def to_tuple(gtk_iter):
    """
    Transform the gtk_iter passed as parameter into a tuple representation
    :type gtk_iter: Gtk.TextIter
    :rtype (int, int)
    """
    return (gtk_iter.get_line(), gtk_iter.get_line_offset())


def iter_from_tuple(gtk_ed, tuple_instance):
    """
    Recreate a Gtk.TextIter from a tuple

    :type gtk_ed: Gtk.TextBuffer
    :type tuple_instance: (int, int)
    :rtype Gtk.TextIter
    """
    return gtk_ed.get_iter_at_line_index(*tuple_instance)


def iter_to_str(gtk_iter):
    """
    Return a better string representation of a text iter
    :type gtk_iter: Gtk.TextIter
    """
    return "<TextIter {0} {1}>".format(*to_tuple(gtk_iter))


def iter_eq(iter_1, iter_2):
    """
    Structural comparison for Gtk.TextIter
    :type iter_1: Gtk.TextIter
    :type iter_2: Gtk.TextIter
    :rtype: bool
    """
    return iter_1.to_tuple() == iter_2.to_tuple()


def tag_to_str(gtk_tag):
    return "<TextTag {0}>".format(gtk_tag.props.name)


def make_wrapper(method_name):
    fn = getattr(Gtk.TextIter, method_name)

    def wrapper(gtk_iter, *args, **kwargs):
        new_iter = gtk_iter.copy()
        fn(new_iter, *args, **kwargs)
        return new_iter

    return wrapper


def to_line_end(textiter):
    """
    :type textiter: Gtk.TextIter
    :return: Gtk.TextIter
    """
    if textiter.get_char() == "\n":
        return textiter
    return textiter.forward_to_line_end_n()


try:
    # Might fail while building the doc
    import re as r

    for name in dir(Gtk.TextIter):
        if r.match("(forward|set|backward).*$", name):
            setattr(Gtk.TextIter, name + "_n", make_wrapper(name))

    Gtk.TextIter.__str__ = iter_to_str
    Gtk.TextIter.__repr__ = iter_to_str
    Gtk.TextIter.__eq__ = iter_eq
    Gtk.TextIter.to_line_end = to_line_end
    Gtk.TextTag.__str__ = tag_to_str
    Gtk.TextTag.__repr__ = tag_to_str
    Gtk.TextIter.to_tuple = to_tuple
    Gtk.TextBuffer.iter_from_tuple = iter_from_tuple
    Gtk.TextBuffer.highlighting_initialized = False

except NameError:
    pass


def propagate_change(pref):
    fg_rgba = Gdk.RGBA()
    style_string = pref.get()
    font_style, fg_style, bg_style = style_string.split("@")
    fg_rgba.parse(fg_style)
    pref.tag.set_property("foreground_rgba", fg_rgba)

    bg_rgba = Gdk.RGBA()
    bg_rgba.parse(bg_style)
    pref.tag.set_property("background_rgba", bg_rgba)

    if font_style in ["BOLD", "BOLD_ITALIC"]:
        pref.tag.set_property("weight", Pango.Weight.BOLD)

    if font_style in ["ITALIC", "BOLD_ITALIC"]:
        pref.tag.set_property("style", Pango.Style.ITALIC)

    if font_style in ["DEFAULT", "NORMAL"]:
        pref.tag.set_property("style", Pango.Style.NORMAL)
        pref.tag.set_property("weight", Pango.Weight.NORMAL)


def get_hex_color(rgba_color):
    """
    :type rgba_color: GdkRGBA
    :return: hexadecimal representation of RGB
    """
    fg_color = rgba_color.to_color()
    return "#{:01x}{:01x}{:01x}".format(fg_color.red, fg_color.green, fg_color.blue)


# Data classes for highlighters


class Style(object):
    def __init__(self, style_id, prio, pref):
        """
        :type style_id: string
        :type prio: int
        :type pref: GPS.Preference
        """
        self.pref = pref
        self.prio = prio
        self.style_id = style_id

    def __repr__(self):
        return "<Style : {0}>".format(self.style_id)


class BaseMatcher(object):
    def resolve(self):
        """
        :rtype: Matcher
        """
        raise NotImplemented


class Matcher(BaseMatcher):
    def resolve(self):
        return self

    @property
    def pattern(self):
        raise NotImplemented

    def __init__(self, tag, name=""):
        """
        :type tag: Style
        :type name: string
        """
        self.name = name
        self.tag = tag
        self.gtk_tag = None

    def init_tag(self, gtk_ed):
        self.gtk_tag = gtk_ed.get_tag_table().lookup(self.tag.style_id)
        if not self.gtk_tag:
            self.gtk_tag = gtk_ed.create_tag(self.tag.style_id)
            if self.tag.prio != -1:
                self.gtk_tag.set_priority(self.tag.prio)
            self.tag.pref.tag = self.gtk_tag
            propagate_change(self.tag.pref)

        return self.gtk_tag


class SimpleMatcher(Matcher):
    def __init__(self, tag, pattern, name=""):
        """
        :type tag: Style
        :type pattern: string
        """
        super(SimpleMatcher, self).__init__(tag, name)
        self._pattern = pattern

    @property
    def pattern(self):
        return self._pattern


class RegionMatcher(Matcher):
    ":type: dict[string, RegionMatcher]"
    region_matchers = {}

    def __init__(
        self, tag, start_pattern, end_pattern, hl_spec, matchall, name="", igncase=False
    ):
        """
        :type tag: Style
        :type start_pattern: string
        :type end_pattern: string
        :type hl_spec: Iterable[BaseMatcher]
        :type matchall: boolean
        :type name: string
        """
        Matcher.__init__(self, tag, name)
        self.matchall = matchall
        self.hl_spec = hl_spec
        self.end_pattern = end_pattern
        self.start_pattern = start_pattern

        if self.name:
            RegionMatcher.region_matchers[self.name] = self

        self.subhighlighter = SubHighlighter(
            hl_spec, end_pattern, matchall, igncase=igncase
        )
        self.subhighlighter.parent_cat = self

    @property
    def pattern(self):
        return self.start_pattern

    def init_tag(self, gtk_ed):
        Matcher.init_tag(self, gtk_ed)
        self.subhighlighter.gtk_tag = self.gtk_tag
        return self.gtk_tag


class RegionRef(BaseMatcher):
    def __init__(self, region_name):
        self.region_name = region_name

    def resolve(self):
        return RegionMatcher.region_matchers[self.region_name]


########################
# Highlighter creation #
########################


class HighlighterStacks(object):
    def __init__(self):
        # The stack of highlighter at (0, 0) is necessarily the empty stack,
        # so the stack list comes prepopulated with one empty stack
        self.stacks_list = [()]

    def set(self, index, stack):
        """
        Set the stack of highlighters for line index. Returns true if the
        previous stack is the same as the stack argument.

        :type index: int
        :type stack: tuple[Struct]
        @rtype:      bool
        """
        assert 0 <= index <= len(self.stacks_list)

        tpstack = tuple(stack)
        if index == len(self.stacks_list):
            self.stacks_list.append(tpstack)
            return False
        else:
            current_stack = self.stacks_list[index]
            self.stacks_list[index] = tpstack
            return tpstack == current_stack

    def get(self, start_line):
        """
        :type start_line: int
        @rtype:           tuple[Struct]|None
        """
        if start_line < len(self.stacks_list):
            return self.stacks_list[start_line][:]
        else:
            return None

    def insert_newlines(self, nb_lines, after_line):
        """
        :type after_line: int
        :type nb_lines:   int
        """
        for _ in range(nb_lines):
            self.stacks_list.insert(after_line + 1, ())

    def delete_lines(self, nb_deleted_lines, at_line):
        """
        :param nb_deleted_lines: int
        :param at_line: int
        """
        del self.stacks_list[at_line + 1 : at_line + nb_deleted_lines + 1]

    def __str__(self):
        return "{0}".format(
            "\n".join(
                [
                    "{0}\t{1}".format(num, [c for c in stack])
                    for num, stack in enumerate(self.stacks_list)
                ]
            )
        )


class SubHighlighter(object):
    def __init__(
        self, highlighter_spec, stop_pattern=None, matchall=True, igncase=False
    ):
        """
        :type highlighter_spec: Iterable[BaseMatcher]
        """

        self.matchers = [m.resolve() for m in highlighter_spec]
        patterns = [m.pattern for m in self.matchers]

        if stop_pattern:
            patterns.append(stop_pattern)
            self.matchers.append(None)

        self.pattern = re.compile(
            "|".join("({0})".format(pat) for pat in patterns),
            flags=re.M + (re.S if matchall else 0) + (re.I if igncase else 0),
        )
        self.gtk_tag = None
        self.region_start = None
        self.parent_cat = None

    def get_tags_list(self, gtk_ed):
        """
        :type gtk_ed: Gtk.TextBuffer
        """
        return [m.init_tag(gtk_ed) if m else None for m in self.matchers]

    def __str__(self):
        return "<{0}>".format(
            (self.parent_cat.name if self.parent_cat.name else "")
            if self.parent_cat
            else "Root"
        )

    def __repr__(self):
        return self.__str__()


class Highlighter(object):
    def __init__(self, spec=(), igncase=False, nb_lines=100):
        """
        :type spec: Iterable[BaseMatcher]
        :return:
        """
        self.root_highlighter = SubHighlighter(spec, igncase=igncase)
        self.sync_stop = False
        # Nb lines we will rehighlight after a modification
        self.nb_lines = nb_lines

    def highlight_info_gen(self, gtk_ed, start_line, end_line=0):
        """
        Returns a generator that will highlight the buffer, one token at a
        time, every time the generator is consumed.

        :type gtk_ed: Gtk.TextBuffer
        :type start_line: int
        """
        self.sync_stop = False

        start = gtk_ed.get_iter_at_line(start_line)
        ":type: Gtk.TextIter"

        end = (
            gtk_ed.get_end_iter()
            if (end_line == 0 or end_line > gtk_ed.get_line_count())
            else gtk_ed.get_iter_at_line(end_line)
        )
        ":type: Gtk.TextIter"

        if start.compare(end) < 0:
            strn = gtk_ed.get_text(start, end, True)
            ":type: unicode"
        else:
            # Nothing to do, return empty array
            return []

        current_line = start_line

        if start_line == 0:
            subhl_stack = [self.root_highlighter]
            gtk_ed.stacks.set(0, subhl_stack)
        else:
            try:
                subhl_stack = list(gtk_ed.stacks.get(start_line))
            except TypeError:
                subhl_stack = [self.root_highlighter]

        match_offset = 0
        last_start_offset = 0
        results = []
        hl_tags = {}
        rstarts = []
        start_offset = start.get_offset()
        end_offset = end.get_offset()

        while subhl_stack:
            hl = subhl_stack[-1]
            matches = hl.pattern.finditer(strn, match_offset)

            # Cache tags
            tags = hl_tags.get(hl, None)
            if not tags:
                tags = hl.get_tags_list(gtk_ed)
                hl_tags[hl] = tags

            pop_stack = True
            met_stop_pattern = False

            for m in matches:
                # Get the index of the first matching category
                i = [
                    j for j in range(1, len(hl.matchers) + 1) if m.span(j) != null_span
                ][0]

                matcher, tag = hl.matchers[i - 1], tags[i - 1]
                start_line += strn.count("\n", last_start_offset, m.start(i))
                last_start_offset = m.start(i)
                tk_start_offset = start_offset + m.start(i)
                tk_end_offset = start_offset + m.end(i)

                if start_line > current_line:
                    for ll in range(current_line + 1, start_line):
                        gtk_ed.stacks.set(ll, subhl_stack)
                    current_line = start_line

                    # We exit because the stack we're setting is == to the
                    # existing one, so the buffer is synced
                    if gtk_ed.stacks.set(current_line, subhl_stack):
                        endi = gtk_ed.get_iter_at_line(current_line)
                        endi.backward_char()
                        endo = endi.get_offset()
                        rstart = rstarts.pop() if rstarts else start_offset
                        results.append((subhl_stack[-1].gtk_tag, rstart, endo))
                        self.sync_stop = True
                        return results

                # Stop pattern, this is the end of the region, we want to
                # return to the parent highlighter after having yielded the
                # location of the region stop-pattern.
                if not matcher:
                    assert isinstance(hl.gtk_tag, Gtk.TextTag)
                    # If the region has no region start, we are
                    # rehighlighting a region that was previously created,
                    # and has no stored region start.
                    rstart = rstarts.pop() if rstarts else start_offset
                    results.append((hl.gtk_tag, rstart, tk_end_offset))
                    match_offset = m.end(i)
                    met_stop_pattern = True
                    break

                if isinstance(matcher, RegionMatcher):
                    subhl_stack.append(matcher.subhighlighter)
                    rstarts.append(tk_start_offset)

                    match_offset = m.end(i)
                    pop_stack = False
                    break

                if tag:
                    results.append((tag, tk_start_offset, tk_end_offset))
                else:
                    assert False

            # If a region highlighter is stacked, we haven't met it's stop
            # pattern, but yet exhausted the matcher, and we didn't just put
            # it on the stack, then it means this is an unfinished region,
            # so we can highlight to the end of the buffer with this region's
            # tag
            if len(subhl_stack) > 1 and not met_stop_pattern and pop_stack:
                rstart = rstarts.pop() if rstarts else start_offset
                results.append((hl.gtk_tag, rstart, end_offset))
                # We break out of the while loop to keep the stack intact
                break

            if len(subhl_stack) == 1:
                break

            if pop_stack:
                subhl_stack.pop()

        # If we are here, it means this highlighter went on through the end
        # of the buffer (didn't meet a stop pattern, or is the top level hl).
        #  In this case, we want to set the stack correctly for the remaining
        #  lines
        for ll in range(current_line + 1, end.get_line() + 1):
            gtk_ed.stacks.set(ll, subhl_stack)

        results.append((None, end_offset, end_offset))
        return results

    def highlight_gen(self, gtk_ed, start_line, nb_lines):
        """
        :type gtk_ed: Gtk.TextBuffer
        :type start_line: int
        """
        # t = time()
        start_it = gtk_ed.get_start_iter()
        end_it = gtk_ed.get_start_iter()

        if start_line == -1:
            for tag, start, end in self.highlight_info_gen(gtk_ed, 0):
                if tag:
                    start_it.set_offset(start)
                    end_it.set_offset(end)
                    gtk_ed.apply_tag(tag, start_it, end_it)
        else:
            st_iter = gtk_ed.get_iter_at_line(start_line)
            actions_list = self.highlight_info_gen(
                gtk_ed, start_line, start_line + max(nb_lines, self.nb_lines)
            )

            # if not self.sync_stop:
            #     actions_list = self.highlight_info_gen(gtk_ed, start_line)

            if actions_list:
                end_it.set_offset(actions_list[-1][2])
                gtk_ed.remove_all_tags(st_iter, end_it)

                for tag, start, end in actions_list:
                    start_it.set_offset(start)
                    end_it.set_offset(end)
                    if tag:
                        gtk_ed.apply_tag(tag, start_it, end_it)

        # print time() - t

    def gtk_highlight(self, gtk_ed):
        self.highlight_gen(gtk_ed, -1, -1)

    def gtk_highlight_region(self, gtk_ed, start_line, nb_lines):
        self.highlight_gen(gtk_ed, start_line, nb_lines)

    def init_highlighting(self, ed):
        gtk_ed = get_gtk_buffer(ed)
        gtk_ed.highlighting_initialized = True
        gtk_ed.stacks = HighlighterStacks()

        if not hasattr(gtk_ed, "idle_highlight_id"):
            gtk_ed.idle_highlight_id = None

        def action_handler(loc, nb_lines):
            """:type loc: Gtk.TextIter"""
            if gtk_ed.idle_highlight_id:
                GLib.source_remove(gtk_ed.idle_highlight_id)

            # Highlight the rest of the buffer
            self.gtk_highlight_region(gtk_ed, loc.get_line(), nb_lines)

        # noinspection PyUnusedLocal
        def highlighting_insert_text_before(buf, loc, text, length):
            buf.insert_loc = loc.to_tuple()

        # noinspection PyUnusedLocal
        def highlighting_insert_text(buf, loc, text, length):
            nb_new_lines = len(text.split("\n")) - 1
            itr = buf.iter_from_tuple(buf.insert_loc)
            buf.stacks.insert_newlines(nb_new_lines, itr.get_line())
            action_handler(itr, nb_new_lines + 1)

        def highlighting_delete_range_before(buf, loc, end):
            buf.nb_deleted_lines = len(buf.get_text(loc, end, True).split("\n")) - 1

        # noinspection PyUnusedLocal
        def highlighting_delete_range(buf, loc, end):
            buf.stacks.delete_lines(buf.nb_deleted_lines, loc.get_line())
            # Recompute the highlighting of the 100 next lines
            action_handler(loc, self.nb_lines)

        gtk_ed.connect_after("insert-text", highlighting_insert_text)
        gtk_ed.connect_after("delete-range", highlighting_delete_range)
        gtk_ed.connect("delete-range", highlighting_delete_range_before)
        gtk_ed.connect("insert-text", highlighting_insert_text_before)

    def generate_markup(self, text, allow_nested_tag=False):
        """
        Generate the pango markup representation of text by Highlighter.
        :param allow_nested_tag: When True, insert markup for each tags.
        Otherwise skip nested tags.
        For example the '"Str\!"' in C will return with allow_nested_tag:
           <span foreground="#1">"Str<span foreground="#2">\!"</span></span>";
        and without:
           <span foreground="#1">"Str\!"</span>;
        (#1 and #2 will be rgba hexadecimal colors matching the theme)
        """
        text_ed = StringTextBuffer(text)
        actions_list = self.highlight_info_gen(text_ed, 0, 0)

        # Sort actions_list by start offset
        actions_list.sort(key=lambda x: x[1])

        result = ""
        if allow_nested_tag:
            # Map of list of tag for index in the buffer
            start_tag_map = {}
            end_tag_map = {}
            last_tag = None
            for tag, start, end in actions_list:
                if tag:
                    if start in start_tag_map:
                        start_tag_map[start].append(tag)
                    else:
                        start_tag_map[start] = [tag]

                    if end in end_tag_map:
                        end_tag_map[end].append(tag)
                    else:
                        end_tag_map[end] = [tag]

            current_offset = 0
            max_offset = len(text)
            start_iter = iter(start_tag_map)
            end_iter = iter(end_tag_map)
            start_tag_offset = next(start_iter)
            end_tag_offset = next(end_iter)

            iterator_stopped = 0
            # Iter in both map in parallel until both are empty which means
            # we received 2 StopIteration exception
            while iterator_stopped < 2:
                if start_tag_offset < end_tag_offset:
                    result += text[current_offset:start_tag_offset]
                    current_offset = start_tag_offset
                    for tag in start_tag_map[start_tag_offset]:
                        result += '<span foreground="%s">' % get_hex_color(
                            tag.get_property("foreground_rgba")
                        )
                        try:
                            start_tag_offset = next(start_iter)
                        except StopIteration:
                            iterator_stopped += 1
                            start_tag_offset = max_offset + 1
                elif start_tag_offset == end_tag_offset:
                    result += text[current_offset:end_tag_offset]
                    current_offset = end_tag_offset
                    for tag in end_tag_map[end_tag_offset]:
                        result += "</span>"
                    for tag in start_tag_map[start_tag_offset]:
                        result += '<span foreground="%s">' % get_hex_color(
                            tag.get_property("foreground_rgba")
                        )
                        try:
                            start_tag_offset = next(start_iter)
                        except StopIteration:
                            iterator_stopped += 1
                            start_tag_offset = max_offset + 1
                        try:
                            end_tag_offset = next(end_iter)
                        except StopIteration:
                            iterator_stopped += 1
                            end_tag_offset = max_offset + 1
                elif end_tag_offset <= max_offset:
                    result += text[current_offset:end_tag_offset]
                    current_offset = end_tag_offset
                    for tag in end_tag_map[end_tag_offset]:
                        result += "</span>"
                    try:
                        end_tag_offset = next(end_iter)
                    except StopIteration:
                        iterator_stopped += 1
                        end_tag_offset = max_offset + 1

            # Add the rest of the text
            result += text[current_offset:]
        else:
            current_offset = 0
            for tag, start, end in actions_list:
                # This check allows to ignore nested span
                if current_offset <= start:
                    # Add the text before the one in the span
                    result += text[current_offset:start]
                    if tag:
                        need_close_span = True
                        result += '<span foreground="%s">' % get_hex_color(
                            tag.get_property("foreground_rgba")
                        )
                        result += text[start:end]
                        result += "</span>"
                    else:
                        result += text[start:end]
                    current_offset = end

            # Add the rest of the text
            result += text[current_offset:]

        return result


class StringTextIter:
    """
    An iterator in a string mimicking the implementation of Gtk.TextIter
    """

    def __init__(self, line, column, offset):
        self.line = line
        self.column = column
        self.offset = offset

    def compare(self, other):
        if self.offset < other.offset:
            return -1
        else:
            return 1

    def get_offset(self):
        return self.offset

    def get_line(self):
        return self.line

    def get_column(self):
        return self.column


class StringTextBuffer:
    """
    A text buffer mimicking the implementation of Gtk.TextBuffer.
    """

    def __init__(self, text):
        self.text = text
        self.lines = self.text.splitlines()
        self.stacks = HighlighterStacks()
        self.tagtable = Gtk.TextTagTable()

    def _create_text_iter(self, line, column):
        offset = 0
        for i in range(0, len(self.lines)):
            if offset > 0:
                offset += 1  # Add the newline
            if i == line:
                offset += column
                return StringTextIter(line, column, offset)
            else:
                offset += len(self.lines[i])
        return None

    def get_iter_at_line(self, line):
        return self._create_text_iter(line, 0)

    def get_end_iter(self):
        return self._create_text_iter(len(self.lines) - 1, len(self.lines[-1]))

    def get_line_count(self):
        return len(self.text.splitlines())

    def get_text(self, start, end, ignored):
        return self.text[start.offset : end.offset]

    def get_tag_table(self):
        return self.tagtable

    def create_tag(self, id):
        tag = Gtk.TextTag(name=id)
        self.tagtable.add(tag)
        return tag


def markup_for_text(language, text, allow_nested_tag=False):
    """
    Reuse the Highlighter engine on a string for the given language.
    The result is the same string with markup tags.
    :param language: the language used for highlighting
    :type language: str
    :param text: the text to highlight
    :type text: str
    :param allow_nested_tag: should the markup be complex or simple
    :type allow_nested_tag: bool
    """
    highlighter = HighlighterModule.highlighters.get(language.lower(), None)
    if highlighter:
        return highlighter.generate_markup(text, allow_nested_tag=allow_nested_tag)
    else:
        return text


def gps_fun(fun):
    def __gps_to_gtk_fun(start, end, *args, **kwargs):
        gtk_ed = get_gtk_buffer(start.buffer())
        gtk_start = gtk_ed.get_iter_at_offset(start.offset())
        gtk_end = gtk_ed.get_iter_at_offset(end.offset())
        fun(gtk_ed, gtk_start, gtk_end, *args, **kwargs)

    return __gps_to_gtk_fun
