import GPS
from modules import Module
from gi.repository import Gtk, GLib, Gdk
from pygps import get_gtk_buffer
import re
from time import time

logger = GPS.Logger("Lol")
# import rpdb2
# rpdb2.start_embedded_debugger("lol")


#############
# Utilities #
#############

null_span = (-1, -1)


def take(gen, nb):
    """
    Consumes nb element from the generator gen, and returns them as a generator
    @type gen: __generator[T]
    @type nb: int
    @rtype : __generator[T]
    """
    for _ in range(nb):
        yield next(gen)


def partition(gen, nb):
    """
    Returns a new generator where an element is a list of nb elements from gen
    @type gen: __generator[T]
    @type nb: int
    @rtype : __generator[list[T]]
    """
    l = list(take(gen, nb))
    while l:
        yield l
        l = list(take(gen, nb))


def to_tuple(gtk_iter):
    """
    Transform the gtk_iter passed as parameter into a tuple representation
    @type gtk_iter: Gtk.TextIter
    @rtype (int, int)
    """
    return (
        gtk_iter.get_line(),
        gtk_iter.get_line_offset()
    )


def iter_from_tuple(gtk_ed, tuple_instance):
    """
    Recreate a Gtk.TextIter from a tuple

    @type gtk_ed: Gtk.TextBuffer
    @type tuple_instance: (int, int)
    @rtype Gtk.TextIter
    """
    return gtk_ed.get_iter_at_line_index(*tuple_instance)


def iter_to_str(gtk_iter):
    """
    Return a better string representation of a text iter
    @type gtk_iter: Gtk.TextIter
    """
    return "<TextIter {0} {1}>".format(*to_tuple(gtk_iter))


def iter_eq(iter_1, iter_2):
    """
    Structural comparison for Gtk.TextIter
    @type iter_1: Gtk.TextIter
    @type iter_2: Gtk.TextIter
    @rtype: bool
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


for name in dir(Gtk.TextIter):
    import re as r
    if r.match("(forward|set|backward).*$", name):
        setattr(Gtk.TextIter, name + "_n", make_wrapper(name))

Gtk.TextIter.__str__ = iter_to_str
Gtk.TextIter.__repr__ = iter_to_str
Gtk.TextIter.__eq__ = iter_eq
Gtk.TextTag.__str__ = tag_to_str
Gtk.TextTag.__repr__ = tag_to_str
Gtk.TextIter.to_tuple = to_tuple
Gtk.TextBuffer.iter_from_tuple = iter_from_tuple


class Struct:
    def __init__(self, **entries):
        self.__dict__.update(entries)

    def __repr__(self):
        return str(self.__dict__)


##############################
# Highlight classes creation #
##############################


def simple(regexp_string, tag="", matchall=True, **kwargs):
    """
    Return a simple matcher for a regexp string
    @type regexp_string: str
    @type tag: str
    """
    return Struct(kind="simple", re=regexp_string, tag=tag,
                  matchall=matchall, **kwargs)


def words(words_list, **kwargs):
    """
    Return a matcher for a list of words
    @type words_list: str|list[str]
    """
    if type(words_list) is list:
        words_list = "|".join(words_list)

    return simple(r"\b(?:{0})\b".format(words_list), **kwargs)


def region(start_re, end_re,
           name="", multiline=False, tag="", highlighter=(),
           matchall=True, **kwargs):
    """
    Return a matcher for a region, which can contain a whole specific
    highlighter
    @type start_re: string
    @type end_re: string
    """
    return Struct(
        kind="region",
        start_re=start_re,
        end_re=end_re,
        name=name,
        multiline=multiline,
        tag=tag,
        highlighter_spec=highlighter,
        matchall=matchall,
        **kwargs
    )


def region_ref(name):
    return Struct(
        kind="region_ref",
        region_ref_name=name
    )


def newtag(name, prio=20, **props):
    return Struct(
        name=name,
        prio=prio,
        props=props
    )


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

        @type index: int
        @type stack: tuple[Struct]
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
        @type start_line: int
        @rtype:           tuple[Struct]|None
        """
        if start_line < len(self.stacks_list):
            return self.stacks_list[start_line][:]
        else:
            return None

    def insert_newlines(self, nb_lines, after_line):
        """
        @type after_line: int
        @type nb_lines:   int
        """
        for _ in range(nb_lines):
            self.stacks_list.insert(after_line + 1, ())

    def delete_lines(self, nb_deleted_lines, at_line):
        """
        @param nb_deleted_lines: int
        @param at_line: int
        """
        del self.stacks_list[at_line+1:at_line+nb_deleted_lines+1]

    def __str__(self):
        return "{0}".format(
            "\n".join(["{0}\t{1}".format(num, [c for c in stack])
                       for num, stack in enumerate(self.stacks_list)])
        )

region_cats = {}


class SubHighlighter(object):

    def __init__(self, cats, matcher, gtk_tag=None, parent_cat=None):
        """
        @type cats: list
        @type matcher: __Regex
        @type gtk_tag: Gtk.TextTag|None
        """

        self.cats = cats
        self.matcher = matcher
        self.gtk_tag = gtk_tag
        self.region_start = None
        self.parent_cat = parent_cat

    def clone(self):
        return SubHighlighter(self.cats, self.matcher, self.gtk_tag,
                              self.parent_cat)

    def get_tags_list(self, gtk_ed):
        """
        @type gtk_ed: Gtk.TextBuffer
        """
        # Cache or create tags before highlighting
        tags = []
        for cat in self.cats:
            if not cat:
                tags.append(None)
                continue
            if type(cat.tag) is str:
                tags.append(gtk_ed.get_tag_table().lookup(cat.tag))
            else:
                t = gtk_ed.get_tag_table().lookup(cat.tag.name)
                if not t:
                    t = gtk_ed.create_tag(cat.tag.name)
                    t.set_priority(cat.tag.prio)
                    for k, v in cat.tag.props.items():
                        color = Gdk.RGBA()
                        color.parse(v)
                        t.set_property(k, color)
                tags.append(t)

            cat.gtk_tag = tags[-1]
            if cat.kind == "region":
                cat.subhighlighter.gtk_tag = cat.gtk_tag

        return tags

    def __str__(self):
        return "<{0}>".format((self.parent_cat.name if self.parent_cat.name
                               else "") if self.parent_cat else "Root")

    def __repr__(self):
        return self.__str__()


def gen_to_fn(gen, granularity, max_time):
    def fn():
        ret = False
        try:
            t = time()
            while time() - t < max_time:
                for _ in range(granularity):
                    gen.next()
            # print time() - t
            ret = True
        except StopIteration:
            pass
        return ret

    return fn


class Highlighter(object):

    def __init__(self, spec=()):

        def construct_subhighlighter(highlighter_spec, stop_pattern=None,
                                     matchall=True):
            subhl_spec = list(highlighter_spec)
            patterns = []
            for i in range(len(subhl_spec)):
                cat = subhl_spec[i]

                if cat.kind == "simple":
                    patterns.append(cat.re)
                elif cat.kind == "region":
                    if cat.name:
                        region_cats[cat.name] = cat
                    patterns.append(cat.start_re)
                    highlighter_spec = cat.highlighter_spec
                    cat.subhighlighter = construct_subhighlighter(
                        highlighter_spec, stop_pattern=cat.end_re,
                        matchall=cat.matchall
                    )
                    cat.subhighlighter.parent_cat = cat
                elif cat.kind == "region_ref":
                    subhl_spec[i] = region_cats[cat.region_ref_name]
                    patterns.append(subhl_spec[i].start_re)

            if stop_pattern:
                patterns.append(stop_pattern)
                subhl_spec.append(())

            matcher = re.compile(
                "|".join("({0})".format(pat) for pat in patterns),
                flags=re.M + (re.S if matchall else 0)
            )
            return SubHighlighter(subhl_spec, matcher)

        self.root_highlighter = construct_subhighlighter(spec)
        self.sync_stop = False

    def highlight_info_gen(self, gtk_ed, start_line, end_line=0):
        """
        Returns a generator that will highlight the buffer, one token at a
        time, every time the generator is consumed.

        @type gtk_ed: Gtk.TextBuffer
        @type start_line: int
        """
        self.sync_stop = False
        start = gtk_ed.get_iter_at_line(start_line)
        end = (gtk_ed.get_end_iter()
               if not end_line else gtk_ed.get_iter_at_line(end_line))
        strn = gtk_ed.get_text(start, end, True).decode('utf-8')
        current_line = start_line

        if start_line == 0:
            subhl_stack = [self.root_highlighter]
            gtk_ed.stacks.set(0, subhl_stack)
        else:
            subhl_stack = list(gtk_ed.stacks.get(start_line))

        match_offset = 0

        while subhl_stack:
            hl = subhl_stack[-1]
            matches = hl.matcher.finditer(strn, match_offset)
            tags = hl.get_tags_list(gtk_ed)
            pop_stack = True
            met_stop_pattern = False

            for m in matches:

                # Get the index of the first matching category
                i = [j for j in range(1, len(hl.cats) + 1)
                     if m.span(j) != null_span][0]

                cat, tag = hl.cats[i - 1], tags[i - 1]
                tk_start = start.forward_chars_n(m.start(i))
                tk_end = start.forward_chars_n(m.end(i))

                if tk_start.get_line() > current_line:
                    for l in range(current_line + 1, tk_start.get_line()):
                        gtk_ed.stacks.set(l, subhl_stack)
                    current_line = tk_start.get_line()

                    # We exit because the stack we're setting is == to the
                    # existing one, so the buffer is synced
                    if gtk_ed.stacks.set(current_line, subhl_stack):
                        self.sync_stop = True
                        return

                # Stop pattern, this is the end of the region, we want to
                # return to the parent highlighter after having yielded the
                # location of the region stop-pattern.
                if not cat:
                    assert isinstance(hl.gtk_tag, Gtk.TextTag)
                    # If the region has no region start, we are
                    # rehighlighting a region that was previously created,
                    # and has no stored region start.
                    rstart = hl.region_start or start
                    yield (hl.gtk_tag, rstart, tk_end)
                    hl.region_start = None
                    match_offset = m.end(i)
                    met_stop_pattern = True
                    break

                if cat.kind == "region":
                    subhl_stack.append(cat.subhighlighter.clone())
                    subhl_stack[-1].region_start = tk_start
                    match_offset = m.end(i)
                    pop_stack = False
                    break

                if tag:
                    yield (tag, tk_start, tk_end)
                else:
                    assert False

            # If a region highlighter is stacked, we haven't met it's stop
            # pattern, but yet exhausted the matcher, and we didn't just put
            # it on the stack, then it means this is an unfinished region,
            # so we can highlight to the end of the buffer with this region's
            # tag
            if len(subhl_stack) > 1 and not met_stop_pattern and pop_stack:
                yield (hl.gtk_tag, hl.region_start, end)
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
        for l in range(current_line + 1, end.get_line()):
            gtk_ed.stacks.set(l, subhl_stack)

    def highlight_gen(self, gtk_ed, start_line=-1, remove_tag_step=100):
        """
        @type gtk_ed: Gtk.TextBuffer
        @type start_line: int
        @type remove_tag_step: int
        """
        def get_action_list(sl, el=0):
            return sorted(
                self.highlight_info_gen(gtk_ed, sl, sl + (el if el else 0)),
                key=lambda (_, s, e): s.get_offset()
            )
        max_loc = None

        if start_line == -1:
            for tag, start, end in self.highlight_info_gen(gtk_ed, 0):
                max_loc = end
                gtk_ed.apply_tag(tag, start, end)
                yield
        else:
            is_end = (gtk_ed.get_iter_at_line(start_line + 100) == gtk_ed
                      .get_end_iter())

            actions_list = get_action_list(start_line, start_line+100)

            if not self.sync_stop and not is_end:
                actions_list += get_action_list(start_line+100)

            all_actions = (a for a in actions_list)

            if actions_list:
                gtk_ed.remove_all_tags(gtk_ed.get_iter_at_line(start_line),
                                       actions_list[0][1])

            for actions in partition(all_actions, remove_tag_step):
                max_loc = max(actions, key=lambda (_, s, e): e.get_offset())[2]
                gtk_ed.remove_all_tags(actions[0][1], max_loc)

                for tag, start, end in actions:
                    gtk_ed.apply_tag(tag, start, end)
                    yield

        if max_loc and not self.sync_stop:
            gtk_ed.remove_all_tags(max_loc, gtk_ed.get_end_iter())

    def gtk_highlight(self, gtk_ed):
        for _ in self.highlight_gen(gtk_ed, -1):
            pass

    def gtk_highlight_region(self, gtk_ed, start_line):
        for _ in self.highlight_gen(gtk_ed, start_line):
            pass

    @staticmethod
    def gtk_idle_highlight(gtk_ed, gen, gran=1000, max_time=0.10):
        fn = gen_to_fn(gen, gran, max_time)
        if fn():
            gtk_ed.idle_highlight_id = GLib.timeout_add(120, fn)

    def init_highlighting(self, ed):
        ed.highlighting_initialized = True
        gtk_ed = get_gtk_buffer(ed)
        gtk_ed.stacks = HighlighterStacks()

        if not hasattr(gtk_ed, "idle_highlight_id"):
            gtk_ed.idle_highlight_id = None

        def action_handler(loc):
            """@type loc: Gtk.TextIter"""
            if gtk_ed.idle_highlight_id:
                GLib.source_remove(gtk_ed.idle_highlight_id)

            # Highlight all the rest of the buffer
            # TODO: Highlight N lines synchronously, and if there are any
            # TODO: remaining, the rest in async
            self.gtk_highlight_region(gtk_ed, loc.get_line())

        # noinspection PyUnusedLocal
        def highlighting_insert_text_before(buf, loc, text, length):
            buf.insert_loc = loc.to_tuple()

        # noinspection PyUnusedLocal
        def highlighting_insert_text(buf, loc, text, length):
            nb_new_lines = len(text.split("\n")) - 1
            itr = buf.iter_from_tuple(buf.insert_loc)
            buf.stacks.insert_newlines(nb_new_lines, itr.get_line())
            action_handler(itr)

        def highlighting_delete_range_before(buf, loc, end):
            buf.nb_deleted_lines = len(
                buf.get_text(loc, end, True).split("\n")
            ) - 1

        # noinspection PyUnusedLocal
        def highlighting_delete_range(buf, loc, end):
            buf.stacks.delete_lines(buf.nb_deleted_lines, loc.get_line())
            action_handler(loc)

        gtk_ed.connect_after("insert-text", highlighting_insert_text)
        gtk_ed.connect_after("delete-range", highlighting_delete_range)
        gtk_ed.connect("delete-range", highlighting_delete_range_before)
        gtk_ed.connect("insert-text", highlighting_insert_text_before)


def gps_fun(fun):

    def __gps_to_gtk_fun(start, end, *args, **kwargs):
        gtk_ed = get_gtk_buffer(start.buffer())
        gtk_start = gtk_ed.get_iter_at_offset(start.offset())
        gtk_end = gtk_ed.get_iter_at_offset(end.offset())
        fun(gtk_ed, gtk_start, gtk_end, *args, **kwargs)

    return __gps_to_gtk_fun

Highlighter.idle_highlight = gps_fun(Highlighter.gtk_idle_highlight)


def register_highlighter(language, *args, **kwargs):
    highlighter = Highlighter(*args, **kwargs)
    HighlighterModule.highlighters[language] = highlighter


class HighlighterModule(Module):

    highlighters = {}

    def setup(self):
        pass

    def init_highlighting(self, f):
        highlighter = self.highlighters.get(f.language(), None)
        if isinstance(highlighter, Highlighter):
            ed = GPS.EditorBuffer.get(f)
            gtk_ed = get_gtk_buffer(ed)
            highlighter.init_highlighting(ed)
            gen = highlighter.highlight_gen(gtk_ed)
            highlighter.gtk_idle_highlight(gtk_ed, gen)

    def gps_started(self):
        self.init_highlighting(GPS.EditorBuffer.get().file())

    def file_edited(self, f):
        """
        This hook is called when a new file editor is being opened
        """
        self.init_highlighting(f)
