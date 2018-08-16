"""Defines editor-specific functions

YOU SHOULD ALMOST ALWAYS LOAD THIS FILE

This script defines a number of python functions and GPS actions that can
be used inside GPS editors. These can be used to move the cursor around or
edit the text.
They are often programmed so that they emulate the Emacs editor, but they
are independent of the Emacs mode and do not override any key shortcut. As
a result you can define your own shortcuts for the actions defined in this
package.
See also emacs.xml
"""


############################################################################
# No user customization below this line
############################################################################

import GPS
from gi.repository import Gtk
from gps_utils import interactive, filter_text_actions, with_save_excursion, \
    in_ada_file, get_focused_widget, make_interactive, hook


should_extend_selection = False
# Whether the selection should be extended when moving the cursor

transient_mark_pref = GPS.Preference("Src-Editor-Transient-Mark")

SUBPROGRAM_BLOCKS = set(["CAT_PROCEDURE", "CAT_FUNCTION", "CAT_ENTRY",
                         "CAT_PROTECTED", "CAT_TASK", "CAT_PACKAGE"])
# The block_types that are considered to be "subprogram" blocks


def parse_parentheses(editor, begin=None, end=None):
    """
    Parse parentheses of editor
    range: begin to end inclusive.
    Returns the parenthesis stack. Each element is parentheses: location int
    """
    # set the default begin and end if not provided
    if begin is None:
        begin = editor.beginning_of_buffer()
    if end is None:
        end = editor.end_of_buffer()

    pairs = {"(": ")", "[": "]", "{": "}"}
    h = pairs.keys()
    t = pairs.values()
    stack = []
    source = editor.get_chars(begin, end).rstrip("\n").splitlines()
    last = end.line()-1

    # parse all parentheses, find open parentheses
    for i in range(0, len(source)):
        if source[i].lstrip(" ").startswith("#"):
            source[i] = ""
        for j, c in enumerate(source[i]):
            if c in h:
                stack.append((i, j))
            elif c in t:
                if not stack:
                    continue
                elif pairs[source[stack[-1][0]][stack[-1][1]]] == c:
                    # when parenthesis is closed, remember its line number
                    last = stack.pop()[0]

    closed = (len(stack) == 0)
    # get the last char of parsed text
    tail = editor.at(end.line(), end.end_of_line().column()-1).get_char()

    # if the parsed text is ending a parenthesis -->
    # last char is a closing parentheses, then the cursor should
    # return to where the openning counterparts's line start
    if tail in t and closed:
        tmp = source[last]
        start = len(tmp) - len(tmp.lstrip(" "))
        stack.append((last, start-1))
    return (stack, closed)


def forward_until(loc, pred,
                  skip_first_char=False,
                  stop_at_eol=False,
                  backwards=False,
                  give_up=True):

    step = -1 if backwards else 1
    cur_loc = loc

    if skip_first_char:
        cur_loc = cur_loc.forward_char(step)

    while not pred(cur_loc.get_char()):
        if cur_loc.get_char() == "\n" and stop_at_eol:
            if give_up:
                return loc
            return cur_loc

        if cur_loc == cur_loc.forward_char(step):
            if give_up:
                return loc
            return cur_loc

        cur_loc = cur_loc.forward_char(step)
    return cur_loc


def replace(frm, to, text):
    """Replace a part of the buffer by the given text"""
    frm.buffer().delete(frm, to)
    frm.buffer().insert(frm, text)


def goto_subprogram_start(cursor):
    """
    Return an EditorLocation corresponding to the subprogram in which
    cursor is
    """

    if cursor.block_type() == "CAT_UNKNOWN":
        return None

    min = cursor.buffer().beginning_of_buffer()
    cursor = cursor.block_start()
    while cursor.block_type() not in SUBPROGRAM_BLOCKS and cursor > min:
        cursor = cursor.block_start() - 1

    if cursor >= min:
        return cursor
    else:
        return None


def get_local_vars(subprogram):
    """
    Return a list of GPS.Entity that are variables local to the subprogram. It
    might not work accurately with nested subprograms
    """
    result = []
    if subprogram:
        locFile = subprogram.body().file()
        locFrom = subprogram.body().line()
        locTo = subprogram.end_of_scope().line()

        for e in locFile.entities(local=True):
            decl = e.declaration()
            if not e.is_type() \
               and decl.file() == locFile \
               and decl.line() >= locFrom \
               and decl.line() <= locTo:
                result.append(e)

    return result


def delete_until_char(char, buffer=None):
    """
    Delete all characters forward from the current cursor position, until CHAR
    is seen. CHAR itself is also deleted.  If the current character is CHAR, it
    is skipped and the next occurrences of CHAR is searched.
    """
    if not buffer:
        buffer = GPS.EditorBuffer.get()

    start = buffer.current_view().cursor()
    end = start + 1
    while end.get_char() != char:
        end = end + 1
    buffer.delete(start, end)


@interactive("Editor", name="zap to char")
class Zap_To_Char(GPS.CommandWindow):

    """
    Deletes all characters from the cursor position up to and including the
    next occurrence of a character. The character is queried interactively
    """

    def __init__(self):
        GPS.CommandWindow.__init__(
            self,
            prompt="Zap to char:",
            on_changed=self.on_changed)

    @with_save_excursion
    def on_changed(self, input, cursor_pos):
        delete_until_char(char=input)
        self.hide()


@interactive("Editor",  name="toggle wrapping")
def toggle_editor_wrapping():
    """Toggle word wrapping in the current editor"""

    buffer = GPS.EditorBuffer.get()
    v = buffer.current_view()
    from pygps import get_widgets_by_type
    text_view = get_widgets_by_type(Gtk.TextView, v.pywidget())[0]
    if text_view.get_wrap_mode() == Gtk.WrapMode(0):
        text_view.set_wrap_mode(Gtk.WrapMode(2))
    else:
        text_view.set_wrap_mode(Gtk.WrapMode(0))


@interactive("Editor", in_ada_file, name="subprogram box")
@with_save_excursion
def add_subprogram_box():
    """
    Search backward for the first subprogram or package declaration. Before
    the start of this declaration, insert a comment box containing the name of
    the subprogram. This provides helpful separations between subprograms, and
    is similar to the style used in the GNAT compiler or GPS themselves
    """

    buffer = GPS.EditorBuffer.get()
    loc = goto_subprogram_start(buffer.current_view().cursor())
    if loc:
        name = loc.block_name()
        loc = loc.block_start().beginning_of_line()
        dashes = '-' * (len(name) + 6)
        box = dashes + "\n" + "-- " + name + " --\n" + dashes + "\n\n"
        buffer.insert(loc, box)
        buffer.indent(loc, loc.forward_line(3))


@interactive("Editor", "Source editor", name="select line")
def select_line():
    """
    Select the current line in the current editor, including trailing newline
    This moves the cursor to the end of the line
    """
    buffer = GPS.EditorBuffer.get()
    loc = buffer.current_view().cursor()
    buffer.select(loc.beginning_of_line(), loc.end_of_line() + 1)


@interactive("Editor", "Source editor", name="select subprogram")
def select_enclosing_block():
    """
    Select the subprogram which contains the current selection.

    If there is no selection, select the block that contains the cursor.
    If there is a selection, select the subprogram which contains the first
    line of the selection - so you can call this action multiple times in
    a row to select parent subprograms.
    """
    b = GPS.EditorBuffer.get()
    sel_start = b.selection_start()

    # the first line is already selected? There isn't an "enclosing"
    # subprogram, so let's select everything.
    if sel_start.line() == 1:
        b.select()
        return

    # Find the enclosing subprogram
    loc = sel_start

    while (loc.block_type() not in SUBPROGRAM_BLOCKS and
           loc.line() > 1) or (sel_start == loc.block_start()):
        start = loc.block_start()
        if start.line() == 1:
            loc = start
            break
        else:
            loc = b.at(start.line() - 1, 1)

    if loc.line() <= 1:
        b.select()
        return

    b.select(loc.block_start(), loc.block_end())


def get_selection_or_buffer(buffer=None):
    """
    If a selection exists, returns its beginning and end. Otherwise return the
    beginning and end of buffer.  The buffer is returned as the first field of
    the tuple.
    """
    if not buffer:
        buffer = GPS.EditorBuffer.get()
    start = buffer.selection_start()
    end = buffer.selection_end()
    if start == end:
        return (buffer, buffer.beginning_of_buffer(), buffer.end_of_buffer())
    else:
        return (buffer, start, end)


def get_selection_or_word(buffer=None):
    """
    If a selection exists, returns its beginning and end. Otherwise return the
    beginning and end of the current word..  The buffer is returned as the
    first field of the tuple
    """
    if not buffer:
        buffer = GPS.EditorBuffer.get()
    start = buffer.selection_start()
    end = buffer.selection_end()
    if start == end:
        loc = buffer.current_view().cursor()
        return (buffer, goto_word_start(loc), goto_word_end(loc))
    else:
        return (buffer, start, end)


def get_selection_or_line(buffer, location):
    """
    If a selection exists, returns its beginning and end. Otherwise return the
    beginning and end of line.  The buffer is returned as the first field of
    the tuple
    """

    if isinstance(location, GPS.FileLocation):
        location = buffer.at(location.line(), location.column())

    buffer = location.buffer()
    start = buffer.selection_start()
    end = buffer.selection_end()
    if start == end:
        return (buffer, location.beginning_of_line(), location.end_of_line())
    else:
        return (buffer, start, end)


@interactive("Editor", "Source editor", name="Move block right")
def move_block(chars=1):
    """
    Move the current selection chars characters to the right. If chars is
    negative, moves to the left. If there is no selection, indent the current
    line.
    """

    buffer = GPS.EditorBuffer.get()
    tab_width = 8

    # Determine extents of the selection
    start_line = buffer.selection_start().line()
    end_line = buffer.selection_end().line()

    beg_loc = buffer.selection_start().beginning_of_line()
    end_loc = buffer.selection_end().end_of_line()

    had_selection = not (buffer.selection_start() == buffer.selection_end())

    if not had_selection:
        cursor_loc = buffer.current_view().cursor()
        cursor_line = cursor_loc.line()
        cursor_col = cursor_loc.column()

    end_loc = end_loc.forward_char(-1)

    text = buffer.get_chars(beg_loc, end_loc)

    newtext = []
    for line in text.split('\n'):
        if chars > 0:
            # Insert x chars at the beginning of the line
            newtext += [" " * chars + line]
        else:
            # ... remove x blanks from the beginning of the text ...

            for c in range(-chars):
                if line == "":
                    break
                if line[0] == '\t':
                    line = " " * (tab_width - 1) + line[1:]
                elif line[0] == ' ':
                    line = line[1:]
                else:
                    break
            newtext += [line]

    with buffer.new_undo_group():
        buffer.delete(beg_loc, end_loc)
        buffer.insert(buffer.at(start_line, 1), "\n".join(newtext))

    if had_selection:
        # Reselect the range of lines
        start_loc = buffer.at(start_line, 1)
        end_loc = buffer.at(end_line, 1).end_of_line()
        buffer.select(start_loc, end_loc)
    else:
        # Replace the cursor
        buffer.current_view().goto(
            buffer.at(
                cursor_line,
                max(0, cursor_col + chars)))


make_interactive(lambda: move_block(-1),
                 category="Editor", filter="Source editor",
                 name="Move block left")


@interactive("Editor", "Source editor")
@with_save_excursion
def untabify():
    """
    Replace tab characters in the current selection (or the whole buffer) with
    the correct amount of spaces. The tab stops are every n columns where n is
    specified by a preference in the Preferences dialog.
    """

    tab_width = 8
    buffer, start, end = get_selection_or_buffer()
    while start < end:
        start = start.search("\t", dialog_on_failure=False)
        if not start:
            break
        size = tab_width - ((start[0].column() - 1) % tab_width)
        replace(start[0], start[1] - 1, " " * size)
        start = start[1]


def lines_with_digit(buffer, loc, max=None):
    """
    Return an EditorLocation pointing to the last line adjacent to loc that
    contains a digit in the same column as loc. See description of serialize ()
    for an example. The search can be limited to a specific max location
    """

    if max:
        max = max.end_of_line()
    else:
        max = buffer.end_of_buffer()

    col = loc.column() - 1
    loc2 = loc.end_of_line() + 1  # to beginning of next line
    while loc2 < max:
        eol = loc2.end_of_line()
        check = loc2 + col
        if check > eol or not buffer.get_chars(check, check).isdigit():
            return loc2 - 1
        loc2 = eol + 1  # to beginning of next line

    return max


@interactive("Editor", "Source editor")
@with_save_excursion
def serialize(increment=1):
    """
    Increment a set of numbers found on adjacent lines.  The exact behavior
    depends on whether there is a current selection or not.  If there is no
    selection, then the set of lines considered is from the current line on and
    includes all adjacent lines that have at least one digit in the original
    columns. In the following example, | marks the place where the cursor is at
    the beginning:

        AAA |10 AAA
        CCC 34567 CCC
        DDD DDD

    then only the first two lines will be modified, and will become

        AAA 10 AAA
        CCC 11 CCC
        DDD DDD

    If there is a selection, all the lines in the selection are
    modified. For each line, the columns that had digits in the first
    line are modified, no matter what they actually contain. In the
    example above, if you select all three lines, the replacement becomes

        AAA 10 AAA
        CCC 11567 CCC
        DDD 12D

    ie only the fifth and sixth columns are modified since only those
    columns contained digits in the first line. This feature assumes that
    you are selecting a relevant set of lines. But it allows you to
    transform blank lines more easily. For instance, if you have

        AAA 1
        BBB
        CCC

    this is transformed into

        AAA 1
        BBB 2
        CCC 3
    """

    buffer = GPS.EditorBuffer.get()
    start = buffer.selection_start()
    end = buffer.selection_end()
    if start == end:
        has_sel = False
        start = buffer.current_view().cursor()
        end = lines_with_digit(buffer, start)
    else:
        has_sel = True
    loc = start

    # From start .. end, all lines are equal now
    end = end.end_of_line()

    # Find the range of text to replace on each line
    repl = loc
    while buffer.get_chars(repl, repl).isdigit():
        repl = repl + 1

    frm_col = loc.column() - 1    # columns start at 0 on a line
    end_col = (repl - 1).column() - 1

    try:
        value = int(buffer.get_chars(loc, repl - 1)) + increment
    except Exception:
        GPS.Console().write("Cursor must be before a number")
        return

    format = "%0" + str(end_col - frm_col + 1) + "d"

    # And now do the replacement
    repl = loc.end_of_line() + 1  # to beginning of next line
    while repl < end:
        if has_sel:
            # We had a selection: make sure the column range exists on the
            # line, and fill it with the value
            eol = repl.end_of_line()
            if repl + frm_col > eol:
                buffer.insert(eol,
                              " " * ((eol - repl) - frm_col + 2) +
                              format % value)
            else:
                replace(repl + frm_col, min(repl + end_col, eol),
                        format % value)
        else:
            # We had no selection: replace the digit, no matter how many cols
            to = repl + frm_col
            while buffer.get_chars(to, to).isdigit():
                to = to + 1
            replace(repl + frm_col, to - 1, format % value)

        repl = repl.end_of_line() + 1
        value = value + increment


@interactive("Editor", "Writable source editor", name="kill forward")
def delete_forward():
    """Delete the character just after the cursor in the current editor"""
    buffer = GPS.EditorBuffer.get()
    cursor = buffer.current_view().cursor()
    buffer.delete(cursor, cursor)


@interactive("Editor", "Writable source editor", name="Delete Line")
def delete_line():
    """
    Delete the current line and place the cursor on the beginning of the next
    line.
    """
    buffer = GPS.EditorBuffer.get()   # get the current buffer
    view = buffer.current_view()      # get the current view of this buffer
    location = view.cursor()          # get the location of the cursor

    # Get the bounds to delete
    start = location.beginning_of_line()
    end = location.end_of_line()

    # Do the deletion
    with buffer.new_undo_group():
        buffer.delete(start, end)


def kill_line(location=None, count=1):
    """
    Kills the end of the line on which LOCATION is. If LOCATION is unspecified,
    the current cursor location in the current editor is used.  If the line is
    empty or contains only white spaces, the whole line is deleted.  This is a
    better emulation of Emacs's behavior than the one provided by default by
    gtk+, which doesn't handle whitespaces correctly.  When called several
    times from the same line, entries are appended in the clipboard.  Count is
    the number of lines to delete. If greater than 1, then the whole lines are
    deleted, including newline characters.
    """
    append = GPS.last_command() == "kill line"

    def kill_line_in_entry(entry):
        """
        Kills lines in a Gtk.Entry widget.
        """
        start = entry.get_position()

        if count == 1 and entry.get_chars(start, start) == "\n":
            entry.select_region(start, start)
            entry.cut_clipboard()
        else:
            text = entry.get_chars(start, -1)
            end = text.find('\n')
            text = entry.get_chars(start, end)
            GPS.Clipboard.copy(text, append=append)
            entry.delete_text(start, end)

    def kill_line_in_text_view(text_view):
        """
        Kills lines in a Gtk.TextView widget.
        """
        text_buffer = text_view.get_buffer()
        start = text_buffer.get_iter_at_mark(text_buffer.get_insert())
        eol = start.copy()

        if count == 1 and text_buffer.get_text(start, eol, False) == "\n":
            text_buffer.cut(start, start, append)
        else:
            for line in range(1, count + 1):
                eol.forward_to_line_end()
                text = text_buffer.get_text(start, eol, False)
                strip_text = text.rstrip()
                if (count == 1 and
                   len(text) > 0 and
                   text[len(text) - 1] == '\n' and strip_text != ""):
                        eol = eol.backward_char()
            eol.forward_char()

        text = text_buffer.get_text(start, eol, include_hidden_chars=False)
        GPS.Clipboard.copy(text, append=append)
        text_buffer.delete(start, eol)

    def kill_line_in_editor_buffer(location):
        """
        Kills lines in a GPS.EditorBuffer, starting from the given location.
        """

        buffer = location.buffer()
        start = location

        # In case the current location points to a line terminator we just
        # cut it.
        if count == 1 and start.get_char() == "\n":
            buffer.cut(start, start, append)
        else:
            bol = start
            for line in range(1, count + 1):
                end = bol.end_of_line()
                str = buffer.get_chars(start, end)
                strip_str = str.rstrip()
                if (count == 1 and
                   len(str) > 0 and
                   str[len(str) - 1] == '\n' and strip_str != ""):
                        end = end.forward_char(-1)
                bol = end + 1

            buffer.cut(start, end, append)

    if not location:
        buffer = GPS.EditorBuffer.get(only_if_focused=True)

        # Check if an editor is focused when no location is given.
        # If it's the case, kill the lines this editor.
        # Otherwise, check if the currently focused widget is a Gtk.Entry
        # or a Gtk.TextView.
        if buffer:
            location = buffer.current_view().cursor()
            kill_line_in_editor_buffer(location)
        else:
            focused_widget = get_focused_widget()

            if isinstance(focused_widget, Gtk.Entry):
                kill_line_in_entry(focused_widget)
            elif isinstance(focused_widget, Gtk.TextView):
                kill_line_in_text_view(focused_widget)
    else:
        kill_line_in_editor_buffer(location)


################################################
# Moving the cursor
################################################


@interactive("Editor", "Source editor",
             name="goto beginning of buffer")
def beginning_of_buffer():
    """Move the cursor to the beginning of the buffer"""
    buffer = GPS.EditorBuffer.get()
    buffer.current_view().goto(buffer.beginning_of_buffer(),
                               should_extend_selection)


@interactive("Editor", "Source editor", name="goto end of buffer")
def end_of_buffer():
    """Move the cursor to the end of the buffer"""
    buffer = GPS.EditorBuffer.get()
    buffer.current_view().goto(
        buffer.end_of_buffer(), should_extend_selection)


def _goto_line_bound(beginning, extend_selection):
    """
    Move the cursor in the current focus widget to various places in the line:
    * if beginning is True, move to the beginning of the line
      If the cursor is already in column 1, move to the first non-blank
      character on the line when in a GPS code editor.
    * else move to the end of the line
    """

    from pygps import get_widgets_by_type
    widget = get_focused_widget()

    # When in a standard Gtk_Entry field:

    if isinstance(widget, Gtk.Entry):
        bounds = widget.get_selection_bounds()
        cursor_pos = widget.get_position()
        if bounds:
            start_pos = bounds[0] if bounds[1] == cursor_pos else bounds[1]
        else:
            start_pos = widget.get_position()
        end_pos = 0 if beginning else widget.get_text_length()

        if extend_selection:
            widget.select_region(start_pos, end_pos)
        else:
            widget.set_position(end_pos)

        return

    elif not isinstance(widget, Gtk.TextView):
        # We don't know how to handle these other widgets
        return

    else:
        # We are in a GPS code editor or standard Gtk.TextView

        ed = GPS.EditorBuffer.get(open=False)
        if ed:
            gtk_ed_view = get_widgets_by_type(
                Gtk.TextView, ed.current_view().pywidget())[0]

        if not ed or gtk_ed_view != widget:
            # in a Gtk.TextView, but not a GPS code editor

            b = widget.get_buffer()
            it = b.get_iter_at_mark(b.get_mark("insert"))
            if beginning:
                if it.get_line_index() == 0:
                    # Already at beginning ? move to first non blank
                    while it.get_char() in (u' ', u'\t'):
                        it.forward_char()

                else:
                    b.place_cursor(b.get_iter_at_line_offset(it.get_line(), 0))

            else:
                it.forward_to_line_end()
                b.place_cursor(it)

        else:
            for c in ed.cursors():
                d = c.mark().location()

                if beginning:
                    if d.column() == 1:
                        # Already at beginning ? move to first non blank

                        while d.get_char() in (u' ', u'\t'):
                            d = d.forward_char(1)
                    else:
                        d = d.beginning_of_line()

                else:
                    d = d.end_of_line()

                c.move(d, extend_selection or ed.extend_existing_selection)


@interactive("Editor", filter_text_actions,
             name="goto beginning of line (extend selection)")
def goto_beginning_of_line_ext_sel():
    """
    Move the cursor to the beginning of the line:
    * if the cursor is anywhere within the line, move back to column 1
    * if the cursor is already on column 1, move to the first non-blank
      character of the line.
    This function extends the current selection while moving the cursor.
    """
    GPS.Action("Cancel completion").execute_if_possible()
    _goto_line_bound(beginning=True, extend_selection=True)


@interactive("Editor", filter_text_actions,
             name="goto beginning of line", for_learning=True)
def goto_beginning_of_line():
    """
    Move the cursor to the beginning of the line:
    * if the cursor is anywhere within the line, move back to column 1
    * if the cursor is already on column 1, move to the first non-blank
      character of the line.
    """
    GPS.Action("Cancel completion").execute_if_possible()
    _goto_line_bound(beginning=True, extend_selection=False)


@interactive("Editor", filter_text_actions,
             name="goto end of line (extend selection)")
def goto_end_of_line_ext_sel():
    GPS.Action("Cancel completion").execute_if_possible()
    _goto_line_bound(beginning=False, extend_selection=True)


@interactive("Editor", filter_text_actions, name="goto end of line",
             for_learning=True)
def goto_end_of_line():
    GPS.Action("Cancel completion").execute_if_possible()
    _goto_line_bound(beginning=False, extend_selection=False)


def end_of_line(file, line):
    """Goto to the end of the line in file"""
    buffer = GPS.EditorBuffer.get(GPS.File(file))
    loc = buffer.at(line, 1)
    buffer.current_view().goto(loc.end_of_line() - 1)


@interactive("Editor", "Writable source editor", name="forward delete")
def forward_delete():
    delete(forward=True)


@interactive("Editor", "Writable source editor", name="backward delete")
def backward_delete():
    e = GPS.EditorBuffer.get()
    cursor = e.selection_start()
    end = e.selection_end()

    # if it's python code and no block is selected

    if e.file().language() == "python":
        if end.line() == cursor.line() and end.column() == cursor.column():

            did = False

            # not include the first position
            if cursor.column() != 1:

                # see if I should forward deletion by 4
                try:
                    did = python_forward_indent(e, cursor)
                except Exception:
                    pass

            # if justice(auto indent by 4) has been done
            if did:
                return

    # otherwise python-indent not done, behave normally
    delete(forward=False)


def python_forward_indent(e, cursor):
    """
       Indent with backspace in the leading white spaces with 4
       * e is EditorBuffer
       * cursor is EditorLocation of cursor
    """
    line = e.get_chars(cursor.beginning_of_line(), cursor.end_of_line())
    spaces_len = len(line) - len(line.lstrip(" "))
    indent = 4 if spaces_len % 4 == 0 else spaces_len % 4
    # if cursor is in the middle of the leading whitespaces

    if spaces_len > 0 and spaces_len >= cursor.column()-1:

        # remove 4 blanks if possible
        e.delete(e.at(cursor.line(), 1), e.at(cursor.line(), indent))

        # adjust cursor position
        e.main_cursor().move(e.at(cursor.line(), cursor.column()-indent))

        return True

    return False


def delete(forward=True):
    """
    Helper for backward_delete/forward_delete actions, to factorize common
    functionality. Will delete the selection if there is one, or one character
    backward/forward depending on the forward parameter
    """
    ed = GPS.EditorBuffer.get()
    mc = ed.main_cursor()

    def do():
        def _delete(s, e):
            s, e = (s, e) if s < e else (e, s)
            ed.delete(s, e.forward_char(-1))

        no_selection = mc.mark().location() == mc.sel_mark().location()
        for c in ed.cursors():
            try:
                if no_selection:
                    start = c.mark().location()
                    end = start.forward_char(1 if forward else -1)
                    if end and start != end:
                        _delete(start, end)
                else:
                    _delete(c.mark().location(), c.sel_mark().location())
            except GPS.Exception:
                return

    if ed.has_slave_cursors():
        with ed.new_undo_group():
            do()
    else:
        do()


def is_space(char):
    return char == ' ' or char == '\t'


def goto_word_start(loc, underscore_is_word=True):
    """
    Move to the beginning of the current word (or leave the cursor where it
    is). This properly handles '_'
    """
    if underscore_is_word:
        while not loc.starts_word():
            loc = loc.forward_word(-1)
        return loc
    else:
        while not loc.starts_word():
            prev = loc
            loc = loc.forward_char(-1)
            c = loc.get_char()
            if c == '_':
                return prev
        return loc


def goto_word_end(loc, underscore_is_word=True):
    if underscore_is_word:
        while True:
            loc = loc.forward_word()
            try:
                if loc.get_char() != '_':
                    return loc.forward_char(-1)
            except Exception:
                return loc.buffer().end_of_buffer()

    else:
        while not loc.ends_word():
            prev = loc
            loc = loc.forward_char(1)
            try:
                if loc.get_char() == '_':
                    return prev
            except Exception:
                # Probably an invalid position.
                return loc.buffer().end_of_buffer()
        return loc


def isword(a):
    # test if a (a char) is a word
    return (a.isalpha() or a.isdigit() or a == "_")


def delete_spaces(backward=True, forward=True, leave_one=False):
    """Delete all spaces around cursor, possibly leaving one"""
    buffer = GPS.EditorBuffer.get()
    start = buffer.current_view().cursor()
    end = start
    if forward:
        max = end.end_of_line()
        while is_space(end.get_char()) and end < max:
            end = end + 1
        end = end - 1
    if backward:
        max = start.beginning_of_line()
        start = start - 1
        while is_space(start.get_char()) and start >= max:
            start = start - 1
        start = start + 1
    if start <= end:
        buffer.delete(start, end)

    if leave_one:
        buffer.insert(start, " ")


@interactive("Editor", "Writable source editor",
             name="delete horizontal space")
@with_save_excursion
def delete_horizontal_space(backward=1, forward=1):
    """
    Delete all spaces and tabs around the cursor in the current editor.  The
    two parameters can be used to control in what directions white spaces are
    searched for.
    """
    delete_spaces(leave_one=False)


@interactive("Editor", "Writable source editor", name="just one space")
@with_save_excursion
def just_one_space():
    """
    Delete all spaces and tabs around the cursor, leaving one space.  If there
    are no spaces around, a new space is inserted.
    """
    delete_spaces(leave_one=True)


@interactive("Editor", "Writable source editor", name="transpose chars")
def transpose_chars():
    """Transpose characters around cursor, moving forward one character."""
    buffer = GPS.EditorBuffer.get()
    cursor = buffer.current_view().cursor()
    if cursor > buffer.beginning_of_buffer():
        c = cursor.get_char()
        with buffer.new_undo_group():
            buffer.delete(cursor, cursor)
            buffer.insert(cursor - 1, c)
            buffer.current_view().goto(cursor + 1)


@interactive("Editor", "Writable source editor", name="Transpose lines")
def transpose_lines(location=None):
    """
    Transpose the line at LOCATION (or current line) and the previous one,
    leaving the cursor after both.
    """
    if not location:
        location = GPS.EditorBuffer.get().current_view().cursor()
    buffer = location.buffer()
    if location.line() < buffer.lines_count():
        with buffer.new_undo_group():
            start = location.beginning_of_line()
            end = location.end_of_line()
            text = buffer.get_chars(start, end)
            buffer.delete(start, end)
            buffer.insert(start.forward_line(-1), text)
            buffer.current_view().goto(start.end_of_line() + 1)


@interactive("Editor", "Writable source editor", name="open line")
@with_save_excursion
def open_line():
    """Insert a newline and leave cursor at its current place."""
    buffer = GPS.EditorBuffer.get()
    buffer.insert(buffer.current_view().cursor(), "\n")


@interactive("Editor", "Writable source editor", name="Join line")
def join_line():
    """
    Join the current line and the following one, separated by a single space,
    and leaves the cursor on the space
    """
    buffer = GPS.EditorBuffer.get()
    eol = buffer.current_view().cursor().end_of_line()
    with buffer.new_undo_group():
        buffer.current_view().goto(eol)
        buffer.delete(eol, eol)  # Newline character
        delete_spaces(backward=False, forward=True, leave_one=False)
        if not is_space(eol.forward_char(-1).get_char()):
            buffer.insert(eol, " ")


def apply_func_to_word(func, location=None):
    """
    Apply a function to the current word (starting at the current character).
    FUNC takes one argument, the text it replaces, and should return the
    replacement text
    """
    if not location:
        location = GPS.EditorBuffer.get().current_view().cursor()
    buffer = location.buffer()
    with buffer.new_undo_group():
        end = location.forward_word()
        text = func(buffer.get_chars(location, end))
        replace(location, end, text)


@interactive("Editor", "Writable source editor", name="Upper case word")
def upper_case_word(location=None):
    """Upper case the current word (starting at the current character)"""
    apply_func_to_word(str.upper, location)


@interactive("Editor", "Writable source editor", name="Lower case word")
def lower_case_word(location=None):
    """Lower case the current word (starting at the current character)"""
    apply_func_to_word(str.lower, location)


@interactive("Editor", "Writable source editor", name="Capitalize word")
def capitalize_case_word(location=None):
    """Capitalize the current word (starting at the current character)"""
    apply_func_to_word(str.capitalize, location)


@interactive("Editor", "Writable source editor", name="Center line")
def center_line():
    """
    Center the current line on the screen. If a comment line then the text
    inside the comment is centered, the comment markers remain unchanged.
    """
    buffer = GPS.EditorBuffer.get()
    location = buffer.current_view().cursor()
    initial = location.create_mark()

    with buffer.new_undo_group():
        start = location.beginning_of_line()
        end = location.end_of_line()
        text = buffer.get_chars(start, end)
        if text[0:2] == "--" or text[0:2] == "//" or text[0:2] == "##":
            start = start + 2

        if text[-3:] == "--\n" or text[-3:] == "//\n" or text[-3:] == "##\n":
            # Use right comment characters to center the text
            end = end - 3
            text = buffer.get_chars(start, end).strip()
            spaces = end.column() - start.column() + 1 - len(text)
            before = spaces / 2
            after = spaces / 2
            if before + after != spaces:
                after = after + 1
            buffer.delete(start, end)
            buffer.insert(start, ' ' * before + text + ' ' * after)
        else:
            # No right comment characters, use the highlight column to center
            # the text
            col = GPS.Preference("Src-Editor-Highlight-Column").get()
            text = buffer.get_chars(start, end).strip()
            spaces = int(col) - start.column() - len(text)
            before = spaces / 2
            buffer.delete(start, end - 1)
            buffer.insert(start, ' ' * before + text)

        # Move to next line
        buffer.current_view().goto(GPS.EditorLocation
                                   (buffer,
                                    line=initial.location().forward_line(
                                        1).line(),
                                    column=location.column()))


class BlockIterator:

    """
    An iterator for the various sections of an editor.
    Each step in the iteration returns a tuple (start, end) of EditorLocation
    instances for the section.
    The constructor parameter overlay_name can be one of:
        - "":          The whole buffer is returned
        - "selection": The current selection in the buffer is returned
        - "word":      The current word in the buffer is returned
        - overlay name: All sections for which this overlay applies are
                       returned. The name could be one of "comment",
                       "keywords", "string" or "character"
    Example of use:
        buffer = EditorBuffer.get()
        for start, end in BlockIterator (buffer, "comment"):
           ...
    """

    def __init__(self, buffer, overlay_name):
        self.mark = buffer.beginning_of_buffer().create_mark()
        if overlay_name != "" \
                and overlay_name != "selection" \
                and overlay_name != "word":
            self.overlay = buffer.create_overlay(overlay_name)
            self.in_comment = \
                buffer.beginning_of_buffer().has_overlay(self.overlay)
        else:
            self.overlay = None
            self.overlay_name = overlay_name

    def __iter__(self):
        return self

    def next(self):
        loc = self.mark.location()
        if not self.overlay:
            if loc < loc.buffer().end_of_buffer():
                self.mark.move(loc.buffer().end_of_buffer())
                if self.overlay_name == "selection":
                    return (loc.buffer().selection_start(),
                            loc.buffer().selection_end())
                elif self.overlay_name == "word":
                    cursor = loc.buffer().current_view().cursor()
                    start = cursor
                    while not start.starts_word():
                        start = start - 1
                    while not cursor.ends_word():
                        cursor = cursor + 1
                    return (start, cursor)
                else:
                    return (loc.buffer().beginning_of_buffer(),
                            loc.buffer().end_of_buffer())
            raise StopIteration
        else:
            # Find beginning of next section
            if not loc.has_overlay(self.overlay):
                loc = loc.forward_overlay(self.overlay)

            if loc >= loc.buffer().end_of_buffer():
                raise StopIteration

            loc2 = loc.forward_overlay(self.overlay)
            self.mark.move(loc2 + 1)
            return (loc, loc2 - 1)


class WordIterator:

    """
    An iterator for all words in a block. Each iteration returns a tuple
    (start, end) of EditorLocation instances.
    Example of use:
      buffer = EditorBuffer.get()
      for blockStart, blockEnd in BlockIterator (buffer, "comments"):
         for wordStart, wordEnd in WordIterator (blockStart, blockEnd):
            ...
    """

    def __init__(self, start, end):
        self.mark = start.create_mark()
        self.end = end

    def __iter__(self):
        return self

    def starts_at(self, loc):
        self.mark.move(loc)

    def next(self):
        loc = self.mark.location()
        while loc < self.end:
            loc2 = loc.forward_word()
            if loc.get_char().isalpha():
                # Use a mark, in case the buffer is modified
                self.mark.move(loc2 + 1)
                return (loc, loc2 - 1)
            else:
                loc = loc + 1
        raise StopIteration


class LineIterator:

    """
    An iterator for all lines in a block. Each iteration returns a
    tuple (start, end) of EditorLocation instances.
    """

    def __init__(self, start, end):
        self.mark = start.create_mark()
        self.end = end.create_mark()

    def __iter__(self):
        return self

    def next(self):
        loc = self.mark.location()
        if loc >= self.end.location():
            raise StopIteration
        loc2 = loc.end_of_line()
        if loc2 >= self.end.location():
            self.mark.move(self.end.location() + 1)
            return (loc, self.end.location())
        else:
            self.mark.move(loc2 + 1)
            return (loc, loc2)


# Emulating Emacs selection:
# In Emacs, one sets the mark first, then when the cursor is moved the
# selection is extended appropriately. This is rather tricky to emulate
# in gtk+.
# There are two implementations: when pygtk is available, we simply
# temporarily override the key bindings so that the selection is always
# extended. This avoids all flickering, has no run-time cost, and is
# certainly the nicest. Not quite perfect though, since other functions
# that move the cursor will not extend the selection, only the basic
# key bindings defined for a gkt.TextView do.
# However, if pygtk is not available, we emulate it by monitoring all
# location changes. The slow down is almost invisible, but since the
# selection is first cancelled by gtk+ when the cursor is moved, and we
# then reselect it, there is some flickering

HOME = 65360
LEFT = 65361
UP = 65362
RIGHT = 65363
DOWN = 65364
PAGE_UP = 65365
PAGE_DOWN = 65366
END = 65367

KP_HOME = 65429
KP_LEFT = 65430
KP_UP = 65431
KP_RIGHT = 65432
KP_DOWN = 65433
KP_PAGE_UP = 65434
KP_PAGE_DOWN = 65435
KP_END = 65436


def override_key_bindings(select):
    """Override the default TextView keybinding to either always force
       the extension the selection, or not"""

    global should_extend_selection

    Gtk.TextView()   # make sure the BindingSet was created
    bind = Gtk.binding_set_find("GtkTextView")

    def override(key, mvt, step):
        # pygobject does not have a binding to
        # gtk_binding_entry_add_signal, which would be more convenient and
        # efficient than going through a string.
        # Gtk.binding_entry_remove(bind, key, modifier)
        subst = (key, mvt, step, 1 if select else 0)
        Gtk.binding_entry_add_signal_from_string(
            bind, 'bind "%s" {"move_cursor" (%s,%s,%s)}' % subst)

    should_extend_selection = select

    override("Right",    "visual-positions", 1)
    override("KP_Right", "visual-positions", 1)
    override("Left",     "visual-positions", -1)
    override("KP_Left",  "visual-positions", -1)

    override("<ctrl>Right", "words", 1)
    override("<ctrl>KP_Right", "words", 1)
    override("<ctrl>Left", "words", -1)
    override("<ctrl>KP_Left", "words", -1)

    override("Up", "display-lines", -1)
    override("KP_Up", "display-lines", -1)
    override("Down", "display-lines", 1)
    override("KP_Down", "display-lines", 1)

    override("<ctrl>Up", "paragraph", -1)
    override("<ctrl>KP_Up", "paragraph", -1)
    override("<ctrl>Down", "paragraph", 1)
    override("<ctrl>KP_Down", "paragraph", 1)

    override("Home", "display-line-ends", -1)
    override("KP_Home", "display-line-ends", -1)
    override("End", "display-line-ends", 1)
    override("KP_End", "display-line-ends", 1)

    override("<ctrl>Home", "buffer-ends", -1)
    override("<ctrl>KP_Home", "buffer-ends", -1)
    override("<ctrl>End", "buffer-ends", 1)
    override("<ctrl>KP_End", "buffer-ends", 1)

    override("Page_Up", "pages", -1)
    override("KP_Page_Up", "pages", -1)
    override("Page_Down", "pages", 1)
    override("KP_Page_Down", "pages", 1)

    override("<ctrl>Page_Up", "horizontal-pages", -1)
    override("<ctrl>KP_Page_Up", "horizontal-pages", -1)
    override("<ctrl>Page_Down", "horizontal-pages", 1)
    override("<ctrl>KP_Page_Down", "horizontal-pages", 1)


prev_char = ''  # To pre-fill the dialog with the last char


@interactive("Editor", "Source editor", name="insert extended character")
def insert_extended_character(location=None):
    """
    Present a dialog asking for a character codepeoint (in decimal), and
    insert the character at the cursor location in the current editor.
    """
    global prev_char
    if location:
        buffer = location.buffer()
    else:
        buffer = GPS.EditorBuffer.get()
        location = buffer.current_view().cursor()

    r = GPS.MDI.input_dialog("Insert Extended Character",
                             "Character code={}".format(prev_char))

    try:
        prev_char = r[0]
        num = int(r[0].strip())
    except Exception:
        GPS.Console().write("Please enter a decimal number")
        return

    buffer.insert(location, unichr(num))


@interactive("Editor", "Source editor", name="set mark command")
def set_mark_command(location=None):
    """
    Set mark at LOCATION (or current cursor if LOCATION is unspecified) This is
    similar to Emacs's behavior: a mark is put at the current cursor position.
    You can then move the cursor elsewhere, and delete the text between this
    mark and the new cursor position. See also the action 'Cancel mark
    command'
    """
    if not location:
        location = GPS.EditorBuffer.get().current_view().cursor()

    location.buffer().extend_existing_selection = True

    location.create_mark("selection_bound")
    override_key_bindings(select=True)


@interactive("Editor", "Source editor", name="Cancel mark command")
def cancel_mark_command(buffer=None):
    """
    Cancel the mark in BUFFER Remove the emacs-emulation mark in the current
    editor. See also the action 'Set mark command'
    """
    if not buffer:
        buffer = GPS.EditorBuffer.get()

    buffer.extend_existing_selection = False

    try:
        buffer.unselect()
        override_key_bindings(select=False)
    except Exception:
        pass  # No such mark


@hook("clipboard_changed")
def __on_clipboard_changed():
    """Called when the contents of the clipboard has changed"""
    if transient_mark_pref.get():
        cancel_mark_command()


@interactive(name='New View Horizontal reuse', category='MDI')
def new_view_horizontal_reuse():
    """
    When on an editor, splits the current notebook into two side-by-side
    windows, so that the two windows show two views of the same file.

    If another window already exists to the side, a new view is created
    inside that existing notebook, rather than create a new one.
    """
    GPS.MDI.current().split(vertically=False, new_view=True, reuse=True)


@interactive(name='New View Horizontal', category='MDI')
def new_view_horizontal():
    """
    When on an editor, splits the current notebook into two side-by-side
    windows, so that the two windows show two views of the same file.
    """
    GPS.MDI.current().split(vertically=False, new_view=True, reuse=False)


@interactive(name='New View Vertical reuse', category='MDI')
def new_view_vertical_reuse():
    """
    When on an editor, splits the current notebook into two windows
    vertically, so that the two windows show two views of the same file.

    If another window already exists above or below, a new view is created
    inside that existing notebook, rather than create a new one
    """
    GPS.MDI.current().split(vertically=True, new_view=True, reuse=True)


@interactive(name='New View Vertical', category='MDI')
def new_view_vertical():
    """
    When on an editor, splits the current notebook into two windows
    vertically, so that the two windows show two views of the same file.
    """
    GPS.MDI.current().split(vertically=True, new_view=True, reuse=False)


GPS.parse_xml("""
   <action name="kill line" output="none" category="Editor">
      <description>
      This is similar to Emacs' kill-line function. It deletes the end of the
      line after the cursor's current column. If the cursor is at the
      end of the line, it deletes the newline character and therefore
      joins the current line and the next.  The text that is deleted is
      copied to the clipboard. If you call this action multiple times
      from the same location, all deleted text is merged into a single
      clipboard, so that a single Paste will put it all back.  When
      this command is executed after a repeat_next command, the whole
      line is deleted to provide a more intuitive behavior.
      </description>
      <filter id="is_text_widget" />
      <shell lang="python">
if $repeat == 1: text_utils.kill_line(None, $remaining+1)
      </shell>
   </action>
""")
