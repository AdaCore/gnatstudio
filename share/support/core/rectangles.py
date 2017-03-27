"""
This file provides support for rectangle-mode in an editor.

In particular, it is possible to select a rectangular area,
cut it, and paste it elsewhere.

To perform a selection, use the standard selection mechanisms (e.g.
mouse+click selection, or keyboard shift+arrows).

The highlighting of the selection itself is done on the whole
lines, not on the rectangle itself, but the rectangle is the
part between the column of the start of the selection, and the
column of the end of the selection. For instance:

    ABCD
    EFGH
    IJKL

If the selection goes from B to K, the rectangle will be:
     BC
     FG
     JK
However, the highlighting will include:
     BCD
    EFGH
    IJK

Cutting the rectangle will result in:
    AD
    EH
    IL

If you move the cursor before "A", and paste the rectangle:
    BCAD
    FGEH
    JKIL
"""


############################################################################
# No user customization below this line
############################################################################

from GPS import *
import traceback
import string
from gps_utils import interactive, with_save_excursion


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_cut():
    """Cut the selected rectangle into the clipboard"""
    Rectangle.from_buffer(EditorBuffer.get()).cut()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_copy():
    """Copy the selected rectangle into the clipboard"""
    Rectangle.from_buffer(EditorBuffer.get()).copy()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_paste():
    """Paste the last entry in the clipboard as a rectangle
       in the current editor
    """
    Rectangle.paste(loc=EditorBuffer.get().current_view().cursor())


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_delete():
    """Delete the selected rectangle"""
    Rectangle.from_buffer(EditorBuffer.get()).delete()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_clear():
    """Replaces the contents of the rectangle with spaces"""
    Rectangle.from_buffer(EditorBuffer.get()).clear()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_open():
    """
Insert blank spaces to fill the selected rectangle.
This pushes its text to the right"""
    Rectangle.from_buffer(EditorBuffer.get()).open()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_string(text=None):
    """
Replaces the contents of the rectangle with TEXT on each line.
If TEXT is narrower or wider than the rectangle, the text is shifted
right or left as appropriate.
If TEXT is unspecified, an interactive dialog is open.
    """

    if not text:
        text = MDI.input_dialog('Text to replace each line with:', """""")
        if not text:
            return
        text = text[0]
    Rectangle.from_buffer(EditorBuffer.get()).string(text)


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_insert(text=None):
    """
Insert TEXT at the beginning of each line of the selected rectangle.
If TEXT is unspecified, an interactive dialog is open"""

    if not text:
        text = MDI.input_dialog('Text to insert before each line:', """""")
        if not text:
            return
        text = text[0]
    Rectangle.from_buffer(EditorBuffer.get()).insert(text)


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_sort():
    """
Sort the lines included in the rectangle, based on the contents of
the rectangle.
    """
    Rectangle.from_buffer(EditorBuffer.get()).sort()


@interactive("Editor", "Source editor")
@with_save_excursion
def rectangle_sort_reverse():
    """
Sort in reverse order the lines included in the rectangle, based on
the contents of the rectangle.
    """
    Rectangle.from_buffer(EditorBuffer.get()).sort(revert=True)


##############################################################################
# No public function below this
##############################################################################


class Rectangle(object):

    @staticmethod
    def from_buffer(buffer):
        start = buffer.selection_start()
        end = buffer.selection_end()

        if start.column() == end.column():
            # An empty rectangle ?
            return Rectangle(
                buffer=buffer,
                start_line=start.line(),
                start_col=start.column(),
                end_line=end.line(),
                end_col=end.column(),
                empty=True)
        else:
            end -= 1
            return Rectangle(
                buffer=buffer,
                start_line=start.line(),
                start_col=start.column(),
                end_line=end.line(),
                end_col=end.column(),
                empty=False)

    def __init__(self, buffer, start_line, start_col, end_line,
                 end_col, empty=False):
        """Create a new rectangle.
         Internally, ensures that start_line <= end_line and
         start_col <= end_col
        """

        self.buffer = buffer
        self.start_line = min(start_line, end_line)
        self.end_line = max(start_line, end_line)
        self.start_col = min(start_col, end_col)
        self.end_col = max(start_col, end_col)
        self.empty = empty

    def insert(self, text):
        """Insert TEXT at the beginning of each line of the rectangle."""
        self.__apply(self.__insert_func, self.__open_line_func, text)

    def open(self):
        """Insert blank spaces to fill the selected rectangle.
         This pushes its text to the right"""
        self.__apply(self.__insert_func, self.__open_line_func, ' ' *
                     (self.end_col - self.start_col + 1))

    def copy(self):
        """Copy the selected rectangle into the clipboard"""

        start = EditorLocation(self.buffer, self.start_line,
                               self.start_col).create_mark()
        end = EditorLocation(self.buffer, self.end_line,
                             self.end_col).create_mark()
        self.__apply(self.__cut_func, self.__copy_empty_func, True, True)
        self.buffer.select(start.location(), end.location())

    def sort(self, revert=False):
        """Sort the selected lines using the rectangle columns as the key"""
        self.__do_sort(revert)

    def delete(self):
        """Delete the selected rectangle"""
        self.__apply(self.__cut_func, None, False, False)

    def cut(self):
        """Cut the selected rectangle into the clipboard"""
        self.__apply(self.__cut_func, self.__copy_empty_func, True, False)

    def clear(self):
        """Replaces the contents of the rectangle with spaces"""
        self.__apply(self.__replace_func, None,
                     ' ' * (self.end_col - self.start_col + 1))

    def string(self, text):
        """Replaces the contents of the rectangle with TEXT on each line.
         If TEXT is narrower or wider than the rectangle, the text is shifted
         right or left as appropriate."""
        self.__apply(self.__replace_func, self.__open_and_insert_func, text)

    @staticmethod
    def paste(loc):
        """Paste the last entry in the clipboard as a rectangle at LOC"""
        try:
            buffer = loc.buffer()
            start = loc
            selection = Clipboard.contents()[Clipboard.current()]

            with buffer.new_undo_group():
                for line in selection.splitlines():
                    buffer.insert(start, line)
                    start = buffer.at(start.line() + 1,
                                      start.column())
        except:

            Logger('TESTSUITE').log('Unexpected exception: ' +
                                    traceback.format_exc())

    def __cut_func(self, start, end, in_clipboard, copy):
        if in_clipboard:
            append = start.line() != self.start_line
            if start <= end:
                if copy:
                    self.buffer.copy(start, end, append=append)
                else:
                    self.buffer.cut(start, end, append=append)
            if end.line() != self.end_line:
                Clipboard.copy(text='\n', append=True)
        else:
            self.buffer.delete(start, end)

    def __insert_func(self, start, end, text):
        "Fills the range START..END with spaces and moves the text rightward"
        self.buffer.insert(start, text)

    def __replace_func(self, start, end, text):
        """Replaces the range START .. END with TEXT"""

        if not self.empty:
            self.buffer.delete(start, end)
        self.buffer.insert(start, text)

    def __open_line_func(self, func, eol, *args):
        self.buffer.insert(eol, ' ' * (self.start_col - eol.column()))
        current = EditorLocation(self.buffer, eol.line(), self.start_col)
        func(current, current - 1, *args)

    def __open_and_insert_func(self, func, eol, text):
        self.buffer.insert(eol, ' ' * (self.start_col - eol.column()) + text)

    def __copy_empty_func(self, func, eol, *args):
        Clipboard.copy(text='\n', append=True)

    def __apply(self, func, short_line_func, *args):
        """Applies FUNC for each line segment in SELF.
         FUNC is called wth two parameters of type EditorLocation for the
         range that FUNC should apply on.
         SHORT_LINE_FUNC is called when the current line is shorter than
         self.start_col.
         It receives a single parameter (+ args), the last char in the line.
         Any additional parameter specified in ARGS is passed to FUNC.
        """

        try:
            line = self.start_line
            with self.buffer.new_undo_group():
                while line <= self.end_line:
                    # Some lines might not include enough characters for the
                    # rectangle
                    eol = EditorLocation(self.buffer, line, 1).end_of_line()
                    if eol.column() > self.end_col:
                        endcolumn = EditorLocation(self.buffer,
                                                   line, self.end_col)
                        current = EditorLocation(self.buffer,
                                                 line, self.start_col)
                        func(current, endcolumn, *args)
                    else:
                        if eol.column() < self.start_col:
                            if short_line_func:
                                short_line_func(func, eol, *args)
                        else:
                            current = EditorLocation(self.buffer, line,
                                                     self.start_col)
                            eol = current.end_of_line()
                            if eol.column() != 1:
                                func(current, eol - 1, *args)
                            else:
                                func(current, eol, *args)

                    line += 1

        except:
            Logger('TESTSUITE').log('Unexpected exception: ' +
                                    traceback.format_exc())

    def __sort_func(self, s1, s2):
        """Internal routine comparing s1 and s2 values for the column of the
         rectangle selection."""

        l = self.end_col - self.start_col + 1

        ls1 = """"""
        if len(s1) >= self.end_col:
            ls1 = s1[self.start_col - 1:self.end_col]
        elif len(s1) > self.start_col:
            ls1 = s1[self.start_col - 1:]
        ls1 = ls1 + ' ' * (l - len(ls1))

        ls2 = """"""
        if len(s2) >= self.end_col:
            ls2 = s2[self.start_col - 1:self.end_col]
        elif len(s2) > self.start_col:
            ls2 = s2[self.start_col - 1:]
        ls2 = ls2 + ' ' * (l - len(ls2))

        if ls1 < ls2:
            return -1
        elif ls1 == ls2:
            return 0
        else:
            return 1

    def __do_sort(self, revert=False):
        start = self.buffer.selection_start().beginning_of_line()
        to = self.buffer.selection_end().end_of_line()

        # get all the lines
        selection = self.buffer.get_chars(start, to)

        lines = string.split(selection, '\n')
        # strip off extraneous trailing "" line
        lines = lines[:-1]
        lines.sort(self.__sort_func)

        if revert:
            lines.reverse()
        with self.buffer.new_undo_group():
            self.buffer.delete(start, to)
            self.buffer.insert(start, '\n'.join(lines) + '\n')
