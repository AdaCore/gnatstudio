"""
A simplified version of a reported crash: GNAT Studio aborted, taking any
unsaved work with it, on an Ada file holding curly quotes -- perfectly valid
UTF-8, just not what Ada expects.

Ada_Analyzer scanned the unterminated string literal on line 2 one character
past the end of the line, because it compared a position advanced character by
character against the position of the *last byte* of the line, and the line
ends with a multi-byte character. That left the scan on the line terminator,
which the main loop then stepped over without counting the line, so every
entity reported afterwards paired a stale line number with a column measured
on a later line. Three of those landed in the middle of the curly quotes on
the comment line, and a byte position that does not start a character is fatal
to gtk+:

  Gtk-WARNING **: gtktextiter.c:3905: Incorrect byte offset 29 falls in the
  middle of a UTF-8 character; this will crash the text buffer.
  Gtk:ERROR:gtktextsegment.c:195:_gtk_char_segment_new: assertion failed:
  (gtk_text_byte_begins_utf8_char (text))

Set_Line_Index returns an iterator pointing *inside* a character, and gtk+
aborts the process as soon as that iterator is used to apply a tag.

The expectations below are therefore mostly about lines: the string on line 2,
the comment on line 3 and the keywords on lines 1 and 4 all have to be marked
on the line they are really on.
"""

import GPS
from gs_utils.internal.utils import *

contents = (
    "procedure Main is\n"
    '   S : String := "unterminated ‘\n'
    "   --  ‘0’ .. ‘9’\n"
    "begin null; end Main;\n"
)

expect_string = """.................
.................###############
.................
.....................
"""

expect_comment = """.................
................................
...##############
.....................
"""

expect_keyword = """#########......##
................................
.................
#####.####..###......
"""

#  lone_quote.adb, once the legacy Ada formatter has been run over it

lone_quote_formatted = """procedure Lone_Quote is
   S : String := F ("
     X : Integer := 1;
begin
   null;
end Lone_Quote;
"""


@run_test_driver
def run_test():
    #  Read the curly quotes as the multi-byte characters they are: the
    #  default charset would turn each of them into three separate characters
    #  and defeat the whole point of the test.

    GPS.Preference("General-Charset").set("UTF-8")

    #  Opening the file used to abort the process, so everything below is
    #  unreachable when the line is lost.

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()

    gps_assert(b.get_chars(), contents, "main.adb was not read back intact")
    gps_assert(
        b.debug_dump_syntax_highlighting("String_Text"),
        expect_string,
        "Wrong string highlighting",
    )
    gps_assert(
        b.debug_dump_syntax_highlighting("Comment_Text"),
        expect_comment,
        "Wrong comment highlighting",
    )
    gps_assert(
        b.debug_dump_syntax_highlighting("Keyword_Text"),
        expect_keyword,
        "Wrong keyword highlighting",
    )

    #  A second syntax error handled by the same branch of the parser: an
    #  opening delimiter that is itself the last character of the line. The
    #  scan cannot advance, so the character the recovery tests is the opening
    #  delimiter, and the literal was taken for a terminated one -- leaving
    #  the parenthesis just before it on the stack.
    #
    #  None of that reaches the highlighting: the recovery only resets the
    #  parentheses stack, which the indentation is computed from. Left open,
    #  it drags every line that follows, `begin` and `end` included, out to
    #  the column of the unclosed parenthesis. The exact indentation of a
    #  buffer that does not parse is not meaningful in itself; what this pins
    #  down is that it does not run away.

    GPS.Preference("Editor-Range-Formatter-ada").set("Legacy")

    lone = GPS.EditorBuffer.get(GPS.File("lone_quote.adb"))
    yield wait_idle()

    lone.select()
    GPS.execute_action("Format Selection")
    yield wait_idle()

    gps_assert(
        lone.get_chars(include_hidden_chars=False),
        lone_quote_formatted,
        "Wrong indentation after an unterminated string literal",
    )
