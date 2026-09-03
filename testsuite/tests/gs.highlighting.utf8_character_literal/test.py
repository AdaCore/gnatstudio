"""
Ada_Analyzer used to lose a line on an incomplete character literal whose line
ends with a multi-byte character.

The scan for the closing apostrophe stepped over the opening apostrophe and
the character it quotes with plain byte arithmetic, `P := P + 2`. When the
quoted character is multi-byte that lands in the middle of it, and Next_Char
-- which advances by a whole character -- then jumped from there clear past
the end of the line, onto the line terminator. The enclosing loop stepped over
that terminator without counting the line, so every entity reported afterwards
paired a stale line number with a column measured on a later line. A byte
position that does not start a character is fatal to gtk+:

  Gtk-WARNING **: gtktextiter.c:3905: Incorrect byte offset 29 falls in the
  middle of a UTF-8 character; this will crash the text buffer.
  Gtk:ERROR:gtktextsegment.c:195:_gtk_char_segment_new: assertion failed:
  (gtk_text_byte_begins_utf8_char (text))

Line 2 has nothing to step over at all: an apostrophe that ends the line is
taken for an isolated one -- an attribute tick, say -- rather than the start
of a character literal, so it is marked as an operator and no character
literal is reported for it.

The three incomplete literals that follow are the three places the byte step
could land inside a multi-byte character. Line 3 quotes a multi-byte character
that ends the line, so it landed on a continuation byte; from there Next_Char
advanced a single byte and the scan stumbled out of the character instead of
stepping over it. Line 4 quotes a single-byte character and then ends with a
multi-byte one, so it landed on that character's first byte and Next_Char
jumped over the whole of it, past the end of the line. Line 5 quotes a
multi-byte character and ends with another, so it landed inside the first one
and then reached the second one's first byte, past the end of the line again.
Line 6 is the control: a well formed multi-byte character literal, which was
always scanned correctly and still has to be.

The expectations below are therefore mostly about lines: each literal, the
comment on line 8 and the keywords have to be marked on the line they are
really on.
"""

import GPS
from gs_utils.internal.utils import *

contents = (
    "procedure Char_Literal is\n"
    "   A : Character := '\n"
    "   B : Character := '‘\n"
    "   C : Character := 'a‘\n"
    "   D : Character := '‘’\n"
    "   E : Character := '‘';\n"
    "begin\n"
    "   null;  --  ‘9’\n"
    "end Char_Literal;\n"
)

expect_character = """\
.........................
.....................
....................##
....................###
....................###
....................###.
.....
.................
.................
"""

expect_comment = """\
.........................
.....................
......................
.......................
.......................
........................
.....
..........#######
.................
"""

expect_keyword = """\
#########..............##
.....................
......................
.......................
.......................
........................
#####
...####..........
###..............
"""


@run_test_driver
def run_test():
    #  Read the curly quotes as the multi-byte characters they are: the
    #  default charset would turn each of them into three separate characters
    #  and defeat the whole point of the test.

    GPS.Preference("General-Charset").set("UTF-8")

    b = GPS.EditorBuffer.get(GPS.File("char_literal.adb"))
    yield wait_idle()

    gps_assert(b.get_chars(), contents, "char_literal.adb was not read back intact")
    gps_assert(
        b.debug_dump_syntax_highlighting("Character_Text"),
        expect_character,
        "Wrong character literal highlighting",
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
