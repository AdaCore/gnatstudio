"""
Test the highlighter.engine.markup_for_text API
"""

from GPS import *
from gs_utils.internal.utils import *
from highlighter.engine import markup_for_text

# Random string with Ada, Python and Asm elements
TEXT = """
procedure Foo is
   X : Natural := 1111;  --  0x1fb345
begin
   null; # Not an Ada Comment
   break
end Foo;"""

EXPECTED_PYTHON = """
procedure Foo <span foreground="#f0f08d8d2424">is</span>
   X : Natural := <span foreground="#4242b4b40">1111</span>;  --  0x1fb345
begin
   null; <span foreground="#72729f9fcfcf"># Not an Ada Comment
</span>   <span foreground="#f0f08d8d2424">break</span>
end Foo;"""

EXPECTED_ASM = """<span foreground="#f0f08d8d2424">
procedure</span> Foo is
<span foreground="#f0f08d8d2424">   X</span> : Natural := <span foreground="#4242b4b40">1111</span>;  --  0x1fb345
<span foreground="#f0f08d8d2424">begin</span>
<span foreground="#f0f08d8d2424">   null</span>; <span foreground="#72729f9fcfcf"># Not an Ada Comment
</span><span foreground="#f0f08d8d2424">   break</span>
<span foreground="#f0f08d8d2424">end</span> Foo;"""

ERROR_TEMPLATE = "Issue when generating markup for %s (cond=%s)"


@run_test_driver
def run_test():
    # The language name should be case insensitive, use weird casing
    for cond in (True, False):
        gps_assert(
            markup_for_text("PyThon", TEXT, cond),
            EXPECTED_PYTHON,
            ERROR_TEMPLATE % ("Python", cond),
        )
        gps_assert(
            markup_for_text("ASM", TEXT, cond),
            EXPECTED_ASM,
            ERROR_TEMPLATE % ("ASM", cond),
        )
        gps_assert(
            markup_for_text("Not_A_Language", TEXT, cond),
            TEXT,
            ERROR_TEMPLATE % ("a Fake language", cond),
        )
