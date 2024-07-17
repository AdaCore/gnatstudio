"""
Test the highlighter.engine.markup_for_text API
"""

from GPS import *
from gs_utils.internal.utils import *
from highlighter.engine import markup_for_text

# Random string with Ada, Python and Asm elements
TEXT = r"""
// Foo
int foo()
{
  char x[] = "A String\!";
  return 1;
}"""


EXPECTED_COMPLEX = r"""
<span foreground="#72729f9fcfcf">// Foo</span>
<span foreground="#8e8e6969c9c9">int</span> foo()
{
  <span foreground="#8e8e6969c9c9">char</span> x[] = <span foreground="#f2f2d4d42c2c">"A String<span foreground="#dada74749595">\!"</span></span>";
  <span foreground="#f0f08d8d2424">return</span> <span foreground="#4242b4b40">1</span>;
}"""

EXPECTED_SIMPLE = r"""
<span foreground="#72729f9fcfcf">// Foo</span>
<span foreground="#8e8e6969c9c9">int</span> foo()
{
  <span foreground="#8e8e6969c9c9">char</span> x[] = <span foreground="#f2f2d4d42c2c">"A String\!"</span>;
  <span foreground="#f0f08d8d2424">return</span> <span foreground="#4242b4b40">1</span>;
}"""

ERROR_TEMPLATE = "Issue when generating markup (cond=%s)"


@run_test_driver
def run_test():
    gps_assert(
        markup_for_text("C", TEXT, allow_nested_tag=True),
        EXPECTED_SIMPLE,
        ERROR_TEMPLATE % True,
    )
    gps_assert(
        markup_for_text("C", TEXT, allow_nested_tag=False),
        EXPECTED_COMPLEX,
        ERROR_TEMPLATE % False,
    )
