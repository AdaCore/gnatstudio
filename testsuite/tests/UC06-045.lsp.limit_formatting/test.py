"""
Test the preference LSP-Limit-Formatting.
"""

import GPS
from gs_utils.internal.utils import *


def different_lines(s1, s2):
    l1 = s1.splitlines()
    l2 = s2.splitlines()
    return len([x for x in l1 if x not in l2])


MAIN_EXPECTED = """int main ()
{
     return 0;
   }
"""

FOO_EXPECTED = """int foo ()
{
  int a;
  int b;
    return a + b;
  }
"""


@run_test_driver
def run_test():
    GPS.Preference("LSP-Limit-Formatting").set(True)

    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    init_buf = buf.get_chars(include_hidden_chars=False)
    buf.current_view().goto(buf.at(2, 1))
    send_key_event(GDK_TAB)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(different_lines(buf.get_chars(include_hidden_chars=False),
                               init_buf),
               1,
               "Too many different lines without selection")
    gps_assert(buf.get_chars(include_hidden_chars=False),
               MAIN_EXPECTED,
               "Wrong formatting without selection")

    buf = GPS.EditorBuffer.get(GPS.File("foo.cpp"))
    init_buf = buf.get_chars(include_hidden_chars=False)
    buf.select(buf.at(2, 3), buf.at(4, 8))
    send_key_event(GDK_TAB)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(different_lines(buf.get_chars(include_hidden_chars=False),
                               init_buf),
               3,
               "Too many different lines with selection")
    gps_assert(buf.get_chars(include_hidden_chars=False),
               FOO_EXPECTED,
               "Wrong formatting with selection")
