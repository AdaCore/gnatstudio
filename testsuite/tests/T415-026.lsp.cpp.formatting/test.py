"""
This test checks that formatting works fine through LSP and clangd.
"""

import GPS
from gs_utils.internal.utils import *

expected = """
struct My_Record_Of_Record {
  struct Field1_Record {
    int a;
    int *b;
  } c;
  int d;
};

class My_Class {
public:
  int num;
  char ch;
  My_Record_Of_Record rec;
  /* This is a default constructor of the
   * class, do note that it's name is same as
   * class name and it doesn't have return type.
   */
  My_Class() {
    num = 100;
    ch = 'A';
  }
};
"""


@run_test_driver
def run_test():
    GPS.Preference("clangd-BasedOnStyle").set("LLVM")
    buf = GPS.EditorBuffer.get(GPS.File("my_class.hh"))
    yield wait_idle()

    GPS.execute_action("autoindent file")
    yield wait_language_server("textDocument/formatting", "C++")

    gps_assert(buf.get_chars(include_hidden_chars=False),
               expected,
               "Formatting does not work properly")

