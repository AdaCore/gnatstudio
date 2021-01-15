"""
This test verifies that LSP server (clangd) formats current line on Enter
"""
import GPS
from gs_utils.internal.utils import *

expected = '''
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
  int get_x();
};
'''


@run_test_driver
def test_driver():
    GPS.Preference("clangd-BasedOnStyle").set("LLVM")
    f = GPS.File('my_class.hh')
    b = GPS.EditorBuffer.get(f)
    yield wait_tasks()

    b.current_view().goto(b.at(19, 18))
    send_key_event(GDK_RETURN)
    yield hook("language_server_response_processed")
    yield wait_idle()

    gps_assert(
        b.get_chars(b.beginning_of_buffer(), b.end_of_buffer(), True),
        expected)
