# This tests that all clangd formatting preferences are taken
# into account and work

from gs_utils.internal.utils import run_test_driver, gps_assert, \
    wait_tasks, wait_idle, wait_language_server


LLVM = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main() {
  int i =         //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

Google = """#include <iostream>

#include "my_class.hh"

using namespace std;

int main() {
  int i =          //  VeryVeryVeryVeryVeryLongComment
    longFunction(  // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

Chromium = """#include <iostream>
#include "my_class.hh"

using namespace std;

int main() {
  int i =          //  VeryVeryVeryVeryVeryLongComment
    longFunction(  // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

Mozilla = """#include "my_class.hh"
#include <iostream>

using namespace std;

int
main()
{
  int i =         //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

WebKit = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main()
{
  int i = //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

Microsoft = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main()
{
  int i =         //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

GNU = """#include "my_class.hh"
#include <iostream>

using namespace std;

int
main ()
{
  int i =          //  VeryVeryVeryVeryVeryLongComment
    longFunction ( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x ();

  return 0;
}
"""

ContinuationIndentWidth = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main() {
  int i =           //  VeryVeryVeryVeryVeryLongComment
      longFunction( // Again a long comment
          arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

ColumnLimit = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main() {
  int
    i = //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long
                  // comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""

IndentWidth = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main() {
    int i =         //  VeryVeryVeryVeryVeryLongComment
      longFunction( // Again a long comment
        arg);

    cout << "Hello, World!";
    My_Class obj;

    auto a = obj.ch;
    auto rec = obj.rec;
    int x = obj.get_x();

    return 0;
}
"""

ReflowComments = """#include "my_class.hh"
#include <iostream>

using namespace std;

int main() {
  int
    i = //  VeryVeryVeryVeryVeryLongComment
    longFunction( // Again a long comment
      arg);

  cout << "Hello, World!";
  My_Class obj;

  auto a = obj.ch;
  auto rec = obj.rec;
  int x = obj.get_x();

  return 0;
}
"""


@run_test_driver
def driver():
    GPS.Preference("clangd-BasedOnStyle").set("LLVM")

    b = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    yield wait_tasks()

    # Autoindent file with the LLVM preset
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()

    # Verify that the proper indentation is produced
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, LLVM, "Wrong autoindent")
    b.undo()

    # Autoindent file with the Google preset
    GPS.Preference("clangd-BasedOnStyle").set("Google")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, Google, "Wrong autoindent")
    b.undo()

    # Autoindent file with the Chromium preset
    GPS.Preference("clangd-BasedOnStyle").set("Chromium")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, Chromium, "Wrong autoindent")
    b.undo()

    # Autoindent file with the Mozilla preset
    GPS.Preference("clangd-BasedOnStyle").set("Mozilla")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, Mozilla, "Wrong autoindent")
    b.undo()

    # Autoindent file with the WebKit preset
    GPS.Preference("clangd-BasedOnStyle").set("WebKit")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, WebKit, "Wrong autoindent")
    b.undo()

    # Autoindent file with the Microsoft preset
    GPS.Preference("clangd-BasedOnStyle").set("Microsoft")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, Microsoft, "Wrong autoindent")
    b.undo()

    # Autoindent file with the GNU preset
    GPS.Preference("clangd-BasedOnStyle").set("GNU")
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, GNU, "Wrong autoindent")
    b.undo()

    GPS.Preference("clangd-BasedOnStyle").set("LLVM")

    # Autoindent file with the different ContinuationIndentWidth value
    GPS.Preference("clangd-ContinuationIndentWidth").set(4)
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, ContinuationIndentWidth, "Wrong autoindent")
    GPS.Preference("clangd-ContinuationIndentWidth").set(2)
    b.undo()

    #ColumnLimit
    GPS.Preference("Src-Editor-Highlight-Column").set(40)
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, ColumnLimit, "Wrong autoindent")
    GPS.Preference("Src-Editor-Highlight-Column").set(80)
    b.undo()

    #IndentWidth
    GPS.Preference("C-Indent-Level").set(4)
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, IndentWidth, "Wrong autoindent")
    GPS.Preference("C-Indent-Level").set(2)
    b.undo()

    #UseTab will be tested in TC14-011

    #ReflowComments
    GPS.Preference("C-Indent-Comments").set(False)
    GPS.Preference("Src-Editor-Highlight-Column").set(40)
    GPS.execute_action("autoindent file")
    yield wait_language_server('textDocument/formatting', 'C++')
    yield wait_idle()
    txt = b.get_chars(b.beginning_of_buffer(), b.end_of_buffer())
    gps_assert(txt, ReflowComments, "Wrong autoindent")
    GPS.Preference("C-Indent-Comments").set(True)
    GPS.Preference("Src-Editor-Highlight-Column").set(80)
    b.undo()
