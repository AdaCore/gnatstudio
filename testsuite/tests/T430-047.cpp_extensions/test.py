"""
This tests verifies that navigation works when using an external C/C++ code
base where the extension for header files is ".hpp". (Which is the case for
the boost project).
"""
import GPS
import os
from gs_utils.internal.utils import \
    gps_assert, hook, run_test_driver, timeout, wait_tasks
from workflows.promises import known_tasks


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File('f.cpp'))
    buf.current_view().goto(buf.at(5, 4))

    GPS.execute_action('goto declaration')
    yield timeout(2000)

    buf = GPS.EditorBuffer.get()
    basename = os.path.basename(buf.file().name())
    gps_assert(basename, "foo.hpp",
               "did not navigate to the foo.hpp file, but {}".format(basename))
    gps_assert(buf.get_lang().name, "c++",
               "wrong lang for hop.hpp")
    buf.current_view().goto(buf.at(4, 24))

    GPS.execute_action('goto declaration')
    yield timeout(2000)

    buf = GPS.EditorBuffer.get()
    basename = os.path.basename(buf.file().name())
    gps_assert(basename, "bar.hpp",
               "did not navigate to the bar.hpp file, but {}".format(basename))
    gps_assert(buf.get_lang().name, "c++",
               "wrong lang for bar.hpp")