"""
This tests verifies that navigation works when using an external C/C++ code
base where the extension for header files is ".hpp". (Which is the case for
the boost project).
"""
import GPS
from gs_utils.internal.utils import \
    gps_assert, hook, run_test_driver, timeout, wait_tasks
from workflows.promises import known_tasks


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File('f.cpp'))
    buf.current_view().goto(buf.at(5, 4))

    GPS.execute_action('goto declaration')
    yield timeout(1000)

    buf = GPS.EditorBuffer.get()
    gps_assert(buf.file().name().endswith("foo.hpp"), True,
               "did not navigate to the foo.hpp file")

    buf.current_view().goto(buf.at(4, 24))

    GPS.execute_action('goto declaration')
    yield timeout(1000)

    buf = GPS.EditorBuffer.get()
    gps_assert(buf.file().name().endswith("bar.hpp"), True,
               "did not navigate to the bar.hpp file")
