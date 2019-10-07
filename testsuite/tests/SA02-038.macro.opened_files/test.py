"""
Test the %fo macro: it should expand into the list of opened files without
duplicates.
"""

from GPS import *
from gps_utils.internal.utils import *

FOO = "foo.adb"
BAR = "bar.adb"
FOOBAR = "foobar.c"


def verify_macro(name_list, msg):
    expanded = GPS.BuildTarget.expand_macros(["%fo"])[0].split()
    gps_assert(len(name_list),
               len(expanded),
               "Both list should have the same length " + msg)
    for name in name_list:
        gps_assert(len([s for s in expanded if s.endswith(name)]),
                   1,
                   "Only one match of " + name +
                   " should be present when expanding " + msg)


@run_test_driver
def run_test():
    # No file opened => the macro should expand to ""
    yield verify_macro([], "at startup")

    # Open 2 files => the macro doesn't care about the file extensions
    GPS.EditorBuffer.get(GPS.File(FOO))
    GPS.EditorBuffer.get(GPS.File(FOOBAR))
    yield verify_macro([FOO, FOOBAR], "after opening simple files")

    # Open 2 views of the same file => only one instance should be present
    # in the command line
    GPS.EditorBuffer.get(GPS.File(BAR))
    GPS.execute_action("new view")
    yield verify_macro([FOO, BAR, FOOBAR], "after opening a new view")
