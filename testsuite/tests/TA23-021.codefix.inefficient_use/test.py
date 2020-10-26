"""
This test checks that we don't do anything when an inefficient use
clause has already been removed by a codefix about an unneeded with
clause on the same package.
"""

from GPS import *
from gs_utils.internal.utils import *


EXPECTED_TEXT = """package Quickfix is
end Quickfix;"""

@run_test_driver
def run_test():
    GPS.BuildTarget("Build All").execute()
    yield wait_idle()

    # Apply all the codexifes: the first one is about the unneeded with clause
    # on the 'Pack' package: it should remove both with and use clauses on that
    # package,
    # The second one is about the unefficient use clause on Pack: since the use
    # clause has already been removed with the with clause, the buffer contents
    # should not be modified.
    messages = GPS.Message.list()
    for m in messages:
        m.execute_action()
        yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("quickfix.ads"))
    text = buf.get_chars()

    gps_assert(text.strip(), EXPECTED_TEXT,
               "The buffer contents after applying codefixes is not expected")
