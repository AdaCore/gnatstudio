# -*- coding: utf-8 -*-

""" Check for proper message highlighting after find all refs """

import GPS
import json
from gps_utils.internal.utils import run_test_driver, gps_assert
from workflows.promises import timeout


@run_test_driver
def driver():
    GPS.Preference("General-Charset").set("UTF-8")
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    b.find_all_refs(b.at(2, 15), True)

    # TODO: replace this with a hook, when available
    yield timeout(1000)

    # We expect to have highlighted only the 'X's in the buffer
    expected = ''.join(['#' if x == u'é' else ('\n' if x == '\n' else '.')
                        for x in b.get_chars().decode('utf-8')])
    actual = b.debug_dump_syntax_highlighting("Search results")
    gps_assert(actual, expected, "highlighting is wrong after find all refs")
