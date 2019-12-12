# -*- coding: utf-8 -*-

""" Check for proper message highlighting after find all refs """

import GPS
import json
from gps_utils.internal.utils import run_test_driver, gps_assert
from workflows.promises import timeout, hook


expected = """..................
..............####..............

..............####................
.........
............
.............

.....
...####.....................
...."""


@run_test_driver
def driver():
    GPS.Preference("General-Charset").set("UTF-8")
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    b.find_all_refs(b.at(2, 15), True)
    yield hook('language_server_response_processed')

    actual = b.debug_dump_syntax_highlighting("Search results")
    gps_assert(actual.strip(), expected.strip(),
               "highlighting is wrong after find all refs:\n"
               + "\n%s\n!=\n%s\n" % (actual, expected))
