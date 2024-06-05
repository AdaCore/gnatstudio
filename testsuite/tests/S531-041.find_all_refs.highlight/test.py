# -*- coding: utf-8 -*-

""" Check for proper message highlighting after find all refs """

import GPS
import json
from gs_utils.internal.utils import run_test_driver, gps_assert, wait_tasks
from workflows.promises import timeout, hook, known_tasks


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

    # wait LSP responses has been processed to have folding information
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    b.find_all_refs(b.at(2, 15), True)
    yield hook("language_server_response_processed")

    actual = b.debug_dump_syntax_highlighting("Search results")
    gps_assert(
        actual.strip(),
        expected.strip(),
        "highlighting is wrong after find all refs:\n"
        + "\n%s\n!=\n%s\n" % (actual, expected),
    )
