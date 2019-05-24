# -*- coding: utf-8 -*-

""" Test for "find all references" in a codebase encoded in Cyrillic"""
import os
import GPS
import json
from gps_utils.internal.utils import run_test_driver, gps_assert, \
                                     wait_tasks
from workflows.promises import timeout


def to_str(m):
    """Make a string out of a message"""
    return "{}:{}:{}".format(os.path.basename(m.get_file().name()),
                             m.get_line(), m.get_column())


@run_test_driver
def driver():
    # This is a test encoded in Cyrillic
    GPS.Preference("General-Charset").set("ISO-8859-5")

    main = GPS.EditorBuffer.get(GPS.File("main.adb"))

    #
    # Goodmorning
    #

    GPS.Console().clear()

    main.current_view().goto(main.at(4, 4))
    GPS.execute_action("find all references")

    yield wait_tasks()
    yield timeout(100)

    # Verify the references
    m = [to_str(m)
         for m in GPS.Message.list("References for Goodmorning (main.adb:4)")]
    m.sort()

    gps_assert(m,
               ["main.adb:4:4",
                "p.ads:2:37"],
               "references for goodmorning are off")

    gps_assert(GPS.Console().get_text(), "", "the console should be empty")

    #
    # доброеутро
    #

    GPS.Console().clear()

    main.current_view().goto(main.at(5, 4))
    GPS.execute_action("find all references")

    yield wait_tasks()
    yield timeout(100)

    # Verify the references
    m = [to_str(m) for m in
         GPS.Message.list(u"References for доброеутро (main.adb:5)")]
    m.sort()

    gps_assert(m,
               ['main.adb:5:4',
                'main.adb:6:4',
                'p.ads:2:14'],
               u"references for доброеутро are off")

    gps_assert(GPS.Console().get_text(), "", "the console should be empty")

    #
    # доброеутро*56
    #

    GPS.Console().clear()

    main.current_view().goto(main.at(7, 4))
    GPS.execute_action("find all references")

    yield wait_tasks()
    yield timeout(100)

    # Verify the references
    m = [to_str(m) for m in GPS.Message.list
         ("References for " + u"доброеутро" * 56 + " (main.adb:7)")]
    m.sort()

    gps_assert(m,
               ['main.adb:7:4',
                'p.ads:3:14'],
               u"references for the long identifier are off")

    gps_assert(GPS.Console().get_text(), "", "the console should be empty")
