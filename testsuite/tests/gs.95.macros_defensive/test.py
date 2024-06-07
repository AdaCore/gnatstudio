"""
Test macro expansion at start up. Some are undefined in the start up context
and should not raise an error.
Title: "Hello - %P - %d - %e - %f - %l - %c")
"""

from gs_utils.internal.utils import *


@run_test_driver
def driver():
    main = GPS.MDI.get_main_window()
    gps_assert(
        main.pywidget().get_title(),
        "Hello - Test -  -  -  -  - ",
        "Wrong title at startup",
    )

    f = GPS.File("foo.adb")
    d = f.directory()
    buf = GPS.EditorBuffer.get(f)
    buf.current_view().goto(buf.at(1, 1))
    gps_assert(
        main.pywidget().get_title(),
        "Hello - Test - %s -  - foo.adb - 1 - 1" % (d),
        "Wrong title after opening a file",
    )
