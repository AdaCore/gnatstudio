"""
Test the On_The_Fly_Highlighter python class
=> no python error should be reported.
"""

from gs_utils.internal.utils import *
from gs_utils.highlighter import *
from GPS import *


class My_Highlighter(On_The_Fly_Highlighter):
    def __init__(self):
        On_The_Fly_Highlighter.__init__(
            self, style=OverlayStyle(name="Mine", background="#FF7979")
        )

    def process(self, start, end):
        GPS.Console().write("Hello")
        self.style.apply(start, end)


@run_test_driver
def run():
    my_high = My_Highlighter()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    my_high.start()

    # On a comment line adding a character in a commented line should propagate
    # the overlays.
    expected = buf.at(3, 20).get_overlays()
    buf.insert(buf.at(3, 21), "!")
    gps_assert(
        buf.at(3, 21).get_overlays(),
        expected,
        "The overlay should be retriggered by character added",
    )
    buf.save()

    yield wait_idle()
    my_high.stop()

    gps_assert(
        buf.at(3, 21).get_overlays() != expected, True, "The overlay should be removed"
    )
