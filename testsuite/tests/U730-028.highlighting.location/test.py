"""
Test the Location_Highlighter python class
=> no python error should be reported.
"""

from gs_utils.internal.utils import *
from gs_utils.highlighter import *
from GPS import *


class All_Refs_Highlighter(Location_Highlighter):
    def __init__(self):
        Location_Highlighter.__init__(
            self, style=OverlayStyle(name="All", background="#FF7979")
        )

    def recompute_refs(self, buffer):
        result = []
        for e, r in buffer.file().references():
            result.append((e.name(), r))
        return result


@run_test_driver
def run():
    loc_high = All_Refs_Highlighter()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    GPS.execute_action("Build & Run Number 1")
    yield wait_tasks()
    recompute_xref()
    yield wait_tasks()
    foo_loc = buf.at(1, 13)
    gps_assert(len(foo_loc.get_overlays()), 1, "No location overlay at startup")
    loc_high.start_highlight(buf)
    # The location_highlighter is using a Glib.Timeout wait for its first
    # iteration
    yield wait_idle()
    gps_assert(
        len(foo_loc.get_overlays()),
        2,
        "Ovelay should be triggered by the location_highlighter",
    )
    loc_high.stop_highlight()
    gps_assert(len(foo_loc.get_overlays()), 2, "Stopping doesn't remove the overlay")
    loc_high.remove_highlight()
    gps_assert(
        len(foo_loc.get_overlays()),
        1,
        "Remove the highlighting should remove the overlays",
    )
