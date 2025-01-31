"""
Verify the Outline is escaping functions with a name containing '"'.
"""

from GPS import *
from gs_utils.internal.dialogs import Outline_View
from gs_utils.internal.utils import *

WITH_PROFILE = [
    "Foo",
    [
        "Loc",
        ["X", "Y"],
        '&quot;&lt;&quot; <span foreground="#A0A0A0">(Left : in Loc; Right : in Loc) return Boolean</span>',
        '&quot;&lt;&quot; <span foreground="#A0A0A0">(Left : in Loc; Right : in Loc) return Boolean</span>',
        "A",
        "B",
    ],
]

WITHOUT_PROFILE = [
    "Foo",
    ["Loc", ["X", "Y"], "&quot;&lt;&quot;", "&quot;&lt;&quot;", "A", "B"],
]


@run_test_driver
def run_test():
    outline = Outline_View()
    yield outline.open_and_yield()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    # Wait for the Outline view to be filled
    yield wait_outline("foo.adb")
    gps_assert(outline.model(), WITH_PROFILE, "Issue with show profile")

    GPS.Preference("outline-show-profile").set(False)
    yield wait_outline("foo.adb")
    gps_assert(outline.model(), WITHOUT_PROFILE, "Issue without show profile")
