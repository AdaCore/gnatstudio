"""
Verify that rename errors are properly translated as message in the Locations
view.
"""
import GPS
from gs_utils.internal.utils import *


EXPECT_FIRST = [
    "Diagnostics: Ada (1 item in 1 file)",
    [
        "foo.adb (1 item)",
        [
            "<b>5:11</b>      Can&apos;t rename identifier &apos;Bar&apos;",
            [
                '          <span color="#729FCF"><u>foo.adb:3:13:</u></span> Renaming Bar to Foo_Bar creates a name collision with &lt;Id &quot;Foo_Bar&quot; foo.adb:3:13-3:20&gt;'
            ],
        ],
    ],
]


EXPECT_SECOND = [
    "Diagnostics: Ada (1 item in 1 file)",
    [
        "foo.adb (1 item)",
        [
            "<b>5:17</b>      Can&apos;t rename identifier &apos;Foo_Bar&apos;",
            [
                '          <span color="#729FCF"><u>foo.adb:2:4:</u></span> Renaming Foo_Bar to Bar creates a name collision with &lt;Id &quot;Bar&quot; foo.adb:2:4-2:7&gt;'
            ],
        ],
    ],
]


@run_test_driver
def driver():
    GPS.Preference("LSP-Diagnostics-Display").set("Editor_And_Locations")
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    def rename(loc, new_name):
        buf.current_view().goto(loc)
        yield idle_modal_dialog(lambda: GPS.execute_action("rename entity"))
        new_name_ent = get_widget_by_name("new_name")
        new_name_ent.set_text(new_name)
        dialog = get_window_by_title("Renaming entity")
        yield idle_modal_dialog(lambda: get_stock_button(dialog, STOCK_OK).clicked())
        yield timeout(1000)

    yield rename(buf.at(5, 11), "Foo_Bar")
    gps_assert(dump_locations_tree(), EXPECT_FIRST, "Missing error for first renaming")

    yield rename(buf.at(5, 17), "Bar")
    gps_assert(
        dump_locations_tree(), EXPECT_SECOND, "Missing error for second renaming"
    )
