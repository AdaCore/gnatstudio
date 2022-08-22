"""
Verify that rename errors are properly translated as message in the Locations
view.
"""
import GPS
from gs_utils.internal.utils import *


EXPECT_FIRST = [
    'Renaming (1 item in 1 file)',
    ['foo.adb (1 item)',
     ['<b>3:13</b>      Renaming Bar to Foo_Bar creates a name collision']]]

EXPECT_SECOND = [
    'Renaming (2 items in 1 file)',
    ['foo.adb (2 items)',
     ['<b>3:13</b>      Renaming Bar to Foo_Bar creates a name collision',
      '<b>2:4</b>       Renaming Foo_Bar to Bar creates a name collision']]]


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    def rename(loc, new_name):
        buf.current_view().goto(loc)
        yield idle_modal_dialog(
            lambda: GPS.execute_action("rename entity"))
        new_name_ent = get_widget_by_name("new_name")
        new_name_ent.set_text(new_name)
        dialog = get_window_by_title("Renaming entity")
        yield idle_modal_dialog(
            lambda: get_stock_button(dialog, Gtk.STOCK_OK).clicked())

    yield rename(buf.at(5, 11), "Foo_Bar")
    gps_assert(dump_locations_tree(),
               EXPECT_FIRST,
               "Missing error for first renaming")
    yield rename(buf.at(5, 17), "Bar")
    gps_assert(dump_locations_tree(),
               EXPECT_SECOND,
               "Missing error for second renaming")
