"""Tests that LSP completion is properly formatted"""
import GPS
from gs_utils.internal.utils import *

expected = """with Ada.Text_IO.Float_IO;

procedure Hello is
   type Lat_Long_Degrees_Float_Type is digits 10;

   package Lat_Long_Degrees_Text_IO is
     new Ada.Text_IO.Float_IO (Lat_Long_Degrees_Float_Type);
begin
   Lat_Long_Degrees_Text_IO.Put
     (To   => String,
      Item => Num,
      Aft  => Field,
      Exp  => Field)
end Hello;
"""

@run_test_driver
def driver():
    GPS.Preference("Smart-Completion-Mode").set("3")
    GPS.Preference("Completion-Filter-Mode").set("Fuzzy")
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    view = buf.current_view()
    view.goto(buf.at(9, 1).end_of_line())
    yield wait_idle()
    for c in ' (':
        send_key_event(ord(c))
        yield timeout(300)

    yield wait_until_true(lambda: get_widget_by_name("completion-view") is not None)
    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing...")

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()
    gps_assert(buf.get_chars(),
               expected,
               "The completion should be formatted")
