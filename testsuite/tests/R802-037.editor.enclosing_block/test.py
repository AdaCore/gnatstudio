"""Test the name displays in the src_editor status bar"""
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)
    view = buf.current_view()
    label = get_widget_by_name("Status Bar Name Label")
    gps_assert(label is not [],
               True,
               "Can't retrieve the status bar label")
    gps_assert(label.get_text(),
               "Foo",
               "Wrong value")

    view.goto(buf.at(4, 4))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(label.get_text(),
               "Foo.Bar",
               "Wrong value")

    view.goto(buf.at(5, 4))
    yield wait_tasks(other_than=known_tasks)
    gps_assert(label.get_text(),
               "Foo.FooBar",
               "Wrong value")
