"""
Verify the output of "is called by" and "calls" on a simple example.
"""

import GPS
from gps_utils.internal.utils import *

expected = ['Bar calls ',
            ['FooBar',
             ['computing...']],
            'Bar is called by ',
            ['Foo',
             ['computing...']]]


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    GPS.execute_action("Build Main Number 1")
    yield wait_tasks(other_than=known_tasks)
    buf.current_view().goto(buf.at(5, 15))
    select_editor_contextual("Call Trees/Bar is called by")
    buf.current_view().goto(buf.at(5, 15))
    select_editor_contextual("Call Trees/Bar calls")
    call_tree = get_widget_by_name("Call Graph Tree")
    model = call_tree.get_model()
    gps_assert(dump_tree_model(model, 0),
               expected,
               "Wrong content in the Call Trees")
