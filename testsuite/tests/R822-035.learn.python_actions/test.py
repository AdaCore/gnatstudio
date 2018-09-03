"""
This test verifies that python actions registered before the 'gps_started'
hook can be displayed in the Learn view.
"""

from GPS import *
from gps_utils.internal.utils import *
from gps_utils import hook
import traceback


def do_nothing_action():
    return

@hook('gps_started')
def __on_gps_started():
    gps_utils.make_interactive(
        callback=do_nothing_action,
        name='Do nothing',
        category='General',
        for_learning=True)


@run_test_driver
def run_test():
        # Open the Learn view
    GPS.execute_action("open Learn")
    yield wait_for_mdi_child("Learn")
    learn_view = GPS.MDI.get("Learn").pywidget()

    # Verify that the 'Do nothing' action is shown in the Learn view
    label = get_label_from_text("Do nothing", learn_view)
    is_visible = label is not None and label.is_visible()
    gps_assert(
        is_visible, True,
        "the 'Do nothing' action should be displayed in the Learn view")
