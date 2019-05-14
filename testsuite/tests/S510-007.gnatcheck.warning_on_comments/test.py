"""
This test checks that a warning dialog is correctly displayed
when trying to a edit a coding standard file that contains
comments.
"""

import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    yield wait_tasks()
    yield modal_dialog(lambda: GPS.execute_action("edit gnatcheck rules"))
    dialog = get_window_by_title("Comments lost on file save")

    try:
        get_stock_button(dialog, Gtk.STOCK_OK).clicked()
    except Exception:
        simple_error("No warning dialog displayed")
