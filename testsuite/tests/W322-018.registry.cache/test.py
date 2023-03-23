import time

from gs_utils.internal.utils import run_test_driver, gps_assert, \
    wait_tasks
import gs_utils.internal.dialogs as dialogs
from pygps import get_button_from_label


@run_test_driver
def driver():
    # Wait for the initial tasks (indexing etc) to complete
    yield wait_tasks()

    # Get the time
    t0 = time.time()

    d = dialogs.Custom_Build_Dialog()
    yield d.open_and_yield()
    d.get_command_line_entry().set_text("./hello")
    get_button_from_label("Execute", d.dialog).clicked()
    yield wait_tasks()

    t1 = time.time()

    # Check that the time to load 10000 messages took less than 5 seconds
    gps_assert(t1 - t0 < 5, True,
                "The build took too long")
