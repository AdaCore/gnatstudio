"""
Test the float and unfloat actions.
"""


from gs_utils.internal.utils import *

A_FILE = "a.ads"
B_FILE = "b.ads"


@run_test_driver
def driver():
    a = GPS.EditorBuffer.get(GPS.File(A_FILE))
    a_window = GPS.MDI.get(A_FILE)
    notebook = a_window.pywidget().get_parent()
    b = GPS.EditorBuffer.get(GPS.File(B_FILE))
    b_window = GPS.MDI.get(B_FILE)
    gps_assert(notebook.get_n_pages(), 2, "Initialization issue")

    GPS.execute_action("float view")
    gps_assert(notebook.get_n_pages(), 1, "float action failed")
    gps_assert(b_window.is_floating(), True, "b.ads should float")

    GPS.execute_action("unfloat view")
    gps_assert(notebook.get_n_pages(), 2, "unfloat action failed")
    gps_assert(b_window.is_floating(), False, "b.ads should not float")
