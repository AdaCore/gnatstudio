"""
This test checks that floating windows return to their original
notebook when unfloating them.
"""
from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # Open the Learn view and create a notebook for it
    GPS.execute_action("open Learn")
    GPS.MDI.get("Learn").raise_window()
    GPS.execute_action("split vertically")

    # Open the Bookmarks view in the same notebook
    GPS.execute_action("open Bookmarks")

    # Float the Bookmarks view
    execute_action("/Window/Floating")
    yield timeout(300)

    # Unfloat it
    execute_action("unfloat view")
    yield timeout(300)

    # Check that the Bookmarks view has returned to the Learn
    # view's notebook, as it was oiginally placed

    bookmarks_widget = GPS.MDI.get("Bookmarks").pywidget()
    bookmarks_notebook = bookmarks_widget.get_parent()

    learn_widget = GPS.MDI.get("Learn").pywidget()
    learn_notebook = learn_widget.get_parent()

    gps_assert(
        bookmarks_notebook,
        learn_notebook,
        "The Bookmarks should return to the Learn view's notebook",
    )
