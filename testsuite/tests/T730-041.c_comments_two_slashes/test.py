"""Behavior test for the comments of C with two slashes"""

from gs_utils.internal.utils import run_test_driver, gps_assert


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("t.c"))
    v = b.current_view()
    v.goto(b.at(1, 1))

    # Test the default behavior

    # Toggle to comment
    GPS.execute_action("Toggle Comment")
    gps_assert(b.get_chars(),"/* a */\nb\nc\n",
        "Text wrong after first toggle to comment")

    # Toggle to uncomment
    v.goto(b.at(1, 1))
    GPS.execute_action("Toggle Comment")
    gps_assert(b.get_chars(),"a\nb\nc\n",
        "Text wrong after first toggle to uncomment")

    # Change the preference
    GPS.Preference("C-Comment-Two-Slashes").set(True)

    # Toggle to comment
    v.goto(b.at(2, 1))
    GPS.execute_action("Toggle Comment")
    gps_assert(b.get_chars(),"a\n// b\nc\n",
        "Text wrong after seond toggle to comment")

    # Toggle to uncomment
    v.goto(b.at(2, 1))
    GPS.execute_action("Toggle Comment")
    gps_assert(b.get_chars(),"a\nb\nc\n",
        "Text wrong after second toggle to uncomment")
