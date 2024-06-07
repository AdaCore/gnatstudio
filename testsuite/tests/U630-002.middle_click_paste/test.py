"""Simulate a middle-click paste outside of text bounds
   and verify the soundness of the editor.
"""

from gs_utils.internal.utils import run_test_driver, gps_assert, timeout
from gs_utils.internal.editor import click_in_widget
import pygps

expected_after_click = """-- hi
end h


procedure hello is
  -- hi
end hello;
"""


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    view = buf.current_view()

    expected_orig = buf.get_chars()
    # Select a bit
    buf.select(buf.at(5, 3), buf.at(6, 6))
    yield timeout(200)

    # get the low-level textview
    v = pygps.get_widgets_by_type(Gtk.TextView, view.pywidget())[0]

    # simulate a click at coordinates 40, 5: regardless of the font,
    # this is somewhere on the first line, well past the last character
    win = v.get_window(Gtk.TextWindowType.TEXT)
    click_in_widget(window=win, x=40, y=5, button=2)

    yield timeout(200)

    # Verify that paste happend
    gps_assert(
        buf.get_chars(),
        expected_after_click,
        """Middle click paste didn't happen at the right place""",
    )

    # Verify noncorruption after Undo
    buf.undo()
    gps_assert(
        buf.get_chars(), expected_orig, """Undo didn't restore the right contents"""
    )
