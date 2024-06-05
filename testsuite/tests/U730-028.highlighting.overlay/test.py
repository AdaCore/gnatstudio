"""
Test the OverlayStyle python class => no python error should be reported.
"""

from gs_utils.internal.utils import *
from gs_utils.highlighter import *
from GPS import *


@run_test_driver
def run():
    # Create a complex custom overlay
    over = OverlayStyle(
        name="Hello",
        foreground="#00FFFF",
        background="#00FFFF",
        weight="bold",
        slant="italic",
        editable=True,
        whole_line=True,
        style=None,
    )
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    # Apply it
    over.apply(buf.at(1, 1), buf.at(2, 1))
    # Remove a part of it
    over.remove(buf.at(1, 1), buf.at(1, 2))
    # Not giving the end arg should remove until the end
    over.remove(buf.at(1, 2))
