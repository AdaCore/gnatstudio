"""
This test checks that saving an unnamed buffer does not save/rename
other unnamed buffers that might be opened at the same tiume.
"""

import GPS
import pygps
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    # Create two unnamed buffers
    GPS.execute_action("new file")
    GPS.execute_action("new file")

    # Write some text in the last opened buffer and save it
    # as 'foo.ads'
    new_buffer = GPS.EditorBuffer.get()
    new_buffer.insert("Foo")
    new_buffer.save(interactive=False, file=GPS.File("foo.ads"))

    # Verify that we don't have two 'foo.ads' buffers
    foo_buffers = [c.name(short=True) for c in GPS.MDI.children()
                    if c.name(short=True) == "foo.ads"]

    gps_assert(len(foo_buffers), 1, "There should be only one 'foo.ads'")
