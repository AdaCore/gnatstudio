"""
Test because of a misused of ada_pyobject_from_widget which was leaking
full EditorBuffer inside get_gtk_buffer (get_gtk_buffer is also used inside
the python highlighter)
"""
import gc
import GPS
from gs_utils.internal.utils import *
from pygps import get_gtk_buffer


def count_object(typename, objects=None):
    if objects is None:
        objects = gc.get_objects()
    return len([o for o in objects if type(o).__name__ == typename])


@run_test_driver
def test_driver():
    # Simple open/close case
    buf = GPS.EditorBuffer.get(GPS.File("foo.py"))
    gps_assert(count_object("EditorBuffer"), 1, "Initital state")
    buf.close(force=True)
    del buf
    gc.collect()
    gps_assert(count_object("EditorBuffer"), 0, "The buffer should have been cleaned")

    # Manual call to get_gtk_buffer
    buf = GPS.EditorBuffer.get(GPS.File("foo.py"))
    gtk_buf = get_gtk_buffer(buf)
    buf.close(force=True)
    del buf
    gc.collect()
    gps_assert(
        count_object("EditorBuffer"), 1, "gtk_buf should still have a reference on buf"
    )
    del gtk_buf
    gc.collect()
    gps_assert(
        count_object("EditorBuffer"), 0, "The last reference should have been gtk_buf"
    )
