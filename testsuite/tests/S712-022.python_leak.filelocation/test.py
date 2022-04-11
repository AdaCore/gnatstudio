"""
A FileLocation should not leak a File
"""
import gc
import GPS
from gs_utils.internal.utils import *


def count_object(typename, objects=None):
    if objects is None:
        objects = gc.get_objects()
    return len([o for o in objects if type(o).__name__ == typename])


@run_test_driver
def test_driver():
    initial = count_object("File")

    x = GPS.FileLocation(GPS.File("main.adb"), 1, 1)
    gps_assert(count_object("File"),
               initial,
               "A FileLocation should not increase the number of File")
    f = x.file()
    gps_assert(count_object("File"),
               initial + 1,
               "A File should have been created")
    # Deleting the FileLocation should not affect the File
    del x
    gc.collect()
    gps_assert(count_object("File"),
               initial + 1,
               "File should not be affected by the deletion of FileLocation")
    del f
    gc.collect()
    gps_assert(count_object("File"),
               initial,
               "Python should have the last ref to File")
