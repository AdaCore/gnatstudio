"""
This test checks that we correctly highlight XML multiline
comments.
"""

from GPS import *
from gs_utils.internal.utils import *


expected_tags = ["string 1:15 1:19",
                 "string 1:30 1:41",
                 "keyword 1:43 1:43",
                 "comment 3:1 11:1",
                 "keyword 12:1 12:8",
                 "keyword 13:1 13:9",
                 ""]

@run_test_driver
def run_test():
    buffer = EditorBuffer.get(File("a.xml"))
    tags = get_all_tags(buffer)
    tags_list = tags.split('\n')

    gps_assert(
        tags_list,
        expected_tags,
        "The highlighting is not correct")
