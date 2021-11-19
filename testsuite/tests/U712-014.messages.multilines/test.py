"""
Basic test for python API GPS.Locations.add_multilines
"""

from GPS import *
from gs_utils.internal.utils import *

message_creation = """..........######
#####
#.......
........
"""

buffer_modified = """..........######
###
##
#.......
........
"""


@run_test_driver
def run_test():
    file = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(file)
    # Create a message and verify its existence ...
    GPS.Editor.register_highlighting("My_Highlight", "blue")
    GPS.Locations.add_multilines("My_Category", file, 1, 11, 3, 2,
                                 "Message_Text",
                                 show_in_location=True,
                                 importance=GPS.Message.Importance.MEDIUM,
                                 highlight="My_Highlight")
    gps_assert("Message_Text" in GPS.Locations.locations_dump(),
               True,
               "Missing message")

    # ... and its highlighting
    gps_assert(buf.debug_dump_syntax_highlighting("My_Highlight"),
               message_creation,
               "Wrong highlighting after creating the message")

    # Modifying the text should propagate the hightlighting
    buf.insert(buf.at(2, 4), "\n")
    gps_assert(buf.debug_dump_syntax_highlighting("My_Highlight"),
               buffer_modified,
               "Wrong highlighting after modifying the buffer")

    # Remove the message => it should clear the hightlighting
    GPS.execute_action("locations clear")
    gps_assert("#" not in buf.debug_dump_syntax_highlighting("My_Highlight"),
               True,
               "Wrong highlighting after removing the message")
