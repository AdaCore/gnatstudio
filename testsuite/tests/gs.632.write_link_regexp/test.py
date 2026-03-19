"""
Test create_link with a vague regexp. It should not freeze GS.
"""

import GPS
from gs_utils.internal.utils import *


def __on_click(*args, **kwargs):
    pass


@run_test_driver
def run_test():
    console = GPS.Console("Color")
    console.write_with_links("Hello\n")
    gps_assert(
        console.get_text(), "Hello\n", "Issue with write_with_links without link regexp"
    )
    console.create_link("(.*)", __on_click, foreground="blue", underline=False)
    console.write_with_links("Bye\n")
    gps_assert(
        console.get_text(),
        "Hello\nBye\n",
        "Issue with write_with_links with link regexp",
    )
    console.insert_link("Link", __on_click)
    console.delete_links()
