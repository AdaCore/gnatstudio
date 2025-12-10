"""
This test verifies that we can find all references for "&".
"""

import GPS
from gs_utils.internal.utils import *


expected_msgs = [
    "References for &amp; (main.adb:13) (3 items in 1 file)",
    [
        "main.adb (3 items)",
        [
            "<b>5:13</b>      [reference] function <b>&quot;&amp;&quot;</b> (L,R : Integer) return Integer;",
            "<b>7:13</b>      [reference] function <b>&quot;&amp;&quot;</b> (L,R : Integer) return Integer is",
            "<b>13:43</b>     [call] Ada.Text_IO.Put_Line (Integer&apos;Image (3 <b>&amp;</b> 4));",
        ],
    ],
]

local_expected_msgs = [
    "Local references for &amp; (main.adb:13) in main.adb (3 items in 1 file)",
    [
        "main.adb (3 items)",
        [
            "<b>5:13</b>      [reference] function <b>&quot;&amp;&quot;</b> (L,R : Integer) return Integer;",
            "<b>7:13</b>      [reference] function <b>&quot;&amp;&quot;</b> (L,R : Integer) return Integer is",
            "<b>13:43</b>     [call] Ada.Text_IO.Put_Line (Integer&apos;Image (3 <b>&amp;</b> 4));",
        ],
    ],
]


@run_test_driver
def test_driver():
    yield wait_tasks()

    # Open main.adb and perform the search
    main = GPS.EditorBuffer.get(GPS.File("main.adb"))
    main.current_view().goto(main.at(13, 43))
    yield wait_idle()
    GPS.execute_action("find all references")
    yield wait_language_server("textDocument/references")

    gps_assert(
        dump_locations_tree(),
        expected_msgs,
        "'find all references' does not work",
    )

    GPS.execute_action("locations clear")
    yield wait_idle()
    # Re-grab the editor focus
    GPS.MDI.get("main.adb").raise_window()
    yield wait_idle()
    main.current_view().goto(main.at(13, 43))
    yield wait_idle()
    GPS.execute_action("find all local references")
    yield wait_language_server("textDocument/references")
    gps_assert(
        dump_locations_tree(),
        local_expected_msgs,
        "'find all local references' does not work",
    )
