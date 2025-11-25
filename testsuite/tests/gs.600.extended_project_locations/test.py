"""Check that we have a separate category for locations form the
extending project"""

from GPS import *
from gs_utils.internal.utils import *

expected = [
    "References for Kind in original project (hello.adb:8) (8 items in 6 files)",
    [
        "decl.ads (1 item)",
        ["<b>3:9</b>       [reference] type <b>Kind</b> is (One, Two);"],
        "decl-a.ads (2 items)",
        [
            "<b>3:24</b>      [reference] function Get return <b>Kind</b> is (One);",
            "<b>4:27</b>      [reference] function Do_Get return <b>Kind</b>;",
        ],
        "decl-a.adb (1 item)",
        ["<b>3:27</b>      [reference] function Do_Get return <b>Kind</b> is"],
        "decl-b.ads (2 items)",
        [
            "<b>3:26</b>      [reference] function B_Get return <b>Kind</b> is (One);",
            "<b>4:29</b>      [reference] function Do_Get_B return <b>Kind</b>;",
        ],
        "decl-b.adb (1 item)",
        ["<b>3:29</b>      [reference] function Do_Get_B return <b>Kind</b> is"],
        "hello.adb (1 item)",
        ["<b>8:13</b>      [reference] T : Decl.<b>Kind</b> := Decl.A.Do_Get;"],
    ],
    "References for Kind in extending projects (hello.adb:8) (6 items in 4 files)",
    [
        "decl-a.ads (2 items)",
        [
            "<b>3:24</b>      [reference] function Get return <b>Kind</b> is (One);",
            "<b>4:27</b>      [reference] function Do_Get return <b>Kind</b>;",
        ],
        "decl-a.adb (1 item)",
        ["<b>3:27</b>      [reference] function Do_Get return <b>Kind</b> is"],
        "decl-b.ads (2 items)",
        [
            "<b>3:26</b>      [reference] function B_Get return <b>Kind</b> is (One);",
            "<b>4:29</b>      [reference] function Do_Get_B return <b>Kind</b>;",
        ],
        "decl-b.adb (1 item)",
        ["<b>3:29</b>      [reference] function Do_Get_B return <b>Kind</b> is"],
    ],
]
selected = "<b>3:9</b>       [reference] type <b>Kind</b> is (One, Two);"


@run_test_driver
def driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    buf.current_view().goto(buf.at(8, 14))
    GPS.execute_action("find all references")
    yield wait_language_server("textDocument/references")

    gps_assert(
        dump_locations_tree(),
        expected,
        "'find all references' does not correct",
    )
