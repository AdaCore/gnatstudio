""" Functional tests for the "Move block up" and "Move block down" actions."""
from gs_utils.internal.utils import run_test_driver, gps_assert, wait_tasks


@run_test_driver
def driver():
    GPS.Console().clear()
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()
    initial = b.get_chars()

    # "up" with no selection

    v.goto(b.at(6, 1))
    GPS.execute_action("Move block up")
    gps_assert(
        b.get_chars(b.at(5, 1), b.at(6, 9)),
        "   --  b\n   --  a\n",
        "move block up (no selection) error",
    )
    gps_assert(
        b.selection_start(), b.at(5, 1), "cursor in the wrong place after move block up"
    )
    gps_assert(
        b.selection_start(),
        b.selection_end(),
        "selection unexpected after move block up",
    )

    # "down" with no selection

    GPS.execute_action("Move block down")
    gps_assert(b.get_chars(), initial, "move block down (no selection) error")
    gps_assert(
        b.selection_start(),
        b.at(6, 1),
        "cursor in the wrong place after move block down",
    )
    gps_assert(
        b.selection_start(),
        b.selection_end(),
        "selection unexpected after move block down",
    )

    # "up" with selection

    b.select(b.at(6, 6), b.at(7, 6))
    GPS.execute_action("Move block up")
    gps_assert(
        b.get_chars(b.at(5, 1), b.at(7, 9)),
        "   --  b\n   --  c\n   --  a\n",
        "move block up (selection) error",
    )
    gps_assert(
        b.selection_start(),
        b.at(5, 6),
        "selection start in the wrong place after move block up",
    )
    gps_assert(
        b.selection_end(),
        b.at(6, 6),
        "selection end in the wrong place after move block up",
    )

    # "down" with selection

    GPS.execute_action("Move block down")
    gps_assert(b.get_chars(), initial, "move block down (selection) error")
    gps_assert(
        b.selection_start(),
        b.at(6, 6),
        "selection start in the wrong place after move block down",
    )
    gps_assert(
        b.selection_end(),
        b.at(7, 6),
        "selection end in the wrong place after move block down",
    )

    # Edge cases on first and last lines

    v.goto(b.at(1, 1))
    GPS.execute_action("Move block up")
    gps_assert(b.get_chars(), initial, "move block up (first line) error")
    v.goto(b.at(11, 1))
    GPS.execute_action("Move block down")
    gps_assert(b.get_chars(), initial, "move block down (last line) error")

    # Test compatibility with blocks folding, and test atomicity of undo

    yield wait_tasks()  # Needed to make sure block info is available
    b.blocks_fold()

    # we have one additional block for comments, so unfold it to have
    # previous code representation
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        b.at(6, 1).block_unfold()

    gps_assert(
        b.get_chars(b.at(2, 1), b.at(4, 15), include_hidden_chars=False),
        "   type r is record\n",
        "block folding didn't work",
    )

    v.goto(b.at(5, 5))
    GPS.execute_action("Move block up")
    gps_assert(
        b.get_chars(b.at(2, 1), b.at(4, 15), include_hidden_chars=False),
        "   type r is record\n      b : int;\n   end record;\n",
        "block should have been expanded after move up",
    )
    GPS.execute_action("Move block up")
    gps_assert(
        b.get_chars(b.at(2, 1), b.at(4, 9), include_hidden_chars=False),
        "   type r is record\n      b : int;\n   --  a\n",
        "line should have been moved after move up",
    )

    b.undo()
    gps_assert(b.get_chars(), initial, "undo after move up didn't work")

    v.goto(b.at(7, 5))
    gps_assert(
        b.get_chars(b.at(8, 1), b.at(10, 15), include_hidden_chars=False),
        "   type s is record\n",
        "blok folding didn't work",
    )
    GPS.execute_action("Move block down")
    gps_assert(
        b.get_chars(b.at(8, 1), b.at(10, 15), include_hidden_chars=False),
        "   type s is record\n      b : int;\n   end record;\n",
        "block should have been expanded after move down",
    )
    GPS.execute_action("Move block down")
    gps_assert(
        b.get_chars(b.at(8, 1), b.at(10, 15), include_hidden_chars=False),
        "   --  c\n      b : int;\n   end record;\n",
        "line should have been moved after move down",
    )

    b.undo()
    gps_assert(b.get_chars(), initial, "undo after move down didn't work")
