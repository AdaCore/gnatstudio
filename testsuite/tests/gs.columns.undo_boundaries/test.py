"""
Undo and redo of edits made at the position boundaries: the first column of a
line, the end of a line, and a column past multi-byte characters. Each edit
records its start and end positions, so an off-by-one or a byte count there
shows up as a buffer that does not come back to its previous contents.
"""

import GPS
from gs_utils.internal.utils import *


ORIGINAL = (
    "procedure Main is\n" "   --  café ééé\n" "begin\n" "   X := 1;\n" "end Main;\n"
)


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    gps_assert(buf.get_chars(), ORIGINAL, "wrong initial contents")

    def check_undo_redo(Edit, Expected, Msg):
        Edit()
        gps_assert(buf.get_chars(), Expected, "wrong contents after " + Msg)
        buf.undo()
        gps_assert(
            buf.get_chars(), ORIGINAL, "undo of " + Msg + " did not restore the buffer"
        )
        buf.redo()
        gps_assert(
            buf.get_chars(), Expected, "redo of " + Msg + " did not reapply the edit"
        )
        buf.undo()
        gps_assert(
            buf.get_chars(),
            ORIGINAL,
            "second undo of " + Msg + " did not restore the buffer",
        )

    #  Insert on the first column of a line
    check_undo_redo(
        lambda: buf.insert(buf.at(4, 1), "--"),
        ORIGINAL.replace("   X := 1;", "--   X := 1;"),
        "an insertion on the first column",
    )

    #  Insert at the end of a line
    check_undo_redo(
        lambda: buf.insert(buf.at(4, 1).end_of_line(), "  --  done"),
        ORIGINAL.replace("   X := 1;", "   X := 1;  --  done"),
        "an insertion at the end of a line",
    )

    #  Delete a range that starts past the multi-byte characters
    check_undo_redo(
        lambda: buf.delete(buf.at(2, 8), buf.at(2, 11)),
        ORIGINAL.replace("   --  café ééé", "   --   ééé"),
        "a deletion after the multi-byte characters",
    )

    #  Replace a range holding multi-byte characters, as a single command:
    #  the 8 characters after column 8 are "café ééé".
    check_undo_redo(
        lambda: GPS.Editor.replace_text("main.adb", 2, 8, "tea", 0, 8),
        ORIGINAL.replace("   --  café ééé", "   --  tea"),
        "a replacement of the multi-byte characters",
    )
