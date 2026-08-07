"""
Test that SARIF fix objects are correctly parsed and applied:
- Single fix: clicking the action applies the fix directly
- Multiple fixes sequentially: marks track buffer changes so coordinates
  stay correct after earlier fixes modify the file

We test three fix types: deletion, insertion, and replacement,
applied one after another on the same file without resetting.
"""

from GPS import *
from gs_utils.internal.utils import *
from pygps import get_widget_by_name, get_widgets_by_type
import gs_utils.internal.dialogs as dialogs

# The original foo.adb content
ORIGINAL = """\
with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   S : constant String := "Hello";
   C : Character;
begin
   null;
   null;

   for I in S'Range loop
      C := S (I);
   end loop;

   Put_Line ("Done");
end Foo;
"""

# After applying the deletion fix (remove line 7 "   null;")
AFTER_DELETION = """\
with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   S : constant String := "Hello";
   C : Character;
begin
   null;

   for I in S'Range loop
      C := S (I);
   end loop;

   Put_Line ("Done");
end Foo;
"""

# After also applying the insertion fix (insert "   null;\n" before Put_Line,
# whose mark has tracked the shift from the previous deletion)
AFTER_INSERTION = """\
with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   S : constant String := "Hello";
   C : Character;
begin
   null;

   for I in S'Range loop
      C := S (I);
   end loop;

   null;
   Put_Line ("Done");
end Foo;
"""

# After also applying the replacement fix (replace "in" with "of", remove
# "'Range" on the for loop line, whose marks have tracked all prior shifts)
AFTER_ALL_FIXES = """\
with Ada.Text_IO; use Ada.Text_IO;

procedure Foo is
   S : constant String := "Hello";
   C : Character;
begin
   null;

   for I of S loop
      C := S (I);
   end loop;

   null;
   Put_Line ("Done");
end Foo;
"""


def load_sarif(filename):
    """Load a SARIF file using the 'Load SARIF File' action."""
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load SARIF File")
    load.select_file(filename)
    yield load.ok()
    yield wait_idle()


def find_message_at_line(line):
    """Find a SARIF-loaded message at the given line in foo.adb."""
    for m in GPS.Message.list():
        if m.get_file().path.endswith("foo.adb") and m.get_line() == line:
            return m
    return None


@run_test_driver
def run_test():
    # Load the SARIF file with single fixes
    yield load_sarif("fixes.sarif")

    # --- Apply all three fixes sequentially ---

    # Fix 1: Deletion (remove redundant null at line 7)
    msg = find_message_at_line(7)
    gps_assert(msg is not None, True, "no message found at line 7")
    msg.execute_action()
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    gps_assert(buf.get_chars(), AFTER_DELETION, "deletion fix not applied")

    # Verify the fix action was removed: executing again should be a no-op
    msg.execute_action()
    yield wait_idle()
    gps_assert(
        buf.get_chars(), AFTER_DELETION, "fix action not removed after deletion fix"
    )

    # Fix 2: Insertion (insert null before Put_Line, originally at line 14,
    # now tracked by marks to the correct shifted position)
    msg = find_message_at_line(14)
    gps_assert(msg is not None, True, "no message found at line 14")
    msg.execute_action()
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    gps_assert(
        buf.get_chars(), AFTER_INSERTION, "insertion fix not applied after deletion"
    )

    # Verify the fix action was removed
    msg.execute_action()
    yield wait_idle()
    gps_assert(
        buf.get_chars(), AFTER_INSERTION, "fix action not removed after insertion fix"
    )

    # Fix 3: Replacement (for-of loop, originally at line 10,
    # now tracked by marks to the correct shifted position)
    msg = find_message_at_line(10)
    gps_assert(msg is not None, True, "no message found at line 10")
    msg.execute_action()
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    gps_assert(
        buf.get_chars(),
        AFTER_ALL_FIXES,
        "replacement fix not applied after deletion+insertion",
    )

    # Verify the fix action was removed
    msg.execute_action()
    yield wait_idle()
    gps_assert(
        buf.get_chars(), AFTER_ALL_FIXES, "fix action not removed after replacement fix"
    )

    # --- Test: fix is refused when code has been manually modified ---
    GPS.Analysis.clean()
    buf.delete(buf.beginning_of_buffer(), buf.end_of_buffer())
    buf._insert_at_location(buf.beginning_of_buffer(), ORIGINAL)
    yield wait_idle()

    yield load_sarif("fixes.sarif")

    # Manually edit the region that the deletion fix targets (line 7)
    msg = find_message_at_line(7)
    gps_assert(msg is not None, True, "no message for corruption test")
    loc = buf.at(7, 4)
    buf._insert_at_location(loc, "-- edited\n")
    yield wait_idle()
    modified_text = buf.get_chars()

    # Attempting to apply should be refused: buffer stays as-is
    msg.execute_action()
    yield wait_idle()
    gps_assert(buf.get_chars(), modified_text, "fix was applied despite manual edit")

    # The action should still be present so the user can retry after undoing
    buf.undo()
    yield wait_idle()
    msg.execute_action()
    yield wait_idle()
    gps_assert(buf.get_chars(), AFTER_DELETION, "fix not applied after undo")

    # --- Test: multi-fix menu is displayed with correct proposals ---
    GPS.Analysis.clean()
    buf.delete(buf.beginning_of_buffer(), buf.end_of_buffer())
    buf._insert_at_location(buf.beginning_of_buffer(), ORIGINAL)
    yield wait_idle()

    yield load_sarif("multi_fixes.sarif")

    msg = find_message_at_line(7)
    gps_assert(msg is not None, True, "no message for multi-fix test")
    msg.execute_action()
    yield wait_idle()

    # Wait for the fix proposals menu to appear
    yield wait_until_true(lambda: get_widget_by_name("fix-proposals-menu") is not None)
    menu = get_widget_by_name("fix-proposals-menu")
    gps_assert(menu is not None, True, "fix proposals menu not displayed")

    # Verify the menu lists the two fix alternatives
    tree = get_widgets_by_type(Gtk.TreeView, menu)[0]
    proposals = dump_tree_model(tree.get_model(), 0)
    gps_assert(
        proposals,
        ["Remove first null statement", "Remove second null statement"],
        "wrong fix proposals in menu",
    )

    # Click the first proposal and verify it's applied
    click_in_tree(tree, Gtk.TreePath((0)), column=0)
    yield wait_idle()

    gps_assert(buf.get_chars(), AFTER_DELETION, "multi-fix not applied")
