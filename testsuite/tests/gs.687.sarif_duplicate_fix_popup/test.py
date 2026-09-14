"""
Regression test: a message with a single SARIF fix must not have its
action listed twice in the editor's side column "multiple actions" popup.
"""

from GPS import *
from gs_utils.internal.utils import *
from pygps import get_widget_by_name
import gs_utils.internal.dialogs as dialogs

# After applying the (single) deletion fix at line 7 (remove "   null;")
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


def load_sarif(filename):
    """Load a SARIF file using the 'Load SARIF File' action."""
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load SARIF File")
    load.select_file(filename)
    yield load.ok()
    yield wait_idle()


@run_test_driver
def run_test():
    yield load_sarif("fixes.sarif")

    # Give the background GNAThub filtering command (Filter_Runner_Command)
    # time to re-apply the message filter registered by
    # GNAThub.Module.Display_Data: the bug only reproduces once that
    # asynchronous second Set_Flags call has actually happened.
    yield timeout(1000)
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    # Click on the side column at the single-fix message's line (7). With
    # the bug, this pops up a "multiple actions" menu listing the same fix
    # twice; fixed, a single action just executes directly (no popup).
    yield idle_modal_dialog(lambda: buf.click_on_side_column(7, 1, icon_name=""))
    yield wait_idle()

    multi_actions_menu = get_widget_by_name("gnatstudio_multiple_actions_menu")
    gps_assert(
        multi_actions_menu is None,
        True,
        "a single fix should not open a multi-actions popup (duplicate action)",
    )
    gps_assert(
        buf.get_chars(), AFTER_DELETION, "single fix via side column not applied"
    )
