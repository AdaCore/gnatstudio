"""
This test verifies that autofix commands checks that a source code line 
is not changed before applying the fix
"""
from gs_utils.internal.utils import (
    run_test_driver,
    gps_assert,
    wait_tasks,
    wait_idle,
    wait_for_mdi_child,
    click_in_tree,
    activate_contextual,
    close_contextual,
)
import gs_utils.internal.dialogs as dialogs
from pygps import get_button_from_label

expected = """with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure hello is
   X : Unbounded_String;

begin
   Append (X, "hello");
end;
"""
contextual_name = "Auto Fix/Fix all simple style errors and warnings"


@run_test_driver
def driver():
    yield wait_tasks()
    b = GS.EditorBuffer.get(GS.File("main.adb"))

    d = dialogs.Custom_Build_Dialog()
    yield d.open_and_yield()
    d.get_command_line_entry().set_text("cat output")
    get_button_from_label("Execute", d.dialog).clicked()
    yield wait_tasks()

    yield wait_for_mdi_child("Locations")
    tree = pygps.get_widgets_by_type(Gtk.TreeView, GPS.MDI.get("Locations").pywidget())[
        0
    ]
    windows = Gtk.Window.list_toplevels()
    click_in_tree(tree, path=Gtk.TreePath("0:0:0"), column=1, button=3)
    activate_contextual(windows, contextual_name)
    close_contextual(windows)
    yield wait_idle()

    gps_assert(b.get_chars(), expected, "Autofix failed")
