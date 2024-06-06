"""
Test the creation of messages related to a codeaction and undo it.
"""

from gs_utils.internal.editor import click_in_widget
from gs_utils.internal.utils import *
from GPS import *


after_codeaction = """procedure Foo is
   procedure Bar ;
   procedure Bar  is
   begin
      null;
   end Bar;
begin
   Bar;
end Foo;
"""


@run_test_driver
def driver():
    # Open an editor and go to a line where there's a code action
    b = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    def check(nb_messages, text, comment):
        gps_assert(
            len(GPS.Message.list(category="Apply Workspace Edit")),
            nb_messages,
            "Wrong message list " + comment,
        )
        gps_assert(b.get_chars(), text, "Wrong text " + comment)

    expected = b.get_chars()

    # go to the function def and select remove all param
    b.current_view().goto(b.at(2, 16))
    yield wait_language_server("textDocument/codeAction")

    # Click on the side area
    yield idle_modal_dialog(lambda: b.click_on_side_column(2, 1, icon_name=""))
    # Verify that the multiactions menu is correclty displayed even if
    # containing only one element
    multi_actions_menu = get_widget_by_name("gnatstudio_multiple_actions_menu")
    gps_assert(
        multi_actions_menu is not None, True, "The multi actions menu should be shown"
    )

    # Click on the action menu item and verify that it executed the action
    items = get_widgets_by_type(Gtk.MenuItem, multi_actions_menu)
    items[1].activate()
    items[1].destroy()
    yield wait_tasks()

    check(3, after_codeaction, "after executing the codeaction")

    GPS.execute_action("undo")
    yield wait_idle()
    check(0, expected, "after executing undo")

    GPS.execute_action("redo")
    yield wait_idle()
    check(3, after_codeaction, "after executing the redo")
