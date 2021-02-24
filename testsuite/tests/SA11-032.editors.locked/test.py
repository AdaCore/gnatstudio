"""
This test checks the good behavior of locked editors on several situations:

. Locked editors should still be editable

. Navigation actions (e.g: got declaration) performed on locked editors
  should open a new unlocked editor for the same file

. Clicking on messages associated to a locked editor's file should
  open a new editor for the same file

. Opening a file via the omnisearch should create a separate editor
  if there is a locked one for the same file
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    test_buf = GPS.EditorBuffer.get(GPS.File("test.ads"))
    GPS.execute_action("Lock or unlock current editor")

    test_buf.current_view().goto(test_buf.at(3, 1).end_of_line())
    GPS.execute_action("backward delete")
    gps_assert(test_buf.is_modified(), True,
               "Locked editors should still be editable")
    GPS.execute_action("undo")

    main_buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    main_buf.current_view().goto(main_buf.at(5, 10))

    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration", "Ada")

    gps_assert(len(test_buf.views()), 2,
               "Opened view for test.ads is locked: another view should have "
               + "been opened")

    test_buf.views()[0].destroy()

    GPS.MDI.get("test.ads").raise_window()
    yield wait_idle()

    GPS.execute_action("backward delete")
    GPS.execute_action("Compile File")
    yield wait_tasks(other_than=known_tasks)

    msgs = GPS.Message.list(category="Builder results")
    gps_assert(len(msgs), 1, "We should have an error after compiling")

    gps_assert(len(test_buf.views()), 2,
               "Opened view for test.ads is locked: another view should have "
               + "been opened after appearance of compilation error in "
               + "Locations")

    test_buf.views()[0].destroy()

    GPS.execute_action("Global Search in context: file names")
    yield wait_idle()

    field = get_widget_by_name("global_search")
    field.set_text("test.ads")
    yield wait_idle()

    send_key_event(GDK_DOWN)
    send_key_event(GDK_RETURN)
    yield wait_idle()

    gps_assert(len(test_buf.views()), 2,
               "Opened view for test.ads is locked: another view should have "
               + "been opened when selecting test.ads via the omnisearch")
