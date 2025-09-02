"""
This test checks that when we open a file from the project view GS after
a rename operation, we correctly load the last (modified) version of the file.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

expect = """package kale_pkg is

end kale;
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("kale.ads"))

    prj_view = Project_View()
    yield prj_view.open_and_yield()
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.expand_row(Gtk.TreePath((0, 0)), True)
    prj_view.select_by_name(column=1, value="kale.ads")
    yield wait_idle()

    windows = Gtk.Window.list_toplevels()
    click_in_tree(explorer, button=3)
    yield idle_modal_dialog(
        lambda: activate_contextual(windows, "File operations/Rename file kale.ads")
    )

    dialog = get_window_by_title("Please enter the file's new name:")
    entry = get_widgets_by_type(Gtk.Entry, dialog)[0]
    entry.set_text("kale_pkg.ads")
    yield wait_idle()
    ok = get_button_from_label("OK", dialog)

    yield idle_modal_dialog(lambda: ok.clicked())
    dialog = get_window_by_title("Warning")
    get_button_from_label("OK", dialog).clicked()

    buf = GPS.EditorBuffer.get(GPS.File("kale_pkg.ads"))
    buf.insert(buf.at(1, 13), "_pkg")
    buf.save(False)
    buf.close()
    yield wait_idle()

    buf = GPS.EditorBuffer.get(GPS.File("kale_pkg.ads"))
    yield wait_idle()
    gps_assert(buf.get_chars(), expect, "Wrong content")
