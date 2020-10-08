import GPS
from gs_utils.internal.utils import *
import os
import os.path
from workflows import run_as_workflow


@run_as_workflow
def create_directory(prj_view, dir_name, add_to_source_dirs=False):
    prj_view = Project_View()
    yield prj_view.open_and_yield()
    prj_view.select_by_name(column=1, value=".")
    yield wait_idle()

    yield idle_modal_dialog(lambda: GPS.execute_action("create new directory"))
    dialog = get_window_by_title(
        "Please enter the new directory's name:", Gtk.Window.list_toplevels())

    check = get_widgets_by_type(Gtk.CheckButton, dialog)[0]
    check.set_active(add_to_source_dirs)

    ent = get_widgets_by_type(Gtk.Entry, dialog)[0]
    ent.set_text(dir_name)

    ok_button = get_button_from_label("OK", dialog)
    ok_button.clicked()

    yield timeout(300)


@run_test_driver
def run_test():
    prj_view = Project_View()
    yield prj_view.open_and_yield()

    yield create_directory(prj_view, "src_1")

    prj_view.compare_contents(
        ['Default',
         ['src',
          ['main.adb'],
          '.']],
        msg='src 1 should not be visible in the Project view')

    gps_assert(os.path.exists(os.path.join(os.getcwd(), "src_1")), True,
               "src_1 should exist on the filesystem")

    yield create_directory(prj_view, "src_2", add_to_source_dirs=True)

    prj_view.compare_contents(
        ['Default',
         ['src',
          ['main.adb'],
          'src_2',
          '.']],
        msg='src_2 should be visible in the Project view')

    gps_assert(os.path.exists(os.path.join(os.getcwd(), "src_2")), True,
               "src_2 should exist on the filesystem")

    yield timeout(30000)
