"""
This test checks that current working directory is set to containing
directory of the running executable when root project is aggregated
project.
"""

import GPS
from gs_utils.internal.utils import *


class RunMainDialog(Dialog):
    def open_and_yield(self):
        yield self._open_and_yield("/Build/Run/P1/Run Main main1")
        self.dialog = get_window_by_title("Run Main")
        yield wait_tasks()

    def execute(self):
        return get_button_from_label("Execute", self.dialog)

    def run_in_executable_directory(self):
        return get_widgets_by_type(Gtk.CheckButton, self.dialog)[0]


@run_test_driver
def driver():
    GPS.execute_action("Build all")
    yield wait_tasks()

    dialog = RunMainDialog()
    yield dialog.open_and_yield()
    dialog.run_in_executable_directory().set_active(True)
    yield wait_tasks()
    dialog.execute().clicked()
    yield wait_tasks()

    title = "Run: main1" + dot_exe
    buf = get_widgets_by_type(Gtk.TextView, GPS.MDI.get(title).get_child().pywidget())[
        0
    ].get_buffer()
    text = buf.get_text(buf.get_start_iter(), buf.get_end_iter(), True)
    exec_path = text.splitlines()[0]
    work_dir = text.splitlines()[1]
    exec_name = exec_path[len(work_dir) + 1 :]
    gps_assert(
        exec_name.startswith("main1"),
        True,
        "Wrong name of the executable '{}', path '{}', cwd '{}'".format(
            exec_name, exec_path, work_dir
        ),
    )
