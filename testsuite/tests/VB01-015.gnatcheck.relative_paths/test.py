"""
This test checks that we do not ask for a rules file if the gnatcheck
rules are directly defined in the .gpr file itself.
"""
import GPS
from gs_utils.internal.utils import *


class Coding_Standard_Editor(Dialog):
    def open_and_yield(self):
        yield self._open_and_yield("edit gnatcheck rules")
        self.dialog = get_window_by_title("Coding Standard editor")


@run_test_driver
def test_driver():
    # Open the gnatcheck rules file editor: it should resolve to
    # the rules file specified via a relative path in the project
    # file
    editor = Coding_Standard_Editor()
    yield editor.open_and_yield()

    rules_file_entry = get_widgets_by_type(Gtk.Entry, editor.dialog)[0]
    gps_assert(
        rules_file_entry.get_text(),
        os.path.join(GPS.pwd(), ".", "dir", "coding_standard.txt"),
        "The rules file has not been resolved properly",
    )

    # Save the rules file
    get_button_from_label("Save", editor.dialog).clicked()
    yield wait_idle()

    # Check that we are able to laucnh gnatcheck correctly
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    GPS.execute_action("gnatcheck root project")
    yield wait_tasks(other_than=known_tasks)

    location = GPS.Locations.list_locations(
        "Coding Standard violations", os.path.join(GPS.pwd(), "main.adb")
    )[0]

    gps_assert(
        str(location),
        "main.adb:3:7",
        "gnatcheck has not been launched with the right rules file",
    )
