"""
Modify a file outside GPS and verify that the reloading correctly work.
"""

from gps_utils.internal.utils import *

DEFAULT = "there is no text\n"
EXPECTED = "some text\n"


def write_something_to_file(timeout):
    with open("hello.txt", "wb") as f:
        f.write(EXPECTED)

    timeout.remove()


def save_file(timeout):
    GPS.execute_action("save")
    timeout.remove()

dialog_found = 0


def check_dialog(timeout):
    global dialog_found
    # Look for our confirmation dialog
    windows = Gtk.Window.list_toplevels()
    dialogs = filter(lambda x: x.get_title() and
                     x.get_title().startswith("Files changed on disk"),
                     windows)
    if dialogs:
        dialog_found += 1
        get_button_from_label("Reload", dialogs[0]).clicked()
        timeout.remove()


def verify_dialog(b, number, should_exist):
    # Attempt a save. This save should bring up the "Files have changed"
    # dialog, which is creating its own main loop, so we'll use a timeout
    # to detect it.
    GPS.Timeout(200, check_dialog)
    GPS.execute_action("save")

    # At this point, the text of the buffer should not contain "some text"
    gps_assert(b.get_chars(),
               EXPECTED,
               "after reloading number: " + str(number))

    # Let's wait a moment and make sure we have received the dialog
    count = 0
    while dialog_found != number and count < 10:
        count += 1
        yield timeout(500)

    gps_assert(dialog_found == number,
               should_exist,
               "Issue with the dialog number: " + str(number))


@run_test_driver
def driver():
    GPS.Preference("Auto-Reload-Files").set(False)
    b = GPS.EditorBuffer.get(GPS.File("hello.txt"))
    b.insert(b.at(1, 1), "there is ")

    # Sanity check
    gps_assert(b.get_chars(), DEFAULT, "at Init")

    # This test needs someone to write to the file outside of GPS,
    # this is simulated by write_something_to_file here.
    GPS.Timeout(500, write_something_to_file)

    # Wait here until the file has actually changed on disk
    while True:
        with open("hello.txt", "r") as f:
            if f.read() == "some text\n":
                break
        yield timeout(100)

    # Find the dialog and reload the file ...
    yield verify_dialog(b, 1, True)
    # ... a second dialog should not spawn
    yield verify_dialog(b, 2, False)
