from GPS import *
from gs_utils.internal.utils import *


"""
Verify that clicking on the 'Computing...' item closes the completion
window.
"""


@run_test_driver
def test_driver():
    GPS.Preference("Smart-Completion-Mode").set("Dynamic")
    initial_toplevels = len(Gtk.Window.list_toplevels())

    buffer = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    view = buffer.current_view()

    view.goto(buffer.at(3,1))

    send_key_event(ord('a'))
    send_key_event(ord('a'))
    send_key_event(ord('a'))
    send_key_event(ord('a'))
    send_key_event(ord('a'))
    send_key_event(ord('a'))

    tree = get_widget_by_name("completion-view")
    gps_assert(
        tree.is_visible(),
        True,
        "The completion tree should be visible")

    model = tree.get_model()
    tree.get_selection().select_iter(model.get_iter_first())
    send_key_event(GDK_RETURN)

    yield wait_idle()

    # Verify that the completion window is gone
    tree = get_widget_by_name("completion-view")
    gps_assert(tree, None, "The completion tree should not exist")

    yield timeout(3000)
