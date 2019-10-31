from gs_utils.internal.utils import *

"""
Verify that omnisearch popup is hidden when we switch to another application and
is restored when we go back.
"""


@run_test_driver
def driver():
    editor = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    view = editor.current_view()

    w = pygps.get_widget_by_name('global_search')
    w.get_toplevel().grab_focus()
    yield wait_idle()
    w.grab_focus()
    yield wait_idle()
    w.set_text("Ada")

    popup = pygps.get_widget_by_name('global_search-results-list')
    progress_bar = pygps.get_widgets_by_type(Gtk.ProgressBar, popup)[0]
    fraction = progress_bar.get_fraction()

    gps_assert(popup.is_visible(), True,
               "The omnisearch popup is not visible")

    new_w = Gtk.Window()
    new_w.present()

    yield wait_until_true(lambda: not popup.is_visible())
    gps_assert(popup.is_visible(), False,
               "The ommniseach popup should not be visible when GPS has "
               + "not the focus")
    new_w.close()
    w.get_toplevel().grab_focus()

    yield wait_until_true(lambda: popup.is_visible())
    gps_assert(popup.is_visible(), True,
               "The ommniseach popup should be visible again now that "
               + "GPS gained the focus again")

    if progress_bar.is_visible():
        gps_assert(fraction < progress_bar.get_fraction(), True,
                   "The searching was postponed while another app is active")
