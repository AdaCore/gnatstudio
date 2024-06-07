"""
Basic test for the workflows.promises.wait_signal function
which allows to wait for a Gtk+ signal using workflows.
"""
import GPS
from gs_utils.internal.utils import *
from gi.repository import Gtk
from workflows.promises import wait_signal


@run_test_driver
def run_test():
    custom_dialog = Gtk.Dialog("Custom")
    custom_dialog.add_button("_Ok", Gtk.ResponseType.OK)
    custom_dialog.set_default_size(400, 400)
    custom_dialog.show_all()

    GPS.Timeout(300, lambda x: custom_dialog.response(Gtk.ResponseType.OK))
    response = yield wait_signal(custom_dialog, "response")

    gps_assert(response, Gtk.ResponseType.OK, "Wrong response has been returned")
