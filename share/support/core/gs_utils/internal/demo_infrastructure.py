# This is where we're putting together the scaffolding for the demo.
# Demo writers are not meant to edit this.

import sys
import GPS
from gi.repository import Gtk
from modules import Module
from workflows.promises import Promise
from gs_utils.internal.utils import simple_error
from gs_utils import make_interactive


class WaiterPromise(Promise):
    def __init__(self, oracle, action, timeout, error_msg, dw):
        # Init the superclass
        super(WaiterPromise, self).__init__()
        self.oracle = oracle
        self.action = action
        self.dw = dw
        self.dw.current_promise = self
        self.error_msg = error_msg
        self.timer = timeout
        GPS.Timeout(100, self.timeout_handler)

    def timeout_handler(self, timeout):
        try:
            self.timer -= 100
            if self.timer <= 0:
                timeout.remove()
                self.dw.current_promise = None
                self.dw.say(self.error_msg)
                self.resolve()
                if GPS.Logger("TESTSUITE").active:
                    simple_error(self.error_msg)

            if self.oracle():
                timeout.remove()
                self.dw.current_promise = None
                self.dw.say("Well done!")
                self.resolve()

        except Exception:
            timeout.remove()
            # If something went wrong, we print the exception
            exception_text = sys.exc_info()[1]
            if self.dw.auto_run:
                GPS.Logger("TESTSUITE").log("Error: %s" % exception_text)
                GPS.exit(1, 1)
            else:
                GPS.MDI.dialog("Error: %s" % exception_text)


class DemoWindow(Module):
    view_title = "Demo"
    mdi_position = GPS.MDI.POSITION_LEFT
    mdi_group = 106

    def __init__(self):
        self.setup()
        GPS.execute_action("open Demo")

    def setup(self):
        # Create an "open Demo" action
        if not GPS.Action("open Demo").exists():
            make_interactive(
                self.get_view,
                category="Views",
                description=("Open (or reuse if it already exists) the 'Demo' view"),
                name="open Demo",
            )

    def create_view(self):
        # Whether the demo is done
        self.done = False

        # Whether the demo should auto-run
        self.auto_run = GPS.Logger("TESTSUITE").active

        # We create a label and a button
        self.label = Gtk.Label("")
        self.done_button = Gtk.Button("Done")
        self.done_button.set_no_show_all(True)
        self.current_promise = None

        # Add a "next" button
        self.next_button = Gtk.Button("Next")

        # Add an "auto-run" checkbox
        self.auto_run_checkbox = Gtk.CheckButton("Auto-run")

        # We pack the label and the button in a vertical box
        self.vbox = Gtk.VBox()
        self.vbox.pack_start(self.label, True, True, 0)
        self.hbox = Gtk.HBox()
        self.hbox.pack_start(self.next_button, True, True, 0)
        self.hbox.pack_start(self.auto_run_checkbox, False, False, 0)
        self.vbox.pack_start(self.hbox, False, False, 0)
        self.vbox.pack_start(self.done_button, False, False, 0)

        # We connect the buttons to a callback
        self.done_button.connect("clicked", self.done_button_clicked)
        self.next_button.connect("clicked", self.next_button_clicked)

        # We connect the auto-run checkbox to a callback
        self.auto_run_checkbox.connect("toggled", self.auto_run_checkbox_toggled)

        return self.vbox

    def on_view_destroy(self):
        GPS.exit(force=True)

    def say(self, markup):
        self.label.set_markup(markup)

    def done_button_clicked(self, widget):
        self.done = True

    def next_button_clicked(self, widget):
        if self.current_promise is not None:
            self.current_promise.action()

    def auto_run_checkbox_toggled(self, widget):
        self.auto_run = widget.get_active()
        if self.auto_run:
            if self.current_promise is not None:
                self.current_promise.action()

    def wait(
        self,
        markup="waiting",
        oracle=lambda: False,
        action=lambda: None,
        error_msg="delay over",
        timeout=60_000,
    ):
        """
        Wait until the oracle is true, then execute the action
        """
        self.say(markup)
        self.current_promise = WaiterPromise(oracle, action, timeout, error_msg, self)
        if self.auto_run:
            action()
        return self.current_promise

    def fin(self):
        """As a promise, wait until the user clicks on the "Done" button"""
        if GPS.Logger("TESTSUITE").active:
            return
        else:
            self.done_button.set_no_show_all(False)
            self.done_button.show_all()
            return self.wait("Press Done to exit", lambda: self.done, lambda: None)
