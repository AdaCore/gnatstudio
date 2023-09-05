"""
This is where we're putting together the scaffolding for the demo.
Demo writers are not meant to edit this.
"""

import sys
import GS
from gi.repository import Gtk
from workflows.promises import Promise
from gs_utils.internal.utils import simple_error


class WaiterPromise(Promise):
    def __init__(self, oracle, action, error_msg, timeout, dw):
        # Init the superclass
        super(WaiterPromise, self).__init__()
        self.oracle = oracle
        self.action = action
        self.dw = dw
        self.dw.current_promise = self
        self.timer = timeout
        self.error_msg = error_msg
        GS.Timeout(100, self.timeout_handler)

    def timeout_handler(self, timeout):
        try:
            self.timer -= 100
            if self.timer <= 0:
                timeout.remove()
                self.dw.current_promise = None
                self.dw.say(self.error_msg)
                self.resolve()
                if GS.Logger("TESTSUITE").active:
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
                GS.Logger("TESTSUITE").log("Error: %s" % exception_text)
                GS.exit(1, 1)
            else:
                GS.MDI.dialog("Error: %s" % exception_text)


class DemoWindow(object):
    def __init__(self, title):
        self.title = title
        self.window = Gtk.Window(title=title)

        # Whether the demo is done
        self.done = False

        # Whether the demo should auto-run
        self.auto_run = GS.Logger("TESTSUITE").active

        # We create a label and a button
        self.label = Gtk.Label("")
        self.done_button = Gtk.Button("Done")
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

        # We pack the vertical box in the window
        self.window.add(self.vbox)

        # We connect the buttons to a callback
        self.done_button.connect("clicked", self.done_button_clicked)
        self.next_button.connect("clicked", self.next_button_clicked)

        # We connect the auto-run checkbox to a callback
        self.auto_run_checkbox.connect("toggled", self.auto_run_checkbox_toggled)

        # We show the window
        self.window.show_all()

        # By default, hide the button
        self.done_button.hide()

        # Set the position of the window to 100, 100
        self.window.move(100, 100)

        # Set the size of the window
        self.window.resize(300, 600)

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
        Wait until the oracle is true, then execute the action.
        """
        self.say(markup)
        self.current_promise = WaiterPromise(oracle, action, error_msg, timeout, self)
        if self.auto_run:
            action()
        return self.current_promise

    def fin(self):
        """As a promise, wait until the user clicks on the "Done" button"""
        if GS.Logger("TESTSUITE").active:
            return
        else:
            self.done_button.show_all()
            return self.wait("Press Done to exit", lambda: self.done, lambda: None)
