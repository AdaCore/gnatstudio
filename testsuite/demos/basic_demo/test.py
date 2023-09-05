# This is how you write an interactive demo.

import os
import sys
import GS
from gi.repository import Gdk, Gtk
from gs_utils.internal.demo_infrastructure import DemoWindow
from pygps import GDK_RETURN

from gs_utils.internal.utils import run_test_driver, get_widgets_by_type, send_key_event, gps_assert

@run_test_driver
def driver():
    try:
        # Create our demo window
        dw = DemoWindow()
        dw.say("<b>GNAT Studio Demo</b>")

        ###############
        # Open a file #
        ###############

        yield dw.wait(
            # Print this text in the demo window
            markup="Open the file <b>common/sdc.adb</b>",
            # Wait until this is True
            oracle=lambda: GS.EditorBuffer.get(GS.File("common/sdc.adb"), open=False)
            is not None,
            # When in auto run mode, do this action
            action=lambda: GS.EditorBuffer.get(GS.File("common/sdc.adb")),
        )

        #########
        # Build #
        #########

        GS.Console().clear()
        yield dw.wait(
            markup="Build the project",
            oracle=lambda: "process terminated successfully" in GS.Console().get_text(),
            action=lambda: GS.execute_action("Build Main Number 1"),
        )

        #########
        # Debug #
        #########

        def debugger_is_active():
            return GS.MDI.get("Debugger Execution") is not None

        yield dw.wait(
            markup="Launch the debugger on the executable",
            oracle=debugger_is_active,
            action=lambda: GS.execute_action("debug initialize Sdc:sdc"),
            error_msg="The Debugger should be present",
        )
        # A bit of scripting is needed to get buffer of the execution window
        view = get_widgets_by_type(Gtk.TextView,
                                   GS.MDI.get("Debugger Execution").pywidget())[0]
        buf = view.get_buffer()

        # Wait until the debuggee is ready
        yield dw.wait(
            markup="Run the debugger and wait until it's ready",
            oracle=lambda: "Welcome to sdc" in buf.get_text(buf.get_start_iter(), buf.get_end_iter(), True),
            action=lambda: GS.Debugger.get().non_blocking_send("run"),
        )

        text = "1 2 + print"

        def send_text_to_debuggee():
            buf.insert_interactive_at_cursor(f"{text}", len(text), True)
            view.grab_focus()
            send_key_event(GDK_RETURN)

        yield dw.wait(
            markup=f"Now enter '{text}' in the execution window",
            oracle=lambda: buf.get_text(buf.get_start_iter(), buf.get_end_iter(), True).find("Error") != -1,
            action=send_text_to_debuggee,
        )

        ###########
        # The end #
        ###########

        # Wait here until the user clicks on the "Done" button
        yield dw.fin()

    except Exception:
        # If something went wrong, we print the exception
        exception_text = sys.exc_info()[1]
        if GS.Logger("TESTSUITE").active:
            GS.Logger("TESTSUITE").log("Error: %s" % exception_text)
            GS.exit(1, 1)
        else:
            GS.MDI.dialog("Error: %s" % exception_text)
