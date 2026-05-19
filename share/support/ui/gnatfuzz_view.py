"""The GNATfuzz control View"""

import GPS
from modules import Module
from gs_utils import make_interactive
import pygps
import workflows
from workflows.promises import ProcessWrapper

import os
import json
import glob
from gi.repository import Gtk, Gdk

# The columns in the model
COL_TEST_LABEL = 0
COL_TEST_MESSAGE = 1
COL_TEST_FILENAME = 2
COL_FOREGROUND = 3

counter = 1

# Expected json attributes in the decoded test output
DECODED_IN_PARAMETERS = "Decoded_In_Parameters"
DECODED_OUT_PARAMETERS = "Decoded_Out_Parameters"
PARAM_NAME = "Parameter_Name"
PARAM_TYPE = "Parameter_Type"
PARAM_VALUE = "Parameter_Value"
DECODED_FUNCTION_RETURN = "Decoded_Function_Return"
FUNCTION_RETURN_TYPE = "Function_Return_Type"
FUNCTION_RETURN_VALUE = "Function_Return_Value"

TESTCASE_EXCEPTION = "Testcase_Exception"
EXC_NAME = "Exception_Name"
EXC_MESSAGE = "Exception_Message"
EXC_INFO = "Exception_Information"


def fuzz_executable(is_verbose):
    """Utility function, returns the executable instrumented for fuzzing"""
    # We don't know which AFL mode was built (PLAIN, PERSIST, DEFER,
    # DEFER_AND_PERSIST). They should be equivalent for the purposes of
    # the GNAT Studio integration: try each in turn.

    for mode in ("AFL_PERSIST", "AFL_PLAIN", "AFL_DEFER", "AFL_DEFER_AND_PERSIST"):
        candidate = os.path.join(
            os.path.dirname(GPS.Project.root().file().name()),
            "build",
            f"obj-{mode}",
            "gnatfuzz-test_harness." + ("verbose" if is_verbose else "afl_fuzz"),
        )
        if os.path.exists(candidate):
            return candidate
    return None


class FuzzCrash(object):
    """This represents one crash identified by GNATfuzz"""

    def __init__(self, file):
        self.file = file
        self.label = ""
        self.message = ""
        self.params = []  # A list of tuples of the form ("param N", "value N")


class FuzzCrashList(object):
    """The widget for listing crashes"""

    def __init__(self):
        self.store = Gtk.TreeStore(
            # Label  # Message  # Filename  # Foregreound
            str,
            str,
            str,
            Gdk.RGBA,
        )

        # The tree view
        self.view = Gtk.TreeView(self.store)
        self.view.set_name("fuzz_crash_list_view")  # For debugging/testing

        # For now, render only the URI of the element
        self.label_col = Gtk.TreeViewColumn("Test")
        cell = Gtk.CellRendererText()
        self.label_col.pack_start(cell, False)
        self.label_col.add_attribute(cell, "text", COL_TEST_LABEL)
        # Uncomment this when we want to change the foreground
        # self.label_col.add_attribute(cell, "foreground-rgba", COL_FOREGROUND)

        self.view.append_column(self.label_col)

        self.msg_col = Gtk.TreeViewColumn("Message")
        cell = Gtk.CellRendererText()
        self.msg_col.pack_start(cell, False)
        self.msg_col.add_attribute(cell, "text", COL_TEST_MESSAGE)
        self.view.append_column(self.msg_col)

        self.default_fg = Gdk.RGBA(0, 0, 0)
        self.highlight_fg = Gdk.RGBA(255, 0, 0)

        # Connect to button click
        self.view.connect("button_press_event", self._on_view_button_press)

        # Pack widgets in a box
        scroll = Gtk.ScrolledWindow()
        scroll.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scroll.add(self.view)

        self.box = Gtk.VBox()
        self.box.pack_start(scroll, True, True, 0)

    def _selected_row(self):
        """Return the selected row in self, if any"""
        _, paths = self.view.get_selection().get_selected_rows()
        if not paths:
            return None

        it = self.store.get_iter(paths[0])
        return self.store[it]

    def debug_candidate(self, task):
        """Workflow to launch a debugger session on the given crash"""
        # No build step: the harness is already built by "gnatfuzz build"
        # as part of the Start Fuzzing workflow, with the scenario
        # variables and runtime library that plain gprbuild can't set up.
        exec = fuzz_executable(False)
        if exec is None:
            GPS.Console("Messages").write(
                "Cannot find a built AFL harness to debug. "
                "Run 'Start Fuzzing Session' first.\n",
                mode="error",
            )
            return
        d = GPS.Debugger.spawn(executable=GPS.File(exec))
        d.send("delete")
        d.send(f"start {self.target_candidate}")
        d.send("catch exception")
        d.send("cont")
        d.send("up")  # Hack, wrong frame for gdb here.
        d.send("down")

        # Hide away the commands we sent...
        d.get_console().clear()

        # ... then show the current frame
        d.send("frame", show_in_console=True)
        # Yield once so this remains a valid workflow generator.
        if False:
            yield None

    def _on_view_button_press(self, _, event):
        """React to a click on a row in the list"""

        if event.get_click_count() == (True, 2):
            # On a double click, start debugging

            row = self._selected_row()
            if not row or not row[COL_TEST_FILENAME]:
                return False
            filename = row[COL_TEST_FILENAME]
            if not os.path.exists(filename):
                return False

            self.target_candidate = filename
            workflows.task_workflow("debug fuzzing crash", self.debug_candidate)

    def add_crash(self, crash):
        """Add the info for the given crash to the model"""
        it = self.store.append(None)
        self.store[it] = [crash.label, crash.message, crash.file, self.default_fg]

        # Fill the parameters part of the crash
        for name, val in crash.params:
            param_it = self.store.append(it)
            self.store[param_it] = [name, val, "", self.default_fg]


class GNATfuzzView(Module):
    """We're making use of the Module functionality to provide a native view"""

    view_title = "Fuzz crashes"
    mdi_position = GPS.MDI.POSITION_RIGHT

    def __init__(self):
        self.crashes = {}  # The known FuzzCrashes indexed by filename
        self.fcl = FuzzCrashList()

    def setup(self):
        make_interactive(
            self.get_view, category="Views", name="open GNATfuzz fuzz crashes view"
        )
        make_interactive(
            self.clear_view, category="Views", name="clear GNATfuzz fuzz crashes view"
        )

    def clear_view(self):
        """Clear the Fuzz crashes view"""
        global counter
        counter = 1
        self.crashes.clear()
        # Mark the view as awaiting a new fuzzing session. Refresh()
        # treats the empty string as "skip disk read", so a refresh
        # that races with the workflow (e.g. a re-open) cannot pull
        # results from the previous run's session_dir back in.
        self.session_dir = ""
        t = pygps.get_widget_by_name("fuzz_crash_list_view")
        if t is not None:
            t.get_model().clear()

    def preferences_changed(self, name="", pref=None):
        """React to preferences changed"""
        # Placeholder for getting the foreground and background colours
        pass

    def on_view_destroy(self):
        """React to the destruction of the view"""
        # Nullify the widget field to avoid a dangling reference
        self.widget = None

    def process_crashes(self, task):
        """Workflow to read the crashes from the fuzzing session"""

        global counter

        while self.candidate_crash_files:
            candidate = self.candidate_crash_files.pop()
            if candidate not in self.crashes:
                executable = fuzz_executable(True)
                # We're actually launching the executable to get the
                # parameters that were passed to the crash, along with
                # the actual crash message.
                cl = [executable, candidate]
                p = ProcessWrapper(cl)
                status, output = yield p.wait_until_terminate()
                c = FuzzCrash(candidate)

                splits = candidate.split(os.sep)
                issue_dir = splits[-2]

                if issue_dir == "crashes":
                    issue_label = "Crash"
                elif issue_dir == "hangs":
                    issue_label = "Hang"
                else:
                    issue_label = "Issue"

                c.label = f"{str(counter)} ({issue_label})"
                counter += 1
                c.params = []

                # Extract the json section of the output;
                # it should start after "GNATfuzz : " and end at "^}"
                json_str = ""
                accumulating = False

                # Replace this code when the output of harness programs
                # is simpler to parse.
                for line in output.splitlines():
                    if line.startswith("@@@GNATFUZZ_OUTPUT_START@@@"):
                        accumulating = True
                        json_str = ""
                    elif line == "@@@GNATFUZZ_OUTPUT_END@@@":
                        accumulating = False
                    elif accumulating:
                        json_str += line

                try:
                    decoded = json.loads(json_str)

                    # Let's see if we have an exception
                    if TESTCASE_EXCEPTION in decoded:
                        exc = decoded[TESTCASE_EXCEPTION]
                        if (EXC_NAME in exc) and (EXC_MESSAGE in exc):
                            c.message = f"{exc[EXC_NAME]} : {exc[EXC_MESSAGE]}"
                        else:
                            c.message = "exception"

                    # Let's decode parameters
                    # First In parameters
                    if DECODED_IN_PARAMETERS in decoded:
                        for param in decoded[DECODED_IN_PARAMETERS]:
                            if (
                                (PARAM_NAME in param)
                                and (PARAM_TYPE in param)
                                and (PARAM_VALUE in param)
                            ):
                                value = param[PARAM_VALUE].strip()
                                typ = param[PARAM_TYPE]
                                c.params.append(
                                    (
                                        f"{param[PARAM_NAME]} : in {typ} :=",
                                        f"{value}",
                                    ),
                                )
                            else:
                                c.params.append(("(unknown)", "(unknown)"))

                    # Now Out parameters
                    if DECODED_OUT_PARAMETERS in decoded:
                        for param in decoded[DECODED_OUT_PARAMETERS]:
                            if (
                                (PARAM_NAME in param)
                                and (PARAM_TYPE in param)
                                and (PARAM_VALUE in param)
                            ):
                                value = param[PARAM_VALUE].strip()
                                typ = param[PARAM_TYPE]
                                c.params.append(
                                    (
                                        f"{param[PARAM_NAME]} : out {typ} :=",
                                        f"{value}",
                                    ),
                                )
                            else:
                                c.params.append(("(unknown)", "(unknown)"))

                    # Finally the function return
                    if DECODED_FUNCTION_RETURN in decoded:
                        function_return = decoded[DECODED_FUNCTION_RETURN]
                        if (FUNCTION_RETURN_TYPE in function_return) and (
                            FUNCTION_RETURN_VALUE in function_return
                        ):
                            value = function_return[FUNCTION_RETURN_VALUE].strip()
                            rt = function_return[FUNCTION_RETURN_TYPE]
                            c.params.append(
                                (
                                    f"function return value : {rt} :=",
                                    f"{value}",
                                ),
                            )
                        else:
                            c.params.append(("(unknown)", "(unknown)"))

                except json.decoder.JSONDecodeError:
                    c.message = f"could not decode:\n{json_str}"

                self.crashes[candidate] = c
                self.fcl.add_crash(c)

    def refresh(self):
        """Refresh the view"""
        self.project_dir = os.path.dirname(GPS.Project.root().file().name())
        self.candidate_crash_files = []

        # The fuzz workflow sets self.session_dir to the live session_N
        # directory once it's discovered. The empty string means the view
        # was just cleared and is waiting for a new session: skip the
        # disk read so we do not surface results from the previous run.
        # None means standalone use (no workflow running); fall back to
        # the default "session" layout for backward compatibility.
        session_dir = getattr(self, "session_dir", None)
        if session_dir == "":
            return
        if session_dir is None:
            session_dir = os.path.join(self.project_dir, "session")

        # Read crashes and hangs from the consolidated results layout
        # populated by the GNATfuzz campaign-sync task. This unifies AFL,
        # CMPLOG, libfuzzer and SymCC findings into a single location.
        results_path = os.path.join(session_dir, "results")
        for issue_type in ("crashes", "hangs"):
            self.candidate_crash_files.extend(
                glob.glob(os.path.join(results_path, issue_type, "*"))
            )

        # Process the candidate crash files
        workflows.task_workflow("processing crashes", self.process_crashes)

    def create_view(self):
        self.refresh()
        return self.fcl.box

    def save_desktop(self, child):
        """Save the contents of the view in the desktop"""
        return self.name()

    def load_desktop(self, data):
        """Restore the contents from the desktop"""
        return self.get_child()


def get_gnatfuzz_view():
    """Utility function, to retrieve the registered gnatfuzz view"""
    try:
        from modules import Module_Metaclass

        for inst in Module_Metaclass.modules_instances:
            if inst.__class__ == GNATfuzzView:
                return inst
    except Exception:
        pass
    return None
