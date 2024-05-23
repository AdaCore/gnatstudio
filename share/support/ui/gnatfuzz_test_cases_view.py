"""The GNATfuzz Test Cases View"""

import GPS
from modules import Module
from gs_utils import make_interactive
import pygps
import workflows
from workflows.promises import ProcessWrapper, TargetWrapper
from gnatfuzz_view import fuzz_executable

import os
import json
import glob
from gi.repository import Gtk, Gdk

# The columns in the model
COL_TEST_CASE_ID = 0
COL_TEST_CASE_DETAILS = 1
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


class FuzzTestCase(object):
    """This represents one test case identified by GNATfuzz"""

    def __init__(self, file):
        self.file = file
        self.id = ""
        self.details = ""
        self.params = []  # A list of tuples of the form ("param N", "value N")


class FuzzTestCaseList(object):
    """The widget for listing test cases"""

    def __init__(self):
        self.store = Gtk.TreeStore(
            # ID  # Details  # Filename  # Foregreound
            str,
            str,
            str,
            Gdk.RGBA,
        )

        self.store.set_sort_column_id(2, 0)

        # The tree view
        self.view = Gtk.TreeView(self.store)
        self.view.set_name("fuzz_test_case_view")  # For debugging/testing

        # For now, render only the URI of the element
        self.test_case_id_col = Gtk.TreeViewColumn("ID")
        cell = Gtk.CellRendererText()
        self.test_case_id_col.pack_start(cell, False)
        self.test_case_id_col.add_attribute(cell, "text", COL_TEST_CASE_ID)
        # Uncomment this when we want to change the foreground
        # self.label_col.add_attribute(cell, "foreground-rgba", COL_FOREGROUND)

        self.view.append_column(self.test_case_id_col)

        self.test_case_details_col = Gtk.TreeViewColumn("Details")
        cell = Gtk.CellRendererText()
        self.test_case_details_col.pack_start(cell, False)
        self.test_case_details_col.add_attribute(cell, "text", COL_TEST_CASE_DETAILS)
        self.view.append_column(self.test_case_details_col)

        self.default_fg = Gdk.RGBA(0, 0, 0)

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
        t = TargetWrapper("Build Main")
        yield t.wait_on_execute(main_name="gnatfuzz-fuzz_test_harness.adb")
        exec = fuzz_executable(False)
        d = GPS.Debugger.spawn(executable=GPS.File(exec))
        d.send("delete")
        d.send(f"start {self.target_candidate}")

        # It would be nice if we could add a breakpoint at the start of
        # the user subprogram under test...

        # Hide away the commands we sent...
        d.get_console().clear()

        # ... then show the current frame
        d.send("frame", show_in_console=True)

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

    def add_test_case(self, test_case):
        """Add the info for the given crash to the model"""
        it = self.store.append(None)
        self.store[it] = [
            test_case.id,
            test_case.details,
            test_case.file,
            self.default_fg,
        ]

        # Fill the parameters part of the crash
        for name, val in test_case.params:
            param_it = self.store.append(it)
            self.store[param_it] = [name, val, "", self.default_fg]


class GNATfuzzTestCaseView(Module):
    """We're making use of the Module functionality to provide a native view"""

    view_title = "Fuzz test cases"
    mdi_position = GPS.MDI.POSITION_RIGHT

    def __init__(self):
        self.test_cases = {}  # The known test cases indexed by filename
        self.fcl = FuzzTestCaseList()

    def setup(self):
        make_interactive(
            self.get_view, category="Views", name="open GNATfuzz test cases view"
        )
        make_interactive(
            self.clear_view, category="Views", name="clear GNATfuzz test cases view"
        )

    def clear_view(self):
        """Clear the Fuzz test cases view"""
        global counter
        counter = 1
        t = pygps.get_widget_by_name("fuzz_test_case_view")
        self.test_cases.clear()
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

    def process_test_cases(self, task):
        """Workflow to read the test cases from the fuzzing session"""

        global counter

        while self.candidate_test_case_files:
            candidate = self.candidate_test_case_files.pop()
            if candidate not in self.test_cases:
                executable = fuzz_executable(True)
                # We're actually launching the executable to get the
                # parameters that were passed to the test case
                cl = [executable, candidate]
                p = ProcessWrapper(cl)
                status, output = yield p.wait_until_terminate()
                c = FuzzTestCase(candidate)

                # Derive and set the test ID and test case details
                # from the base filename
                # Turn: id:000000 time:0, execs:0, orig:GNATfuzz_XX into
                #       ID: 0 | Details: time:0, execs:0, orig:GNATfuzz_XX into
                basename = os.path.basename(candidate)
                test_case_id = basename.split(",")[0]
                id_len = len(test_case_id) + 1
                test_case_details = basename[id_len:]
                c.details = test_case_details
                test_case_id = test_case_id[3:]
                # if test_case_id == "000000":
                #    test_case_id = "0"
                # else:
                #    test_case_id = test_case_id.lstrip('0')
                c.id = test_case_id

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

                self.test_cases[candidate] = c
                self.fcl.add_test_case(c)

    def refresh(self):
        """Refresh the view"""
        self.project_dir = os.path.dirname(GPS.Project.root().file().name())
        self.candidate_test_case_files = []

        # Get a list of all candidate testcases
        self.candidate_test_case_files.extend(
            sorted(
                glob.glob(
                    os.path.join(
                        self.project_dir,
                        "session",
                        "fuzzer_output",
                        "gnatfuzz_1_master",
                        "queue",
                        "id*",
                    )
                )
            )
        )
        # Process the candidate test case files
        workflows.task_workflow("processing test cases", self.process_test_cases)

    def create_view(self):
        self.refresh()
        return self.fcl.box

    def save_desktop(self, child):
        """Save the contents of the view in the desktop"""
        return self.name()

    def load_desktop(self, data):
        """Restore the contents from the desktop"""
        return self.get_child()


def get_gnatfuzz_test_case_view():
    """Utility function, to retrieve the registered gnatfuzz test case view"""
    try:
        from modules import Module_Metaclass

        for inst in Module_Metaclass.modules_instances:
            if inst.__class__ == GNATfuzzTestCaseView:
                return inst
    except Exception:
        pass
    return None
