"""The GNATfuzz control View"""

import GPS
from modules import Module
from gs_utils import make_interactive
import workflows
from workflows.promises import ProcessWrapper, TargetWrapper

import os
import glob
from gi.repository import Gtk, Gdk

# The columns in the model
COL_TEST_LABEL = 0
COL_TEST_MESSAGE = 1
COL_TEST_FILENAME = 2
COL_FOREGROUND = 3

counter = 1


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
            str, str, str, Gdk.RGBA
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
        t = TargetWrapper("Build Main")
        yield t.wait_on_execute(main_name="fuzz_test_harness.adb")
        proj = GPS.Project.root()
        main_adb = GPS.File(proj.get_main_units()[0])
        exec = proj.get_executable_name(main_adb)
        d = GPS.Debugger.spawn(
            executable=GPS.File(os.path.join(proj.object_dirs()[0], exec))
        )
        d.send("delete")
        d.send(f"start < {self.target_candidate}")
        d.send("catch exception")
        d.send("cont")
        d.send("up")  # Hack, wrong frame for gdb here.
        d.send("down")

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
        make_interactive(self.get_view, category="Views", name="open GNATfuzz View")

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
                executable = os.path.join(
                    self.project_dir,
                    "fuzz_test",
                    "build",
                    "obj-COVERAGE",
                    "fuzz_test_harness.coverage",
                )
                # We're actually launching the executable to get the
                # parameters that were passed to the crash, along with
                # the actual crash message.
                cl = ["/bin/bash", "-c", f"{executable} < {candidate}"]
                p = ProcessWrapper(cl)
                status, output = yield p.wait_until_terminate()
                c = FuzzCrash(candidate)
                c.label = str(counter)
                counter += 1
                c.params = []
                # Replace this code when the output of harness programs
                # is simpler to parse.
                for line in output.splitlines():
                    if line.startswith("Parameter:"):
                        param, value = line.split("=", 1)
                        c.params.append((param, value))

                    # Very crude, need a proper parsable output for this
                    if "raised" in line:
                        _, msg = line.split("raised")
                        c.message = msg

                self.fcl.add_crash(c)

    def refresh(self):
        """Refresh the view"""
        self.project_dir = os.path.dirname(GPS.Project.root().file().name())

        # Get a list of all candidate crash files
        self.candidate_crash_files = glob.glob(
            os.path.join(
                self.project_dir,
                "fuzz_test",
                "fuzzer_output",
                "GNATfuzz_*",
                "crashes",
                "id*",
            )
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
