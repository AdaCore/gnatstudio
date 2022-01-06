"""This plugin implements a basic system which saves gnatprove runs and allows
   recalling them via a simple view.
"""

# The way this works is the following:
#    - clients call run_manager.add_run to save their runs

import GPS
import os
import os_utils
import shutil
import datetime
import yaml
import glob
import tool_output
import spark2014
from modules import Module
from gi.repository import Gtk
from gs_utils import make_interactive

NB_MAX_PREF = "Plugins/gnatprove_runs/nb_runs"
GPS.Preference(NB_MAX_PREF).create(
    "Number of runs saved", "integer",
    """Number of runs saved in the artifacts directory.""", 2, 1, 10)


class SavedRunManager(object):
    """A singleton which handles the global list of saved runs"""

    def __init__(self):
        self.widget = None  # The view
        self.runs = {}  # The saved runs, indexed by their timestamp
        self.reload_from_disk()

        def on_project_changed(*args):
            self.reload_from_disk()
        GPS.Hook("project_view_changed").add(on_project_changed)

    def _get_archive_file(self):
        return os.path.join(
            GPS.Project.root().artifacts_dir(),
            'runs.yaml')

    def reload_from_disk(self):
        f = self._get_archive_file()
        if os.path.exists(f):
            with open(f, 'rb') as fd:
                self.runs = yaml.load(fd.read(), Loader=yaml.FullLoader)

        # Refresh the widget
        if self.widget:
            self.widget.refresh()

    def _save_to_disk(self):
        with open(self._get_archive_file(), 'w') as fd:
            fd.write(yaml.dump(self.runs))

    def _purge_old_runs(self):
        search_dir = os.path.join(
            GPS.Project.root().artifacts_dir(), 'saved_runs')
        full_dirs = [
            os.path.join(search_dir, d) for d in os.listdir(search_dir)]
        full_dirs.sort(key=lambda x: os.path.getmtime(x))

        cpt = 1
        for d in reversed(full_dirs):
            if cpt > GPS.Preference(NB_MAX_PREF).get():
                try:
                    del self.runs[os.path.basename(d).replace('_', ':')]
                except KeyError:
                    # Corruption somewhere: the dir is not listed anymore in
                    # the runs.
                    pass
                shutil.rmtree(d)
            else:
                cpt += 1

    def _save_dir(self, run):
        base = os.path.join(
            GPS.Project.root().artifacts_dir(), 'saved_runs')
        if not os.path.exists(base):
            os.mkdir(base)
        return os.path.join(base, run['timestamp'].replace(':', '_'))

    def restore_run(self, run_timestamp):
        run = self.runs[run_timestamp]
        # Restore the files from the saved dir
        src = self._save_dir(run)
        dest = GPS.Project.root().artifacts_dir()
        for f in glob.glob(os.path.join(src, '*')):
            tgt = os.path.join(dest, os.path.basename(f))
            if os.path.exists(tgt):
                shutil.rmtree(tgt)
            shutil.copytree(f, tgt)

        # Clear the Messages view
        GPS.Console("Messages").clear()
        # Clear the locations
        GPS.Locations.remove_category(run["category"])

        parser = spark2014.GNATprove_Parser(None)
        parser.on_stdout(run['output'], None)
        parser.on_exit(0, None)

    def add_run(self, label, output_parser, files, output):
        """ Add a stored run.

            label: a string in pango markup format, used for display in
            the tree
        """
        # Add the run to the list
        run = {'label': label,
               'category': output_parser.split('_')[0],
               'output_parser': output_parser,
               'files': files,
               'output': output,
               'timestamp': datetime.datetime.now().isoformat()}
        self.runs[run['timestamp']] = run

        # Copy the files to the save dir
        dest = self._save_dir(run)
        if os.path.exists(dest):
            shutil.rmtree(dest)
        os.mkdir(dest)
        for f in files:
            shutil.copytree(f, os.path.join(dest, os.path.basename(f)))

        # Purge the old runs and save the list to disk
        self._purge_old_runs()
        self._save_to_disk()

        # Refresh the widget
        if self.widget:
            self.widget.refresh()


run_manager = SavedRunManager()


class Job_Recorder(tool_output.OutputParser):

    def __init__(self, child):
        tool_output.OutputParser.__init__(self, child)
        self.raw_text = ''
        self.child = child

    def on_stdout(self, text, command):
        self.raw_text = self.raw_text + text
        # Pass the ball to the next child in the chain
        self.child.on_stdout(text, command)

    def on_exit(self, status, command):
        # Save the run: Do this only if it's a real run, ie if command exists.

        if command:
            run_manager.add_run(
                command.name(),
                self.child.__class__.__name__,
                [os.path.join(GPS.Project.root().artifacts_dir(),
                              'gnatprove')],
                self.raw_text)

        # Pass the ball to the next child in the chain
        self.child.on_exit(status, command)


COL_LABEL = 0
COL_TIMESTAMP_LABEL = 1
COL_INDEX = 2


class GNATprove_Runs_View_Widget():
    """The widget for the Jobs view"""

    def __init__(self):
        self.saved_runs = []
        self.box = Gtk.VBox()

        self.store = Gtk.TreeStore(str,  # the label
                                   str,  # the timestamp label
                                   str,  # the index of the entry in saved_runs
                                   )

        # Initialize the tree view
        self.view = Gtk.TreeView(self.store)
        self.view.connect("button_press_event", self._on_view_button_press)

        self.label_col = Gtk.TreeViewColumn("Action")
        self.label_col.set_sort_column_id(COL_LABEL)
        cell = Gtk.CellRendererText()
        self.label_col.pack_start(cell, True)
        self.label_col.add_attribute(cell, "markup", COL_LABEL)
        self.view.append_column(self.label_col)

        self.timestamp_col = Gtk.TreeViewColumn("Date")
        self.timestamp_col.set_sort_column_id(COL_TIMESTAMP_LABEL)
        cell = Gtk.CellRendererText()
        self.timestamp_col.pack_start(cell, True)
        self.timestamp_col.add_attribute(cell, "markup", COL_TIMESTAMP_LABEL)
        self.view.append_column(self.timestamp_col)
        # Sort now
        self.timestamp_col.set_sort_indicator(True)
        self.store.set_sort_column_id(1, Gtk.SortType.DESCENDING)

        # Pack things together
        scroll = Gtk.ScrolledWindow()
        scroll.set_policy(Gtk.PolicyType.AUTOMATIC, Gtk.PolicyType.AUTOMATIC)
        scroll.add(self.view)
        self.box.pack_start(scroll, True, True, 3)

        # fill the contents
        self.refresh()

    def _selected_row(self):
        """Return the selected row in self, if any"""
        _, paths = self.view.get_selection().get_selected_rows()
        if not paths:
            return None

        it = self.store.get_iter(paths[0])
        return self.store[it]

    def _on_view_button_press(self, _, event):
        """React to a button_press on the view."""

        if event.get_click_count() == (True, 2):
            # On a double click, restore the given entry
            row = self._selected_row()
            if row:
                run_manager.restore_run(row[COL_INDEX])

            return False

    def refresh(self):
        """Refresh the contents of the view"""
        self.store.clear()
        for run_id in run_manager.runs:
            run = run_manager.runs[run_id]
            it = self.store.append(None)
            self.store[it] = [
                run['label'],
                run['timestamp'],
                run['timestamp']]


class GNATprove_Runs_View(Module):
    """ A GPS module, providing the Jobs view """

    view_title = "GNATprove Runs"
    mdi_position = GPS.MDI.POSITION_LEFT
    mdi_group = GPS.MDI.GROUP_VIEW

    def __init__(self):
        run_manager.widget = None

    def setup(self):
        if os_utils.locate_exec_on_path('gnatprove'):
            make_interactive(
                self.get_view,
                category="Views",
                description=("Open (or reuse if it already exists)" +
                             " the 'GNATprove Runs' view"),
                menu="SPARK/Show Previous Runs",
                name="open gnatprove runs")

    def on_view_destroy(self):
        run_manager.widget = None

    def create_view(self):
        run_manager.reload_from_disk()
        run_manager.widget = GNATprove_Runs_View_Widget()
        return run_manager.widget.box
