"""This plugin implements a basic system which stores analyses and allows
   recalling them via a simple view.

   It supports only SPARK runs for now.
"""
# TODO: integrate gnathub runs in this view? Other runs?
# The way this works is the following:
#    - clients call run_manager.add_run to save their runs

import GPS
import os
import shutil
import datetime
import yaml
import glob
import tool_output
from modules import Module
from gi.repository import Gtk
from gps_utils import make_interactive


MAX_SAVED_RUNS = 16  # The maximum number of runs to remember

record_jobs_pref = GPS.Preference('Advanced/record_jobs')
record_jobs_pref.create(
    'Record runs of external tools (currently SPARK)',
    'boolean',
    'Record on disk the results of jobs runs, making them available'
    ' for replaying in the Jobs view',
    False)


class SavedRunManager(object):
    """A singleton which handles the global list of saved runs"""

    def __init__(self):
        self.widget = None  # The view
        self.runs = {}  # The saved runs, indexed by their timestamp
        self.reload_from_disk()

    def _get_archive_file(self):
        return os.path.join(
            GPS.Project.root().artifacts_dir(),
            'runs.yaml')

    def reload_from_disk(self):
        f = self._get_archive_file()
        if os.path.exists(f):
            with open(f, 'rb') as fd:
                self.runs = yaml.load(fd.read())

        # Refresh the widget
        if self.widget:
            self.widget.refresh()

    def _save_to_disk(self):
        with open(self._get_archive_file(), 'wb') as fd:
            fd.write(yaml.dump(self.runs))

    def _save_dir(self, run):
        base = os.path.join(GPS.Project.root().artifacts_dir(),
                            'saved_runs')
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

        # Get the output parser and run the output through it
        parser_text = run['output_parser']
        # TODO: make this part more generic
        if parser_text == "GNATprove_Parser":
            import spark2014
            parser = spark2014.GNATprove_Parser(None)
        parser.on_stdout(run['output'], None)
        parser.on_exit(0, None)  # TODO save status?

    def add_run(self, label, output_parser, files, output):
        """ Add a stored run.

            label: a string in pango markup format, used for display in
            the tree
        """
        # Add the run to the list
        run = {'label': label,
               'category': output_parser.split('_')[0],  # TODO: improve
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

        # Save the list to disk
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
        # Save the run in the jobs view.
        # Do this only if
        #   - the corresponding preference is set
        #   - it's a real run, ie 'command' exists.

        if record_jobs_pref.get() and command:
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


class Jobs_View_Widget():
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


class Jobs_View(Module):
    """ A GPS module, providing the Jobs view """

    view_title = "Jobs"
    mdi_position = GPS.MDI.POSITION_LEFT
    mdi_group = GPS.MDI.GROUP_VIEW

    def __init__(self):
        run_manager.widget = None

    def setup(self):
        # Create an "open jobs" action
        # TODO: make a menu of this.
        make_interactive(
            self.get_view,
            category="Views",
            name="open jobs")

    def project_view_changed(self):
        run_manager.reload_from_disk()

    def on_view_destroy(self):
        run_manager.widget = None

    def create_view(self):
        run_manager.reload_from_disk()
        run_manager.widget = Jobs_View_Widget()
        return run_manager.widget.box
