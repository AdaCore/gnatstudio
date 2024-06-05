"""
This plugin provides actions and menus that can be useful to report GNAT
Studio issues.
"""

import GPS
from gs_utils import interactive
import os
from datetime import datetime
from shutil import copy, copyfile, move, rmtree
from os_utils import locate_exec_on_path
from gi.repository import Gtk
from workflows.promises import wait_signal
import workflows


INCLUDE_LOG_HIST_KEY = "bug-include-log"
INCLUDE_ALS_LOG_HIST_KEY = "bug-include-als-log"
INCLUDE_PREFS_HIST_KEY = "bug-include-prefs"
INCLUDE_PLUGINS_HIST_KEY = "bug-include-plugins"
BUG_REPORT_FOLDER_HIST_KEY = "bug-report-folder"


class BugReportDialog(Gtk.Dialog):
    def __init__(self):
        Gtk.Dialog.__init__(
            self,
            title="Create bug report",
            transient_for=GPS.MDI.current().pywidget().get_toplevel(),
        )
        self.set_name("gps-bug-report-dialog")
        self.set_default_size(400, 400)
        self.add_button("_OK", Gtk.ResponseType.OK)
        self.add_button("_Cancel", Gtk.ResponseType.CANCEL)
        self.connect("response", self.on_response)
        self.set_default_response(Gtk.ResponseType.OK)

        label = Gtk.Label(label="Included information")
        label.set_xalign(0.0)
        label.get_style_context().add_class("gps-bug-report-section-desc")
        self.vbox.pack_start(label, False, False, 0)

        self.include_log_check = None
        self.include_als_log_check = None
        self.include_prefs_check = None
        self.include_plugins_check = None

        gs_log_file = GPS.get_log_file()
        als_log_file = get_als_log_file()
        preferences_file = get_preferences_file()
        plugins_file = get_plugins_file()

        # Create a checkbox for each file that we can possibly include
        # in the bug report archive

        if gs_log_file:
            self.include_log_check = self.create_checkbox(
                "GNAT Studio log file",
                INCLUDE_LOG_HIST_KEY,
                "The GNAT Studio log file. WARNING: it might contain "
                + "information on your sources (filenames, variable names "
                + "etc.).",
                gs_log_file,
            )

        if als_log_file:
            self.include_als_log_check = self.create_checkbox(
                "Ada Language Server log file",
                INCLUDE_LOG_HIST_KEY,
                "The Ada Language Server log file. "
                + "WARNING: it might contain source code.",
                als_log_file,
            )

        if preferences_file:
            self.include_prefs_check = self.create_checkbox(
                "Preferences file",
                INCLUDE_PREFS_HIST_KEY,
                "Contains all the explicitly enabled/disabled " + "user preferences.",
                preferences_file,
            )

        if plugins_file:
            self.include_plugins_check = self.create_checkbox(
                "Active plugins file",
                INCLUDE_PLUGINS_HIST_KEY,
                "Contains all the active plugins' names.",
                plugins_file,
            )

        # Create the bug report archive root folder entry

        label = Gtk.Label(label="Bug reports root directory")
        label.set_xalign(0.0)
        label.get_style_context().add_class("gps-bug-report-section-desc")
        self.vbox.pack_start(label, False, False, 0)

        doc_label = Gtk.Label(
            "The directory where the bug report archives are created."
        )
        doc_label.set_xalign(0.0)
        doc_label.get_style_context().add_class("dialog-views-doc-labels")
        self.vbox.pack_start(doc_label, False, False, 0)

        hbox = Gtk.HBox()
        hbox.show()
        self.vbox.pack_start(hbox, False, False, 0)

        self.fileEntry = Gtk.Entry()
        self.fileEntry.set_editable(True)
        self.fileEntry.connect(
            "focus-in-event", lambda x, y: GPS.MDI.set_focus_widget(self.fileEntry)
        )
        self.fileEntry.set_text(os.path.join(GPS.get_home_dir(), "log"))
        hbox.pack_start(self.fileEntry, True, True, 0)
        default_val = GPS.History.get(BUG_REPORT_FOLDER_HIST_KEY, most_recent=True)
        self.fileEntry.set_text(
            default_val
            if default_val is not None
            else os.path.join(GPS.get_home_dir(), "log")
        )
        button = Gtk.Button("Browse")
        button.connect("clicked", self.on_bug_report_location_browse)
        hbox.pack_start(button, False, False, 0)

    def on_bug_report_location_browse(self, *args):
        """
        Called when clicking on the 'Browse' button to choose the bug archive
        root location.
        """
        file = GPS.MDI.directory_selector(base_dir=self.fileEntry.get_text())
        if file.path != "":
            self.fileEntry.set_text(file.path)

    def get_bug_report_location(self):
        """
        Return the bug report root location.
        """
        return self.fileEntry.get_text()

    def get_included_info_options(self):
        """
        Return a boolean tuple for the files that should be included, in the
        following order: (<gs_log>, <als_log>, <prefs_file>, <plugins_file>).
        """

        def get_include_val(check):
            return check.get_active() if check else False

        return (
            get_include_val(self.include_log_check),
            get_include_val(self.include_als_log_check),
            get_include_val(self.include_prefs_check),
            get_include_val(self.include_plugins_check),
        )

    def create_checkbox(self, label, hist_key, doc, file):
        """
        Create a checkbox for the given file, allowing the user to choose
        whether it should be included in the bug report or not.
        A 'show' linkbutton will be placed on the right, allowing to open
        the file and to modify it if needed before including it in the bug
        report.
        """
        hbox = Gtk.HBox()
        self.vbox.pack_start(hbox, False, False, 0)

        checkbox = Gtk.CheckButton(label)
        default_val = GPS.History.get(hist_key, most_recent=True)
        checkbox.set_active(default_val if default_val is not None else True)
        hbox.pack_start(checkbox, False, False, 0)

        label = Gtk.Label(" (")
        label.set_xalign(0.0)
        hbox.pack_start(label, False, False, 0)

        def on_show_button_clicked(*args):
            extension = os.path.splitext(file)[1]
            file_copy = file.replace(extension, "_copy" + extension)
            copyfile(file, file_copy)
            GPS.EditorBuffer.get(GPS.File(file_copy))

        linkbutton = Gtk.LinkButton.new_with_label(
            uri="Show file contents.", label="show"
        )
        linkbutton.connect("clicked", on_show_button_clicked)

        # Connect to the 'activate-link' signal to handle it by doing nothing
        # to avoid an unnecessary warning on Windows due to Gtk+
        linkbutton.connect("activate-link", lambda x: True)

        hbox.pack_start(linkbutton, False, False, 0)

        label = Gtk.Label(")")
        label.set_xalign(0.0)
        hbox.pack_start(label, False, False, 0)

        doc_label = Gtk.Label(doc)
        doc_label.set_xalign(0.0)
        doc_label.get_style_context().add_class("dialog-views-doc-labels")
        self.vbox.pack_start(doc_label, False, False, 0)

        return checkbox

    def on_response(self, dialog, response, *args):
        """
        Called when the user clisk on the 'Ok' or 'Cancel' button.
        Store the options in the history on OK responses.
        """
        if response == Gtk.ResponseType.OK:
            if self.include_log_check:
                GPS.History.add(
                    INCLUDE_LOG_HIST_KEY, self.include_log_check.get_active()
                )

            if self.include_als_log_check:
                GPS.History.add(
                    INCLUDE_ALS_LOG_HIST_KEY, self.include_als_log_check.get_active()
                )

            if self.include_prefs_check:
                GPS.History.add(
                    INCLUDE_PREFS_HIST_KEY, self.include_prefs_check.get_active()
                )

            if self.include_plugins_check:
                GPS.History.add(
                    INCLUDE_PLUGINS_HIST_KEY, self.include_plugins_check.get_active()
                )

            GPS.History.add(BUG_REPORT_FOLDER_HIST_KEY, self.fileEntry.get_text())


@workflows.run_as_workflow
def open_bug_report_in_folder(text):
    """
    Called when clicking on the bug report location hyperlink displayed
    in th Messages view after creating it. Open the location of the bug
    archive.
    """
    dialog = Gtk.FileChooserNative()
    dialog.set_filename(text)
    dialog.run()


def get_als_log_file():
    """
    Return the ALS log file if any, or None when there is no log.
    """
    server = GPS.LanguageServer.get_by_language_name("Ada")
    if server:
        return server.get_log_file().path
    else:
        return None


def get_plugins_file():
    """
    Return the plugins file if any, or None when not present
    """
    plugins_file = os.path.join(GPS.get_home_dir(), "startup.xml")
    if os.path.exists(plugins_file):
        return plugins_file
    else:
        return None


def get_preferences_file():
    """
    Return the preferences file if any, or None when not present
    """
    preferences_file = os.path.join(GPS.get_home_dir(), "preferences.xml")
    if os.path.exists(preferences_file):
        return preferences_file
    else:
        return None


def include_file(file, bug_report_dir):
    """
    Include the given file in the bug report diretory.
    it looks for the file copy if it exists (created when clicking on the
    'show' buttons).
    """
    extension = os.path.splitext(file)[1]
    file_copy = file.replace(extension, "_copy" + extension)
    if os.path.exists(file_copy):
        move(file_copy, bug_report_dir)
    else:
        copy(file, bug_report_dir)


@interactive(
    "General",
    name="Create bug report",
    description="Create an archive containing information "
    + "(logs, plugins file etc.) that can be used for bug reports.",
)
def create_bug_report():
    # Create the bug report dialog and wait for the user's response
    dialog = BugReportDialog()
    dialog.show_all()

    response = yield wait_signal(dialog, "response")

    if response != Gtk.ResponseType.OK:
        dialog.destroy()
        return

    (
        include_log,
        include_als_log,
        include_prefs,
        include_plugins,
    ) = dialog.get_included_info_options()

    bug_report_location = dialog.get_bug_report_location()
    dialog.destroy()
    GPS.Console().write("Creating bug report archive...\n")

    if not os.path.isdir(bug_report_location):
        os.mkdir(bug_report_location)

    # Create the bug report archive directory
    log_file = GPS.get_log_file()
    date = datetime.now()
    bug_report_dir = os.path.join(
        bug_report_location, "bug_report.%s" % date.strftime("%Y-%m-%dT%H%M%S")
    )
    os.mkdir(bug_report_dir)

    # Copy the GNAT Studio log file
    if include_log:
        include_file(log_file, bug_report_dir)

    # Copy the Ada Language Server log file
    if include_als_log:
        als_log_file = get_als_log_file()
        include_file(als_log_file, bug_report_dir)

    # Copy the preferences file
    if include_prefs:
        preferences_file = get_preferences_file()
        include_file(preferences_file, bug_report_dir)

    # Copy startup.xml if asked
    if include_plugins:
        plugins_file = get_plugins_file()
        include_file(plugins_file, bug_report_dir)

    # Create a tar.bz2 archive if tar is available
    if locate_exec_on_path("tar"):
        archive_file = bug_report_dir + ".tar.bz2"
        GPS.cd(bug_report_location)
        GPS.Process(
            [
                "tar",
                "-cjf",
                os.path.basename(bug_report_dir) + ".tar.bz2",
                os.path.basename(bug_report_dir),
            ]
        ).get_result()
        rmtree(bug_report_dir)
        GPS.Console().write("Bug report archive created at: ")
        GPS.Console().insert_link(archive_file, open_bug_report_in_folder)
        GPS.Console().write("\n")
    else:
        GPS.Console().write("Bug report folder created at: ")
        GPS.Console().insert_link(bug_report_dir, open_bug_report_in_folder)
        GPS.Console().write("\n")
