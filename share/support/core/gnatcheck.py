"""gnatcheck support for GPS

This plugin adds support for gnatcheck, a coding standard checker
"""

###########################################################################
# No user customization below this line
###########################################################################

from enum import Enum
from dataclasses import dataclass

import GPS
import os
import os.path
from os_utils import locate_exec_on_path
import re
import traceback
import os_utils
from gi.repository import Gtk
from gs_utils import interactive, hook
from gs_utils.gnatcheck_rules_editor import rulesEditor, get_supported_rules

gnatsas = os_utils.locate_exec_on_path("gnatsas")

default_category = "Coding Standard violations"
category_map = {
    "error": "GNATcheck: Errors",
    "warning": "GNATcheck: Warnings",
    "info": "GNATcheck: Info",
    "rule violation": default_category,
    "rule violation (exempted)": default_category,
}


class RuleFileKind(Enum):
    """
    Represents possible types of rule file.
    """

    LEGACY = 1
    LKQL = 2


@dataclass
class RuleFile:
    """
    This class represents a GNATcheck rule file.
    """

    file: GPS.File
    """
    Absolute path to the rule file.
    """

    kind: RuleFileKind
    """
    The kind of rule file. This have influence on how the file si handled by
    GNATcheck and how it should be considered.
    """


class RulesSelector(Gtk.Dialog):
    """
    Dialog used to select a coding standard file before launching gnatcheck.
    """

    def __init__(self, projectname, defaultfile):
        Gtk.Dialog.__init__(
            self,
            title="Select a coding standard file",
            parent=GPS.MDI.current().pywidget().get_toplevel(),
            flags=Gtk.DialogFlags.MODAL,
        )

        # OK - Cancel buttons
        self.okButton = self.add_button("OK", Gtk.ResponseType.OK)
        self.cancelButton = self.add_button("Cancel", Gtk.ResponseType.CANCEL)

        label = Gtk.Label(
            label="No check switches are defined for project {}"
            "\nPlease enter a coding standard file containing the"
            " desired gnatcheck rules:".format(projectname)
        )
        label.show()
        self.vbox.pack_start(label, False, False, 0)

        hbox = Gtk.HBox()
        hbox.show()
        self.vbox.pack_start(hbox, False, False, 0)

        self.fileEntry = Gtk.Entry()
        self.fileEntry.set_editable(True)
        self.fileEntry.show()
        hbox.pack_start(self.fileEntry, True, True, 0)

        if defaultfile is not None:
            self.fileEntry.set_text(defaultfile.path)
        self.fileEntry.connect("changed", self.on_file_entry_changed)
        self.on_file_entry_changed()

        button = Gtk.Button("Browse")
        button.connect("clicked", self.on_coding_standard_file_browse)
        button.show()
        hbox.pack_start(button, False, False, 0)

    def get_file(self):
        return GPS.File(self.fileEntry.get_text())

    def on_file_entry_changed(self, *args):
        """Callback when the file entry changed"""
        name = self.fileEntry.get_text()
        if name == "":
            self.okButton.set_sensitive(False)
        else:
            self.okButton.set_sensitive(True)

    def on_coding_standard_file_browse(self, *args):
        """Callback to coding standard 'Browse' button"""
        file = GPS.MDI.file_selector()
        if file.path != "":
            self.fileEntry.set_text(file.path)


class GnatcheckProc:
    """This class controls the gnatcheck execution"""

    def __init__(self):
        self.rules_file: RuleFile = RuleFile(None, RuleFileKind.LEGACY)
        self.rules = None

        self.locations_string = "Coding Standard violations"
        self.gnatCmd = ""
        self.gnatArgs = None
        self.checkCmd = ""
        self.full_output = ""

        self.ruleseditor = None  # The GUI to edit rules

    def get_rules_file(self):
        def project_relative_rule_file(
            file_name: str,
            kind: RuleFileKind,
        ) -> RuleFile:
            """
            Return a `RuleFile` object after resolving the `file_name`
            relatively to the current project.
            """
            return RuleFile(
                file=GPS.File(
                    os.path.join(
                        GPS.Project.root().file().directory(),
                        file_name,
                    )
                ),
                kind=kind,
            )

        # First, we try to retrieve the coding standard file from the project's
        # "Switches" and "Default_Switches" attributes.
        for attribute in ["switches", "default_switches"]:
            for opt in GPS.Project.root().get_attribute_as_list(
                attribute, package="check", index="ada"
            ):
                res = re.split(r"^\-from\=(.*)$", opt)
                if len(res) > 1:
                    # Resolve the coding standard relatively to the current
                    # project file.
                    return project_relative_rule_file(
                        res[1],
                        RuleFileKind.LEGACY,
                    )
        # Then try to get the LKQL rule file from the "Check'Rule_File" GPR
        # attribute.
        lkql_rule_file = GPS.Project.root().get_attribute_as_string(
            "rule_file", package="check"
        )
        if lkql_rule_file != "":
            return project_relative_rule_file(lkql_rule_file, RuleFileKind.LKQL)

        # If no rule file was found, return the current value of the rules file
        return self.rules_file

    def update_gnat_cmd(self):
        target = GPS.get_target()

        # Use the correct GNAT driver to spawn GNATcheck. On old versions,
        # GNATcheck used to be prefixed by the target (i.e:
        # arm-eabi-gnatcheck).
        # This is not the case with newer versions, where there is only
        # one 'gnatcheck' executable: the target and runtime should be
        # specified via the '--target' and the '--RTS' option.

        if target == "":
            self.gnatCmd = "gnat"
            self.checkCmd = "gnatcheck"
        else:
            # If a target is specified in the GPR project file, check if we have
            # a non-prefixed gnatcheck in the PATH, which supports the '--target'
            # option. Fallback to the old prefixed gnatcheck if not.
            if (
                locate_exec_on_path("gnatcheck")
                and "--target" in GPS.Process("gnatcheck --help").get_result()
            ):
                runtime = GPS.get_runtime()
                self.gnatCmd = "{}-gnat".format(target)
                self.checkCmd = "gnatcheck"
                self.gnatArgs = ["--target=%s" % target]

                if runtime != "":
                    self.gnatArgs.append("--RTS=%s" % runtime)
            else:
                self.gnatCmd = "{}-gnat".format(target)
                self.checkCmd = "{}-gnatcheck".format(target)

        if not locate_exec_on_path(self.checkCmd):
            GPS.Console("Messages").write(
                "Error: '%s' is not in the path.\n" % self.checkCmd
            )
            self.gnatCmd = ""
            self.checkCmd = ""
            GPS.Console("Messages").write(
                "Error: Could not initialize the gnatcheck module.\n"
            )
        elif not locate_exec_on_path(self.gnatCmd):
            self.gnatCmd = ""
            GPS.Console("Messages").write(
                "Warning: 'gnat' is not in the path.\n"
                + "The following rules' sections will be missing:\n"
                + "    Warnings     - compiler warnings\n"
                + "    Style_Checks - compiler style checks\n"
                + "    Restrictions - compiler restrictions\n"
            )

    def on_response(self, dialog, response_id):
        if response_id == Gtk.ResponseType.APPLY:
            fname = self.ruleseditor.get_filename()
            if fname:
                self.rules_file = RuleFile(fname, RuleFileKind.LEGACY)

        self.ruleseditor.destroy()
        self.ruleseditor = None

    def edit(self):
        if self.ruleseditor:
            # Already opened
            return

        prev_gnat = self.gnatCmd
        prev_check = self.checkCmd
        self.update_gnat_cmd()

        if self.checkCmd == "":
            return

        # gnat check command changed: we reinitialize the rules list
        if (
            prev_check != self.checkCmd
            or prev_gnat != self.gnatCmd
            or self.rules is None
        ):
            self.rules = get_supported_rules(self.checkCmd, self.gnatCmd)

        # We retrieve the coding standard file from the project
        self.rules_file = self.get_rules_file()

        # Then, according to the rule file kind, we either open the rule editor
        # or open the LKQL rule file in a buffer.
        if self.rules_file.kind == RuleFileKind.LEGACY:
            self.ruleseditor = rulesEditor(self.rules, self.rules_file.file)
            self.ruleseditor.connect("response", self.on_response)
        else:
            GPS.EditorBuffer.get(self.rules_file.file)

    def parse_output(self, msg):
        # gnatcheck sometimes displays incorrectly formatted warnings (not
        # handled by GS correctly then)
        # let's reformat those here:
        # expecting "file.ext:nnn:nnn: msg"
        # receiving "file.ext:nnn:nnn msg"
        category = default_category
        res = re.split("^([^:]*:[0-9]+:[0-9]+): ([^:]+): (.*)$", msg)
        if len(res) > 4:
            category = category_map.get(res[2], default_category)
        else:
            # Detect unknown rules by gnatcheck.
            # Expected format example: gnatcheck: unknown rule : Abort_Statement,
            # ignored (/path/to/coding_standard.rules:1:1)
            res = re.split(r"unknown rule: (\w*), ignored \((.+)[:]([0-9]+):([0-9]+)", msg)
            if len(res) > 3:
                category = category_map['error']
                GPS.Message(
                    category=category,
                    file=GPS.File(res[2]),
                    line=int(res[3]),
                    column=int(res[4]),  # index in python starts at 0
                    text="Unknown rule: " + res[1],
                    show_on_editor_side=True,
                    show_in_locations=True,
                    importance=GPS.Message.Importance.MEDIUM,
                )

        GPS.Locations.parse(msg, category)

        # Aggregate output in self.full_output: CodeFix needs to be looking at
        # the whole output in one go.
        self.full_output += msg + "\n"

    def on_match(self, process, matched, unmatched):
        if unmatched.startswith("\n"):
            GPS.Console("Messages").write(self.msg + unmatched)
            self.parse_output(self.msg)
            self.msg = ""
        self.msg += matched

    def on_exit(self, process, status, remaining_output):
        if self.msg != "":
            GPS.Console("Messages").write(self.msg)
            self.parse_output(self.msg)
            self.msg = ""

        if self.full_output:
            # There is a full output: run CodeFix.
            GPS.Codefix.parse(self.locations_string, self.full_output)

    def on_spawn(self, filestr, project, recursive):
        """
        Spawn gnatcheck.
        Must be called only after we have set the rules file
        """

        self.update_gnat_cmd()

        if self.checkCmd == "":
            GPS.Console("Messages").write("Error: could not find gnatcheck")
            return

        # launch gnat check with specified project
        cmd = [
            self.checkCmd,
            "-P",
            project.file().name("Tools_Server"),
            "-dd",
        ]  # progress

        if self.gnatArgs:
            cmd += self.gnatArgs

        # also analyse subprojects ?
        if recursive:
            cmd.append("-U")

        # define the scenario variables
        scenario = GPS.Project.scenario_variables()
        if scenario is not None:
            for i, j in scenario.items():
                cmd.append("-X%s=%s" % (i, j))

        # now specify the files to check
        cmd.append(filestr)

        if self.rules_file.file is not None:
            if self.rules_file.kind == RuleFileKind.LEGACY:
                cmd.extend(
                    ["-rules", "-from=%s" % self.rules_file.file.name("Tools_Server")]
                )

        # clear the Checks category in the Locations view
        if GPS.Locations.list_categories().count(self.locations_string) > 0:
            GPS.Locations.remove_category(self.locations_string)

        self.msg = ""
        GPS.Process(
            cmd,
            "^.+$",
            on_match=self.on_match,
            on_exit=self.on_exit,
            progress_regexp=r"^ *completed (\d*) out of (\d*) .*$",
            progress_current=1,
            progress_total=2,
            remote_server="Tools_Server",
            show_command=True,
        )

    def internal_spawn(self, filestr, project, recursive=False):
        if GPS.Preference("General-Auto-Save").get():
            # Force, since otherwise we get a modal dialog while within
            # a GPS action, which gtk+ doesn't like
            modified = GPS.Project.root().is_modified(recursive=True)
            GPS.MDI.save_all(force=True)
            if modified:
                GPS.Project.root().recompute()

        self.full_output = ""
        opts = []
        lkql_rule_file = ""
        rules = []
        opts = project.get_attribute_as_list("switches", package="check", index="ada")
        if len(opts) == 0:
            opts = project.get_attribute_as_list(
                "default_switches", package="check", index="ada"
            )
        lkql_rule_file = project.get_attribute_as_string("rule_file", package="check")
        rules = project.get_attribute_as_list("rules", package="check")

        # We need a rules file if no rules are specified in the project,
        # either directly or via a dedicated rules file.
        need_rules_file = not (
            any(["-rules" in opt for opt in opts])
            or lkql_rule_file != ""
            or len(rules) > 0
        )

        if need_rules_file:
            # Display a dialog, but without using run(), since we are
            # running a GS action in the task manager and that would
            # crash GS on some platforms
            selector = RulesSelector(project.name(), self.rules_file.file)

            def on_response(dialog, response_id):
                if response_id == Gtk.ResponseType.OK:
                    self.rules_file = RuleFile(
                        selector.get_file(),
                        RuleFileKind.LEGACY,
                    )
                    dialog.destroy()
                    self.on_spawn(filestr, project, recursive)
                else:
                    dialog.destroy()

            selector.connect("response", on_response)
            selector.show_all()
        else:
            self.on_spawn(filestr, project, recursive)

    def check_project(self, project, recursive=False):
        try:
            self.internal_spawn("", project, recursive)
        except Exception:
            GPS.Console("Messages").write(
                "Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc())
            )

    def check_file(self, file):
        try:
            self.internal_spawn(file.name("Tools_Server"), GPS.Project.root())
        except Exception:
            GPS.Console("Messages").write(
                "Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc())
            )

    def check_files(self, files):
        try:
            filestr = ""
            for f in files:
                filestr += '"""' + f.name("Tools_Server") + '""" '
            self.internal_spawn(filestr, GPS.Project.root())
        except Exception:
            GPS.Console("Messages").write(
                "Unexpected exception in gnatcheck.py:\n%s\n" % (traceback.format_exc())
            )


# Contextual menu for checking files
# The filter does some computation, and caches the result in the context so
# that we do not need to recompute it if the action is executed


class __ContextualMenuData(object):
    pass


def __contextual_menu_filter(context):
    global gnatcheckproc
    data = __ContextualMenuData()
    context.gnatcheck = data

    data.desttype = "none"

    data.file = context.file()
    if data.file:
        data.desttype = "file"
        if data.file.language().lower() != "ada":
            return False

        # Does this file belong to the project tree ?
        return data.file.project(False) is not None

    data.dir = context.directory()
    if data.dir:
        data.desttype = "dir"
        # check this directory contains ada sources
        srcs = GPS.Project.root().sources(True)
        found = False
        data.files = []
        for f in srcs:
            filename = f.path
            if filename.find(data.dir) == 0:
                if f.language().lower() == "ada":
                    data.files.append(f)
                    found = True
        return found

    data.project = context.project()
    if data.project:
        data.desttype = "project"
        srcs = data.project.sources(recursive=False)
        found = False
        data.files = []
        for f in srcs:
            if f.language().lower() == "ada":
                data.files.append(f)
                found = True
        return found

    return False


def __contextual_menu_label(context):
    data = context.gnatcheck
    if data.desttype == "file":
        fmt = "Check Coding standard of <b>{}</b>"
        name = os.path.basename(data.file.path)
    elif data.desttype == "dir":
        fmt = "Check Coding standard of files in <b>{}</b>"
        name = os.path.basename(os.path.dirname(data.dir))
    elif data.desttype == "project":
        fmt = "Check Coding standard of files in <b>{}</b>"
        name = data.project.name()
    else:
        return ""
    return fmt.format(os_utils.display_name(name))


@interactive(
    name="Check Coding Standard",
    contextual=__contextual_menu_label,
    filter=__contextual_menu_filter,
    static_path="Check Coding standard",
)
def on_activate():
    context = GPS.contextual_context()
    data = context.gnatcheck
    global gnatcheckproc
    if data.desttype == "file":
        gnatcheckproc.check_file(data.file)
    elif data.desttype == "project":
        gnatcheckproc.check_project(data.project)
    else:
        gnatcheckproc.check_files(data.files)


def __open_gnatsas_gnatcheck_help(*args):
    link_url = (
        "https://docs.adacore.com/live/wave/gnatsas/html/user_guide/"
        + "project_setup.html#differences-with-standalone-gnatcheck-tool",
    )
    GPS.HTML.browse(link_url)


def gnatsas_warning():
    if gnatsas:
        header = (
            "Running GNATcheck as standalone tool.\n"
            + "The results will not be available in GNAT SAS report"
            + ' and settings from "Analyzer" package will be ignored.\n'
            + "See section: "
        )
        link_name = "Differences with standalone GNATcheck tool"
        tail = " from the GNAT SAS User Guide for more details.\n\n"

        GPS.Console("Messages").write(header, mode="error")
        GPS.Console("Messages").insert_link(link_name, __open_gnatsas_gnatcheck_help)
        GPS.Console("Messages").write(tail, mode="error")


# create the menus instances.
gnatcheckproc = GnatcheckProc()


@interactive(name="gnatcheck root project", category="Coding Standard")
def check_root_project():
    "Check coding standard of the root project"
    gnatcheckproc.check_project(GPS.Project.root())
    gnatsas_warning()


@interactive(name="gnatcheck root project recursive", category="Coding Standard")
def check_root_project_recursive():
    "Check coding standard for the root project and its subprojects"
    gnatcheckproc.check_project(GPS.Project.root(), True)
    gnatsas_warning()


@interactive(name="gnatcheck file", filter="Source editor", category="Coding Standard")
def check_file():
    "Check coding standard of the selected file"
    gnatcheckproc.check_file(GPS.EditorBuffer.get().file())
    gnatsas_warning()


@interactive(name="edit gnatcheck rules", category="Coding Standard")
def edit_gnatcheck_rules():
    "Edit the coding standard file"
    gnatcheckproc.edit()


GPS.parse_xml(
    """
<tool name="GNATcheck" package="Check" index="Ada" override="false"
attribute="Default_Switches">
    <language>Ada</language>
    <switches sections="-rules">
        <check label="process RTL units" switch="-a" line="1"/>
        <check label="debug mode" switch="-d" line="1"/>
        <field label="Coding standard file"
               switch="-from"
               separator="="
               as-file="true"
               line="1"
               section="-rules"
               tip="Warning: this switch is now deprecated.
Use the Check'Rule_File project attribute instead.\n\n
Rules file to use as coding standard."/>
    </switches>
</tool>
<project_attribute package="Check"
    name="Rules"
    editor_page="GNATCheck"
    list="true"
    hide_in="wizard library_wizard properties">
</project_attribute>
  <project_attribute
    name="Rule_File"
    label="Rule File"
    package="Check"
    editor_page="GNATcheck"
    editor_section="Rule File"
    description="LKQL file to use as coding standard.">
    <string type="file"/>
  </project_attribute>"""
)
