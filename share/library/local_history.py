"""Local history for files

This script provides a local history for files: every time a file is saved,
it is also committed in a local RCS directory, which can later be used to
easily revert to a previous version.
Compared to the standard undo feature in GPS, this provides a persistent
undo across GPS sessions.

You must install RCS. On Unix systems, this is generally available by
default. On Windows, this is available through the cygwin environment.
If RCS is not detected on your PATH, this module will do nothing.

A new contextual menu is shown for files that have a local history. This
menu allows you to view the diff between the current version of the file
and the version at the selected time, or to revert to a specific version.

Revert doesn't ask for confirmation! But if you have saved the file before
the revert, the current version is in the local revision history, and can
therefore be reverted.
"""

###########################################################################
# No user customization below this line
############################################################################

from GPS import Console, Contextual, EditorBuffer, File, Hook, Logger, \
    Preference, Process, Vdiff, XMLViewer, current_context
import gps_utils
import os
import shutil
import datetime
import traceback
import time
import re

Preference("Plugins/local_history/rcsdir").create(
    "Local RCS dir", "string",
    """Name of the local directory created to store history locally.
One such directory will be created in each object directory of the project
and its subprojects""",
    ".gpsrcs")

Preference("Plugins/local_history/maxdays").create(
    "Max age", "integer",
    """Keep revisions for that many days at most""",
    2, 0, 1000)

Preference("Plugins/local_history/maxrevisions").create(
    "Max revisions", "integer",
    """Maximal number of revisions to keep""",
    200, 0, 10000)

Preference("Plugins/local_history/diff_switches").create(
    "Diff switches", "string",
    """Additional switches to pass to rcsdiff. In particular, this can be used
to specify your preferred format for diff""",
    "-u")

Preference("Plugins/local_history/when_no_prj").create(
    "When no project", "boolean",
    """Have a local history for files outside of a project.
In such a case, the local history will be put in a local_rcs_dir subdirectory
of the directory that contains the file. When the file belongs to a project,
the local history goes to the object directory of the project.""",
    False)


class LocalHistory:

    """This class provides access to the local history of a file"""

    def __init__(self, file):
        """Create a new instance of LocalHistory.
           File must be an instance of GPS.File"""

        self.file = file.path
        project = file.project(default_to_root=False)
        if project:
            dir = project.object_dirs(recursive=False)[0]
        elif Preference("Plugins/local_history/when_no_prj").get():
            dir = os.path.dirname(self.file)
        else:
            return

        self.rcs_dir = os.path.join(
            dir, Preference("Plugins/local_history/rcsdir").get())
        self.rcs_file = os.path.join(self.rcs_dir,
                                     os.path.basename(self.file)) + ",v"

        # convert all \ to / for Cygwin toolset, note that forward
        # slashes are handled by the native Win32 API. So it is safe to
        # do that on Windows.

        self.file = self.file.replace("\\", "/")
        self.rcs_file = self.rcs_file.replace("\\", "/")

    def get_revisions(self):
        """Extract all revisions and associated dates.
           Result is a list of tuples: (revision_number, date), where
           revision is the RCS revision number less the "1." prefix.
           First in the list is the most recent revision."""
        if not self.rcs_dir:
            return
        try:
            f = file(self.rcs_file)
            result = []

            previous = None

            for line in f.readlines():
                if line.startswith("log"):
                    break
                if line.startswith("date\t") and previous is not None:
                    date = line.split()[1]
                    result.append((int(previous[2:]), date[:-1]))
                previous = line

            f.close()
            return result
        except:
            return None

    def on_lock_taken(self, proc, exit_status, output):
        """
        Called when we have finished taking the lock on the local RCS file
        """
        pwd = os.getcwd()
        os.chdir(self.rcs_dir)
        # Specify our own date, so that the date associated with the revision
        # is the one when the file was saved. Otherwise, it defaults to the
        # last modification of the date, and this might dependent on the local
        # time zone
        proc = Process(
            "ci -d" +
            datetime.datetime.now().strftime("%Y/%m/%d\\ %H:%M:%S") +
            " " + os.path.basename(self.file))
        os.chdir(pwd)
        proc.send(".\n")

    def add_to_history(self):
        """Expand the local history for file, to include the current version"""
        if not self.rcs_dir:
            Logger("LocalHist").log("No RCS dir for file " + self.file)
            return
        if not os.path.isdir(self.rcs_dir):
            os.makedirs(self.rcs_dir)
            Logger("LocalHist").log("creating directory %s" % self.rcs_dir)

        shutil.copy2(self.file, self.rcs_dir)
        if os.path.isfile(self.rcs_file):
            Process("rcs -l " + self.rcs_file, on_exit=self.on_lock_taken)
        else:
            self.on_lock_taken(None, 0, "")

    def cleanup_history(self):
        """Remove the older revision histories for self"""
        if not self.rcs_dir:
            return

        max_days = Preference("Plugins/local_history/maxdays").get()
        older = datetime.datetime.now() - datetime.timedelta(days=max_days)
        older = older.strftime("%Y.%m.%d.%H.%M.%S")

        revisions = self.get_revisions()
        max_revisions = Preference("Plugins/local_history/maxrevisions").get()
        if revisions:
            version = max(0, revisions[0][0] - max_revisions)
            for r in revisions:
                if r[1] < older:
                    version = max(version, r[0])
                    break

            if version >= 1:
                Logger("LocalHist").log(
                    "Truncating file %s to revision %s" % (
                        self.rcs_file, version))
                proc = Process("rcs -o:1.%s %s" % (version, self.rcs_file))
                proc.wait()

    def local_checkout(self, revision):
        """Do a local checkout of file at given revision in the RCS directory.
           Return the name of the checked out file"""
        if self.rcs_dir and os.path.isdir(self.rcs_dir):
            try:
                os.unlink(os.path.join(self.rcs_dir,
                                       os.path.basename(self.file)))
            except:
                pass

            pwd = os.getcwd()
            os.chdir(self.rcs_dir)
            proc = Process("co -r" + revision + " " + self.rcs_file)
            os.chdir(pwd)
            if proc.wait() == 0:
                return os.path.join(self.rcs_dir, os.path.basename(self.file))
        return None

    def revert_file(self, revision):
        """Revert file to a local history revision"""
        if not self.rcs_dir:
            return
        Logger("LocalHist").log("revert " + self.file + " to " + revision)
        local = self.local_checkout(revision)
        if local:
            shutil.copymode(self.file, local)
            shutil.move(local, self.file)
            EditorBuffer.get(File(self.file), force=True)

    def diff_file(self, revision, file_ext="old"):
        """Compare the current version of file with the given revision.
           The referenced file will have a name ending with file_ext"""
        if not self.rcs_dir:
            return
        local = self.local_checkout(revision)
        file_ext = file_ext.replace("/", ".").replace(":", "-")
        local2 = os.path.basename(local) + " " + file_ext
        local2 = os.path.join(self.rcs_dir, local2)
        shutil.move(local, local2)
        Vdiff.create(File(local2), File(self.file))
        try:
            os.chmod(local2, 0777)
            os.unlink(local2)
        except:
            pass

    def show_diff(self, revision, date):
        """Show, in a console, the diff between the current version and
           revision"""
        if self.rcs_dir and os.path.isdir(self.rcs_dir):
            pwd = os.getcwd()
            os.chdir(os.path.dirname(self.file))
            diff_switches = Preference(
                "Plugins/local_history/diff_switches").get()
            proc = Process("rcsdiff " + diff_switches +
                           " -r" + revision + " " + self.rcs_file)
            diff = proc.get_result()
            os.chdir(pwd)
            Console("Local History").clear()
            Console("Local History").write("Local history at " + date + "\n")
            Console("Local History").write(diff)

    def has_local_history(self):
        """Whether there is local history information for self"""
        return os.path.isfile(self.rcs_file)

    def on_select_xml_node(self, node_name, attrs, value):
        if node_name == "revision":
            attr = dict()
            for a in re.findall("""(\\w+)=['"](.*?)['"]\B""", attrs):
                attr[a[0]] = a[1]
            self.show_diff(attr["name"], attr["date"])

    def create_xml_node(self, node_name, attrs, value):
        attr = dict()
        for a in re.findall("""(\\w+)=['"](.*?)['"]\B""", attrs):
            attr[a[0]] = a[1]
        if node_name == "revision":
            return ["[<b>" + attr["date"] + "</b>] " + attr["name"]]

    def view_all(self, revisions, dates):
        """View all revisions of self in a graphical tree"""
        if self.rcs_dir and os.path.isdir(self.rcs_dir):
            pwd = os.getcwd()
            os.chdir(os.path.dirname(self.file))

            xml = "<local_history>\n"
            for index, r in enumerate(revisions):
                xml = xml + "  <revision name='" + r + "' date='" \
                    + dates[index] + "' />"

            xml = xml + "</local_history>"

            view = XMLViewer("History",
                             columns=1,
                             on_select=self.on_select_xml_node,
                             parser=self.create_xml_node)
            view.parse_string(xml)
            os.chdir(pwd)


def has_RCS_on_path():
    """True if RCS was found on the PATH"""
    for path in os.getenv("PATH").split(os.pathsep):
        if os.path.isfile(os.path.join(path, "ci")) \
                or os.path.isfile(os.path.join(path, "ci.exe")):
            return True
    return False


def on_file_saved(hook, file):
    """Called when a file has been saved"""
    try:
        Logger("LocalHist").log("saving file in local history: " + file.path)
        hist = LocalHistory(file)
        hist.add_to_history()
        hist.cleanup_history()
    except:
        Logger("LocalHist").log(
            "Unexpected exception " + traceback.format_exc())


def contextual_filter(context):
    try:
        hist = LocalHistory(context.file())
        return hist.has_local_history()
    except:
        return False


def contextual_factory(context):
    try:
        hist = LocalHistory(context.file())
        revisions = hist.get_revisions()

        # Save in the context the result of parsing the file. This factory is
        # used for multiple contextual menus, so this saves some processing.
        # Part of this parsing is also needed when performing the action.

        try:
            return context.revisions_menu
        except:
            context.revisions = ["1.%s" % a[0] for a in revisions]
            result = []
            for a in revisions:
                date = datetime.datetime(
                    *(time.strptime(a[1], "%Y.%m.%d.%H.%M.%S")[0:6]))
                result.append(date.strftime("%Y-%m-%d/%H:%M:%S"))
            context.revisions_menu = result
            return context.revisions_menu
    except:
        return None


def on_revert(context, choice, choice_index):
    hist = LocalHistory(context.file())
    hist.revert_file(context.revisions[choice_index])


def on_diff(context, choice, choice_index):
    hist = LocalHistory(context.file())
    hist.diff_file(context.revisions[choice_index],
                   context.revisions_menu[choice_index])


def on_patch(context, choice, choice_index):
    hist = LocalHistory(context.file())
    hist.show_diff(context.revisions[choice_index],
                   context.revisions_menu[choice_index])


def on_view_all():
    context = current_context()
    hist = LocalHistory(context.file())
    hist.view_all(context.revisions, context.revisions_menu)


def register_module(hook):
    """Activate this local history module if RCS is found on the path"""

    if has_RCS_on_path():
        Hook("file_saved").add(on_file_saved, last=True)
        Contextual("Local History Revert to").create_dynamic(
            factory=contextual_factory,
            on_activate=on_revert,
            label="Local History/Revert To",
            filter=contextual_filter)
        Contextual("Local History Diff").create_dynamic(
            factory=contextual_factory,
            on_activate=on_diff,
            label="Local History/Diff",
            filter=contextual_filter)
        Contextual("Local History Patch").create_dynamic(
            factory=contextual_factory,
            on_activate=on_patch,
            label="Local History/Show Patch",
            filter=contextual_filter)

        gps_utils.make_interactive(
            callback=on_view_all,
            filter=contextual_filter,
            contextual='Local History/View')

Hook("gps_started").add(register_module)
