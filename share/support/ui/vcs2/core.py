"""
Base type to implement support for new VCS engines in GPS
"""

import GPS
import os
import gps_utils
import workflows
import time
from workflows.promises import Promise
import types
import difflib
import io
import platform


GPS.VCS2.Status = gps_utils.enum(
        NO_VCS=0,
        UNMODIFIED=2**0,
        MODIFIED=2**1,
        STAGED_MODIFIED=2**2,
        STAGED_ADDED=2**3,
        DELETED=2**4,
        STAGED_DELETED=2**5,
        STAGED_RENAMED=2**6,
        STAGED_COPIED=2**7,
        UNTRACKED=2**8,
        IGNORED=2**9,
        CONFLICT=2**10,
        LOCAL_LOCKED=2**11,
        LOCKED_BY_OTHER=2**12,
        NEEDS_UPDATE=2**13)
# Valid statuses for files (they can be combined)

GPS.VCS2.Actions = gps_utils.enum(
        DOUBLE_CLICK=0,
        TOOLTIP=1,
        ADD=2,
        REMOVE=3,
        RENAME=4)


Traverse_Limit_Pref = GPS.Preference(":VCS/Traverse-Limit")
Traverse_Limit_Pref.create(
    "Max project depth",
    "integer",
    "How much directories should GPS traverse when looking for " +
    "VCS root counting from project file directory.", 99, 0)


class _Branch(list):
    """
    A convenience class to create the tuples to pass to
    `GPS.VCS2_Task_Visitor.branches`
    """

    def __init__(self, name, active, annotation, id):
        """
        :param str name: name of the branch, as displayed in the Branches view
        :param bool active: whether the branch is "active" (for instance the
            current branch, or a persistent tag,... An active branch is
            highlighted in the Branches view.
        :param str annotation: extra information to display on the right in
            the Branches view. For instance, the number of commits that are
            in this branch but not upstream.
        :param str id: a unique id for the branch. This is used in python
            callbacks, but the exact value of the id is irrelevant outside
            of the plug-in that creates this id. It could for instance be
            the result of `json.dumps` on python data.
        """
        list.__init__(self, (name, active, annotation, id))


class _Commit(list):
    """
    A convenience wrapper to create the tuples to pass to
    `GPS.VCS2_Task_Visitor.history_lines`
    """

    Kind = gps_utils.enum(
        HEAD=0,     # current working dir
        LOCAL=1,    # a local branch name exists for this commit
        REMOTE=2,   # a remote branch name exists for this commit
        TAG=3)      # a tag exists for this commit

    Flags = gps_utils.enum(
        UNPUSHED=2**1,     # an unpushed local commit
        UNCOMMITTED=2**2)  # uncommitted local changes

    def __init__(self, id, author, date, subject, parents, names=None,
                 flags=0):
        """
        :param str id: the unique id for the commit
        :param str author: the author of the commit
        :param str date: the date of the commit
        :param str subject: the first line of the commit message
        :param List(str) parents: a list of commit ids, the parents
           of the commit. There are multiple parents when this is
           a merge commit.
        :param List((str, GPS.VCS2.Commit.Kind)) names: a list of tag names
           or branch names associated with this commit. Each of the items
           indicates the type of the name
        :param GPS.VCS2.Commit.Flags: a combination of flags, which
           influence of the commit is represented in the History view.
        """
        list.__init__(self, (id, author, date, subject, parents, names, flags))

    @property
    def subject(self):
        return self[3]

    @subject.setter
    def set_subject(self, str):
        self[3] = str


GPS.VCS2.Branch = _Branch
GPS.VCS2.Commit = _Commit


def run_in_background(func):
    """
    A decorator to be applied to a method of VCS (below), which monitors
    whether background processing is being done. This is used to avoid
    spawning multiple commands in the background in parallel, in particular
    because the first one could already be computed information required by
    the next one (for instance, with git, a user need status for file1.adb
    and file2.adb -- but since git always compute the status for all files,
    the second command is not needed).

    Use this instead of workflows.run_as_workflow, as in::

        class MyVCS(vcs2.core.VCS):

            @vcs2.core.run_in_background
            def async_fetch_status_for_files(self):
                pass

    :return: a function that when executed returns a promise that is resolved
      to the return of `func`. Until this promise is resolved (in the
      background), the VCS engine is marked as busy, and no other command will
      be started.
    """

    def __func(self, *args, **kwargs):
        r = func(self, *args, **kwargs)
        if isinstance(r, types.GeneratorType):
            if isinstance(self, Extension):
                vcs = self.base
            else:
                vcs = self
            vcs.set_run_in_background(True)
            promise = workflows.driver(r)
            promise.then(lambda x: vcs.set_run_in_background(False),
                         lambda x: vcs.set_run_in_background(False))
        else:
            promise = Promise()
            promise.resolve(r)
        return promise
    return __func


class Profile:
    """
    A Context that runs the function inside the profiler, and display
    the result in the log.
    """

    def __init__(self, time_only=False):
        """
        :param bool time_only: if true, only display the time it topok to
           execute, not the whole profile
        """
        import cProfile
        self.time_only = time_only
        if time_only:
            self.start = time.time()
        else:
            self.c = cProfile.Profile()

    def __enter__(self):
        if not self.time_only:
            self.c.enable()
        return self

    def __exit__(self, exc_type=None, exc_val=None, exc_tb=None):
        if self.time_only:
            GPS.Logger("VCS2").log(
                "Total time: %ss" % (time.time() - self.start, ))
        else:
            import pstats
            import StringIO
            self.c.disable()
            s = StringIO.StringIO()
            ps = pstats.Stats(self.c, stream=s).sort_stats('cumulative')
            ps.print_stats()
            GPS.Logger("VCS2").log(s.getvalue())


class Extension(object):
    """
    A class similar to core.VCS, which is used to decorate an existing VCS
    engine to add more features.
    For instance, it is used to add Gerrit or github support.
    A decorator has the same primitive operations as a VCS engine, which can
    be overridden. However, each VCS engine is responsible for using its
    decorators when appropriate.
    Such extensions must be registered with core.register_extension().
    """

    def __init__(self, base_vcs):
        self.base = base_vcs

    def applies(self):
        """
        Check if self should be applied to its base VCS system.
        """
        return False


class VCS(GPS.VCS2):
    """
    To create a new engine, extend this class, and then call:

        @core.register_vcs
        class MyVCS(core.VCS):
            pass
    """

    _class_extensions = []  # a list of classes derived from Extension

    #######################
    # Overridable methods #
    #######################

    def __init__(self, working_dir, default_status):
        """
        Instances are created in `register` below. If you need additional
        parameters, they need to be given to `register.
        When __init__ is called, `self` is not fully setup and in particular
        you cannot call any of the functions exported by GPS. Do this from
        `setup` instead.

        :param GPS.File working_dir: the location of the working directory,
           computed from `discover_working_dir`
        :param GPS.VCS2.Status: the default assumed status of files.
           See `register_vcs`
        """
        self.working_dir = working_dir
        self.default_status = default_status
        self._extensions = []   # the decorators that apply to self

        # Check which decorators apply
        for d in self._class_extensions:
            inst = d(base_vcs=self)
            if inst.applies():
                GPS.Logger("VCS2").log(
                    "Extension %s applied to %s (%s)" % (
                        inst, self, working_dir))
                self._extensions.append(inst)

    def setup(self):
        """
        Called after `self` has been constructed (via __init__) and all
        functions exported by GPS are available.
        In particular, this can be used to override how statuses are displayed
        via `GPS.VCS2._override_status_display`.
        """
        vcs_action.register_all_vcs_actions(self)

    @staticmethod
    def discover_working_dir(file):
        """
        Starting from file, check whether it could belong to a working
        directory for the engine. Often implemented using
        `find_admin_directory`, or perhaps using an environment
        variable.

        :param GPS.File file:
        :return: a string
        """
        return ''

    def async_fetch_status_for_files(self, files):
        """
        Fetch status information for `file`.
        Use `set_status_for_all_files`.

        :param List[GPS.File] files:
        """
        self.async_fetch_status_for_all_files(from_user=False)

    def async_fetch_status_for_project(self, project):
        """
        Fetch status information for all files in `project`.
        Use `set_status_for_all_files`.

        :param GPS.File file:
        """
        self.async_fetch_status_for_all_files(from_user=False)

    def async_fetch_status_for_all_files(self, from_user):
        """
        Fetch status for all files in the project tree.
        Use `set_status_for_all_files`.

        :param bool from_user: True if this was called as a result of the
            user pressing the 'Reload' button in the VCS views.
        """
        pass

    def stage_or_unstage_files(self, files, stage):
        """
        Mark all the files in the list to be part of the next commit (if
        `stage` is True), or not part of the next commit (if `stage` is
        False).
        Some VCS systems support this natively (git), while for others it
        needs to be emulated.
        Extend the vcs2.core_staging.Emulate_Staging class to emulate.

        :param List(GPS.File) files: The list of files to stage or unstage.
        :param bool stage: whether to stage or unstage.
        """

    def make_file_writable(self, file, writable):
        """
        You can override the method named `make_file_writable` if you need
        a special operation to make a file writable on the disk.

        This must be SYNCHRONOUS (no background operation).

        :param GPS.File file: the file to make writable
        :param bool writable: whether the file should be made writable or
            read-only.
        :return: if it returns True, any needed operation is assumed to have
            been performed (synchronously). If False, the default behavior is
            applied, i.e. simply change permissions on the disk.
        """
        return False

    def async_discard_local_changes(self, files):
        """
        Revert the local changes for all of the files in the list.
        This can be run asynchronously.

        :param [GPS.File] files: the list of files
        """
        GPS.Console().write("Revert not supported for %s" % (self.name, ))

    def async_commit_staged_files(self, visitor, message):
        """
        Commit all staged files.
        The cached status of files is automatically refreshed on exit.

        :param GPS.VCS2_Task_Visitor visitor: the object used to report
           success via `visitor.success`.
        :param str message: the commit message
        """

    def async_fetch_history(self, visitor, filter):
        """
        Fetch history for the whole repository asynchronously.
        For each line in the history, should call `self._add_log_line`

        :param GPS.VCS2_Task_Visitor visitor: the object used to report
           when new lines have been pared for the history.
        :param List filter: A list of various filters to apply. This list
           is currently defined as:
              [lines               : int,
               file                : GPS.File,
               filter              : str,
               current_branch_only : bool,
               branch_commits      : bool]
           where `lines` is the number of lines that will be displayed
           (returning more is useless), `file` is set if the log should be for
           a specific file, `filter` is a string that is interpreted by the
           VCS system, `current_branch_only` is set if a single branch
           should be examined (as opposed to all branches) and
           `branch_commits` is true if only commits related to branching
           points should be returned.
        """

    def async_fetch_commit_details(self, ids, visitor):
        """
        Fetch the details for each of the commits in the list.
        These details are returned asynchronously to GPS by calling
        `visitor.set_details`.

        :param List(str) ids: the list of commits for which we want the
          details.
        :param GPS.VCS2_Task_Visitor visitor: the object used to
          report the details.
        """

    def async_diff(self, visitor, ref, file):
        """
        Compute a diff.

        :param GPS.VCS2_Task.Visitor visitor: the object used to
           report the diff, via its 'diff_computed` method.
        :param str ref: the ref to which we want to compare. This is
           typically the id of a commit (as returned from
           `async_fetch_history`, although it can also be the name of a
           branch, or "HEAD" to indicate the last commit done on the
           current branch.
        :param GPS.File file: the file for which we want a diff. This is
            set to None to get a full repository diff.
        """

    def async_view_file(self, visitor, ref, file):
        """
        Show the full contents of the file for the given revision.

        :param GPS.VCS2_Task.Visitor visitor: the object used to
           report the diff, via its 'file_computed` method.
        :param str ref: the ref to which we want to compare. This is
           typically the id of a commit (as returned from
           `async_fetch_history`, although it can also be the name of a
           branch, or "HEAD" to indicate the last commit done on the
           current branch.
        :param GPS.File file: the file for which we want a diff.
        """

    def async_annotations(self, visitor, file):
        """
        Compute the information to display on the side of editors for
        the given file. This information should include last commit date,
        author, commit id,...

        :param GPS.VCS2_Task_Visitor visitor: the object used to report
           the information, via its `annotation` method.
        :param GPS.File file: the file for which the information should be
           computed
        """

    def async_branches(self, visitor):
        """
        Retrieve the list of branches, tags, ... available for self.

        :param GPS.VCS2_Task_Visitor visitor: the object used to report
           the information, via its `branches` method.
        """

    def async_action_on_branch(self, visitor, action, category, id, text=''):
        """
        React to a double-click action in the Branches view.

        :param GPS.VCS2_Task_Visitor visitor: the object used to report
           asynchronously.
           If action is ACTION_TOOLTIP, use `visitor.tooltip`.
        :param GPS.VCS2.Action action: the action to perform
        :param str category: the upper-cased category, i.e. the first
           parameter to `visitor.branches` in the call to `async_branches`.
        :param str id: the id of the specific line that was selected.
        :param str text: the new name, when action is ACTION_RENAME
        """

    ############
    # Services #
    ############

    def set_status_for_all_files(self, files=set()):
        """
        A proxy that lets you set statuses of individual files, and on
        exit automatically set the status of remaining files to unmodified::

            with self.set_status_for_all_files(project.sources()) as s:
                s.set_status(file1, ...)
                s.set_status(file2, ...)
            # on exit, automatically set status of remaining files

        You can also use the returned value as a standard object:

            s = self.set_status_for_all_files()
            s.set_status('file1.adb', ...)
            # does nothing when you are done, unless you call
            s.set_status_for_remaining_files(['file1.adb', 'file2.adb',...])

        The default status comes from the call to `register_vcs`.

        This function takes into account emulated staging: when a VCS does not
        natively support staging (like git does), GPS emulates it by saving
        some data across session. This function takes into account this saved
        data and modifies the status as needed.

        :param Set(GPS.File): the set of files to update. This parameter is
           only used when using this function as a context manager (the 'with'
           statement in python).
        """

        vcs = self

        class _CM(object):
            def __init__(self):
                self._seen = set()
                self._cache = {}    # (status,version,repo_version) -> [File]

            def __enter__(self):
                return self

            @property
            def files_with_explicit_status(self):
                """
                Return the set of files for which an explicit status was set
                """
                return self._seen

            def set_status(
                    self, file,
                    status,
                    version="",
                    repo_version=""):
                """
                Set the status for one file
                :param GPS.File file:
                :param GPS.VCS2.Status status:
                :param GPS.VCS2.Attributes attributes:
                :param str version:
                :param str repo_version:
                """
                self._seen.add(file)
                self._cache.setdefault(
                    (status, version, repo_version), []).append(file)

            def set_status_for_remaining_files(self, files=set()):
                """
                Set the status for all files in `files` for which no status
                has been set yet.

                :param set(GPS.File)|list(GPS.File) files:
                """
                GPS.Logger("VCS2").log("Emit file statuses")
                for s, s_files in self._cache.iteritems():
                    vcs._set_file_status(s_files, s[0], s[1], s[2])
                GPS.Logger("VCS2").log("Done emit file statuses")

                to_set = []
                for f in files:
                    if f not in self._seen:
                        to_set.append(f)
                vcs._set_file_status(to_set, vcs.default_status)
                GPS.Logger("VCS2").log("Done emit default statuses")

            def __exit__(self, exc_type=None, exc_val=None, exc_tb=None):
                self.set_status_for_remaining_files(files)
                return False   # do not suppress exceptions

        return _CM()

    def _relpath(self, path):
        """
        Return a relative filepath to path from the working dir.

        :param str path:
        """
        relpath = os.path.relpath(path, self.working_dir.path)

        if platform.system() == 'Windows':
            #  Use posix path for cygwin tools arguments
            return relpath.replace('\\', '/')
        else:
            return relpath

    @classmethod
    def register_extension(klass, extension):
        klass._class_extensions.append(extension)

    def extensions(self, name, *args, **kwargs):
        """
        Execute all extension's method, if they exists.
        Typically, a method that executes in the background will do
        something like::

            def method(self, ...):
                def _internal():
                    ...
                yield join(_internal(), *self.extensions('method', ...))

        to execute the extensions

        :returntype: a list of generators, one for each method that is
           executing in the background.
        """
        result = []
        for ext in self._extensions:
            m = getattr(ext, name, None)
            if m is not None:
                gen = m(*args, **kwargs)
                if isinstance(gen, types.GeneratorType):
                    result.append(gen)
        return result


class File_Based_VCS(VCS):
    """
    Abstract base class for file-based vcs systems.
    """

    def _compute_status(self, all_files, args=[]):
        """
        Run a "status" command with extra args

        :param List(GPS.File) all_files: all files for which a status
           should be set.
        :param List(str) args: extra arguments to 'cvs/svn/... status'
        """

    def async_fetch_status_for_files(self, files):
        self._compute_status(
            all_files=files,
            args=[f.path for f in files])

    def async_fetch_status_for_project(self, project):
        self._compute_status(
            all_files=project.sources(recursive=False),
            args=[d for d in project.source_dirs(recursive=False)])

    def async_fetch_status_for_all_files(self, from_user):
        self._compute_status([])  # all files


class register_vcs:
    """
    A decorator to register a new VCS engine
    :param str name: the name of the engine, as used in project properties
    :param default_status: the VCS status to use for files not specifically
       set from "status". See `set_status_for_all_files`. This is also the
       status applied by GPS for files not in the cache yet. This value has
       a significant impact on the initial loading of the status for all
       files.
    :param args: passed to the class constructor
    :param kwargs: pass to the class constructor
    """

    def __init__(self, default_status, name="", *args, **kwargs):
        self.default_status = default_status
        self.name = name
        self.args = args
        self.kwargs = kwargs

    def __call__(self, klass):
        GPS.VCS2._register(
            self.name or klass.__name__,
            construct=lambda working_dir: klass(
                working_dir, self.default_status, *self.args, **self.kwargs),
            default_status=self.default_status,
            discover_working_dir=klass.discover_working_dir)
        return klass


def find_admin_directory(file, basename, allow_file=False):
    """
    Starting from the location of `file`, move up the directory tree to
    find a directory named `basename`.
    Used for the implementation of discoved_working_dir

    :param bool allow_file: if true, search for files with the given name,
      not just directories.
    :return: A str
      The parent directory `basename`, i.e. the root repository
    """
    limit = os.path.dirname(GPS.Project.root().file().path)
    for step in range(-1, Traverse_Limit_Pref.get()):
        limit = os.path.dirname(limit)

    parent = os.path.expanduser('~')
    prev = file.path
    dir = os.path.dirname(prev)
    while dir not in [prev, parent, limit]:
        d = os.path.join(dir, basename)
        if os.path.isdir(d) or (allow_file and os.path.isfile(d)):
            return os.path.normpath(os.path.join(d, '..'))
        prev = dir
        dir = os.path.dirname(prev)
    return ""


class vcs_action:
    """
    A decorator to create actions associated with a VCS.
    These actions only exist while at least one instance of the VCS is in
    use in the project, and are automatically unregistered otherwise::

        @core.register_vcs(...)
        class MyVCS(core.VCS):

            @core.vcs_action(...)
            def _mymethod(self):
                pass   # standard method, or generator with yield statements

    :param func: the function to execute for this action. It receives the
       instance of klass as a parameter. This should thus in general be
       a method of the class.
    :param str name: name of the action.
    :param klass: The action is only enabled when the selected VCS is
       an instance of klass.
    :param str toolbar: if set, this action will be added to the local
       toolbar of the corresponding view.
    :param str toolbar_section: what part of the toolbar this should be
       added to.
    """

    _actions = set()  # all registered actions

    def __init__(self, name, icon='', toolbar='', toolbar_section='',
                 menu='', after=''):
        self.name = name
        self.icon = icon
        self.menu = menu
        self.after = after
        self.toolbar = toolbar
        self.toolbar_section = toolbar_section

    def __call__(self, func):
        func._vcs2_is_action = self  # Mark for later
        return func

    @staticmethod
    def create_action(method, vcs_name, extension_class=None):
        """
        Register a GPS action.
        Nothing is done if the action has already been registered for another
        instance of the same VCS kind.

        :param Func method: the function to call.
           It receives either the instance of the active VCS or the instance
           of the extension, depending on whether extension_name is specified.
           This must be a method that was decorated with @vcs_action.
        :param str vcs_name: the name of the VCS system to which the action
           applies. The action is only enabled when this is the current VCS
           (any instance of that VCS, in case for instance the user has
           multiple git working directories in a project.
        :param type extension_class: the class of the VCS extension to which
           this action applies. When specified, the parameter given to the
           method is the instance of the VCS extension, not the VCS itself.
        """

        action = method._vcs2_is_action
        if action.name in vcs_action._actions:
            return

        vcs_action._actions.add(action.name)

        class __Proxy:
            def __init__(self, method):
                self.method = run_in_background(method)

            def filter(self, context):
                return (GPS.VCS2.active_vcs() and
                        GPS.VCS2.active_vcs().name == vcs_name)

            def __call__(self):
                v = GPS.VCS2.active_vcs()
                if extension_class is None:
                    self.method(v)
                else:
                    for e in v._extensions:
                        if e.__class__ == extension_class:
                            self.method(e)
                            break

        p = __Proxy(method)
        gps_utils.make_interactive(
            p, description=method.__doc__,
            name=action.name, category='VCS2', menu=action.menu,
            after=action.after, icon=action.icon, filter=p.filter)

        if action.toolbar:
            act = GPS.Action(action.name)
            act.button(toolbar=action.toolbar,
                       section=action.toolbar_section,
                       hide=True)

    @staticmethod
    def register_all_vcs_actions(vcs):
        """
        Called internally by GPS.
        This makes sure that all actions registered for a VCS class are active
        only when a VCS is in use for the current project.

        :param VCS vcs: an instance of the VCS class
        """
        for name, meth in vcs.__class__.__dict__.iteritems():
            if hasattr(meth, "_vcs2_is_action"):
                vcs_action.create_action(meth, vcs.name, None)

        for d in vcs._extensions:
            for name, meth in d.__class__.__dict__.iteritems():
                if hasattr(meth, "_vcs2_is_action"):
                    vcs_action.create_action(meth, vcs.name, d.__class__)


@staticmethod
def _diff(left_file, right_file, result_file):
    """
    Compare files line by line and save into result file.
    Result is the same as for "diff --normal left right".

        :param GPS.File left_file: first file to compare
        :param GPS.File right_file: second file to compare
        :param GPS.File result_file: file with result
    """
    f = io.open(left_file.path, "rb")
    left = f.readlines()
    f.close()
    f = io.open(right_file.path, "rb")
    right = f.readlines()
    f.close()
    diff = difflib.SequenceMatcher(isjunk=None, a=left, b=right)
    codes = diff.get_opcodes()
    out = []
    for code in codes:
        if code[0] == "replace":
            op = "c"
        elif code[0] == "insert":
            op = "a"
        elif code[0] == "delete":
            op = "d"
        else:
            op = None

        if op:
            out.append("%s%s%s\n" % (code[2] if code[1] + 1 >= code[2] else
                                     "{},{}".format(code[1] + 1, code[2]),
                                     op,
                                     code[4] if code[3] + 1 >= code[4] else
                                     "{},{}".format(code[3] + 1, code[4])))
            for x in range(code[1], code[2]):
                out.append("< %s" % (left[x]))
            for x in range(code[3], code[4]):
                out.append("> %s" % (right[x]))

    f = io.open(result_file.path, "wb")
    f.writelines(out)
    f.close()


GPS.VCS2._diff = _diff
