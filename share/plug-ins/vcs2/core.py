"""
Base type to implement support for new VCS engines in GPS
"""

import GPS
import os
import gps_utils
import workflows
from workflows.promises import Promise
import types


GPS.VCS2.Status = gps_utils.enum(
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
            self.set_run_in_background(True)
            promise = workflows.driver(r)
            promise.then(lambda x: self.set_run_in_background(False),
                         lambda x: self.set_run_in_background(False))
        else:
            promise = Promise()
            promise.resolve(r)
        return promise
    return __func


class VCS(GPS.VCS2):
    """
    To create a new engine, extend this class, and then call:

        @core.register_vcs
        class MyVCS(core.VCS):
            pass
    """

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
        self.async_fetch_status_for_all_files()

    def async_fetch_status_for_project(self, project):
        """
        Fetch status information for all files in `project`.
        Use `set_status_for_all_files`.

        :param GPS.File file:
        """
        self.async_fetch_status_for_all_files()

    def async_fetch_status_for_all_files(self):
        """
        Fetch status for all files in the project tree.
        Use `set_status_for_all_files`.
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

    def commit_staged_files(self, message):
        """
        Commit all staged files.
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

            def __enter__(self):
                return self

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
                vcs._set_file_status(file, status, version, repo_version)

            def set_status_for_remaining_files(self, files=set()):
                """
                Set the status for all files in `files` for which no status
                has been set yet.

                :param Set(GPS.File) files:
                """
                if not isinstance(files, set):
                    files = set(files)
                files.difference_update(self._seen)
                vcs._set_file_status(list(files), vcs.default_status)

            def __exit__(self, exc_type=None, exc_val=None, exc_tb=None):
                self.set_status_for_remaining_files(files)
                return False   # do not suppress exceptions

        return _CM()


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

    def async_fetch_status_for_all_files(self):
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


def find_admin_directory(file, basename):
    """
    Starting from the location of `file`, move up the directory tree to
    find a directory named `basename`.
    Used for the implementation of discoved_working_dir

    :return: A str
      The parent directory `basename`, i.e. the root repository
    """
    parent = os.path.expanduser('~')
    dir = os.path.dirname(file.path)
    while dir != '/' and dir != parent:
        d = os.path.join(dir, basename)
        if os.path.isdir(d):
            return os.path.normpath(os.path.join(d, '..'))
        dir = os.path.dirname(dir)
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

    def __init__(self, name, icon='', toolbar='Commits', toolbar_section=''):
        self.name = name
        self.icon = icon
        self.toolbar = toolbar
        self.toolbar_section = toolbar_section

    def __call__(self, func):
        func._vcs2_is_action = self  # Mark for later
        return func

    @staticmethod
    def register_all_vcs_actions(inst):
        """
        Called internally by GPS.
        This makes sure that all actions registered for a VCS class are active
        only when a VCS is in use for the current project.

        :param VCS inst: an instance of the VCS class
        """
        for name, method in inst.__class__.__dict__.iteritems():
            if hasattr(method, "_vcs2_is_action"):
                a = method._vcs2_is_action

                if a.name not in vcs_action._actions:
                    vcs_action._actions.add(a.name)

                    class __Proxy:
                        def __init__(self, method, inst):
                            self.method = run_in_background(method)
                            self.vcs = inst

                        def filter(self, context):
                            return GPS.VCS2.active_vcs() == self.vcs

                        def __call__(self):
                            self.method(GPS.VCS2.active_vcs())

                    p = __Proxy(method, inst)
                    gps_utils.make_interactive(
                        p, name=a.name, category='VCS2',
                        icon=a.icon, filter=p.filter)

                    if a.toolbar:
                        act = GPS.Action(a.name)
                        act.button(toolbar=a.toolbar,
                                   section=a.toolbar_section,
                                   hide=True)
