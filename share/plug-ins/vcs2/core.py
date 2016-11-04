"""
Base type to implement support for new VCS engines in GPS
"""

import GPS
import os
import gps_utils
import workflows
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
    """

    def __func(self, *args, **kwargs):
        r = func(self, *args, **kwargs)
        if isinstance(r, types.GeneratorType):
            self.set_run_in_background(True)
            promise = workflows.driver(r)
            promise.then(lambda x: self.set_run_in_background(False),
                         lambda x: self.set_run_in_background(False))
    return workflows.run_as_workflow(__func)


class VCS(GPS.VCS2):
    """
    To create a new engine, extend this class, and then call:

        @core.register_vcs
        class MyVCS(core.VCS):
            pass
    """

    def __init__(self, repo):
        """
        Instances are created in `register` below. If you need additional
        parameters, they need to be given to `register.
        When __init__ is called, `self` is not fully setup and in particular
        you cannot call any of the functions exported by GPS. Do this from
        `setup` instead.

        :param str repo: the location of the repo, computed from
          `discover_repo`
        """
        self.repo = repo

    def setup(self):
        """
        Called after `self` has been constructed (via __init__) and all
        functions exported by GPS are available.
        In particular, this can be used to override how statuses are displayed
        via `GPS.VCS2._override_status_display`
        """

    @staticmethod
    def discover_repo(file):
        """
        Starting from file, check whether it could belong to a working
        directory for the engine. Often implemented using
        `find_admin_directory`, or perhaps using an environment
        variable.

        :param GPS.File file:
        :return: a string
        """

    def async_fetch_status_for_files(self, files):
        """
        Fetch status information for `file`.
        Use `set_status_for_all_files`.

        :param GPS.VCS2 repo: the specific repository
        :param List[GPS.File] files:
        """
        self.async_fetch_status_for_all_files()

    def async_fetch_status_for_project(self, project):
        """
        Fetch status information for all files in `project`.
        Use `set_status_for_all_files`.

        :param GPS.VCS2 repo: the specific repository
        :param GPS.File file:
        """
        self.async_fetch_status_for_all_files()

    def async_fetch_status_for_all_files(self):
        """
        Fetch status for all files in the project tree.
        Use `set_status_for_all_files`.

        :param GPS.VCS2 repo: the specific repository
        """
        pass

    def set_status_for_all_files(self, files, default_status):
        """
        A proxy that lets you set statuses of individual files, and on
        exit automatically set the status of remaining files to unmodified::

            with self.set_status_for_all_files(project.sources()) as s:
                s.set_status(file1, ...)
                s.set_status(file2, ...)
            # on exit, automatically set status of remainingg files

        You can also use the returned value as a standard object:

            s = self.set_status_for_all_files()
            s.set_status(file1, ...)
            # does nothing when you are done, unless you call
            s.__exit__()

        :param GPS.VCS2 repo: the specific repository
        :param List(GPS.File): the list of files to update
        :param GPS.VCS2.Status default_status: the default status for all
           files for which `set_status` wasn't called.
        """

        vcs = self

        class _CM(object):
            def __init__(self, files):
                """
                :param List(GPS.File) files:
                """
                self._files = set(files)

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
                self._files.discard(file)
                vcs._set_file_status(file, status, version, repo_version)

            def __exit__(self, exc_type=None, exc_val=None, exc_tb=None):
                for f in self._files:
                    vcs._set_file_status(f, default_status, "", "")

                return False   # do not suppress exceptions

        return _CM(files)


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
        self._compute_status(
            all_files=GPS.Project.root().sources(recursive=True))


def register_vcs(klass, name="", *args, **kwargs):
    """
    A decorator to register a new VCS engine
    :param str name: the name of the engine, as used in project properties
    :param args: passed to the class constructor
    :param kwargs: pass to the class constructor
    """
    GPS.VCS2._register(
        name or klass.__name__,
        construct=lambda repo: klass(repo, *args, **kwargs),
        discover_repo=klass.discover_repo)


def find_admin_directory(file, basename):
    """
    Starting from the location of `file`, move up the directory tree to
    find a directory named `basename`.
    """
    parent = os.path.expanduser('~')
    dir = os.path.dirname(file.path)
    while dir != '/' and dir != parent:
        d = os.path.join(dir, basename)
        if os.path.isdir(d):
            return d
        dir = os.path.dirname(dir)
    return ""
