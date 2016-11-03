from . import core
import GPS
import os
import re
import workflows
from workflows.promises import ProcessWrapper


# Match cvs status output to internal status for GPS
STATUSES = {
    "locally modified": GPS.VCS2.Status.MODIFIED,
    "locally added": GPS.VCS2.Status.STAGED_ADDED,
    "locally removed": GPS.VCS2.Status.DELETED,
    "needs checkout": GPS.VCS2.Status.NEEDS_UPDATE,
    "needs patch": GPS.VCS2.Status.NEEDS_UPDATE,
    "needs merge": GPS.VCS2.Status.MODIFIED,
    "file had conflicts on merge": GPS.VCS2.Status.CONFLICT,
    "unknown": GPS.VCS2.Status.UNTRACKED
}


@core.register_vcs
class CVS(core.VCS):

    def __init__(self, repo):
        self.repo = repo
        self.__re_status = re.compile(
            '^(?:' +
            '(?:cvs status: Examining (?P<dir>.+))|' +
            '(?:File: (?P<file>\S+)\s+Status: (?P<status>.+))|' +
            '(?:\s+Working revision:\s*(?P<rev>[\d.]+).*)|' +
            '(?:\s+Repository revision:\s*(?P<rrev>[\d.]+).*)' +
            ')$')

    @staticmethod
    def discover_repo(file):
        return core.find_admin_directory(file, 'CVS')

    @workflows.run_as_workflow
    def __compute_status(self, all_files, args=[]):
        """
        Run a "cvs status" command with extra args

        :param List(GPS.File) all_files: all files for which a status
           should be set.
        :param List(str) args: extra arguments to 'cvs status'
        """

        with self.set_status_for_all_files(
                all_files, GPS.VCS2.Status.UNTRACKED) as s:

            p = ProcessWrapper(
                ['cvs', '-f', 'status'] + args,
                directory=os.path.join(self.repo, '..'))
            current_file = None
            dir = None
            while True:
                line = yield p.wait_until_match('^.+$')
                if line is None:
                    break

                m = self.__re_status.search(line)
                if m is None:
                    pass
                elif m.group('dir'):
                    dir = m.group('dir')
                elif m.group('file'):
                    if current_file is not None:
                        s.set_status(current_file, status, rev, repo_rev)
                        current_file = None

                    # CVS doesn't show path information when a list of files is
                    # given. However, it seems to query the status in the same
                    # order as on the command line, so we take advantage of
                    # that.

                    f = m.group('file')
                    if dir is not None:
                        current_file = GPS.File(os.path.join(dir, f))
                    elif all_files and all_files[0].path.endswith(f):
                        current_file = all_files[0]
                    if all_files:
                        all_files.pop(0)

                    status = STATUSES.get(
                        m.group('status').lower(), GPS.VCS2.Status.UNMODIFIED)
                    rev = None
                    repo_rev = None
                elif m.group('rev'):
                    rev = m.group('rev')
                elif m.group('rrev'):
                    repo_rev = m.group('rrev')

            if current_file is not None:
                s.set_status(current_file, status, rev, repo_rev)

    def async_fetch_status_for_files(self, files):
        self.__compute_status(
            all_files=files,
            args=[f.path for f in files])

    @workflows.run_as_workflow
    def async_fetch_status_for_project(self, project):
        self.__compute_status(
            all_files=project.sources(recursive=False),
            args=[d for d in project.source_dirs(recursive=False)])

    @workflows.run_as_workflow
    def async_fetch_status_for_all_files(self):
        self.__compute_status(
            all_files=GPS.Project.root().sources(recursive=True))
