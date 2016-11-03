import GPS
from . import core
import os
import types
import workflows
from workflows.promises import ProcessWrapper


@core.register_vcs
class Git(core.VCS):

    def __init__(self, repo):
        self.repo = repo
        self.all_files = None

    def setup(self):
        self._override_status_display(
            GPS.VCS2.Status.STAGED_MODIFIED,
            'modified (staged)', 'gps-emblem-vcs-modified')

    @staticmethod
    def discover_repo(file):
        return core.find_admin_directory(file, '.git')

    def __compute_all_files(self):
        """
        Compute all files under version control
        """
        if self.all_files is None:
            self.all_files = set()
            p = ProcessWrapper(
                ['git', 'ls-tree', '-r', 'HEAD', '--name-only'],
                directory=os.path.join(self.repo, '..'))
            while True:
                line = yield p.wait_until_match('^.*$')
                if line is None:
                    break
                self.all_files.add(GPS.File(line))

    @workflows.run_as_workflow
    def async_fetch_status_for_all_files(self):
        yield self.__compute_all_files()

        with self.set_status_for_all_files(self.all_files) as s:
            p = ProcessWrapper(
                ['git', 'status', '--porcelain', '--ignored'],
                directory=os.path.join(self.repo, '..'))

            while True:
                line = yield p.wait_until_match('^.+$')
                if line is None:
                    break

                if len(line) > 3:
                    if line[0:2] in ('DD', 'AU', 'UD', 'UA', 'DU', 'AA', 'UU'):
                        status = GPS.VCS2.Status.CONFLICT
                    else:
                        status = 0

                        if line[0] == 'M':
                            status = GPS.VCS2.Status.STAGED_MODIFIED
                        elif line[0] == 'A':
                            status = GPS.VCS2.Status.STAGED_ADDED
                        elif line[0] == 'D':
                            status = GPS.VCS2.Status.STAGED_DELETED
                        elif line[0] == 'R':
                            status = GPS.VCS2.Status.STAGED_RENAMED
                        elif line[0] == 'C':
                            status = GPS.VCS2.Status.STAGED_COPIED
                        elif line[0] == '?':
                            status = GPS.VCS2.Status.UNTRACKED
                        elif line[0] == '!':
                            status = GPS.VCS2.Status.IGNORED

                        if line[1] == 'M':
                            status = status | GPS.VCS2.Status.MODIFIED
                        elif line[1] == 'D':
                            status = status | GPS.VCS2.Status.DELETED

                    s.set_status(GPS.File(line[3:]), status)
