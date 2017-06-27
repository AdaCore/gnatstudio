"""
The following class is used to emulate staging.

Some VCS systems do not support staging natively, so GPS emulates that.
You only need to derive from this class to get staging emulation.
"""

import GPS
import json
from workflows.promises import ProcessWrapper


class Emulate_Staging(object):

    def __init__(self, *args, **kwargs):
        super(Emulate_Staging, self).__init__(*args, **kwargs)
        self.__load_staged_files()

    def _internal_commit_staged_files(self, visitor, args):
        """
        Execute the command `args` and pass all staged files.

        :param List(str) args: the command and its arguments
        """
        # Need to copy the list, since '_staged' will be changed below
        files = list(self._staged)
        p = ProcessWrapper(
            args + [self._relpath(f.path) for f in files],
            directory=self.working_dir.path)
        (status, output) = yield p.wait_until_terminate()
        if status:
            GPS.Console().write("%s %s" % (" ".join(args), output))
        else:
            self.stage_or_unstage_files(files, stage=False)
            visitor.success('Commit successful')

    def _set_file_status(self, files, status, version="", repo_version=""):
        """Override Ada behavior"""
        if isinstance(files, GPS.File):
            files = set([files])
        elif not isinstance(files, set):
            files = set(files)

        s = files.intersection(self._staged)
        if s:
            # One file from the set, at random
            f = next(iter(s))
            super(Emulate_Staging, self)._set_file_status(
                list(s),
                self._override_status_for_file(f, status),
                version,
                repo_version)

        files.difference_update(s)
        super(Emulate_Staging, self)._set_file_status(
            list(files), status, version, repo_version)

    def __load_staged_files(self):
        """
        For VCS that emulate staging, this loads data that GPS might have
        saved in previous sessions, with the list of staged files.
        """
        try:
            staged = self.working_dir.get_property('staged_vcs')
            self._staged = set(GPS.File(f) for f in json.loads(staged))
        except:
            self._staged = set()   # set of staged files

    def __save_staged_files(self):
        """
        Save some persistent data about which files are staged, so that they
        can be retrieved in later GPS sessions.
        """
        self.working_dir.set_property(
            'staged_vcs', json.dumps([f.path for f in self._staged]),
            persistent=True)

    def stage_or_unstage_files(self, files, stage):
        for f in files:
            status, version, repo_version = self.get_file_status(f)
            if stage:
                self._staged.add(f)
            elif f in self._staged:
                self._staged.remove(f)
            new_status = self._override_status_for_file(f, status)
            if status != new_status:
                self._set_file_status([f], new_status, version, repo_version)

        self.__save_staged_files()

    def _override_status_for_file(self, file, status):
        """
        Return a modified version of `status` (as computed by the VCS) to take
        into account the emulated staging status of the file.

        :param GPS.File file:
        :param GPS.VCS2.Status status:
        :returntype: GPS.VCS2.Status status
        """
        if file in self._staged:
            if status & GPS.VCS2.Status.MODIFIED:
                status |= GPS.VCS2.Status.STAGED_MODIFIED
                status &= ~GPS.VCS2.Status.MODIFIED
            if status & GPS.VCS2.Status.UNTRACKED:
                status |= GPS.VCS2.Status.STAGED_ADDED
                status &= ~GPS.VCS2.Status.UNTRACKED
            if status & GPS.VCS2.Status.DELETED:
                status |= GPS.VCS2.Status.STAGED_DELETED
                status &= ~GPS.VCS2.Status.DELETED
        else:
            if status & GPS.VCS2.Status.STAGED_MODIFIED:
                status |= GPS.VCS2.Status.MODIFIED
                status &= ~GPS.VCS2.Status.STAGED_MODIFIED
            if status & GPS.VCS2.Status.STAGED_ADDED:
                status |= GPS.VCS2.Status.UNTRACKED
                status &= ~GPS.VCS2.Status.STAGED_ADDED
            if status & GPS.VCS2.Status.STAGED_DELETED:
                status |= GPS.VCS2.Status.DELETED
                status &= ~GPS.VCS2.Status.STAGED_DELETED

        return status
