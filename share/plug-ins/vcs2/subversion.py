from . import core, core_staging
import GPS
import re
import os
from workflows.promises import ProcessWrapper


@core.register_vcs(default_status=GPS.VCS2.Status.UNTRACKED)
class SVN(core_staging.Emulate_Staging,
          core.File_Based_VCS):

    __re_status = re.compile(
        '^(?P<status>....... .)\s+(?P<rev>\S+)\s+' +
        '(?P<lastcommit>\S+)\s+(?P<author>\S+)\s+(?P<file>.+)$')

    @staticmethod
    def discover_working_dir(file):
        return core.find_admin_directory(file, '.svn')

    @core.run_in_background
    def _compute_status(self, all_files, args=[]):
        with self.set_status_for_all_files(all_files) as s:

            p = ProcessWrapper(
                ['svn', 'status', '-v',
                 '-u'] +   # Compare with server (slower but more helpful)
                args,
                directory=self.working_dir.path)

            while True:
                line = yield p.wait_line()
                if line is None:
                    break

                m = self.__re_status.search(line)
                if m:
                    f = os.path.join(self.working_dir.path, m.group('file'))
                    rev = m.group('rev')   # current checkout
                    rrev = m.group('lastcommit')  # only if we use '-u'

                    if line[0] == ' ':
                        status = GPS.VCS2.Status.UNMODIFIED
                    elif line[0] == 'A':
                        status = GPS.VCS2.Status.STAGED_ADDED
                    elif line[0] == 'D':
                        status = GPS.VCS2.Status.STAGED_DELETED
                    elif line[0] == 'M':
                        status = GPS.VCS2.Status.MODIFIED
                    elif line[0] == 'C':
                        status = GPS.VCS2.Status.CONFLICT
                    elif line[0] == 'X':
                        status = GPS.VCS2.Status.UNTRACKED
                    elif line[0] == 'I':
                        status = GPS.VCS2.Status.IGNORED
                    elif line[0] == '?':
                        status = GPS.VCS2.Status.UNTRACKED
                    elif line[0] == '!':
                        status = GPS.VCS2.Status.DELETED
                    elif line[0] == '-':
                        status = GPS.VCS2.Status.CONFLICT
                    else:
                        status = 0

                    # Properties
                    if line[1] == 'M':
                        status = status | GPS.VCS2.Status.MODIFIED
                    elif line[1] == 'C':
                        status = status | GPS.VCS2.Status.CONFLICT

                    if line[2] == 'L':
                        status = status | GPS.VCS2.Status.LOCAL_LOCKED

                    if line[5] == 'K':
                        status = status | GPS.VCS2.Status.LOCAL_LOCKED
                    elif line[5] in ('O', 'T'):
                        status = status | GPS.VCS2.Status.LOCKED_BY_OTHER

                    if line[6] == 'C':
                        status = status | GPS.VCS2.Status.CONFLICT

                    if line[7] == '*':   # Only if we use -u
                        status = status | GPS.VCS2.Status.NEEDS_UPDATE

                    s.set_status(GPS.File(f), status, rev, rrev)

    def commit_staged_filed(self, message):
        self._internal_commit_staged_files(['svn', 'commit', '-m', message])
