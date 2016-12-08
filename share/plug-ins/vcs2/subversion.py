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
                block_exit=False,
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

    @core.run_in_background
    def commit_staged_files(self, message):
        for f in self._staged:
            status, version, repo_version = self.get_file_status(f)
            if status & GPS.VCS2.Status.STAGED_ADDED:
                p = ProcessWrapper(['svn', 'add', f.path])
                yield p.wait_until_terminate()
            elif status & GPS.VCS2.Status.STAGED_DELETED:
                p = ProcessWrapper(['svn', 'delete', f.path])
                yield p.wait_until_terminate()

        self._internal_commit_staged_files(['svn', 'commit', '-m', message])

    def _log_stream(self, args=[]):
        """
        Run 'svn log' and return a stream that emits one event for each commit.
        """

        _re_log = re.compile(
            '^r(?P<rev>\d+)' + '\s\|\s' +
            '(?P<author>[^\|]+)' + '\s\|\s' +
            '(?P<date>[^|]+)')

        class line_to_block:
            def __init__(self):
                self.current = None

            def __call__(self, out_stream, line):
                if line.startswith('--------------------------------------'):
                    if self.current:
                        out_stream.emit(self.current)
                    self.current = None
                else:
                    m = _re_log.search(line)
                    if m:
                        rev = int(m.group('rev'))
                        if rev == 1:
                            parents = None
                        else:
                            parents = [str(rev - 1)]

                        self.current = [m.group('rev'),
                                        m.group('author'),
                                        m.group('date'),
                                        '',    # subject
                                        parents,
                                        None]  # names
                    elif self.current:
                        if self.current[3]:
                            self.current[3] += '\n'
                        self.current[3] += line   # subject

        p = ProcessWrapper(
            ['svn', 'log', '--non-interactive'] + args,
            block_exit=False,
            directory=self.working_dir.path)
        return p.lines.flatMap(line_to_block())

    @core.run_in_background
    def async_fetch_history(self, visitor, filter):
        max_lines = filter[0]
        for_file = filter[1]
        pattern = filter[2]
        result = []

        def add_log(log):
            log[3] = log[3].split('\n', 1)[0]  # first line only
            if (len(result) <= max_lines and
                    (not pattern or pattern in log[3])):
                result.append(log)

        yield self._log_stream([
            '-rHEAD:1',
            '--stop-on-copy',
            for_file.path if for_file else ''
        ]).subscribe(add_log)
        visitor.add_lines(result)

    @core.run_in_background
    def async_fetch_commit_details(self, ids, visitor):
        def _emit(log):
            visitor.set_details(
                log[0],  # id
                'Revision r%s\nAuthor: %s\nDate: %s' % (
                    log[0], log[1], log[2]),
                '\n%s\n' % log[3])

        for id in ids:
            yield self._log_stream(
                ['-r%s' % id, '-v',
                 '--diff' if len(ids) == 1 else '']
                ).subscribe(_emit)
