from . import core, core_staging
import GPS
import os
import re
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


@core.register_vcs(default_status=GPS.VCS2.Status.UNTRACKED)
class CVS(core_staging.Emulate_Staging,
          core.File_Based_VCS):

    __re_status = re.compile(
        '^(?:' +
        '(?:cvs status: Examining (?P<dir>.+))|' +
        '(?:File: (?P<deleted>no file )?(?P<file>\S+)\s+' +
        'Status: (?P<status>.+))|' +
        '(?:\s+Working revision:\s*(?P<rev>[\d.]+).*)|' +
        '(?:\s+Repository revision:\s*(?P<rrev>[\d.]+).*)' +
        ')$')

    def __cvs(self, args, block_exit=True):
        """
        Execute cvs with the given arguments.

        :param List(str) args: list of arguments
        :returntype: a ProcessWrapper
        """
        return ProcessWrapper(
            ['cvs'] + args,
            block_exit=block_exit,
            directory=self.working_dir.path)

    @staticmethod
    def discover_working_dir(file):
        return core.find_admin_directory(file, 'CVS')

    @core.run_in_background
    def _compute_status(self, all_files, args=[]):
        with self.set_status_for_all_files(all_files) as s:
            p = self.__cvs(['-f', 'status'] + args, block_exit=False)
            current_file = None
            dir = None
            while True:
                line = yield p.wait_line()
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

                    if m.group('deleted'):
                        status = GPS.VCS2.Status.DELETED
                    else:
                        status = STATUSES.get(
                            m.group('status').lower(),
                            GPS.VCS2.Status.UNMODIFIED)
                    rev = None
                    repo_rev = None
                elif m.group('rev'):
                    rev = m.group('rev')
                elif m.group('rrev'):
                    repo_rev = m.group('rrev')

            if current_file is not None:
                s.set_status(current_file, status, rev, repo_rev)

    @core.run_in_background
    def commit_staged_files(self, message):
        for f in self._staged:
            status, version, repo_version = self.get_file_status(f)
            if status & GPS.VCS2.Status.STAGED_ADDED:
                p = self.__cvs(['add', f.path])
                yield p.wait_until_terminate()
            elif status & GPS.VCS2.Status.STAGED_DELETED:
                p = self.__cvs(['remove', f.path])
                yield p.wait_until_terminate()

        self._internal_commit_staged_files(['cvs', 'commit', '-m', message])

    def _build_unique_id(self, rev, file):
        return '%s %s' % (rev, file)

    def _parse_unique_id(self, id):
        return id.split(' ', 1)

    def _log_stream(self, args=[]):
        """
        Run 'cvs log', and return a stream that emits one event for
        each commit message.
        """
        cvs = self
        base = self.working_dir.path

        class line_to_block:
            def __init__(self):
                self.file = ''
                self.in_header = False
                self.revision = ''
                self.current = None
                self.__re_log = re.compile(
                    '^date: (?P<date>[^;]+);\s+author: (?P<author>[^;]+)')

            def emit_previous(self, out_stream):
                if self.previous:
                    out_stream.emit(self.previous)
                    self.previous = None

            def __call__(self, out_stream, line):
                if line.startswith('Working file: '):
                    self.file = line[14:]
                    self.previous = None  # previous commit
                    self.current = None

                elif line.startswith('=========================='):
                    self.emit_previous(out_stream)
                    self.previous = self.current
                    self.current = None
                    self.emit_previous(out_stream)
                    # self.current[3] = subject

                elif line.startswith('--------------'):
                    self.in_header = True
                    self.emit_previous(out_stream)
                    self.previous = self.current
                    self.current = None

                elif self.in_header:
                    if line.startswith('revision '):
                        self.revision = cvs._build_unique_id(
                            line[9:], self.file)
                        if self.previous is not None:
                            self.previous[4] = [self.revision]   # parents

                    else:
                        m = self.__re_log.search(line)
                        if m:
                            self.current = [
                                self.revision,
                                m.group('author'),
                                m.group('date'),
                                '',     # subject
                                None,   # parents
                                [self.file]]  # names

                            self.in_header = False
                            self.subject = ''
                elif self.current:
                    if self.current[3]:
                        self.current[3] += '\n'
                    self.current[3] += line   # subject

            def oncompleted(self, out_stream, status):
                self.emit_previous(out_stream)

        p = ProcessWrapper(
            ['cvs', 'log', '-N'] + args,
            block_exit=False,
            directory=base)
        return p.lines.flatMap(line_to_block())

    @core.run_in_background
    def async_fetch_history(self, visitor):
        result = []

        def add_log(log):
            log[3] = log[3].split('\n', 1)[0]  # first line only
            result.append(log)
        yield self._log_stream([]).subscribe(add_log)
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
            rev, file = self._parse_unique_id(id)
            yield self._log_stream(['-r%s' % rev, file]).subscribe(_emit)
