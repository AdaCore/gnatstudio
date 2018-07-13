from . import core, core_staging
import GPS
import re
import os
from workflows.promises import ProcessWrapper, join


CAT_BRANCHES = 'BRANCHES'
CAT_TAGS = 'TAGS'

CAN_RENAME = True


@core.register_vcs(name='subversion',
                   default_status=GPS.VCS2.Status.UNTRACKED)
class SVN(core_staging.Emulate_Staging,
          core.File_Based_VCS):

    __re_status = re.compile(
        '^(?P<status>....... .)\s+(?P<rev>\S+)\s+' +
        '(?P<lastcommit>\S+)\s+(?P<author>\S+)\s+(?P<file>.+)$')

    @staticmethod
    def discover_working_dir(file):
        return core.find_admin_directory(file, '.svn')

    def _svn(self, args, block_exit=False, spawn_console=False):
        """
        Execute svn with the given arguments
        """
        return ProcessWrapper(
            ['svn', '--non-interactive'] + args,
            block_exit=block_exit,
            spawn_console=spawn_console,
            directory=self.working_dir.path)

    @core.vcs_action(icon='vcs-cloud-symbolic',
                     name='svn update',
                     menu='/VCS/svn update',
                     after='update section')
    def _update(self):
        p = self._svn(['update'], spawn_console='')
        yield p.wait_until_terminate()

    @core.run_in_background
    def _compute_status(self, all_files, args=[]):
        with self.set_status_for_all_files(all_files) as s:
            list = [self._relpath(arg) for arg in args]
            p = self._svn(
                # -u: Compare with server (slower but more helpful)
                ['status', '-v', '-u'] + list)

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
    def async_commit_staged_files(self, visitor, message):
        for f in self._staged:
            status, version, repo_version = self.get_file_status(f)
            if status & GPS.VCS2.Status.STAGED_ADDED:
                p = self._svn(['add', self._relpath(f.path)], block_exit=True)
                yield p.wait_until_terminate()
            elif status & GPS.VCS2.Status.STAGED_DELETED:
                p = self._svn(['delete', self._relpath(f.path)],
                              block_exit=True)
                yield p.wait_until_terminate()

        yield self._internal_commit_staged_files(
            visitor,
            ['svn', 'commit', '-m', message])

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
                # keep previous commit until we find id of next one
                self.prev = None

            def __call__(self, out_stream, line):
                if line.startswith('--------------------------------------'):
                    if self.current:
                        if self.prev:
                            # Point Prev.parents to current.id
                            self.prev[4] = [self.current[0]]
                            out_stream.emit(self.prev)
                        self.prev = self.current
                    self.current = None
                else:
                    m = _re_log.search(line)
                    if m:
                        self.current = GPS.VCS2.Commit(
                            id=m.group('rev'),
                            author=m.group('author'),
                            date=m.group('date'),
                            subject='',
                            parents=None)
                    elif self.current:
                        if self.current[3]:
                            self.current[3] += '\n'
                        self.current[3] += line   # subject

            def oncompleted(self, out_stream, status):
                if self.prev:
                    out_stream.emit(self.prev)

        p = self._svn(['log', '--non-interactive'] + args)
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

        f = [self._relpath(for_file.path)] if for_file else []
        yield self._log_stream([
            '-rHEAD:1',
            '--limit=%d' % (max_lines + 1),
            '--stop-on-copy'] + f).subscribe(add_log)
        visitor.history_lines(result)

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

    @core.run_in_background
    def async_diff(self, visitor, ref, file):
        p = self._svn(['diff', '-r%s' % ref,
                       self._relpath(file.path) if file else ''])
        status, output = yield p.wait_until_terminate()
        visitor.diff_computed(output)

    @core.run_in_background
    def async_view_file(self, visitor, ref, file):
        p = self._svn(['cat', '-r%s' % ref, self._relpath(file.path)])
        status, output = yield p.wait_until_terminate()
        visitor.file_computed(output)

    @core.run_in_background
    def async_annotations(self, visitor, file):
        r = re.compile(
            "^\s*(?P<rev>\d\S*)"
            "\s+"
            "(?P<author>\S+)"
            "\s+"
            "(?P<date>....-..-..)")
        lines = []
        ids = []
        p = self._svn(['annotate', '-v', self._relpath(file.path)])
        while True:
            line = yield p.wait_line()
            if line is None:
                visitor.annotations(file, 1, ids, lines)
                break

            m = r.search(line)
            if m:
                lines.append('%s %10s r%s' % (
                    m.group('date'),
                    m.group('author')[:10],
                    m.group('rev')))
                ids.append(m.group('rev'))

    def _branches(self, visitor, parent_url):
        """
        A generator that returns the list of branches via `visitor.branches`
        """
        branches = [('trunk', parent_url.endswith('/trunk'), '',
                     os.path.join(parent_url, 'trunk'))]
        base = parent_url + '/branches'
        p = self._svn(['list', base])
        while True:
            line = yield p.wait_line()
            if line is None:
                visitor.branches(
                    CAT_BRANCHES, 'vcs-branch-symbolic',
                    not CAN_RENAME, branches)
                break
            line = line.rstrip('/')
            b = os.path.join(base, line)
            branches.append(GPS.VCS2.Branch(
                name=line, active=b == parent_url, annotation='', id=b))

    def _tags(self, visitor, parent_url):
        """
        A generator that returns the list of tags via `visitor.tags`
        """
        tags = []
        base = parent_url + 'tags'
        p = self._svn(['list', base])
        while True:
            line = yield p.wait_line()
            if line is None:
                visitor.branches(
                    CAT_TAGS, 'vcs-tag-symbolic', not CAN_RENAME, tags)
                break
            line = line.rstrip('/')
            b = os.path.join(base, line)
            tags.append(GPS.VCS2.Branch(
                name=line, active=b == parent_url, annotation='', id=b))

    @core.run_in_background
    def async_branches(self, visitor):
        url = ''

        p = self._svn(['info'])
        while True:
            line = yield p.wait_line()
            if line is None:
                break
            if line.startswith('URL: '):
                url = line[5:]
                break

        if url:
            # Assume the standard 'trunk', 'branches' and 'tags' naming
            parent = url
            while parent:
                parent, tail = os.path.split(parent)
                if tail in ('trunk', 'branches', 'tags'):
                    break

            if parent:
                yield join(self._branches(visitor, parent),
                           self._tags(visitor, parent))

    @core.run_in_background
    def async_action_on_branch(self, visitor, action, category, id, text=''):
        if category in (CAT_BRANCHES, CAT_TAGS):
            if action == GPS.VCS2.Actions.DOUBLE_CLICK and id:
                p = self._svn(['switch', '--ignore-ancestry', id])
                yield p.wait_until_terminate(shof_if_error=True)
            elif action == GPS.VCS2.Actions.TOOLTIP:
                visitor.tooltip(
                    '\nDouble-click to checkout this tag or branch'
                    if id else '')
            elif action == GPS.VCS2.Actions.ADD and not id:
                pass
            elif action == GPS.VCS2.Actions.REMOVE and id:
                pass

    @core.run_in_background
    def async_discard_local_changes(self, files):
        n = [self._relpath(f.path) for f in files]
        yield self._svn(['revert'] + n).wait_until_terminate()

    @core.run_in_background
    def async_checkout(self, visitor, commit):
        p = self._svn(['merge', '-r HEAD:%s' % commit, '.'])
        status, output = yield p.wait_until_terminate()
        if status == 0:
            visitor.success('Reverting successful')

    @core.run_in_background
    def async_checkout_file(self, visitor, commit, file):
        p = self._svn(['up', '-r', commit, self._relpath(file.path)])
        status, output = yield p.wait_until_terminate()
        if status == 0:
            visitor.success('Reverting successful')
