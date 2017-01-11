import GPS
from . import core
import os
import re
from workflows.promises import ProcessWrapper, join
import datetime
import gps_utils


@core.register_vcs(default_status=GPS.VCS2.Status.UNMODIFIED)
class Git(core.VCS):

    @staticmethod
    def discover_working_dir(file):
        return core.find_admin_directory(file, '.git')

    def __init__(self, *args, **kwargs):
        super(Git, self).__init__(*args, **kwargs)

        self._non_default_files = None
        # Files with a non-default status

    def _git(self, args, block_exit=False, **kwargs):
        """
        Return git with the given arguments
        :param List(str) args: git arguments
        :param bool block_exit: if True, GPS won't exit while this process
            is running.
        :returntype: a ProcessWrapper
        """
        return ProcessWrapper(
            ['git', '--no-pager'] + args,
            block_exit=block_exit,
            directory=self.working_dir.path,
            **kwargs)

    def __git_ls_tree(self, all_files):
        """
        Compute all files under version control
        :param list all_files: will be modified to include the list of files
        """
        def on_line(line):
            all_files.append(
                GPS.File(os.path.join(self.working_dir.path, line)))
        p = self._git(['ls-tree', '-r', 'HEAD', '--name-only'])
        yield p.lines.subscribe(on_line)   # wait until p terminates

    def __git_status(self, s):
        """
        Run and parse "git status"
        :param s: the result of calling self.set_status_for_all_files
        """
        def on_line(line):
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

                # Filter some obvious files to speed things up
                if line[-3:] != '.o' and line[-5:] != '.ali':
                    s.set_status(
                        GPS.File(
                            os.path.join(self.working_dir.path, line[3:])),
                        status)

        p = self._git(['status', '--porcelain', '--ignored'])
        yield p.lines.subscribe(on_line)   # wait until p terminates

    def async_fetch_status_for_files(self, files):
        self.async_fetch_status_for_all_files(
            from_user=False, extra_files=files)

    @core.run_in_background
    def async_fetch_status_for_all_files(self, from_user, extra_files=[]):
        """
        :param List(GPS.File) extra_files: files for which we need to
           set the status eventually
        """

        s = self.set_status_for_all_files()
        files = set(extra_files)

        # Do we need to reset the "ls-tree" cache ? After the initial
        # loading, this list no longer changes without also impacting the
        # output of "git status", so we do not need to execute it again.
        if from_user or self._non_default_files is None:
            all_files = []   # faster to update than a set
            yield join(self.__git_ls_tree(all_files), self.__git_status(s))
            self._non_default_files = s.files_with_explicit_status
            files.update(all_files)

        else:
            # Reuse caches: we do not need to recompute the full list of files
            # for git, since this will not change without also changing the
            # output of "git status". We also do not reset the default status
            # for all the files not in the git status output: that might be a
            # slow operation that is blocking GPS. Instead, we only reset the
            # default status for files that used to be in "git status" (for
            # instance modified files), and are no longer there (either after
            # a "reset" or a "commit").

            yield self.__git_status(s)
            nondefault = s.files_with_explicit_status
            now_default = self._non_default_files.difference(nondefault)
            self._non_default_files = nondefault
            for f in now_default:
                s.set_status(f, self.default_status)

        s.set_status_for_remaining_files(files)

    @core.run_in_background
    def __action_then_update_status(self, params, files=[]):
        """
        :param List(str) params: the "git ..." action to perform
        :param List(GPS.File) files: list of files
        """
        p = self._git(params + [f.path for f in files], block_exit=True)
        (status, output) = yield p.wait_until_terminate()
        if status:
            GPS.Console().write("git %s: %s" % (" ".join(params), output))
        else:
            # update statuses
            yield self.async_fetch_status_for_all_files(from_user=False)

    def stage_or_unstage_files(self, files, stage):
        self.__action_then_update_status(['add' if stage else 'reset'], files)

    @core.run_in_background
    def commit_staged_files(self, message):
        yield self.__action_then_update_status(['commit', '-m', message])
        yield GPS.Hook('vcs_commit_done').run(self)

    @core.vcs_action(icon='git-commit-amend-symbolic',
                     name='git amend previous commit',
                     toolbar='Commits', toolbar_section='commits')
    def _commit_amend(self):
        """
        Commit all staged files and add these to the previous commit.
        """
        # ??? Should do nothing if the previous commit has been pushed
        # already.
        yield self.__action_then_update_status(
            ['commit', '--amend', '--reuse-message=HEAD'])
        GPS.Hook('vcs_commit_done').run(self)

    @core.vcs_action(icon='vcs-pull-symbolic',
                     name='git pull rebase',
                     menu='/VCS/Pull & rebase',
                     after='update section')
    def _pull_rebase(self):
        p = self._git(['pull', '--rebase'], spawn_console='')
        yield p.wait_until_terminate()

    @core.vcs_action(icon='vcs-pull-symbolic',
                     name='git pull',
                     menu='/VCS/Pull',
                     after='update section')
    def _pull(self):
        p = self._git(['pull'], spawn_console='')
        yield p.wait_until_terminate()

    @core.run_in_background
    def async_fetch_history(self, visitor, filter):
        max_lines = filter[0]
        for_file = filter[1]
        pattern = filter[2]
        current_branch_only = filter[3]
        branch_commits_only = filter[4]

        p = self._git(
            ['log',
             # use tformat to get final newline
             '--pretty=tformat:%H@@%P@@%an@@%d@@%cD@@%s',
             '--branches' if not current_branch_only else '',
             '--tags' if not current_branch_only else '',
             '--remotes' if not current_branch_only else '',
             '--topo-order',  # children before parents
             '--grep=%s' % pattern if pattern else '',
             '--max-count=%d' % max_lines if not branch_commits_only else '',
             '%s' % for_file.path if for_file else ''])

        children = {}   # number of children for each sha1
        result = []
        count = 0

        while True:
            line = yield p.wait_line()
            if line is None or '@@' not in line:
                GPS.Logger("GIT").log("finished git-status")
                break

            id, parents, author, branches, date, subject = line.split('@@')
            parents = parents.split()
            branches = None if not branches else branches.split(',')
            current = (id, author, date, subject, parents, branches)

            if branch_commits_only:
                for pa in parents:
                    children[pa] = children.setdefault(pa, 0) + 1

                # Count only relevant commits
                if (len(parents) > 1 or
                        branches is not None or
                        id not in children or
                        children[id] > 1):
                    count += 1

            result.append(current)
            if count >= max_lines:
                break

        GPS.Logger("GIT").log(
            "done parsing git-log (%s lines)" % (len(result), ))
        visitor.add_lines(result)

    @core.run_in_background
    def async_fetch_commit_details(self, ids, visitor):
        p = self._git(
            ['show',
             '-p' if len(ids) == 1 else '--name-only',
             '--stat' if len(ids) == 1 else '',
             '--notes',   # show notes
             '--pretty=fuller'] + ids)
        id = ""
        message = []
        header = []
        in_header = False

        def _emit():
            if id:
                visitor.set_details(
                    id, '\n'.join(header), '\n'.join(message))

        while True:
            line = yield p.wait_line()
            if line is None:
                _emit()
                break

            if line.startswith('commit '):
                _emit()
                id = line[7:]
                message = []
                header = [line]
                in_header = True

            elif in_header:
                if not line:
                    in_header = False
                    message = ['']
                else:
                    header.append(line)

            else:
                message.append(line)

    @core.run_in_background
    def async_view_file(self, visitor, ref, file):
        f = os.path.relpath(file.path, self.working_dir.path)
        p = self._git(['show', '%s:%s' % (ref, f)])
        status, output = yield p.wait_until_terminate()
        visitor.file_computed(output)

    @core.run_in_background
    def async_diff(self, visitor, ref, file):
        p = self._git(
            ['diff', '--no-prefix',
             ref, '--', file.path if file else ''])
        status, output = yield p.wait_until_terminate()
        if status == 0:
            visitor.diff_computed(output)
        else:
            GPS.Logger("GIT").log("Error computing diff: %s" % output)

    @core.run_in_background
    def async_annotations(self, visitor, file):
        info = {}   # for each commit id, the annotation
        current_id = None
        first_line = 1
        lines = []
        ids = []

        p = self._git(['blame', '--porcelain', file.path])
        while True:
            line = yield p.wait_line()
            if line is None:
                break

            if current_id is None:
                current_id = line.split(' ', 1)[0]

            elif line[0] == '\t':
                # The line of code, which we ignore
                lines.append(info[current_id])
                ids.append(current_id)
                current_id = None

            elif line.startswith('author '):
                info[current_id] = line[7:17]  # at most 10 chars

            elif line.startswith('committer-time '):
                d = datetime.datetime.fromtimestamp(
                    int(line[15:])).strftime('%Y%m%d')
                info[current_id] = '%s %10s %s' % (
                    d, info[current_id], current_id[0:7])

        visitor.annotations(file, first_line, ids, lines)

    @core.run_in_background
    def async_branches(self, visitor):
        def __branches():
            branches = []
            remotes = []
            r = re.compile(
                "^(?P<current>\*)?\s+"
                "(?P<name>\S+)"
                "\s+"
                "(?P<id>[a-z0-9]+)"
                "\s+"
                "(\[(?P<tracking>.*\]))?")
            emblem_r = re.compile(
                "^(?P<tracking>.*):\s"
                "(ahead (?P<ahead>\d+),?)?\s*"
                "(behind (?P<behind>\d+))?")

            p = self._git(['branch', '-a', '--list', '--no-color', '-vv'])
            while True:
                line = yield p.wait_line()
                if line is None:
                    visitor.branches(
                        'branches', 'vcs-branch-symbolic', branches)
                    visitor.branches('remotes', 'vcs-cloud-symbolic', remotes)
                    break
                m = r.search(line)
                if m:
                    n = m.group('name')
                    emblem = []
                    m2 = emblem_r.search(m.group('tracking') or '')
                    if m2:
                        n = '%s (%s)' % (n, m2.group('tracking'))
                        if m2.group('ahead'):
                            emblem.append("%s%s%s%s" % (
                                chr(226), chr(134), chr(145),
                                m2.group('ahead')))
                        if m2.group('behind'):
                            emblem.append("%s%s%s%s" % (
                                chr(226), chr(134), chr(147),
                                m2.group('behind')))

                    emblem = ' '.join(emblem)

                    if n.startswith('remotes/'):
                        remotes.append(
                            (n[8:],
                             m.group('current') is not None,
                             emblem,
                             m.group('name')))
                    else:
                        branches.append(
                            (n,
                             m.group('current') is not None,
                             emblem,
                             m.group('name')))

        def __tags():
            p = self._git(['tag'])
            tags = []
            while True:
                line = yield p.wait_line()
                if line is None:
                    visitor.branches('tags', 'vcs-tag-symbolic', tags)
                    break
                tags.append((line, False, '', line))

        def __stashes():
            p = self._git(['stash', 'list'])
            stashes = []
            while True:
                line = yield p.wait_line()
                if line is None:
                    visitor.branches('stashes', 'vcs-stash-symbolic', stashes)
                    break
                name, branch, descr = line.split(':', 3)
                stashes.append(('%s: %s' % (name, descr), False, branch, name))

        a = yield join(__branches(), __tags(), __stashes())

    @core.run_in_background
    def async_select_branch(self, id):
        if id.startswith('stash@'):
            p = self._git(['stash', 'apply', id])
        else:
            p = self._git(['checkout', id])

        status, output = yield p.wait_until_terminate()
        if status != 0:
            GPS.Console().write(output)

    @core.run_in_background
    def async_discard_local_changes(self, files):
        n = [f.path for f in files]
        yield self._git(['reset'] + n).wait_until_terminate()
        yield self._git(['checkout'] + n).wait_until_terminate()
