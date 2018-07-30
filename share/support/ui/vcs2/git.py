import GPS
from . import core
import os
import re
import workflows
from workflows.promises import ProcessWrapper, join, Promise
import datetime


CAT_BRANCHES = 'BRANCHES'
CAT_REMOTES = 'REMOTES'
CAT_TAGS = 'TAGS'
CAT_STASHES = 'STASHES'
CAT_WORKTREES = 'WORKTREES'
CAT_SUBMODULES = 'SUBMODULES'

CAN_RENAME = True

LOCAL_CHANGES_ID = 'localchanges'
# A dummy id to represent local changes in the History view

_version = None
# Git version


@core.register_vcs(default_status=GPS.VCS2.Status.NO_VCS)
class Git(core.VCS):

    @staticmethod
    def discover_working_dir(file):
        # When using "git worktree", ".git" is a file, not a directory
        return core.find_admin_directory(file, '.git', allow_file=True)

    def __init__(self, *args, **kwargs):
        super(Git, self).__init__(*args, **kwargs)

        self._non_default_files = None
        # Files with a non-default status
        self.__set_git_version()

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

        if _version < [1, 7, 2]:
            ignored = []
        else:
            ignored = ['--ignored']

        p = self._git(['status', '--porcelain'] + ignored)
        yield p.lines.subscribe(on_line)   # wait until p terminates

    @workflows.run_as_workflow
    def __set_git_version(self):
        """Find GIT version."""
        global _version
        if not _version:
            p = self._git(['--version'])
            status, output = yield p.wait_until_terminate()
            # The version is the first three dot separated digits of the
            # third word.
            # Examples of git --version output:
            #   git version 2.11.0 (Apple Git-81)
            #   git version 2.11.0
            #   git version 1.7.8.msysgit
            version = output.split(' ')[2].split('.')[0:3]
            _version = [int(x) for x in version]

    def async_fetch_status_for_files(self, files):
        self.async_fetch_status_for_all_files(
            from_user=False,
            extra_files=files)

    @core.run_in_background
    def async_fetch_status_for_all_files(self, from_user, extra_files=[]):
        """
        :param List(GPS.File) extra_files: files for which we need to
           set the status eventually
        """

        s = self.set_status_for_all_files()

        # Do we need to reset the "ls-tree" cache ? After the initial
        # loading, this list no longer changes without also impacting the
        # output of "git status", so we do not need to execute it again.
        if from_user or self._non_default_files is None:
            all_files = []   # faster to update than a set
            yield join(self.__git_ls_tree(all_files), self.__git_status(s))
            self._non_default_files = s.files_with_explicit_status
            now_default = set(all_files).difference(self._non_default_files)
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
            s.set_status(f, GPS.VCS2.Status.UNMODIFIED)

        s.set_status_for_remaining_files()

    @core.run_in_background
    def stage_or_unstage_files(self, files, stage):
        p = self._git(['add' if stage else 'reset'] + [f.path for f in files],
                      block_exit=True)
        yield p.wait_until_terminate(show_if_error=True)
        yield self.async_fetch_status_for_all_files(from_user=False)

    @core.run_in_background
    def async_commit_staged_files(self, visitor, message):
        p = self._git(['commit', '-m', message], block_exit=True)
        status, _ = yield p.wait_until_terminate(show_if_error=True)
        if status == 0:
            visitor.success('Commit successful')

    @core.vcs_action(icon='git-commit-amend-symbolic',
                     name='git amend previous commit',
                     toolbar='Commits', toolbar_section='commits')
    def _commit_amend(self):
        """
        Commit all staged files and add these to the previous commit.
        """
        # ??? Should do nothing if the previous commit has been pushed
        # already.
        p = self._git(['commit', '--amend', '--reuse-message=HEAD'],
                      block_exit=True)
        status, _ = yield p.wait_until_terminate(show_if_error=True)
        if status == 0:
            GPS.MDI.information_popup(
                'Commit successful', 'github-commit-symbolic')
            yield self.async_fetch_status_for_all_files(from_user=False)

    @core.vcs_action(icon='vcs-pull-symbolic',
                     name='git pull rebase',
                     menu='/VCS/Pull & rebase',
                     after='update section')
    def _pull_rebase(self):
        p = self._git(['pull', '--rebase'], spawn_console='')
        status, _ = yield p.wait_until_terminate()
        if status == 0:
            GPS.MDI.information_popup('Pulled', 'github-commit-symbolic')

    @core.vcs_action(icon='vcs-pull-symbolic',
                     name='git pull',
                     menu='/VCS/Pull',
                     after='update section')
    def _pull(self):
        p = self._git(['pull'], spawn_console='')
        status, _ = yield p.wait_until_terminate()
        if status == 0:
            GPS.MDI.information_popup('Pulled', 'github-commit-symbolic')

    @core.vcs_action(icon='vcs-cloud-symbolic',
                     name='git push',
                     menu='/VCS/Push',
                     after='server section')
    def _push(self):
        """
        Push all changes to the remote repository.
        """
        p = self._git(['push'], spawn_console='')
        status, _ = yield p.wait_until_terminate()
        if status == 0:
            GPS.MDI.information_popup('Pushed', 'vcs-cloud-symbolic')

    def _unpushed_local_changes(self):
        """
        Compute the list of changes that either haven't been pushed
        to the remote yet, or haven't been pulled from the remote.

        :returntype set: will contain the sha1 of the commits
        """
        unpushed = set()
        p = self._git(['cherry'])
        while True:
            line = yield p.wait_line()
            if line is None:
                break
            unpushed.add(line[2:])
        yield unpushed

    def _has_local_changes(self):
        """
        Check whether there is any uncomitted change.
        """
        p = self._git(['diff', '--quiet', 'HEAD', '--'])
        status, _ = yield p.wait_until_terminate()
        yield status != 0

    @core.run_in_background
    def async_fetch_history(self, visitor, filter):
        # Compute, in parallel, needed pieces of information
        (unpushed, has_local) = yield join(
            self._unpushed_local_changes(),
            self._has_local_changes())

        # Then fetch the history

        max_lines = filter[0]
        for_file = filter[1]
        pattern = filter[2]
        current_branch_only = filter[3]
        branch_commits_only = filter[4]

        filter_switch = ''
        if pattern:
            if pattern.startswith('author:'):
                filter_switch = '--author=%s' % pattern[7:]
            elif pattern.startswith('code:'):
                filter_switch = '-S=%s' % pattern[5:]
            else:
                filter_switch = '--grep=%s' % pattern

        git_cmd = [
            'log',
            # use tformat to get final newline
            '--pretty=tformat:%H@@%P@@%an@@%D@@%cD@@%s',
            '--branches' if not current_branch_only else '',
            '--tags' if not current_branch_only else '',
            '--remotes' if not current_branch_only else '']
        if for_file:
            git_cmd.append('--follow')
        git_cmd += [
            '--topo-order',  # children before parents
            filter_switch,
            '--max-count=%d' % max_lines if not branch_commits_only else '',
            '%s' % for_file.path if for_file else '']
        p = self._git(git_cmd)

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

            has_head = None

            flags = 0
            if id in unpushed:
                flags |= GPS.VCS2.Commit.Flags.UNPUSHED

            if branches is None:
                branch_descr = None
            else:
                branch_descr = []
                for b in branches:
                    b = b.strip()

                    # ??? How do we detect other remotes
                    if b.startswith('origin/'):
                        f = (b, GPS.VCS2.Commit.Kind.REMOTE)
                    elif b.startswith("HEAD"):
                        has_head = id
                        f = (b, GPS.VCS2.Commit.Kind.HEAD)
                    elif b.startswith("tag: "):
                        f = (b[5:], GPS.VCS2.Commit.Kind.TAG)
                    else:
                        f = (b, GPS.VCS2.Commit.Kind.LOCAL)

                    branch_descr.append(f)

            # Append a dummy entry if we have local changes, and
            # we have the HEAD
            if has_head is not None and has_local:
                result.insert(0, GPS.VCS2.Commit(
                    LOCAL_CHANGES_ID,
                    '',
                    '',
                    '<uncommitted changes>',
                    parents=[has_head],
                    flags=GPS.VCS2.Commit.Flags.UNCOMMITTED |
                    GPS.VCS2.Commit.Flags.UNPUSHED))

            current = GPS.VCS2.Commit(
                id, author, date, subject, parents, branch_descr, flags=flags)

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
        visitor.history_lines(result)

    @core.run_in_background
    def async_fetch_commit_details(self, ids, visitor):
        if LOCAL_CHANGES_ID in ids:
            # If there are unstaged changes, show those, otherwise
            # show the staged changes

            p = self._git(['diff', '--exit-code'])
            status, output = yield p.wait_until_terminate()
            if status != 0:
                visitor.set_details(
                    LOCAL_CHANGES_ID,
                    'Unstaged local changes',
                    output)

            p = self._git(['diff', '--cached', '--exit-code'])
            status, output = yield p.wait_until_terminate()
            if status != 0:
                visitor.set_details(
                    LOCAL_CHANGES_ID,
                    'Staged local changes',
                    output)
            return

        # If there is a single commit, show the full patch (use
        # --stat to also show the list of files).
        # Otherwise, show the list of modified files
        #
        # We use a custom format to be able to display the refnames, which
        # are not displayed otherwise by git.

        format = ('commit %H%n'
                  'Author:     %aN <%ae>%n'
                  'AuthorDate: %aD%n'
                  'Commit:     %cN <%ce>%n'
                  'CommitDate: %cD%n'
                  'Refnames:  %d%n%n'
                  '%B')

        p = self._git(
            ['show',
             '-p' if len(ids) == 1 else '--name-only',
             '--stat' if len(ids) == 1 else '',
             '--notes',   # show notes
             '--pretty=format:%s' % format] + ids)
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

    def _branches(self, visitor):
        """
        A generator that returns via `visitor.branches` the list of all
        known branches
        """
        branches = []
        remotes = []
        r = re.compile(
            "^(?P<current>\*)?\s+"
            "(?P<name>[^(]\S+|\([^)]+\))"   # "(HEAD detached at ...)"
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
                    CAT_BRANCHES, 'vcs-branch-symbolic', CAN_RENAME, branches)
                visitor.branches(
                    CAT_REMOTES, 'vcs-cloud-symbolic', not CAN_RENAME, remotes)
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
                        GPS.VCS2.Branch(
                            name=n[8:],
                            active=m.group('current') is not None,
                            annotation=emblem,
                            id=m.group('name')))
                else:
                    branches.append(
                        GPS.VCS2.Branch(
                            name=n,
                            active=m.group('current') is not None,
                            annotation=emblem,
                            id=m.group('name')))

    def _tags(self, visitor):
        """
        A generator that returns the list of all known tags
        via `visitor.branches`
        """
        p = self._git(['tag'])
        tags = []
        while True:
            line = yield p.wait_line()
            if line is None:
                visitor.branches(
                    CAT_TAGS, 'vcs-tag-symbolic', CAN_RENAME, tags)
                break
            tags.append(GPS.VCS2.Branch(
                name=line, active=False, annotation='', id=line))

    def _stashes(self, visitor):
        """
        A generator that returns the list of all known stashes via
        `visitor.branches`.
        """
        p = self._git(['stash', 'list'])
        stashes = []
        while True:
            line = yield p.wait_line()
            if line is None:
                visitor.branches(
                    'stashes', 'vcs-stash-symbolic', not CAN_RENAME, stashes)
                break
            # Line can contain either
            #   stash@{0}: On master: bla
            # or
            #   stash@{0}: bla
            splits = line.split(':', 2)
            name = splits[0]
            descr = splits[-1]
            annotation = splits[1]
            stashes.append(
                GPS.VCS2.Branch(
                    name='%s: %s' % (name, descr),
                    active=False,
                    annotation=annotation,
                    id=name))

    def _worktrees(self, visitor):
        """
        A generator that returns the list of worktrees via `visitor.branches`
        """
        # "--no-pager" results in segfault with git 2.11
        p = ProcessWrapper(
            ['git', 'worktree', 'list', '--porcelain'],
            directory=self.working_dir.path)
        trees = []
        current = []
        while True:
            line = yield p.wait_line()
            if line is None:
                # Do not report if we only have the current directory
                if len(trees) > 1:
                    visitor.branches(
                        CAT_WORKTREES,
                        'vcs-git-worktrees-symbolic',
                        not CAN_RENAME,
                        trees)
                break
            elif not line:
                trees.append(current)
            elif line.startswith('worktree '):
                current = GPS.VCS2.Branch(
                    name='"%s"' % line[9:],   # quoted not to expand '/'
                    active=self.working_dir == GPS.File(line[9:]),
                    annotation='',
                    id='')   # unique id
            elif line.startswith('HEAD '):
                current[3] = line[5:]   # unique id
            elif line.startswith('branch '):
                current[2] = line[7:]   # details
            elif line.startswith('detached'):
                current[2] = 'detached'  # details

    def _submodules(self, visitor):
        """
        A generator that returns the list of submodules via `visitor.branches`
        """
        p = self._git(['submodule', 'status', '--recursive'])
        modules = []
        while True:
            line = yield p.wait_line()
            if line is None:
                if len(modules) != 0:
                    visitor.branches(
                        CAT_SUBMODULES,
                        'vcs-submodules-symbolic',
                        not CAN_RENAME,
                        modules)
                break
            _, sha1, name, _ = line.split(' ', 3)
            modules.append(
                GPS.VCS2.Branch(
                    name=name, active=False, annotation='', id=sha1))

    @core.run_in_background
    def async_branches(self, visitor):
        yield join(self._branches(visitor),
                   self._tags(visitor),
                   self._stashes(visitor),
                   self._worktrees(visitor),
                   self._submodules(visitor),
                   *self.extensions('async_branches', visitor))

    def _current_branch(self):
        """
        A promise that returns the name of the current branch
        """
        result = Promise()

        def online(line):
            result.resolve(line)

        p = self._git(['rev-parse', '--abbrev-ref', 'HEAD'])
        p.lines.subscribe(online)
        return result

    @core.run_in_background
    def async_action_on_branch(self, visitor, action, category, id, text=''):
        if category == CAT_BRANCHES:
            if action == GPS.VCS2.Actions.DOUBLE_CLICK and id:
                p = self._git(['checkout', id])
                yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.TOOLTIP and id:
                visitor.tooltip(
                    '\nDouble-click to checkout this branch.\n'
                    'Click [+] to create a new branch from this one.\n'
                    'Click [-] to delete current branch.')

            elif action == GPS.VCS2.Actions.ADD and id:
                name = GPS.MDI.input_dialog(
                    'Choose a name for the new branch',
                    'name=%s-new' % id)
                if name:
                    name = name[0]
                    p = self._git(['branch', '--track', name, id])
                    s, _ = yield p.wait_until_terminate(show_if_error=True)
                    if s == 0:
                        # Checkout will not succeed if there are local changes
                        p = self._git(['checkout', name])
                        yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.RENAME and id and text:
                p = self._git(['branch', '-m', id, text])
                yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.REMOVE and id:
                if (id != 'master' and
                        GPS.MDI.yes_no_dialog("Delete branch `%s` ?" % id)):

                    # If this is the current branch, fallback to master
                    current = yield self._current_branch()
                    if current == id:
                        p = self._git(['checkout', 'master'])
                        s, _ = yield p.wait_until_terminate(show_if_error=True)

                    p = self._git(['branch', '-D', id])
                    yield p.wait_until_terminate(show_if_error=True)

        elif category == CAT_TAGS:
            if action == GPS.VCS2.Actions.DOUBLE_CLICK and id:
                p = self._git(['checkout', id])
                yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.TOOLTIP:
                visitor.tooltip(
                    '\nDouble-click to checkout this tag' +
                    ('\nClick [+] to create a new tag on current branch'
                     if not id else '') +
                    ('\nClick [-] to delete tag' if id else ''))

            elif action == GPS.VCS2.Actions.ADD and not id:
                name = GPS.MDI.input_dialog(
                    'Choose a name for the new tag',
                    'name',
                    'Commit Message (will annotate if set)')
                if name and name[0]:   # not cancelled
                    p = self._git(['tag',
                                   '-a' if name[1] else '',
                                   '--message=%s' % name[1] if name[1] else '',
                                   name[0]])
                    yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.REMOVE:
                if id and GPS.MDI.yes_no_dialog("Delete tag `%s` ?" % id):
                    p = self._git(['tag', '-d', id])
                    yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.RENAME and id and text:
                # ??? Can we create an annotated tag ?
                p = self._git(['tag', text, id])
                s, _ = yield p.wait_until_terminate(show_if_error=True)
                if s == 0:
                    p = self._git(['tag', '-d', id])
                    s, _ = yield p.wait_until_terminate(show_if_error=True)
                # ??? Should we push to origin to remote ?

        elif category == CAT_STASHES:
            if action == GPS.VCS2.Actions.DOUBLE_CLICK and id:
                p = self._git(['stash', 'apply', id])
                yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.TOOLTIP:
                visitor.tooltip(
                    ('\nDouble-click to apply this stash on HEAD' +
                     '\nClick [-] to drop this stash' if id else '') +
                    ('' if id else '\nClick [+] to stash all local changes'))

            elif action == GPS.VCS2.Actions.ADD and not id:
                p = self._git(['stash', 'save', 'created from GPS'])
                yield p.wait_until_terminate(show_if_error=True)

            elif action == GPS.VCS2.Actions.REMOVE and id:
                p = self._git(['stash', 'drop', id])
                yield p.wait_until_terminate(show_if_error=True)

        elif category == CAT_REMOTES:
            if action == GPS.VCS2.Actions.DOUBLE_CLICK:
                pass

            elif action == GPS.VCS2.Actions.TOOLTIP:
                visitor.tooltip(
                    '\nDouble-click to checkout this remote branch locally' +
                    '\nClick [-] to delete this remote branch'
                    if id else '')
                pass

            elif action == GPS.VCS2.Actions.ADD:
                pass

            elif action == GPS.VCS2.Actions.REMOVE and id:
                # id is of the form 'remotes/origin/some/name'
                _, origin, name = id.split('/', 2)
                if GPS.MDI.yes_no_dialog("Delete remote branch `%s` ?" % id):
                    p = self._git(['push', origin, ':%s' % name])
                    yield p.wait_until_terminate(show_if_error=True)

        elif category in (CAT_WORKTREES, CAT_SUBMODULES):
            pass

        else:
            yield join(*self.extensions(
                'async_action_on_branch', visitor, action, category, id, text))

    @core.run_in_background
    def async_discard_local_changes(self, files):
        n = [f.path for f in files]
        yield self._git(['reset'] + n).wait_until_terminate()
        yield self._git(['checkout'] + n).wait_until_terminate()
        GPS.MDI.information_popup(
            'Local changes discarded', 'github-commit-symbolic')

    @core.run_in_background
    def async_checkout(self, visitor, commit):
        p = self._git(['checkout', commit], block_exit=True)
        status, _ = yield p.wait_until_terminate(show_if_error=True)
        if status == 0:
            visitor.success('Checkout successful')

    @core.run_in_background
    def async_checkout_file(self, visitor, commit, file):
        p = self._git(['checkout', commit, '--', file.path], block_exit=True)
        status, _ = yield p.wait_until_terminate(show_if_error=True)
        if status == 0:
            visitor.success('Checkout successful')
