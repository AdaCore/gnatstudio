import json
import os
from workflows.promises import ProcessWrapper
import GPS
from . import core
from . import git

CAT_REVIEWS = 'REVIEWS'

CAN_RENAME = True


class Gerrit(core.Extension):
    def __init__(self, base_vcs):
        super(Gerrit, self).__init__(base_vcs)
        self.gerrit_accessible = True

    def applies(self):
        gitreview = os.path.join(self.base.working_dir.path, '.gitreview')

        if os.path.isfile(gitreview):
            self.port = ''
            self.host = ''
            self.project = ''
            for line in open(gitreview).read().splitlines():
                if '=' in line:
                    prefix, value = line.split('=')
                    if prefix.strip().lower() == 'host':
                        self.host = value.strip()
                    elif prefix.strip().lower() == 'port':
                        self.port = '%s' % value.strip()
                    elif prefix.strip().lower() == 'project':
                        self.project = value.strip()

            return self.project and self.host
        else:
            return False

    def async_branches(self, visitor):
        # We use -q to hide warnings which could for instance occur
        # when redirecting ports if ~/.ssh/config contains
        #   Host ...
        #      RemoteForward 3142 localhost:22

        if not self.gerrit_accessible:
            return

        p = ProcessWrapper(
            ['ssh',
             '-x',
             '-q',  # Hide warnings
             '-p' if self.port else '', self.port,
             self.host,
             'gerrit',
             'query',
             '--format=json',
             '--current-patch-set',
             'project:%s' % self.project,
             'status:open'], block_exit=False)

        reviews = []
        while True:
            line = yield p.wait_line()
            if line is None:
                if reviews:
                    visitor.branches(
                        CAT_REVIEWS, 'vcs-gerrit-symbolic',
                        not CAN_RENAME, reviews)
                break

            if line.startswith('Bad port'):
                # Seems like Gerrit can't be accessed
                GPS.Console().write('Can\'t access Gerrit %s:%s\n' % (
                    self.host, self.port))
                self.gerrit_accessible = False
                break

            patch = json.loads(line)
            if patch and patch.get(u'subject', None) is not None:
                review = '0'
                workflow = ''
                patchset = patch[u'currentPatchSet']
                if patchset.get(u'approvals', None) is not None:
                    for a in patchset[u'approvals']:
                        if a[u'type'] == u'Workflow':
                            workflow = '|%s' % a['value']
                        elif a[u'type'] == u'Code-Review':
                            review = a['value']

                id = {'url': patch.get(u'url', ''),
                      'number': patch.get(u'number', '')}

                reviews.append(
                    ('%s: %s' % (patchset[u'author'][u'username'],
                                 patch[u'subject']),
                     False,   # not active
                     '%s%s' % (review, workflow),
                     json.dumps(id)))

    def async_action_on_branch(self, visitor, action, category, id, text=''):
        if not self.gerrit_accessible:
            return

        if category == CAT_REVIEWS:
            if id:
                id = json.loads(id)
            if action == GPS.VCS2.Actions.DOUBLE_CLICK and id:
                import webbrowser
                webbrowser.open(id['url'])

            elif action == GPS.VCS2.Actions.TOOLTIP:
                visitor.tooltip(
                    '\nDouble-click to open Gerrit on this change' +
                    ('\nClick [+] to cherry-pick this review' if id else ''))

            elif action == GPS.VCS2.Actions.ADD and id:
                p = self.base._git(['review', '--cherrypick', id['number']])
                status, _ = yield p.wait_until_terminate(show_if_error=True)
                if status == 0:
                    GPS.Console().write(
                        "Applied to working directory: %s\n" % id['number'])

    @core.vcs_action(icon='vcs-cloud-symbolic',
                     name='git review',
                     menu='VCS/Review',
                     after='server section')
    def _review(self):
        """
        Push all local changes to Gerrit, so that they can be reviewed by
        other team members.
        """
        if not self.gerrit_accessible:
            return

        p = self.base._git(
            ['review', '--yes', '--no-rebase'],
            spawn_console='')
        status, _ = yield p.wait_until_terminate()
        if status == 0:
            GPS.MDI.information_popup(
                'Pushed to review', 'vcs-cloud-symbolic')


git.Git.register_extension(Gerrit)
