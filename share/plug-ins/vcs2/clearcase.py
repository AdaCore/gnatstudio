import GPS
from . import core
from . import core_staging
import os
import re
import types
from workflows.promises import ProcessWrapper


@core.register_vcs(name='ClearCase Native',
                   default_status=GPS.VCS2.Status.UNMODIFIED)
class Clearcase(core_staging.Emulate_Staging,
                core.VCS):

    def _cleartool(self, args, block_exit=False):
        p = ProcessWrapper(
            ['cleartool'] + args,
            block_exit=block_exit,
            directory=self.working_dir.path)
        return p

    @core.run_in_background
    def async_fetch_status_for_all_files(self):
        _re = re.compile(
            '(?P<file>[^@@]+)(?P<sep>@@)?(?P<rev>[^\s]*)(\n|$)')

        with self.set_status_for_all_files() as s:
            p = self._cleartool(['ls', '-short', '.'])
            while True:
                line = yield p.wait_line()
                if line is None:
                    break

                GPS.Logger("CLEARCASE").log(line)
                m = _re.search(line)
                if m:
                    # ??? These are just (bad) guesses for now
                    status = GPS.VCS2.Status.UNMODIFIED
                    rev = m.group('rev')
                    if rev.contains('CHECKEDOUT'):
                        status = GPS.VCS2.Status.MODIFIED
                    elif m.group('sep') == '':
                        status = GPS.VCS2.Status.UNTRACKED
                    elif rev == '':
                        status = GPS.VCS2.Status.IGNORED

                    s.set_status(
                        GPS.File(m.group('file')),
                        0,
                        rev,
                        '')  # repo revision

    @core.run_in_background
    def commit_staged_files(self, message):
        for f in self._staged:
            status, version, rversion = self.get_file_status(f)
            if status & GPS.VCS2.Status.STAGED_ADDED:
                # ??? blind port from the old VCS
                yield self._cleartool(
                    ['co', '-comment', message, '.'],
                    block_exit=True).wait_until_terminate()
                yield self._cleartool(
                    ['mkelem', '-comment', message, f.path],
                    block_exit=True).wait_until_terminate()
                yield self._cleartool(
                    ['ci', '-comment', message, '.'],
                    block_exit=True).wait_until_terminate()

            elif status & GPS.VCS2.Status.STAGED_DELETED:
                yield self._cleartool(
                    ['co', '-comment', message, '.'],
                    block_exit=True).wait_until_terminate()
                yield self._cleartool(
                    ['rm', '-comment', message, f.path],
                    block_exit=True).wait_until_terminate()
                yield self._cleartool(
                    ['ci', '-comment', message, '.'],
                    block_exit=True).wait_until_terminate()

        yield self._cleartool(
            ['ci', '-comment', message] + [f.path for f in self._staged],
            block_exit=True).wait_until_terminate()

    @core.run_in_background
    def async_fetch_history(self, visitor, filter):
        p = self._cleartool(['lshistory', '.'])
        while True:
            line = yield p.wait_line()
            if line is None:
                GPS.Logger("CLEARCASE").log("Done parsing lshistory")
                break
            GPS.Logger("CLEARCASE").log(line)
            # ??? Should parse the output

    @core.run_in_background
    def async_fetch_commit_details(self, ids, visitor):
        for id in ids:
            visitor.set_details(id, '', '')
