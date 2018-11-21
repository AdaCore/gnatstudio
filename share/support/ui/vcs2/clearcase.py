import GPS
import gps_utils
import os
import os_utils
from . import core
from . import core_staging
import subprocess
import re
from workflows.promises import ProcessWrapper

LOG_ID = "CLEARCASE"
VCS_PATH = "Version Control/"
CATEGORY = "VCS"
MESSAGES_VIEW = "Messages"
ID_HEADER = "Event "
# The actions must only be registered once
ALREADY_LOADED = False
VOBS = None


@core.register_vcs(name='ClearCase Native',
                   default_status=GPS.VCS2.Status.UNMODIFIED)
class Clearcase(core_staging.Emulate_Staging,
                core.VCS):

    @staticmethod
    def discover_working_dir(file):
        global VOBS
        if not os_utils.locate_exec_on_path("cleartool"):
            return ""

        # Only query the VOBs once to not slowdown GPS
        if VOBS is None:
            # Map of tag path to VOB dir
            VOBS = []
            p = subprocess.Popen(['cleartool', 'lsvob'],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.STDOUT)
            output, error = p.communicate()
            status = p.wait()
            if status or not output:
                return ""
            else:
                for line in output.splitlines():
                    # The format is {tag} {vob dir} {public | private}
                    tag = line.split()[0]
                    vob_dir = line.split()[1]
                    VOBS.append(os.path.dirname(vob_dir) + tag)

        cur_dir = os.path.dirname(file.path)
        for path in VOBS:
            if path in cur_dir:
                return path
        return ""

    def _cleartool(self, args, block_exit=False):
        p = ProcessWrapper(
            ['cleartool'] + args,
            block_exit=block_exit,
            directory=self.working_dir.path)
        return p

    def __init__(self, *args, **kwargs):
        global ALREADY_LOADED
        super(Clearcase, self).__init__(*args, **kwargs)

        self._file_with_status = None
        self.details = {}

        if not ALREADY_LOADED:
            ALREADY_LOADED = True
            GPS.Logger(LOG_ID).log("Registering the clearcase actions...")
            gps_utils.make_interactive(
                callback=self._checkout_current_file,
                name='clearcase checkout current file',
                category=CATEGORY,
                filter="File",
                contextual=VCS_PATH + 'Checkout file %f')
            gps_utils.make_interactive(
                callback=self._checkin_current_file,
                name='clearcase checkin current file',
                category=CATEGORY,
                filter="File",
                contextual=VCS_PATH + 'Checkin file %f')
            gps_utils.make_interactive(
                callback=self._uncheckout_current_file,
                name='clearcase uncheckout current file',
                category=CATEGORY,
                filter="File",
                contextual=VCS_PATH + 'Uncheckout file %f')
            GPS.Logger(LOG_ID).log("Finishing registering the actions")

    @core.run_in_background
    def async_fetch_status_for_all_files(self, from_user, extra_files=[]):
        _re = re.compile(
            '(?P<file>[^@@]+)(?P<sep>@@)?(?P<rev>[^\s]*)(\n|$)')

        with self.set_status_for_all_files() as s:
            p = self._cleartool(['ls', '-short', '.'])
            while True:
                line = yield p.wait_line()
                if line is None:
                    break

                GPS.Logger(LOG_ID).log(line)
                m = _re.search(line)
                if m:
                    status = GPS.VCS2.Status.UNMODIFIED
                    rev = m.group('rev')
                    if not m.group('sep'):
                        status = GPS.VCS2.Status.UNTRACKED
                    elif rev.endswith('CHECKEDOUT'):
                        status = GPS.VCS2.Status.MODIFIED
                    elif rev == '':
                        status = GPS.VCS2.Status.IGNORED

                    s.set_status(
                        GPS.File(m.group('file')),
                        status,
                        rev,
                        '')  # repo revision
            self._file_with_status = s.files_with_explicit_status

    def _has_defined_activity(self, file):
        """
        Whether there is a defined activity currently defined and if the file
        is not locked which is necessary to be able to do a checkout.

        :returntype: a promise that will be resolved to a boolean, to
            indicate whether there is a defined activity.
        """
        p = subprocess.Popen(['cleartool', 'lslock', str(file) + "@@"],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output, error = p.communicate()
        status = p.wait()
        if status:
            return False  # No Clearcase activity on the file
        elif output:
            return False  # The file is locked
        else:
            return True

    def __log(self, message, visible):
        GPS.Logger(LOG_ID).log(message)
        if visible:
            GPS.Console(MESSAGES_VIEW).write(message + "\n")

    def __log_result(self, command, status, output, writable, file):
        if status == 0:
            self.__log(command + " succeed", True)
            GPS.EditorBuffer.get(file, open=False).set_read_only(writable)
        else:
            self.__log(command + " failed: " + output, True)

    def _checkout_current_file(self):
        ctxt = GPS.contextual_context() or GPS.current_context()
        file = ctxt.file()

        if not self._has_defined_activity(file):
            GPS.Console().write('No clearcase activity set for ' +
                                str(file) + '\n')
            return

        # Ask a checkout comment to the user
        comment = GPS.MDI.input_dialog(
            "Checkout's comment", "multiline:Comment")

        if not comment:
            return  # The dialog was closed or no comment

        p = subprocess.Popen(['cleartool', 'co', '-c', comment[0], file.path],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output, error = p.communicate()
        status = p.wait()
        self.__log_result("checkout", status, output, False, file)

    def _checkin_current_file(self):
        ctxt = GPS.contextual_context() or GPS.current_context()
        file = ctxt.file()

        if not self._has_defined_activity(file):
            GPS.Console().write('No clearcase activity set for ' +
                                str(file) + '\n')
            return

        # Retrieve the checkout message
        p = subprocess.Popen(['cleartool', 'lsco', '-fmt', '%c', file.path],
                             cwd=self.working_dir.path,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output, error = p.communicate()
        status = p.wait()

        if not output:
            output = "GPS checkin"  # Default message for hijacked files
        else:
            output = output[:-1]  # Remove the new line created by the command
        # Confirm the checkin message
        comment = GPS.MDI.input_dialog(
            "Checkin's comment", "multiline:Comment=" + output)

        if not comment:
            return  # The dialog was closed or no comment

        p = subprocess.Popen(['cleartool', 'ci', '-c', comment[0], file.path],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output, error = p.communicate()
        status = p.wait()
        self.__log_result("checkin", status, output, True, file)

    def _uncheckout_current_file(self):
        ctxt = GPS.contextual_context() or GPS.current_context()
        file = ctxt.file()

        if not self._has_defined_activity(file):
            GPS.Console().write('No clearcase activity set for ' +
                                str(file) + '\n')
            return

        p = subprocess.Popen(['cleartool', 'unco', '-keep', file.path],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT)
        output, error = p.communicate()
        status = p.wait()
        self.__log_result("uncheckout", status, output, True, file)

    @core.run_in_background
    def async_commit_staged_files(self, visitor, message):
        for f in self._staged:
            status, version, rversion = self.get_file_status(f)
            if status & GPS.VCS2.Status.STAGED_ADDED:
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
        visitor.success('Commit successful')

    @core.run_in_background
    def async_fetch_history(self, visitor, filter):
        max_lines = filter[0]
        for_file = filter[1]
        pattern = filter[2]
        cmd_line = ['lshistory', '-minor', '-eventid',
                    '-fmt', '%%GPS%On||%d||%Fu||%e||%n||%Na\n%Nc\n']

        if for_file:
            cmd_line.append(for_file.path)
        else:
            cmd_line.append(self.working_dir.path)

        # We can't control the number of events so count them
        nb_added_lines = 1
        parent_id = ""
        current_id = ""
        message_lines = []
        # Reset the details
        self.details = {}
        # True if the event doesn't match the pattern
        is_ignored = False

        oid = ""
        date = ""
        author = ""
        event = ""
        entity = ""
        attrs = ""
        subject = ""

        def _add_event():
            if current_id and not is_ignored:
                visitor.history_line(GPS.VCS2.Commit(
                        current_id, author, date, subject, [parent_id]))
                self.details[current_id] = (
                    'Subject: {}\n'
                    'Path:    {}\n'
                    'OID:     {}\n'
                    'Event:   {}\n'
                    'Author:  {}\n'
                    'Date:    {}\n').format(
                        subject, entity, oid, current_id, author, date)
                if attrs:
                    self.details[current_id] += (
                        "\nAttributes:\n{}".format(attrs))
                if len(message_lines):
                    self.details[current_id] += (
                        "\nMessage:\n" + "\n".join(message_lines))

        p = self._cleartool(cmd_line)
        while True:
            line = yield p.wait_line()
            if line is None or nb_added_lines > max_lines:
                _add_event()
                self.__log("Done parsing lshistory", False)
                break
            elif line.startswith("event "):
                current_id = parent_id
                parent_id = ID_HEADER + line[len("event "):-1]
                _add_event()

                nb_added_lines += 1
            elif line.startswith("%GPS"):
                message_lines = []
                try:
                    oid, date, author, event, entity, attrs = (
                        line[len("%GPS"):].split("||"))
                    subject = (event +
                               " " +
                               os.path.basename(entity.split("@@")[0]) +
                               "@@".join(entity.split("@@")[1:]))

                    if pattern and pattern not in line:
                        is_ignored = True
                        continue
                    else:
                        is_ignored = False
                except ValueError:
                    self.__log("Failed to parse lshistory: " + line, True)
                    break
            # Keep an empty line only if its part of a message
            elif line or message_lines:
                message_lines.append(line)
        self.__log("Finishing lshistory added lines:{}".format(
            str(nb_added_lines)), False)

    def _to_git_format(self, text):
        """Make the diff output parsable by the diff language"""
        diff_title = "diff between {} and {}"

        # Return if we don't have an output or if we don't have a predecessor
        # (will produce an error starting by "cleartool"
        if not text or text.startswith("cleartool"):
            return ""

        pred = ""
        cur = ""

        output = []
        for line in text.splitlines():
            if line.startswith('<<<'):
                pred = line.split(':')[1].lstrip()
            elif line.startswith('>>>'):
                cur = line.split(':')[1].lstrip()
                output.append(diff_title.format(pred, cur))
            elif line.startswith('>'):
                output.append('+' + line[1:])
            elif line.startswith('<'):
                output.append('-' + line[1:])
            elif line.startswith('*'):
                pass
            else:
                formatted = line.replace('-', '')
                if formatted:
                    output.append("\n@@ " + formatted + " @@")
        return "\n".join(output)

    @core.run_in_background
    def async_diff(self, visitor, ref, file):

        def _execute_diff(path, verbose):
            cmd_line = ['diff', '-serial_format', '-pred', path]
            p = self._cleartool(cmd_line)
            status, output = yield p.wait_until_terminate()
            if status == 1:
                yield self._to_git_format(output)
            else:
                self.__log('Diff failed with: ' + output, verbose)
                yield None

        if file:
            text = yield _execute_diff(file.path, True)
            if text:
                visitor.diff_computed(text)
            else:
                self.__log('No diff with the predecessor', True)
        else:
            # Retrieve the list of checkout files/directories
            # and then run diff on each of them
            cmd_line = ['lsco', '-recurse', '-cview', '-fmt', '%n\n']
            p = self._cleartool(cmd_line)
            status, output = yield p.wait_until_terminate()
            if status == 0 and output:
                diff = []
                for line in output.splitlines():
                    text = yield _execute_diff(line, False)
                    if text:
                        diff.append(text)
                visitor.diff_computed("\n\n".join(diff))
            else:
                self.__log('No diff found', True)

    @core.run_in_background
    def async_fetch_commit_details(self, ids, visitor):

        def _get_entity(text):
            """Retrieve the entity path stored in the header"""
            path_header = "Path:"
            for line in text.splitlines():
                if line.startswith(path_header):
                    return line[len(path_header):].lstrip()
            return ""

        header = []
        for id in ids:
            if id in self.details:
                header.append(self.details[id])

        if len(ids) == 1:
            cmd_line = ['diff', '-serial_format', '-pred']
            entity = _get_entity(header[0])
            cmd_line.append(entity)
            p = self._cleartool(cmd_line)
            status, output = yield p.wait_until_terminate()
            if status == 1:
                header.append(self._to_git_format(output))
        visitor.set_details(id, '', '\n\n'.join(header))

    @core.run_in_background
    def async_annotations(self, visitor, file):
        lines = []
        ids = []
        event_to_id = {}
        id = ""
        event = ""
        fmt = '%Sd %u %Vn|%e'

        # We can't directly retrieve the event id via the annotate command
        # Thus we use lshistory to create a map between the event and their
        # description which is also unique.
        cmd_line = ['lshistory', '-minor', '-eventid', '-fmt', fmt + '\n',
                    file.path]
        p = self._cleartool(cmd_line)
        status, output = yield p.wait_until_terminate()
        if status == 0:
            for line in output.splitlines():
                if line.startswith("event "):
                    id = ID_HEADER + line[len("event "):-1]
                else:
                    event_to_id[line.split('|')[0]] = id
        else:
            self.__log("Can't retrieve the events via lshistory" + output,
                       False)

        cmd_line = ['annotate', '-out', '-', '-nheader',
                    '-fmt', fmt, file.path]
        p = self._cleartool(cmd_line)
        while True:
            line = yield p.wait_line()
            if line is None:
                break

            if not line.startswith(' '):
                event = line.split('|')[0]
                lines.append(event)
                ids.append(event_to_id[event])
            else:
                lines.append(event)
                ids.append(event_to_id[event])

        visitor.annotations(file, 1, ids, lines)
