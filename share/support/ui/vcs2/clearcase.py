import GPS
import gps_utils
import os
import os_utils
from . import core
from . import core_staging
from workflows.promises import ProcessWrapper
from enum import Enum

LOG_ID = "CLEARCASE"
VCS_PATH = "VCS/"
VCS_CONTEXT_PATH = "Version Control/"
CATEGORY = "VCS"
MESSAGES_VIEW = "Messages"
ID_HEADER = "Event "
# The actions must only be registered once
ALREADY_LOADED = False
VOBS = None

CC_PATH = "Clearcase/"
UNCHECKOUT_PREF = "Uncheckout behavior"
CHECKOUT_PREF = "Create element in checkout"

GPS.Preference(CC_PATH + UNCHECKOUT_PREF).create(
    UNCHECKOUT_PREF,
    'enum',
    'Keep will create a copy with the changes when uncheckout,' +
    ' Discard will suppress the changes',
    0,  # Default value corresponding to keep
    'Keep',
    'Discard')

GPS.Preference(CC_PATH + CHECKOUT_PREF).create(
    CHECKOUT_PREF,
    'boolean',
    'Add the element in the vob if no activity set',
    True)


class Activity(Enum):
    YES = 0
    NO = 1
    LOCKED = 2


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
            p = GPS.Process(['cleartool', 'lsvob'])
            output = p.get_result()
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

    def _file_or_dir_filter(self, context):
        if context.file() or context.directory():
            return True
        else:
            return False

    def __init__(self, *args, **kwargs):
        global ALREADY_LOADED
        super(Clearcase, self).__init__(*args, **kwargs)

        self.details = {}

        if not ALREADY_LOADED:
            def _register_clearcase_action(name, action):
                gps_utils.make_interactive(
                    callback=action,
                    name='clearcase ' + name + ' current element',
                    category=CATEGORY,
                    filter=self._file_or_dir_filter,
                    menu=(VCS_PATH + 'Clearcase ' + name + ' current element'),
                    contextual=(VCS_CONTEXT_PATH + "Clearcase " +
                                name + " element"))

            ALREADY_LOADED = True
            GPS.Logger(LOG_ID).log("Registering the clearcase actions...")
            _register_clearcase_action("checkout", self._checkout_current)
            _register_clearcase_action("checkin", self._checkin_current)
            _register_clearcase_action("uncheckout", self._uncheckout_current)
            _register_clearcase_action("create", self._create_current)
            _register_clearcase_action("remove", self._remove_current)
            GPS.Logger(LOG_ID).log("Finishing registering the actions")

    def _set_clearcase_status(self, cmd_line):

        def __set_buffer_writable(buf, writable):
            if buf:
                buf.set_read_only(not writable)

        with self.set_status_for_all_files() as s:
            p = self._cleartool(cmd_line)
            while True:
                line = yield p.wait_line()
                if not line:
                    break
                splitted = line.split('@@')
                file = GPS.File(splitted[0])

                buf = GPS.EditorBuffer.get(file, open=False)
                if len(splitted) == 1:
                    status = GPS.VCS2.Status.UNTRACKED
                elif line.endswith('CHECKEDOUT'):
                    status = GPS.VCS2.Status.MODIFIED
                    __set_buffer_writable(buf, True)
                else:
                    status = GPS.VCS2.Status.UNMODIFIED
                    __set_buffer_writable(buf, False)
                s.set_status(file, status, '', '')

    @core.run_in_background
    def async_fetch_status_for_files(self, files):
        cmd_line = ['ls', '-short'] + [file.path for file in files]
        yield self._set_clearcase_status(cmd_line)

    @core.run_in_background
    def async_fetch_status_for_all_files(self, from_user, extra_files=[]):
        cmd_line = ['ls', '-recurse', '-short', '.']
        yield self._set_clearcase_status(cmd_line)

    def _has_defined_activity(self, path, verbose):
        """
        Whether there is a defined activity currently defined and if the file
        is not locked which is necessary to be able to do a checkout.

        :returntype: a promise that will be resolved to a boolean, to
            indicate whether there is a defined activity.
        """
        p = GPS.Process(['cleartool', 'lslock'])
        output = p.get_result()
        status = p.wait()
        if status:
            self.__log(path + " has no activity", verbose)
            return Activity.NO  # No Clearcase activity on the file
        elif output:
            self.__log(path + " is locked", True)
            return Activity.LOCKED  # The file is locked
        else:
            return Activity.YES

    def __log(self, message, visible):
        GPS.Logger(LOG_ID).log(message)
        if visible:
            GPS.Console(MESSAGES_VIEW).write(message + "\n")

    def __log_result(self, command, status, output, visible):
        if status == 0:
            self.__log(command + " succeeded", visible)
        else:
            self.__log(command + " failed: " + output, visible)

    def __data_from_context(self):
        file = GPS.current_context().file()
        if file:
            path = file.path
        else:
            path = GPS.current_context().directory()
        return file, path

    def __invalidate_clearcase_cache(self, file):
        self.invalidate_status_cache()
        if file:
            self.ensure_status_for_files([file])
        else:
            self.ensure_status_for_all_source_files()

    def __user_input(self, title, text=""):
        # Ask a comment to the user
        comment = GPS.MDI.input_dialog(
            title + "'s comment", "multiline:Comment=" + text)
        if not comment:
            comment_option = None
        elif comment[0]:
            comment_option = ['-c', comment[0]]
        else:
            comment_option = ['-nc']
        return comment_option

    def _checkout_current(self):
        """
        Checkout the current element if not locked and has Clearcase activity.
        If no activity, try to create new file element in the current dir.
        """
        file, path = self.__data_from_context()

        activity = self._has_defined_activity(path, False)
        if activity == Activity.LOCKED:
            return

        cmd_line = ['cleartool', 'co']
        comment_option = self.__user_input("Checkout")
        if not comment_option:
            return
        cmd_line += comment_option
        cmd_line.append(path)

        if (activity == Activity.NO and
                GPS.Preference(CC_PATH + CHECKOUT_PREF).get()):
            # Create activity for file
            GPS.Process(['cleartool', 'co'] + comment_option + ['.']).wait()
            p = GPS.Process(['cleartool', 'mkelem'] + comment_option + [path])
            output = p.get_result()
            status = p.wait()
            GPS.Process(['cleartool', 'ci'] + comment_option + ['.']).wait()
        else:
            p = GPS.Process(cmd_line)
            output = p.get_result()
            status = p.wait()
        self.__log_result("checkout", status, output, True)
        self.__invalidate_clearcase_cache(file)

    def _checkin_current(self):
        """
        Checkin the current element if not locked and has Clearcase activity.
        Will retrieve the checkout comment.
        """
        file, path = self.__data_from_context()

        if not (self._has_defined_activity(path, True) == Activity.YES):
            return

        # Retrieve the checkout message
        p = GPS.Process(['cleartool', 'lsco', '-fmt', '%c', path])
        output = p.get_result()
        status = p.wait()

        if output:
            output = output[:-1]  # Remove the new line created by the command

        cmd_line = ['cleartool', 'ci']
        comment_option = self.__user_input("Checkin", output)
        if not comment_option:
            return
        cmd_line += comment_option
        cmd_line.append(path)

        p = GPS.Process(cmd_line)
        output = p.get_result()
        status = p.wait()
        self.__log_result("checkin", status, output, True)
        self.__invalidate_clearcase_cache(file)

    def _uncheckout_current(self):
        """
        Uncheckout the current element if not locked and has activity.
        The preference "Clearcase uncheckout" will determine if a .keep file
        is created.
        """
        file, path = self.__data_from_context()

        if not (self._has_defined_activity(path, True) == Activity.YES):
            return

        cmd_line = ['cleartool', 'unco']
        if GPS.Preference(CC_PATH + UNCHECKOUT_PREF).get() == 'Keep':
            cmd_line.append('-keep')
        else:
            cmd_line.append('-rm')
        cmd_line.append(path)

        p = GPS.Process(cmd_line)
        output = p.get_result()
        status = p.wait()
        self.__log_result("uncheckout", status, output, True)
        self.__invalidate_clearcase_cache(file)

    def _create_current(self):
        """
        Try to add the current element in the VOB.
        """
        file, path = self.__data_from_context()

        cmd_line = ['cleartool', 'mkelem']
        comment_option = self.__user_input("Mkelem")
        if not comment_option:
            return
        cmd_line += comment_option
        cmd_line.append(path)

        p = GPS.Process(cmd_line)
        output = p.get_result()
        status = p.wait()
        self.__log_result("mkelem", status, output, True)
        self.__invalidate_clearcase_cache(file)

    def _remove_current(self):
        """
        Try to remove the current element from the VOB.
        """
        file, path = self.__data_from_context()

        cmd_line = ['cleartool', 'rmelem', '-force']
        comment_option = self.__user_input("Remove")
        if not comment_option:
            return
        cmd_line += comment_option
        cmd_line.append(path)

        p = GPS.Process(cmd_line)
        output = p.get_result()
        status = p.wait()
        if status and file:
            # Close the file if present in the editor
            GPS.EditorBuffer.get(file, open=False).close(force=True)
        self.__log_result("rmelem", status, output, True)
        self.__invalidate_clearcase_cache(file)

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

            elif status & GPS.VCS2.Status.STAGED_MODIFIED:
                status, output = yield self._cleartool(
                    ['ci', '-comment', message, f.path],
                    block_exit=True).wait_until_terminate()
                self.__log_result("checkin", status, output, True)

        visitor.success('Finish staging')

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

    @core.run_in_background
    def async_discard_local_changes(self, files):
        cmd_line = ['unco']
        if GPS.Preference(CC_PATH + UNCHECKOUT_PREF).get() == 'Keep':
            cmd_line.append('-keep')
        else:
            cmd_line.append('-rm')
        cmd_line += [file.path for file in files]
        status, output = yield self._cleartool(cmd_line).wait_until_terminate()
        self.__log_result("uncheckout", status, output, True)
