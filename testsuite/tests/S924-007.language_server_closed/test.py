"""
This tests verifies the mechanism for handling automatic relaunches of
language servers.

Note: this test simulates a language server dying brutally; we're doing this
by using linux-specific commands.
"""
import os
import signal
import subprocess
import GPS
from gps_utils.internal.utils import \
    gps_assert, hook, run_test_driver, timeout


def get_language_server_pid():
    """Get the PID of the language server, None if it doesn't exist"""
    parent_pid = os.getpid()
    ps_command = subprocess.Popen(
        "ps -o pid,cmd --ppid {} --noheaders".format(parent_pid),
        shell=True, stdout=subprocess.PIPE)
    ps_output = ps_command.stdout.read()
    retcode = ps_command.wait()
    assert retcode == 0, "ps command returned %d" % retcode
    for line in ps_output.split("\n")[:-1]:
        pid, cmd = line.strip().split(" ", 1)
        if "ada_language_server" in cmd:
            return int(pid)
    return None


@run_test_driver
def run_test():
    # Wait for the language server to launch
    yield timeout(1000)
    ls_pid = get_language_server_pid()
    gps_assert(ls_pid is not None, True,
               "couldn't get the language server PID")

    # Kill the language server
    os.kill(ls_pid, signal.SIGKILL)

    # Wait for the language server to relaunch
    yield timeout(1000)

    # Get the new language server PID
    new_ls_pid = get_language_server_pid()
    gps_assert(new_ls_pid is not None, True,
               "couldn't get the new language server PID after kill")
    gps_assert(ls_pid != new_ls_pid, True,
               "the language server wasn't killed")

    # Verify the functionality of the new language server
    buf = GPS.EditorBuffer.get(GPS.File('main.adb'))
    buf.current_view().goto(buf.at(5, 10))

    GPS.execute_action('goto declaration')
    yield hook("language_server_response_processed")

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('hello_world.ads'),
               "'goto declaration' did not open the right file")

    # Verify that there isn't the error message in the console
    gps_assert("had to be restarted more than"
               in GPS.Console().get_text(),
               False,
               "the error message about language server showed unexpectedly")

    # Now try to kill the language server too many times
    for j in range(5):
        ls_pid = get_language_server_pid()
        if ls_pid:
            os.kill(ls_pid, signal.SIGKILL)
        yield timeout(200)

    # Verify that there is the error message in the console
    gps_assert("had to be restarted more than"
               in GPS.Console().get_text(),
               True,
               "the error message about language server showed unexpectedly")
