"""
Test the Alire setup state machine with no 'alr' to run.

The other 'alire.*' tests use a fake 'alr' on the PATH, which cannot be done on
Windows, where GNAT Studio cannot spawn a shell script. This one stands in for
the setup targets instead: it loses the coverage of the build targets themselves,
but it runs everywhere, and it reaches the paths a process can hardly be made to
take -- a failing step, a final load that does not happen, a crate switched while
Alire is still running, an abandoned 'alr' answering late, an interruption.
"""

import collections
import os
import GPS
import alire
from workflows.promises import Promise
from gs_utils.internal.utils import *

# GNAT Studio's current directory follows the project being loaded, so record
# the test's directory before anything is loaded.
TEST_DIR = os.getcwd()

# One run of a setup target: the promise the sequence is waiting on, plus what
# the real 'alr' would have been launched with and produced.
Run = collections.namedtuple("Run", "target parser promise project_file")


class FakeAlire:
    """
    Stand in for the 'alr' runs of the setup sequence, and for the output the
    real 'alr' would have produced along the way.
    """

    def __init__(self):
        self.runs = []  # (target, directory) of every run, in order
        self.project_file = "hello.gpr"  # what 'alr show' reports
        self.failing = None  # the target that should exit with a failure
        self.hold = None  # the target whose run should be held pending
        self.held = None  # the run being held, if any

    def run(self, target, directory):
        """Stand in for 'alire._run_setup_target'."""
        self.runs.append((target, directory))

        # The real output parser is created when the target is launched, and is
        # thus bound to the sequence that is running at that moment. So is the
        # output the run is going to produce.
        run = Run(target, alire.Alire_Parser(), Promise(), self.project_file)

        if target == self.hold:
            self.held = run
        else:
            self._finish(run)

        return run.promise

    def release(self, status=0):
        """Let the run being held finish, producing the output it would have."""
        run, self.held = self.held, None
        self._finish(run, status)

    def targets(self):
        """The targets run since the last call to 'forget'."""
        return [target for target, directory in self.runs]

    def directories(self):
        """The directories the targets were run in, since the last 'forget'."""
        return set(
            os.path.normcase(os.path.normpath(directory))
            for target, directory in self.runs
        )

    def forget(self):
        """Forget the runs so far, so that the next ones can be checked alone."""
        self.runs = []

    def _finish(self, run, status=None):
        if run.target == "Alire Show":
            run.parser.on_stdout(
                "  Name: %s\n  Project_File: %s\n"
                % (os.path.splitext(run.project_file)[0], run.project_file),
                None,
            )
        elif run.target == "Alire Printenv":
            # The real 'alr printenv' exports ALIRE itself, on top of the crate's
            # own variables.
            run.parser.on_stdout(
                'export ALIRE="True"\nexport ALIRE_TEST_ENV="alire_was_here"\n', None
            )

        if status is None:
            status = 1 if run.target == self.failing else 0

        run.promise.resolve(status)


def builds_with_alire():
    """
    Whether 'Build All' is aliased to its Alire counterpart, which is how
    building a crate goes through 'alr' rather than through gprbuild.
    """
    return GPS.BuildTarget("Build All").get_command_line()[0] == "alr"


def setup_tasks():
    """The tasks monitoring an Alire setup sequence."""
    return [
        task
        for task in GPS.Task.list()
        if task.name().startswith(alire.ALIRE_SETUP_TASK_NAME)
    ]


@run_test_driver
def test_driver():
    fake = FakeAlire()
    alire._run_setup_target = fake.run

    if not alire.alr:
        # No 'alr' on the PATH, so the plugin has not connected to the hooks.
        alire.register_hooks()

    crate = os.path.join(TEST_DIR, "crate")
    crate2 = os.path.join(TEST_DIR, "crate2")
    plain_project = os.path.join(TEST_DIR, "plain", "plain.gpr")

    def in_crate(name):
        return os.path.join(crate, name)

    def wait_for_setup(error_msg):
        """Wait for the sequence under way to be over, one way or another."""
        return wait_until_true(
            lambda: alire.alire_state is None, timeout=10000, error_msg=error_msg
        )

    # 1. A crate that Alire sets up without a hitch.
    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        fake.targets(),
        list(alire.ALIRE_SETUP_TARGETS),
        "The whole sequence should have been run, in order",
    )
    gps_assert(
        fake.directories(),
        {os.path.normcase(os.path.normpath(crate))},
        "Every setup target should be run in the crate's root directory",
    )
    gps_assert(
        alire.alire_state,
        None,
        "The setup sequence should be over",
    )
    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project reported by 'alr show' should have been reloaded",
    )
    gps_assert(
        GPS.getenv("ALIRE"),
        "True",
        "ALIRE should be set while an Alire crate is loaded",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment reported by 'alr printenv' should have been set",
    )
    gps_assert(
        [os.path.basename(f) for f in alire.alire_project_files],
        ["hello.gpr"],
        "The project files reported by 'alr show' should be known now",
    )
    gps_assert(
        builds_with_alire(),
        True,
        "The default build targets should go through Alire in a crate",
    )

    # 2. Loading a project of a crate that is set up needs no Alire run.
    fake.forget()
    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        fake.targets(),
        [],
        "Alire should not be run again for a project of a crate that is set up",
    )

    # 3. Leaving the crate restores its environment, so its project files are
    #    not 'already set up' anymore either.
    GPS.Project.load(plain_project)
    yield wait_tasks()

    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored when leaving the Alire crate",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "",
        "The crate's environment should have been restored",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "Leaving the crate should invalidate its known project files",
    )
    gps_assert(
        builds_with_alire(),
        False,
        "The default build targets should not go through Alire anymore",
    )

    # 4. A step that fails: the setup is given up on, and the environment it had
    #    entered is given back its former values.
    fake.forget()
    fake.failing = "Alire Printenv"
    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        fake.targets(),
        list(alire.ALIRE_SETUP_TARGETS),
        "The sequence should have run up to the failing step",
    )
    gps_assert(
        alire.alire_state,
        None,
        "The failed setup sequence should be over",
    )
    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project loaded before Alire was run should still be loaded",
    )
    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored after the failed setup",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "",
        "The environment applied before the failure should have been undone",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "A crate that is not set up should have no known project files",
    )

    # 5. A final project load that does not happen: here because 'alr show'
    #    reports a project file that is not there.
    fake.forget()
    fake.failing = None
    fake.project_file = "not_there.gpr"
    GPS.Project.load(in_crate("bye.gpr"))
    yield wait_tasks()

    gps_assert(
        alire.alire_state,
        None,
        "A final load that does not happen should not leave a state behind",
    )
    gps_assert(
        GPS.Project.root().file().base_name(),
        "bye.gpr",
        "The project loaded before Alire was run should still be loaded",
    )
    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored after the failed reload",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "",
        "The environment of a crate that is not set up should be restored",
    )

    # 6. The setup can be retried after all that.
    fake.forget()
    fake.project_file = "hello.gpr"
    GPS.Project.load(in_crate("bye.gpr"))
    yield wait_tasks()

    gps_assert(
        fake.targets(),
        list(alire.ALIRE_SETUP_TARGETS),
        "Alire should be run again for a crate whose setup has been given up on",
    )
    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project reported by 'alr show' should have been reloaded",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment reported by 'alr printenv' should have been set",
    )

    # 7. Another crate opened while Alire is still running for this one. The
    #    sequence left behind must not reload its own project, and its output,
    #    which reaches us after the fact, must not be taken for the new one's.
    fake.forget()
    fake.hold = "Alire Show"
    GPS.Project.load(in_crate("bye.gpr"))

    yield wait_until_true(
        lambda: fake.held is not None,
        timeout=10000,
        error_msg="The setup sequence has not reached 'Alire Show'",
    )

    fake.hold = None
    fake.project_file = "other.gpr"
    GPS.Project.load(os.path.join(crate2, "other.gpr"))

    # Wait for the sequence of the second crate to be over: its task is gone
    # once it is, whereas the abandoned one is still holding its 'Alire Show'.
    yield wait_until_true(
        lambda: len(setup_tasks()) == 1,
        timeout=10000,
        error_msg="The second crate has not been set up",
    )

    gps_assert(
        GPS.Project.root().name(),
        "Other",
        "The project of the crate we switched to should have been reloaded",
    )
    expected_files = [os.path.join(crate2, "other.gpr")]
    gps_assert(
        alire.alire_project_files,
        expected_files,
        "Only the project files of the crate being set up should be known",
    )

    # Now let the abandoned 'Alire Show' answer, with the output it would have
    # produced for the crate we have left.
    fake.release()
    yield wait_tasks()

    gps_assert(
        GPS.Project.root().name(),
        "Other",
        "The abandoned sequence should not have reloaded its own project",
    )
    gps_assert(
        alire.alire_pending_project_files,
        expected_files,
        "The output of an abandoned 'alr' should not be parsed",
    )
    gps_assert(
        alire.project_to_reload,
        None,
        "The output of an abandoned 'alr' should not ask for a reload",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment of the crate we switched to should still be set",
    )

    # 8. An interruption from the Task Manager, which the sequence is never
    #    given a chance to notice by itself.
    fake.forget()
    fake.hold = "Alire Sync"
    GPS.Project.load(in_crate("hello.gpr"))

    yield wait_until_true(
        lambda: fake.held is not None,
        timeout=10000,
        error_msg="The setup sequence has not reached 'Alire Sync'",
    )

    tasks = setup_tasks()
    gps_assert(
        len(tasks),
        1,
        "The Alire setup should be monitored by a task while it runs",
    )
    tasks[0].interrupt()

    yield wait_for_setup("The interrupted Alire setup has not been cleaned up")

    gps_assert(
        fake.targets(),
        ["Alire Sync"],
        "The interrupted sequence should not have gone any further",
    )
    gps_assert(
        GPS.getenv("ALIRE"),
        "",
        "ALIRE should have been restored after the interruption",
    )
    gps_assert(
        alire.saved_env,
        {},
        "No environment should be left to restore",
    )
    gps_assert(
        alire.alire_project_files,
        [],
        "A crate that is not set up should have no known project files",
    )
    gps_assert(
        setup_tasks(),
        [],
        "The interrupted task should be gone",
    )

    # 9. And the plugin is none the worse for the interruption.
    fake.forget()
    fake.hold = None
    fake.project_file = "hello.gpr"
    fake.release()
    GPS.Project.load(in_crate("hello.gpr"))
    yield wait_tasks()

    gps_assert(
        fake.targets(),
        list(alire.ALIRE_SETUP_TARGETS),
        "Alire should be run again after an interrupted setup",
    )
    gps_assert(
        GPS.Project.root().file().base_name(),
        "hello.gpr",
        "The project reported by 'alr show' should have been reloaded",
    )
    gps_assert(
        GPS.getenv("ALIRE_TEST_ENV"),
        "alire_was_here",
        "The environment reported by 'alr printenv' should have been set",
    )
