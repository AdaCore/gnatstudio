import GPS
from modules import Module
import workflows.promises as promises
import workflows
from gps_utils import hook


class WorkflowButtons(object):

    __build_targets_created = False
    # Used to know if the build targets for the build-and-run and
    # build-and-debug buttons have been created.

    __build_succeed = False
    # Used to know if the 'Build Main' BuildTarget has succeed.

    __needs_build = True
    # Used to know if we can skip the 'Build Main' BuildTarget when calling
    # the workflows.

    @staticmethod
    def setup():
        """
        Create the build-and-run and debug-and-run workflow buttons.
        """

        if not WorkflowButtons.__build_targets_created:
            targets_def = [
                ["Build & Run", "build-and-run",
                 WorkflowButtons.__build_and_run_wf,
                 "gps-run-symbolic"],
                ["Build & Debug", "build-and-debug",
                 WorkflowButtons.__build_and_debug_wf,
                 "gps-debugger-initialize-symbolic"]]

            for target in targets_def:
                workflows.create_target_from_workflow(
                    target_name=target[0],
                    workflow_name=target[1],
                    workflow=target[2],
                    icon_name=target[3],
                    parent_menu='/Build/Project/%s/' % target[0])

            WorkflowButtons.__build_targets_created = True
            WorkflowButtons.__connect_hooks()

    @staticmethod
    def __connect_hooks():
        """
        Connect to the 'file_changed_on_disk' and 'buffer_edited' hooks to
        know if we can skip the 'Build Main' BuildTarget when executing
        the workflows or not.
        """

        GPS.Hook('file_changed_on_disk').add(
            WorkflowButtons.__on_file_changed)
        GPS.Hook('buffer_edited').add(
            WorkflowButtons.__on_file_changed)

    @staticmethod
    def __disconnect_hooks():
        """
        Disconnect our hook function from the 'file_changed_on_disk' and
        'buffer_edited' hooks.
        """

        GPS.Hook('file_changed_on_disk').remove(
            WorkflowButtons.__on_file_changed)
        GPS.Hook('buffer_edited').remove(
            WorkflowButtons.__on_file_changed)

    @staticmethod
    def __on_file_changed(hook, file):
        """
        Called each time a file has changed, either directly from GPS or from
        outside.
        """
        WorkflowButtons.__needs_build = True

        # Disconnect our hook function from the 'buffer_edited' hook functions
        # to avoid calling it each time a buffer is modified: we now assume
        # that the 'Build Main' BuildTarget should not be skipped when calling
        # the workflows.
        WorkflowButtons.__disconnect_hooks()

    @staticmethod
    @workflows.run_as_workflow
    def __build_main(main_name):
        """
        Try to launch the 'Build Main' build target with the given
        :param str main_name:.

        Set __build_succeed to True when the build succeed and to False
        otherwise.
        """
        if not WorkflowButtons.__needs_build:
            WorkflowButtons.__build_succeed = True
            return

        if not main_name:
            WorkflowButtons.__display_error("Main is not specified")
            WorkflowButtons.__build_succeed = False
            return

        # Build the executable
        builder = promises.TargetWrapper("Build Main")
        r0 = yield builder.wait_on_execute(main_name)
        if r0 is not 0:
            WorkflowButtons.__build_succeed = False
            return

        WorkflowButtons.__needs_build = False
        WorkflowButtons.__build_succeed = True

        # Reconnect to the hooks to know if we can skip the 'Build Main'
        # BuildTarget next time.
        WorkflowButtons.__connect_hooks()

    @staticmethod
    def __build_and_debug_wf(main_name):
        """
        Workflow that builds and initialize a debugging session on the program
        designated by :param str main_name:.
        """

        # Build the executable
        yield WorkflowButtons.__build_main(main_name)
        if not WorkflowButtons.__build_succeed:
            return

        # Spawn the debugger on the executable
        exe = GPS.File(main_name).executable_path
        debugger_promise = promises.DebuggerWrapper(exe)

    @staticmethod
    def __build_and_run_wf(main_name):
        """
        Workflow that builds and run the program designated by
        :param straight main_name:.
        """

        # Build the executable
        yield WorkflowButtons.__build_main(main_name)
        if not WorkflowButtons.__build_succeed:
            return

        # Run it
        exe = GPS.File(main_name).executable_path
        runner = promises.TargetWrapper("Run Main")
        yield runner.wait_on_execute(str(exe))

    @staticmethod
    def __display_error(msg=""):
        """
        Display an error message in the console
        """

        GPS.Console("Messages").write(
            msg + " [workflow stopped]",
            mode="error")


WorkflowButtons.setup()
