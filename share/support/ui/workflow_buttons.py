import GPS
import workflows.promises as promises
import workflows


class WorkflowButtons(object):

    __needs_build = {}
    # Used to know if we can skip the 'Build Main' BuildTarget when calling
    # the workflows for a particular main.

    __build_targets_created = False
    # Used to know if the build targets for the build-and-run and
    # build-and-debug buttons have been created.

    __build_succeed = False
    # Used to know if the 'Build Main' BuildTarget has succeed.

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
            WorkflowButtons.__connect_editor_hooks()

            GPS.Hook('build_mode_changed').add(
                WorkflowButtons.__on_mode_or_view_changed)
            GPS.Hook('project_view_changed').add(
                WorkflowButtons.__on_mode_or_view_changed)

    @staticmethod
    def force_rebuild_main(main_name):
        """
        Force the rebuild next time 'build_main' is called.
        """

        WorkflowButtons.__needs_build[main_name] = True

    @staticmethod
    def __connect_editor_hooks():
        """
        Connect to the 'file_changed_on_disk' and 'buffer_edited' hooks to
        know if we can skip the 'Build Main' BuildTarget when executing
        the workflows or not.
        """

        GPS.Hook('compilation_finished').add(
            WorkflowButtons.__on_compilation_finished)
        GPS.Hook('file_changed_on_disk').add(
            WorkflowButtons.__on_file_changed)
        GPS.Hook('buffer_edited').add(
            WorkflowButtons.__on_file_changed)

    @staticmethod
    def __on_compilation_finished(hook, category, target_name,
                                  mode_name, status):
        """
        Called each time a Build Target is computed.

        Skip the building phase of the workflow buttons when the 'Build All'
        target is computed and force the rebuild on "Clean All".
        """
        if target_name == "Build All" and status == 0:
            for main_name in WorkflowButtons.__needs_build:
                WorkflowButtons.__needs_build[main_name] = False

        if target_name == "Clean All":
            for main_name in WorkflowButtons.__needs_build:
                WorkflowButtons.__needs_build[main_name] = True

    @staticmethod
    def __on_mode_or_view_changed(hook, build_mode=None):
        """
        Called when the build mode changes. Force the build phase in that case.
        """
        for main_name in WorkflowButtons.__needs_build:
            WorkflowButtons.force_rebuild_main(main_name)

    @staticmethod
    def __on_file_changed(hook, file):
        """
        Called each time a file has changed, either directly from GPS or from
        outside.
        """
        for main_name in WorkflowButtons.__needs_build:
            WorkflowButtons.__needs_build[main_name] = True

    @staticmethod
    @workflows.run_as_workflow
    def build_main(main_name):
        """
        Try to launch the 'Build Main' build target with the given
        :param str main_name:.

        Set __build_succeed to True when the build succeed and to False
        otherwise.
        """

        if WorkflowButtons.__needs_build.get(main_name) is None:
            WorkflowButtons.__needs_build[main_name] = True

        if not WorkflowButtons.__needs_build[main_name]:
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

        WorkflowButtons.__needs_build[main_name] = False
        WorkflowButtons.__build_succeed = True

    @staticmethod
    def __build_and_debug_wf(main_name):
        """
        Workflow that builds and initialize a debugging session on the program
        designated by :param str main_name:.
        """

        # Build the executable
        yield WorkflowButtons.build_main(main_name)
        if not WorkflowButtons.__build_succeed:
            return

        # Spawn the debugger on the executable
        exe = GPS.File(main_name).executable_path
        promises.DebuggerWrapper(exe)

    @staticmethod
    def __build_and_run_wf(main_name):
        """
        Workflow that builds and run the program designated by
        :param straight main_name:.
        """

        # Build the executable
        yield WorkflowButtons.build_main(main_name)
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
