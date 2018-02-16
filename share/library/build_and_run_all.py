"""
This plugin provides a toolbar button that performs a "Build All"
before running successively all the executables that have been built.
"""

import GPS
from modules import Module
import workflows
import workflows.promises as promises


class BuildAndRunAll(Module):

    __buildTarget = None

    def __create_target_lazily(self):
        """
        Create the "Build & Run All" target.
        """

        if not self.__buildTarget:
            workflows.create_target_from_workflow(
                parent_menu='/Build/Project/',
                target_name="Build & Run All",
                workflow_name="build-and-run-all",
                workflow=self.__build_and_run_all_wf,
                icon_name="gps-run-symbolic",
                main_arg="")
            self.__buildTarget = GPS.BuildTarget("Build & Run All")

    def __show_or_hide_target(self):
        project = GPS.Project.root()
        mains = project.get_attribute_as_list(attribute='Main')

        if len(mains) > 0:
            self.__buildTarget.show()
        else:
            self.__buildTarget.hide()

    def __build_and_run_all_wf(self, main_name):
        """
        Workflow that calls "Build All" and then runs all the executables
        sequentially.
        """

        # Build All
        builder = promises.TargetWrapper("Build All")
        r0 = yield builder.wait_on_execute()
        if r0 is not 0:
            return

        # Call "Run Main Number ..." on each main
        i = 1
        while True:
            run_action = GPS.Action("Run Main Number %i" % (i))
            if not run_action.exists():
                break

            run_action.execute_if_possible()
            yield promises.wait_tasks()
            i += 1

    def project_view_changed(self):
        """
        Try to create or hide the build target when the project changes.
        """

        self.__show_or_hide_target()

    def setup(self):
        """
        When setting up the module, create the build target.
        """
        self.__create_target_lazily()
