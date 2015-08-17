""" Provides the tools for loading the cross-reference information in GPS.
"""

import GPS
import os.path
import tool_output


class Sqlite_Cross_References(object):

    """
    A python class to support the xref engine in GPS.
    This class takes care of running gnatinspect as needed to refresh the
    xref info.
    """

    disable_gnatinspect = False
    # If true, gnatinspect is never run. This should only be used for the
    # testsuite in some cases, since this also breaks all cross-referenes.

    xml = """<?xml version="1.0" ?><GPS>
<!-- This is an XML model for launching gnatinspect -->
<target-model name="gnatinspect" category="">
   <description>Launch cross-reference recompilation</description>
   <iconname>gps-custom-build-symbolic</iconname>
   <server>GPS_Server</server>
   <output-parsers>
       console_writer
       gnatinspect_onexit_hook
   </output-parsers>
   <is-run>True</is-run>
   <switches command=""/>
</target-model>

<!-- Targets to launch cross-reference recompilation  -->
<target model="gnatinspect" category="_Project" name="Recompute _Xref info">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <in-contextual-menus-for-projects>False</in-contextual-menus-for-projects>
    <iconname>gps-compute-xref-symbolic</iconname>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <server>GPS_Server</server>
    <output-parsers>
        output_chopper
        utf_converter
        progress_parser
        console_writer
        gnatinspect_onexit_hook
        end_of_build
    </output-parsers>
    <command-line>
       <arg>%system_bin_dir/gnatinspect</arg>
       <arg>-d</arg>
       <arg>--exit</arg>
       <arg>--tracefile=%GPS/gnatinspect_traces.cfg</arg>
       <arg>--config=%O/gpsauto.cgpr</arg>
       <arg>--encoding=iso-8859-1</arg>
       <arg>--check_db_version</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
       <arg>%subdirsarg</arg>
    </command-line>
</target>
</GPS>
"""

    def __init__(self):
        # Whether we trust that there are no links in the project hierarchy
        self.trusted_mode = True

        GPS.parse_xml(self.xml)
        GPS.Hook("project_view_changed").add(self.on_project_view_changed)
        GPS.Hook("compilation_finished").add(self.on_compilation_finished)
        GPS.Hook("preferences_changed").add(self.on_preferences_changed)
        GPS.Hook("rsync_finished").add(self.on_rsync_finished)
        self.gnatinspect_launch_registered = False

        # Initialize self.trusted_mode and other preferences
        self.on_preferences_changed(None)

    def recompute_xref(self):
        """ Launch recompilation of the cross references """

        # The testsuite can disable gnatinspect in some cases.
        # Similarly if the DB xref is frozen, do nothing.
        if self.disable_gnatinspect:
            return

        # The project might not exist, for instance when GPS is loading the
        # default project in a directory

        if not os.path.exists(GPS.Project.root().file().name()):
            return

        # If we are already recomputing Xref info, do not launch another
        # instance of gnatinspect, but register one to be launched

        tasks = GPS.Task.list()

        if tasks:
            if "Recompute Xref info" in [t.name() for t in tasks]:
                self.gnatinspect_launch_registered = True
                return

        # We are about to launch gnatinspect
        self.gnatinspect_launch_registered = False
        target = GPS.BuildTarget("Recompute Xref info")

        # This might fail if we have spaces in the name of the directory, but
        # any quoting we do here is passed directly to gnatinspect, and the
        # switch will not be handled correctly.
        extra_args = ['--db=%s' % (GPS.xref_db(), )]
        if not self.trusted_mode:
            extra_args.append("--symlinks")

        target.execute(synchronous=GPS.Logger(
            "TESTSUITE").active, quiet=True, extra_args=extra_args)

    def on_compilation_finished(self, hook, category,
                                target_name="", mode_name="", status=""):

        if (target_name in ["Compile File", "Build Main",
                            "Build All", "Make", "Compile All Sources",
                            "Build <current file>", "Custom Build...",
                            "Check Semantic", "Update file XRef",
                            "Update file XRef in background"]
                or category in ["Makefile", "CodePeer"]):
            self.recompute_xref()

        if (self.gnatinspect_launch_registered
                and target_name == "Recompute Xref info"):
            # A launch of gnatinspect was registered while this one was
            # running: relaunch one now.

            self.recompute_xref()

    def on_project_view_changed(self, hook):
        self.recompute_xref()

    def on_preferences_changed(self, hook_name):
        self.trusted_mode = GPS.Preference("Prj-Editor-Trusted-Mode").get()

    def on_rsync_finished(self, hook):
        self.recompute_xref()


class GnatInspect_OnExit_Hook(tool_output.OutputParser):
    name = "gnatinspect_onexit_hook"

    def on_exit(self, status, command):
        if status != 0:
            GPS.Logger("XREF").log(
                "gnatinspect returned with status %s" % status)

        GPS.Hook("xref_updated").run()


r = Sqlite_Cross_References()
