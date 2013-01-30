""" Provides the tools for loading the cross-reference information in GPS.
"""

import GPS
import os.path

class Sqlite_Cross_References(object):
    """
    A python class to support the xref engine in GPS.
    This class takes care of running gnatinspect as needed to refresh the
    xref info.
    """

    xml = """<?xml version="1.0" ?><GPS>
<!-- This is an XML model for launching gnatinspect, the cross-references parser -->
<target-model name="gnatinspect" category="">
   <description>Launch cross-reference recompilation</description>
   <icon>gps-custom-build</icon>
   <command-line>
      <arg>gnatinspect</arg>
      <arg>-d</arg>
      <arg>--exit</arg>
      <arg>--db=gnatinspect.db</arg>
      <arg>--tracefile=%GPS/gnatinspect_traces.cfg</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <switches command="">
   </switches>
</target-model>

<!-- Targets to launch cross-reference recompilation  -->
<target model="gnatinspect" category="_Project" name="Recompute _Xref info">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <in-contextual-menus-for-projects>False</in-contextual-menus-for-projects>
    <icon>gps-compute-xref</icon>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>gnatinspect</arg>
       <arg>-d</arg>
       <arg>--exit</arg>
       <arg>--db=gnatinspect.db</arg>
       <arg>--tracefile=%GPS/gnatinspect_traces.cfg</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>
</GPS>"""

    def __init__(self):
        GPS.parse_xml(self.xml)
        GPS.Hook("project_view_changed").add(self.on_project_view_changed)
        GPS.Hook("compilation_finished").add(self.on_compilation_finished)
        GPS.Hook("gps_started").add(self.on_gps_started)
        self.gnatinspect_launch_registered = False

    def recompute_xref(self):
        """ Launch recompilation of the cross references """

        # The project might not exist, for instance when GPS is loading the
        # default project in a directory

        if not os.path.exists(GPS.Project.root().file().name()):
            return

        # If we are already recomputing Xref info, do not launch another instance
        # of gnatinspect, but register one to be launched

        tasks = GPS.Task.list()

        if tasks:
            if "Recompute Xref info" in [t.name() for t in tasks]:
                self.gnatinspect_launch_registered = True
                return

        # We are about to launch gnatinspect
        self.gnatinspect_launch_registered = False
        target = GPS.BuildTarget("Recompute Xref info")

        # ??? should add <arg>--symlinks</arg> if preference "slow project loading"
        # is activated

        target.execute(synchronous=False, quiet=True)

    def on_compilation_finished(self, hook, category,
        target_name="", mode_name="", status=""):


        if (target_name in ["Compile File", "Build Main", "Build All", "Make",
               "Compile All Sources", "Build <current file>", "Custom Build..."]
            or category in ["Makefile"]):
            self.recompute_xref()

        if (self.gnatinspect_launch_registered
            and target_name == "Recompute Xref info"):
            # A launch of gnatinspect was registered while this one was
            # running: relaunch one now.

            self.recompute_xref()

    def on_project_view_changed(self, hook):
        self.recompute_xref()

    def on_gps_started(self, hook):
        GPS.Menu.create("/Build/Recompute _Xref info",
             on_activate=lambda x : self.recompute_xref())


if GPS.Logger("ENTITIES.SQLITE").active:
    Sqlite_Cross_References()
