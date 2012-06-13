""" Provides the tools for loading the cross-reference information in GPS.
"""

import GPS

xml = """<?xml version="1.0" ?><GPS>

<!-- This is an XML model for launching gnatinspect, the cross-references parser -->
<target-model name="gnatinspect" category="">
   <description>Launch cross-reference recompilation</description>
   <icon>gps-custom-build</icon>
   <command-line>
      <arg>gnatinspect</arg>
      <arg>--exit</arg>
      <arg>--db=gnatinspect.db</arg>
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
       <arg>--exit</arg>
       <arg>--db=gnatinspect.db</arg>
       <arg>-P%PP</arg>
       <arg>%X</arg>
    </command-line>
</target>

</GPS>
"""

def recompute_xref():
    """ Launch recompilation of the cross references """
    if not "Recompute Xref info" in [t.name() for t in GPS.Task.list()]:
        target = GPS.BuildTarget("Recompute Xref info")


        #  ??? should add <arg>--symlinks</arg> if preference "slow project loading" is activated

        target.execute(synchronous=False, quiet=True)

def on_compilation_finished(hook, category,
    target_name="", mode_name="", status=""):

    if status:
        return

    if (target_name in ["Compile File", "Build Main", "Build All",
           "Compile All Sources", "Build <current file>", "Custom Build..."]
        or category in ["Makefile"]):
        recompute_xref()

def on_project_view_changed(hook):
    recompute_xref()

def on_gps_started (hook):
    GPS.Menu.create ("/Build/Recompute _Xref info",
         on_activate=lambda x : recompute_xref())

if GPS.Preference("Internal-Use-Sqlite").get():
    GPS.parse_xml (xml)
    GPS.Hook("project_view_changed").add(on_project_view_changed)
    GPS.Hook("compilation_finished").add(on_compilation_finished)
    GPS.Hook("gps_started").add(on_gps_started)
