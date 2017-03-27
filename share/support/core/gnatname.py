"""
This file adds target and target model for the gnatname utility.
See the menu /Build/Settings/Targets to configure toolbar icons to
easily launch gnatname.
"""

import GPS
from gps_utils import interactive

XML = r"""<?xml version="1.0" ?>
<gnatname>
  <target-model name="gnatname" category="">
    <iconname>gps-build-all-symbolic</iconname>
    <description>Generic launch of gnatname</description>
    <command-line>
      <arg>%gnat</arg>
      <arg>name</arg>
      <arg>-P%PP</arg>
    </command-line>
    <switches command="gnatname" columns="1" lines="1">
      <check label="No backup files" switch="--no-backup" line="1"
             tip="Do not create backup of project files" />
    </switches>
  </target-model>

  <target model="gnatname" category="_Project_" name="gnatname">
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnat</arg>
      <arg>name</arg>
      <arg>-P%PP</arg>
    </command-line>
  </target>
</gnatname>
"""

GPS.parse_xml(XML)


def on_exit(status):
    """
    Called when gnatname exited.
    Reload the project view if it succeed.
    """

    if not status:
        GPS.execute_action("reload project")


@interactive(name="run gnatname",
             description="Ask naming patterns to the user and run gnatname " +
             "on the current project to add the files located in the " +
             "project's source directories matching these patterns " +
             "to project's sources files.")
def run_gnatname():
    """
    Run gnatname with the naming patterns entered by the user
    in a simple input dialog.
    """
    naming_patterns = GPS.MDI.input_dialog(
        "Enter the space-separated naming patterns that will be " +
        "used by gnatname to find compilation units " +
        "(e.g: 'body_* spec_*'). These files are searched among the " +
        "project's source directories.",
        "Naming Patterns")

    if naming_patterns:
        naming_patterns = ''.join(naming_patterns)
        source_dirs = GPS.Project.root().source_dirs()

        extra_args = ['-d' + source_dir for source_dir in source_dirs]
        extra_args = ' '.join(extra_args)
        extra_args += ' ' + naming_patterns

        GPS.BuildTarget("gnatname").execute(
            extra_args=extra_args,
            synchronous=False,
            on_exit=on_exit)
