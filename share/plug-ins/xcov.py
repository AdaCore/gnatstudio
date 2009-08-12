""" Provides the "Tools/Run program under Xcov" menu, which executes xcov
automatically.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os.path, os_utils;

def on_xcov_run (menu):
    GPS.BuildTarget ("Run program under Xcov").execute (menu.data)

def on_compute_build_targets (hook, name):
    global xcov_run_menu
    global xcov_mode

    project = GPS.Project.root ();

    if name == "executable":
        if xcov_run_menu:
            xcov_run_menu.destroy ()
            xcov_run_menu = None

        xcov_run_menu = \
            GPS.Menu.create ("/Tools/Covera_ge/Run program under Xcov",
                            ref="Show report",
                            add_before=True)

        if not xcov_mode:
            xcov_run_menu.hide ()

        def add_programs (project):
            for main in project.get_attribute_as_list ("main"):
                exec_name = project.get_executable_name (GPS.File (main))
                exec_dir = project.get_attribute_as_string ("exec_dir")

                if exec_dir [0] == '.':
                    exec_dir = os.path.join (project.file ().directory (),
                                             exec_dir);

                exec_file = os.path.join (exec_dir, exec_name)

                menu = \
                    GPS.Menu.create \
                       ("/Tools/Covera_ge/Run program under Xcov/" + main,
                        on_activate=on_xcov_run)
                menu.data = exec_file

        for p in project.dependencies (True):
            add_programs (p)

    return None

def on_preferences_changed (hook):
    global xcov_run_menu
    global xcov_mode

    xcov_mode = GPS.Preference("coverage-toolchain").get() == "Xcov"

    if xcov_mode:
        xcov_run_menu.show ();

    else:
        xcov_run_menu.hide ();

def on_gps_started (hook_name):
    global xcov_run_menu
    global xcov_mode

    xcov_run_menu = None
    xcov_mode = GPS.Preference("coverage-toolchain").get() == "Xcov"

    GPS.Hook ("compute_build_targets").add (on_compute_build_targets)
    GPS.Hook ("preferences_changed").add (on_preferences_changed)

#  Check for Xcov

if os_utils.locate_exec_on_path ("xcov") != "":
    GPS.parse_xml ("""
  <target-model name="xcov-run" category="">
    <description>Code coverage with Xcov</description>
    <command-line>
      <arg>xcov</arg>
      <arg>--run</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="2" lines="2">
      <combo label="Target" switch="--target" separator="=" column="1" line="1">
        <combo-entry label="powerpc-elf" value="powerpc-elf"/>
        <combo-entry label="leon-elf" value="leon-elf"/>
        <combo-entry label="i386-pok" value="i386-pok"/>
        <combo-entry label="i386-linux" value="i386-linux"/>
        <combo-entry label="prepare" value="prepare"/>
      </combo>
      <field label="Traces file" switch="--output" separator="=" as-file="true" column="1" line="2"/>
      <field label="Tag" switch="--tag" separator="=" column="2" line="1"/>
      <check label="Verbose" switch="--verbose" column="2" line="2"/>
    </switches>
  </target-model>

  <target model="xcov-run" category="Run under Xcov" name="Run program under Xcov">
    <target-type>executable</target-type>
    <in-toolbar>TRUE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>FALSE</read-only>
    <command-line>
      <arg>xcov</arg>
      <arg>--run</arg>
      <arg>--target=powerpc-elf</arg>
      <arg>%TT</arg>
    </command-line>
  </target>
""")
    GPS.Hook ("gps_started").add (on_gps_started)
