""" Provides the "Tools/Run program under Xcov" menu, which executes xcov
automatically.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os.path, os_utils;

def on_xcov_run (menu):
    GPS.BuildTarget ("Run program under Xcov").execute (menu.data)

def on_xcov_report (menu):
    GPS.BuildTarget ("Generate Xcov coverage report").execute ()

def update_xcov_run_menu ():
    global xcov_run_menu
    global xcov_mode

    project = GPS.Project.root ();

    if xcov_run_menu:
        xcov_run_menu.destroy ()
        xcov_run_menu = None

    xcov_run_menu = \
        GPS.Menu.create ("/Tools/Covera_ge/Run program under Xcov",
                        ref="Generate coverage report",
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

def on_project_view_changed (hook):
    update_xcov_run_menu ()

def on_preferences_changed (hook):
    global xcov_run_menu
    global xcov_report_menu
    global xcov_mode

    xcov_mode = GPS.Preference("coverage-toolchain").get() == "Xcov"

    if xcov_mode:
        if xcov_run_menu:
            xcov_run_menu.show ();
        xcov_report_menu.show ()

    else:
        if xcov_run_menu:
            xcov_run_menu.hide ();
        xcov_report_menu.hide ()

def on_gps_started (hook_name):
    global xcov_run_menu
    global xcov_mode
    global xcov_report_menu

    xcov_run_menu = None
    xcov_report_menu = \
        GPS.Menu.create ("/Tools/Covera_ge/Generate coverage report",
                         on_activate=on_xcov_report,
                         ref="Show report",
                         add_before=True)
    xcov_mode = GPS.Preference("coverage-toolchain").get() == "Xcov"

    update_xcov_run_menu ()

    GPS.Hook ("preferences_changed").add (on_preferences_changed)
    GPS.Hook ("project_view_changed").add (on_project_view_changed)

#  Check for Xcov

if os_utils.locate_exec_on_path ("xcov") != "":
    GPS.parse_xml ("""
  <!--  Program execution under instrumented execution environment  -->

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

  <target model="xcov-run" category="Run program under Xcov" name="Run program under Xcov">
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>xcov</arg>
      <arg>--run</arg>
      <arg>--target=powerpc-elf</arg>
      <arg>%TT</arg>
    </command-line>
  </target>

  <!--  Coverage report generation  -->

  <target-model name="xcov-coverage" category="">
    <description>Code coverage with Xcov</description>
    <command-line>
      <arg>xcov</arg>
      <arg>--coverage=insn</arg>
      <arg>--annotate=xcov</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="3" lines="5">
      <combo label="Coverage" switch="--coverage" separator="=" column="1">
        <combo-entry label="Intruction" value="insn" title="Object Instruction Coverage"/>
        <combo-entry label="Branch" value="branch" title="Object Branch Coverage"/>
      </combo>
      <combo label="Annotate" switch="--annotate" separator="=" column="1">
<!--        <combo-entry label="Assembler" value="asm"/>  -->
        <combo-entry label="Xcov" value="xcov"/>
<!--        <combo-entry label="HTML Report" value="html"/>  -->
        <combo-entry label="Xcov + Assembler" value="xcov+asm"/>
<!--        <combo-entry label="HTML Report + Assembler" value="html+asm"/>  -->
<!--        <combo-entry label="Report" value="report"/>  -->
      </combo>
      <field label="Routine list" switch="--routine-list" separator="=" as-file="true"/>
<!--
      <field label="Trace file" switch=" " separator="" as-file="true"/>
-->
    </switches>
  </target-model>

  <target model="xcov-coverage" category="Generate Xcov coverage report" name="Generate Xcov coverage report">
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <command-line>
      <arg>xcov</arg>
      <arg>--coverage=insn</arg>
      <arg>--annotate=xcov</arg>
    </command-line>
  </target>
""")
    GPS.Hook ("gps_started").add (on_gps_started)
