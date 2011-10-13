""" Provides Xcov related menus under Tools->Coverage.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS, os.path, os_utils;

def on_preferences_changed (name):
   global xcov_menu_separator

   xcov_menu = GPS.Menu.get ("/Tools/Coverage/Xcov")
   toolchain = GPS.Preference("Coverage-Toolchain").get()

   if toolchain == "Xcov":
      xcov_menu.show()
      xcov_menu_separator.show()

   else:
      xcov_menu.hide()
      xcov_menu_separator.hide()

#  Check for Xcov

def on_gps_started (hook_name):
  global xcov_menu_separator

  pref = GPS.Preference ("Coverage-Toolchain")

  if os_utils.locate_exec_on_path ("xcov") != "":
    xcov_menu_separator = GPS.Menu.create ("/Tools/Covera_ge/-")
    GPS.parse_xml ("""
  <!--  Program execution under instrumented execution environment  -->

  <target-model name="xcov-run" category="">
    <description>Run under Xcov for code coverage</description>
    <command-line>
      <arg>xcov</arg>
      <arg>run</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="2" lines="2">
      <combo label="Target" switch="--target" separator="=" column="1">
        <combo-entry label="powerpc-elf" value="powerpc-elf"/>
        <combo-entry label="leon-elf" value="leon-elf"/>
        <combo-entry label="i386-pok" value="i386-pok"/>
        <combo-entry label="i386-linux" value="i386-linux"/>
        <combo-entry label="prepare" value="prepare"/>
      </combo>
      <field label="Tag" switch="--tag" separator="=" column="2"/>
      <field label="Trace file" switch="-o" separator=" " as-file="true" column="1"/>
      <check label="Verbose" switch="--verbose" column="2"/>
    </switches>
  </target-model>

  <target model="xcov-run" category="Run with Xcov" name="Run under Xcov"
          menu="/Tools/Coverage/Xcov/">
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <command-line>
      <arg>xcov</arg>
      <arg>run</arg>
      <arg>--target=powerpc-elf</arg>
      <arg>%TT</arg>
      <arg>-o</arg>
      <arg>%TT.trace</arg>
    </command-line>
  </target>

  <!--  Coverage report generation  -->

  <target-model name="xcov-coverage" category="">
    <description>Code coverage with Xcov</description>
    <command-line>
      <arg>xcov</arg>
      <arg>coverage</arg>
      <arg>--level=insn</arg>
      <arg>--annotate=xcov</arg>
    </command-line>
    <icon>gps-build-all</icon>
    <switches command="%(tool_name)s" columns="1" lines="4">
      <combo label="Coverage Level" switch="--level" separator="=" column="1">
        <combo-entry label="Instruction" value="insn"
                     title="Object Instruction Coverage"/>
        <combo-entry label="Branch" value="branch"
                     title="Object Branch Coverage"/>
        <combo-entry label="Statement" value="stmt"
                     title="Source Statement Coverage"/>
        <combo-entry label="Decision" value="stmt+decision"
                     title="Source Decision Coverage"/>
        <combo-entry label="MCDC" value="stmt+mcdc"
                     title="Source MCDC Coverage"/>
      </combo>
      <combo label="Annotate" switch="--annotate" separator="=" column="1">
        <combo-entry label="Xcov" value="xcov"/>
        <combo-entry label="Xcov + Annotations" value="xcov+"/>
      </combo>
      <field label="SCO list" switch="--scos=" separator="@" as-file="true"/>
      <field label="Routine list" switch="--routines=" separator="@"
             as-file="true"/>
      <field label="Trace file" switch="-T" separator=" " as-file="true"/>
    </switches>
  </target-model>

  <target model="xcov-coverage" category="Coverage with Xcov"
          name="Generate Xcov Main Report" menu="/Tools/Coverage/Xcov/">
    <target-type>executable</target-type>
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <command-line>
      <arg>xcov</arg>
      <arg>coverage</arg>
      <arg>--level=insn</arg>
      <arg>--annotate=xcov</arg>
      <arg>--output-dir=%O</arg>
      <arg>-T</arg>
      <arg>%TT.trace</arg>
    </command-line>
  </target>

  <target model="xcov-coverage" category="Coverage with Xcov"
          name="Custom Xcov Report..." menu="/Tools/Coverage/Xcov/">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>TRUE</in-menu>
    <read-only>TRUE</read-only>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <command-line>
      <arg>xcov</arg>
      <arg>coverage</arg>
      <arg>--level=insn</arg>
      <arg>--annotate=xcov</arg>
      <arg>--output-dir=%O</arg>
      <arg>-T</arg>
      <arg>&lt;unknown&gt;</arg>
    </command-line>
  </target>""")

    GPS.Hook ("preferences_changed").add (on_preferences_changed)
    on_preferences_changed("preferences_changed")

GPS.Hook ("gps_started").add (on_gps_started)
