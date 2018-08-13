"""
This plugins defined utilities for the c preprocessor (cpp):

- target and target model
- Analyze/Preprocessor/run cpp on current file:
    an action to display preprocessed the current file and display the output
"""

import GPS
from gps_utils import interactive

XML = """
  <target-model name="c preprocessor">
    <iconname>gps-build-all-symbolic</iconname>
    <description>Run the c preprocessor</description>
    <command-line>
      <arg>%builder</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>-f</arg>
      <arg>-c</arg>
      <arg>-cargs</arg>
      <arg>-E</arg>
    </command-line>
    <switches command="%(tool_name)s" columns="1">
      <title line="1" column="1">Switches</title>
      <check line="1" column="1" label="Keep comments" switch="-CC"
        tip="Do not discard the comments"/>
      <check line="1" column="1" label="Header files' names" switch="-H"
        tip="Print the header files' names"/>
      <check line="1" column="1" label="Traditional format"
        switch="-traditional" tip="Use the traditional format"/>
      <check line="1" column="1" label="Display macros"
        switch="-dD" tip="Display the preprocessing macros"/>
    </switches>
  </target-model>

  <target name="c preprocess file" category="CPP" model="c preprocessor">
    <read-only>TRUE</read-only>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <in-menu>FALSE</in-menu>
    <command-line>
      <arg>%builder</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>-f</arg>
      <arg>-c</arg>
      <arg>-u %F</arg>
      <arg>-cargs</arg>
      <arg>-E</arg>
    </command-line>
  </target>
"""

GPS.parse_xml(XML)


def is_c_file(dummy):
    if GPS.current_context().file():
        return GPS.current_context().file().language() == "c"
    else:
        return False


@interactive(name="run cpp on file",
             description="Run the gcc preprocessor on current file",
             filter=is_c_file,
             menu="Analyze/Preprocessor/cpp on current file")
def cpp_on_file():
    obj_dir = GPS.Project.root().artifacts_dir()
    file = GPS.current_context().file()
    # Put the generated file in the object dir and for "foo.c"
    # creates "foo.prep.c"
    output_file = file.path.replace(file.directory(), obj_dir)
    splitted = output_file.split(".")
    output_file = ".".join(splitted[:-1]) + ".prep." + splitted[-1]
    output = "-o " + output_file

    def on_exit(status):
        if not status:
            GPS.EditorBuffer.get(GPS.File(output_file))
    target = GPS.BuildTarget("c preprocess file")
    target.execute(synchronous=False, on_exit=on_exit, extra_args=output)
