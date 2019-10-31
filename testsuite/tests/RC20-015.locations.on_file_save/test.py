"""
This test verifies that the Locations view is properly clean between runs
triggered by a build target with on_file_save.
"""

from GPS import *
from gs_utils.internal.utils import *

RUNS = {"first":
        ['Builder results (8 items in 1 file)',
            ['foo.adb (8 items)',
                ['<b>6:10</b>      warning: null value not allowed here',
                 '<b>6:10</b>      warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time',
                 '<b>6:22</b>      warning: null value not allowed here',
                 '<b>6:22</b>      warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time',
                 '<b>10:10</b>     warning: null value not allowed here',
                 '<b>10:10</b>     warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time',
                 '<b>10:22</b>     warning: null value not allowed here',
                 '<b>10:22</b>     warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time']]],
        "second":
        ['Builder results (4 items in 1 file)',
            ['foo.adb (4 items)',
                ['<b>10:10</b>     warning: null value not allowed here',
                 '<b>10:10</b>     warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time',
                 '<b>10:22</b>     warning: null value not allowed here',
                 '<b>10:22</b>     warning: &quot;Constraint_Error&quot;' +
                 ' will be raised at run time']]],
        "third": []}


TARGET = """
 <target-model name="Foo" category="Bar">
   <description>Generic GNAT builder</description>
   <command-line>
      <arg>%builder</arg>
      <arg>-d</arg>
      <arg>%eL</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
   </command-line>
   <iconname>gps-build-all-symbolic</iconname>
</target-model>


<target model="Foo" category="Bar" name="Hello world">
    <in-toolbar>TRUE</in-toolbar>
    <iconname>gps-build-all-symbolic</iconname>
    <launch-mode>ON_FILE_SAVE</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
       <arg>%builder</arg>
       <arg>-d</arg>
       <arg>%eL</arg>
       <arg>-P%PP</arg>
       <arg>%config</arg>
       <arg>%autoconf</arg>
       <arg>%X</arg>
       <arg>-largs</arg>
       <arg>-Wl,-Map=map.txt</arg>
    </command-line>
</target>
"""


def modify_and_save(buf, loc, text, run):
    buf.insert(loc, text)
    buf.save(interactive=False)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(dump_locations_tree(),
               RUNS[run],
               "Wrong content in the Locations view after " + run + " save")


@run_test_driver
def run_test():
    # Create the Build target
    GPS.parse_xml(TARGET)
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    view = buf.current_view()
    yield modify_and_save(buf, buf.at(17, 21), "--  Comment", "first")
    yield modify_and_save(buf, buf.at(5, 15), "/", "second")
    yield modify_and_save(buf, buf.at(9, 15), "/", "third")
