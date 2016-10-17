"""
Provides support for gnatmetric.
"""

import os
import GPS
from gps_utils import interactive, hook

# Initialize the targets
xml_base = ("""
<target-model name="gnatmetric" category="">
   <description>Generic launch of gnat metric</description>
   <command-line>
      <arg>%gnat</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-ox</arg>
      <arg>%O/metrix.xml</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
   </command-line>
   <iconname>gps-semantic-check-symbolic</iconname>
   <switches command="%(tool_name)s" columns="2" lines="2">
      <title line="1" column="1">Line metrics</title>
      <title line="1" column="2">Syntax element metrics</title>
      <title line="2" column="1">Complexity metrics</title>
      <title line="2" column="2">Coupling metrics</title>
<check label="All complexity metrics"
                   line="2"  column="1"
                   switch="--complexity-all"
                   tip="All complexity metrics" />
<check label="McCabe Cyclomatic Complexity"
                   line="2"  column="1"
                   switch="--complexity-cyclomatic"
                   tip="McCabe Cyclomatic Complexity" />
<check label="McCabe Essential Complexity"
                   line="2"  column="1"
                   switch="--complexity-essential"
                   tip="McCabe Essential Complexity" />
<check label="Average McCabe CC of a body"
                   line="2"  column="1"
                   switch="--complexity-average"
                   tip="Average McCabe Cyclomatic Complexity of a body" />
<check label="Maximal loop nesting level"
                   line="2"  column="1"
                   switch="--loop-nesting"
                   tip="Maximal loop nesting level" />
<check label="Do not count static loops for CC"
                   line="2"  column="1"
                   switch="--no-static-loop"
                   tip="Do not count static loops for cyclomatic complexity" />
<check label="Do not consider exit statements as gotos"
                   line="2"  column="1"
                   switch="-ne"
                   tip="Do not consider exit statements as gotos when """
            """computing Essential Complexity" />
<check label="Extra exit points in subprograms"
                   line="2"  column="1"
                   switch="--extra-exit-points"
                   tip="Extra exit points in subprograms" />
<check label="All line metrics"
                   line="1"  column="1"
                   switch="--lines-all"
                   tip="All line metrics" />
<check label="Total number of lines"
                   line="1"  column="1"
                   switch="--lines"
                   tip="Total number of lines" />
<check label="Number of code lines"
                   line="1"  column="1"
                   switch="--lines-code"
                   tip="Number of code lines" />
<check label="Number of comment lines"
                   line="1"  column="1"
                   switch="--lines-comment"
                   tip="Number of comment lines" />
<check label="Number of code lines with comments"
                   line="1"  column="1"
                   switch="--lines-eol-comment"
                   tip="Number of code lines with end-of-line comments" />
<check label="Comment lines percentage"
                   line="1"  column="1"
                   switch="--lines-ratio"
                   tip="Ratio between the number of lines that contain """
            """comments and the total number of non-blank lines" />
<check label="Number of blank lines"
                   line="1"  column="1"
                   switch="--lines-blank"
                   tip="Number of blank lines" />
<check label="Average number of code lines in bodies"
                   line="1"  column="1"
                   switch="--lines-average"
                   tip="Average number of code lines in subprogram bodies, """
            """task bodies, entry bodies and statement sequences in """
            """package bodies" />
<check label="All syntax element metrics"
                   line="1"  column="2"
                   switch="--syntax-all"
                   tip="All syntax element metrics" />
<check label="Number of declarations"
                   line="1"  column="2"
                   switch="--declarations"
                   tip="Total number of declarations" />
<check label="Number of statements"
                   line="1"  column="2"
                   switch="--statements"
                   tip="Total number of statements" />
<check label="Number of public subprograms in a unit"
                   line="1"  column="2"
                   switch="--public-subprograms"
                   tip="Number of public subprograms in a compilation unit" />
<check label="Number of subprograms in a unit"
                   line="1"  column="2"
                   switch="--all-subprograms"
                   tip="Number of subprograms in a compilation unit" />
<check label="Number of public types in a unit"
                   line="1"  column="2"
                   switch="--public-types"
                   tip="Number of public types in a compilation unit" />
<check label="Number of types in a unit"
                   line="1"  column="2"
                   switch="--all-types"
                   tip="Number of types in a compilation unit" />
<check label="Maximal unit nesting level"
                   line="1"  column="2"
                   switch="--unit-nesting"
                   tip="Maximal unit nesting level" />
<check label="Maximal construct nesting level"
                   line="1"  column="2"
                   switch="--construct-nesting"
                   tip="Maximal construct nesting level" />
<check label="All coupling metrics"
                   line="2"  column="2"
                   switch="--coupling-all"
                   tip="All coupling metrics" />
<check label="Tagged (class) fan-out coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-out"
                   tip="Tagged (class) fan-out coupling" />
<check label="Tagged (class) fan-in coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-in"
                   tip="Tagged (class) fan-in coupling" />
<check label="Hierarchy (category) fan-out coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-out"
                   tip="Hierarchy (category) fan-out coupling" />
<check label="Hierarchy (category) fan-in coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-in"
                   tip="Hierarchy (category) fan-in coupling" />
<check label="Unit fan-out coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-out"
                   tip="Unit fan-out coupling" />
<check label="Unit fan-in coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-in"
                   tip="Unit fan-in coupling" />
<check label="Control fan-out coupling"
                   line="2"  column="2"
                   switch="--control-coupling-out"
                   tip="Control fan-out coupling" />
<check label="Control fan-in coupling"
                   line="2"  column="2"
                   switch="--control-coupling-in"
                   tip="Control fan-in coupling" />
   </switches>
</target-model>

<target model="gnatmetric" category="_File_" name="GNAT Metrics for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnat</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-ox</arg>
      <arg>%O/metrix.xml</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>%F</arg>
    </command-line>
</target>

<target model="gnatmetric" category="_File_" name="GNAT Metrics for project">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnat</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-ox</arg>
      <arg>%O/metrix.xml</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
    </command-line>
</target>

<target model="gnatmetric" category="_File_" name="GNAT Metrics for """
            """project and subprojects">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnat</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-ox</arg>
      <arg>%O/metrix.xml</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>-U</arg>
    </command-line>
</target>

<action name="GNAT Metric on current file">
   <description>Launch GNAT metric on the current file</description>
   <filter language="ada" error="gnat metric requires an Ada file" />
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for file").execute(synchronous=False)</shell>
</action>

<action name="GNAT Metric on current project">
   <filter id="Project only" />
   <description>Launch GNAT metric on the current project</description>
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for project").execute(synchronous=False)
   </shell>
</action>

<action name="GNAT Metric on current project and subprojects">
   <filter id="Project only" />
   <description>Launch GNAT metric on the current project</description>
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for project and subprojects").execute"""
            """(synchronous=False)</shell>
</action>

<contextual action="GNAT metric on current project" >
   <title>Metrics/Compute metrics for project %p</title>
</contextual>

<contextual action="GNAT metric on current project and subprojects" >
   <title>Metrics/Compute metrics for project %p and subprojects</title>
</contextual>

""")


def get_metrix_file():
    """
    Compute the location of the metrix.xml file created by gnatmetrics
    :return: None if the file could not be found
    """

    metrix = os.path.join(GPS.Project.root().object_dirs()[0], "metrix.xml")
    if not os.path.isfile(metrix):
        GPS.Console().write('File %s not found' % metrix)
        return None
    else:
        return metrix


@hook('compilation_finished')
def __on_compilation_finished(category, target_name="",
                              mode_name="", status=""):
    if not target_name.startswith("GNAT Metric"):
        return

    if status:
        return

    # Try to reuse existing view, if any
    v = GPS.XMLViewer.get_existing("Metrics")

    if not v:
        v = GPS.XMLViewer.create_metric("Metrics")

    metrix = get_metrix_file()
    if metrix:
        v.parse(metrix)


@interactive(name='open metrics view')
def show_metrics_window():
    "Open the Metrics view"
    w = GPS.MDI.get("Metrics")

    if w is None:
        v = GPS.XMLViewer.create_metric("Metrics")
        metrix = get_metrix_file()
        if metrix:
            v.parse(metrix)
    else:
        w.get_child()
        w.raise_window()


GPS.parse_xml(xml_base)
