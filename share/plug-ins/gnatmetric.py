import re
import GPS

# Initialize the targets
xml_base = """
<target-model name="gnatmetrics" category="">
   <description>Generic launch of gnat metrics</description>
   <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
   </command-line>
   <icon>gps-semantic-check</icon>
   <switches command="%(tool_name)s" columns="2" lines="2">
      <title column="1" line="1">Complexity metrics</title>
      <title column="1" line="2">Line metrics</title>
      <title column="2" line="1">Syntax element metrics</title>
      <title column="2" line="2">Coupling metrics</title>
<check label="all complexity metrics"
                   line="1"  column="1"
                   switch="--complexity-all"
                   tip="all complexity metrics" />
<check label="McCabe Cyclomatic Complexity"
                   line="1"  column="1"
                   switch="--complexity-cyclomatic"
                   tip="McCabe Cyclomatic Complexity" />
<check label="Essential Complexity"
                   line="1"  column="1"
                   switch="--complexity-essential"
                   tip="Essential Complexity" />
<check label="average McCabe Cyclomatic Complexity of a body"
                   line="1"  column="1"
                   switch="--complexity-average"
                   tip="average McCabe Cyclomatic Complexity of a body" />
<check label="maximal loop nesting level"
                   line="1"  column="1"
                   switch="--loop-nesting"
                   tip="maximal loop nesting level" />
<check label="do not count static loops for cyclomatic complexity"
                   line="1"  column="1"
                   switch="--no-static-loop"
                   tip="do not count static loops for cyclomatic complexity" />
<check label="do not consider exit statements as gotos"
                   line="1"  column="1"
                   switch="-ne"
                   tip="do not consider exit statements as gotos when computing Essential Complexity" />
<check label="extra exit points in subprograms"
                   line="1"  column="1"
                   switch="--extra-exit-points"
                   tip="extra exit points in subprograms" />
<check label="all line metrics"
                   line="1"  column="2"
                   switch="--lines-all"
                   tip="all line metrics" />
<check label="number of all lines"
                   line="1"  column="2"
                   switch="--lines"
                   tip="number of all lines" />
<check label="number of code lines"
                   line="1"  column="2"
                   switch="--lines-code"
                   tip="number of code lines" />
<check label="number of comment lines"
                   line="1"  column="2"
                   switch="--lines-comment"
                   tip="number of comment lines" />
<check label="number of code lines also containing comments"
                   line="1"  column="2"
                   switch="--lines-eol-comment"
                   tip="number of code lines also containing comments" />
<check label="comment/code lines percentage"
                   line="1"  column="2"
                   switch="--lines-ratio"
                   tip="comment/code lines percentage" />
<check label="number of blank lines"
                   line="1"  column="2"
                   switch="--lines-blank"
                   tip="number of blank lines" />
<check label="average number of code lines in a body"
                   line="1"  column="2"
                   switch="--lines-average"
                   tip="average number of code lines in a body" />
<check label="all syntax element metrics"
                   line="2"  column="1"
                   switch="--syntax-all"
                   tip="all syntax element metrics" />
<check label="total number of declarations"
                   line="2"  column="1"
                   switch="--declarations"
                   tip="total number of declarations" />
<check label="total number of statements"
                   line="2"  column="1"
                   switch="--statements"
                   tip="total number of statements" />
<check label="number of public subprograms in a compilation unit"
                   line="2"  column="1"
                   switch="--public-subprograms"
                   tip="number of public subprograms in a compilation unit" />
<check label="number of subprograms in a compilation unit"
                   line="2"  column="1"
                   switch="--all-subprograms"
                   tip="number of subprograms in a compilation unit" />
<check label="number of public types in a compilation unit"
                   line="2"  column="1"
                   switch="--public-types"
                   tip="number of public types in a compilation unit" />
<check label="number of types in a compilation unit"
                   line="2"  column="1"
                   switch="--all-types"
                   tip="number of types in a compilation unit" />
<check label="maximal unit nesting level"
                   line="2"  column="1"
                   switch="--unit-nesting"
                   tip="maximal unit nesting level" />
<check label="maximal construct nesting level"
                   line="2"  column="1"
                   switch="--construct-nesting"
                   tip="maximal construct nesting level" />
<check label="all coupling metrics"
                   line="2"  column="2"
                   switch="--coupling-all"
                   tip="all coupling metrics" />
<check label="tagged (class) fan-out coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-out"
                   tip="tagged (class) fan-out coupling" />
<check label="tagged (class) fan-in coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-in"
                   tip="tagged (class) fan-in coupling" />
<check label="hierarchy (category) fan-out coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-out"
                   tip="hierarchy (category) fan-out coupling" />
<check label="hierarchy (category) fan-in coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-in"
                   tip="hierarchy (category) fan-in coupling" />
<check label="unit fan-out coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-out"
                   tip="unit fan-out coupling" />
<check label="unit fan-in coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-in"
                   tip="unit fan-in coupling" />
<check label="control fan-out coupling"
                   line="2"  column="2"
                   switch="--control-coupling-out"
                   tip="control fan-out coupling" />
<check label="control fan-in coupling"
                   line="2"  column="2"
                   switch="--control-coupling-in"
                   tip="control fan-in coupling" />
   </switches>
</target-model>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>%F</arg>
    </command-line>
</target>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for project">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
    </command-line>
</target>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for project and subprojects">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide'gnat,gnat)</arg>
      <arg>metric</arg>
      <arg>-dd</arg>
      <arg>-x</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>-U</arg>
    </command-line>
</target>

<action name="GNAT Metric on current file">
   <description>Launch GNAT metric on the current file</description>
   <filter language="ada" error="GNAT metrics requires an Ada file" />
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for file").execute(synchronous=False)</shell>
</action>

<action name="GNAT Metric on current project">
   <filter id="Project only" />
   <description>Launch GNAT metric on the current project</description>
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for project").execute(synchronous=False)</shell>
</action>

<action name="GNAT Metric on current project and subprojects">
   <filter id="Project only" />
   <description>Launch GNAT metric on the current project</description>
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for project and subprojects").execute(synchronous=False)</shell>
</action>

<submenu>
   <title>Tools</title>
    <submenu after="Macro">
      <title>_Metrics</title>
      <menu action="GNAT Metric on current file">
         <title>Compute metrics on current file</title>
      </menu>
      <menu action="GNAT Metric on current project">
         <title>Compute metrics on current p_roject</title>
      </menu>
      <menu action="GNAT Metric on current project and subprojects">
         <title>Compute metrics on current project and _subprojects</title>
      </menu>
    </submenu>
</submenu>

<contextual action="GNAT metric on current project" >
   <title>Metrics/Compute metrics for project %p</title>
</contextual>

<contextual action="GNAT metric on current project and subprojects" >
   <title>Metrics/Compute metrics for project %p and subprojects</title>
</contextual>

"""

def on_compilation_finished(hook, category,
    target_name="", mode_name="", status=""):

    if not target_name.startswith("GNAT Metric"):
        return

    if status:
        return

    v = GPS.XMLViewer.create_metric("Metrics")
    v.parse("metrix.xml")

def show_metrics_window(self):
    w=GPS.MDI.get("Metrics")

    if w == None:
        v = GPS.XMLViewer.create_metric("Metrics")
        v.parse("metrix.xml")
    else:
        v = w.get_child()
        w.raise_window()

GPS.parse_xml(xml_base)
GPS.Hook("compilation_finished").add(on_compilation_finished)
GPS.Menu.create("/Tools/Views/Metrics",
                show_metrics_window,
                ref="Messages", add_before=0)
