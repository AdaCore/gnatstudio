"""
Provides support for gnatmetric.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
import gps_utils

GNATMETRIC_MENU = "/Analyze/Metrics/"
CATEGORY = "GNATmetric"

# Initialize the targets
XML_BASE = ("""
<target-model name="gnathub_gnatmetric">
   <description>Generic launch of gnat metric</description>
   <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg></arg>
   </command-line>
   <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
   </output-parsers>
   <iconname>gps-semantic-check-symbolic</iconname>
   <switches command="%(tool_name)s" columns="2" lines="3"
            sections="--targs:gnatmetric">
      <title line="1" column="1">Line metrics</title>
      <title line="1" column="2">Syntax element metrics</title>
      <title line="2" column="1">Complexity metrics</title>
      <title line="2" column="2">Coupling metrics</title>
      <title line="3" column="1">Contract metrics</title>
      <title line="3" column="2">GNAThub options</title>
<check label="All complexity metrics"
                   line="2"  column="1"
                   switch="--complexity-all"
                   tip="All complexity metrics"
                   section="--targs:gnatmetric" />
<check label="McCabe Cyclomatic Complexity"
                   line="2"  column="1"
                   switch="--complexity-cyclomatic"
                   tip="McCabe Cyclomatic Complexity"
                   section="--targs:gnatmetric" />
<check label="McCabe Essential Complexity"
                   line="2"  column="1"
                   switch="--complexity-essential"
                   tip="McCabe Essential Complexity"
                   section="--targs:gnatmetric" />
<check label="Average McCabe CC of a body"
                   line="2"  column="1"
                   switch="--complexity-average"
                   tip="Average McCabe Cyclomatic Complexity of a body"
                   section="--targs:gnatmetric" />
<check label="Maximal loop nesting level"
                   line="2"  column="1"
                   switch="--loop-nesting"
                   tip="Maximal loop nesting level"
                   section="--targs:gnatmetric" />
<check label="Do not count static loops for CC"
                   line="2"  column="1"
                   switch="--no-static-loop"
                   tip="Do not count static loops for cyclomatic complexity"
                   section="--targs:gnatmetric" />
<check label="Do not consider exit statements as gotos"
                   line="2"  column="1"
                   switch="-ne"
                   tip="Do not consider exit statements as gotos when """
            """computing Essential Complexity"
                   section="--targs:gnatmetric" />
<check label="Extra exit points in subprograms"
                   line="2"  column="1"
                   switch="--extra-exit-points"
                   tip="Extra exit points in subprograms"
                   section="--targs:gnatmetric" />
<check label="All line metrics"
                   line="1"  column="1"
                   switch="--lines-all"
                   tip="All line metrics"
                   section="--targs:gnatmetric" />
<check label="Total number of lines"
                   line="1"  column="1"
                   switch="--lines"
                   tip="Total number of lines"
                   section="--targs:gnatmetric" />
<check label="Number of code lines"
                   line="1"  column="1"
                   switch="--lines-code"
                   tip="Number of code lines"
                   section="--targs:gnatmetric" />
<check label="Number of comment lines"
                   line="1"  column="1"
                   switch="--lines-comment"
                   tip="Number of comment lines"
                   section="--targs:gnatmetric" />
<check label="Number of code lines with comments"
                   line="1"  column="1"
                   switch="--lines-eol-comment"
                   tip="Number of code lines with end-of-line comments"
                   section="--targs:gnatmetric" />
<check label="Comment lines percentage"
                   line="1"  column="1"
                   switch="--lines-ratio"
                   tip="Ratio between the number of lines that contain """
            """comments and the total number of non-blank lines"
                   section="--targs:gnatmetric" />
<check label="Number of blank lines"
                   line="1"  column="1"
                   switch="--lines-blank"
                   tip="Number of blank lines"
                   section="--targs:gnatmetric" />
<check label="Average number of code lines in bodies"
                   line="1"  column="1"
                   switch="--lines-average"
                   tip="Average number of code lines in subprogram bodies, """
            """task bodies, entry bodies and statement sequences in """
            """package bodies"
                   section="--targs:gnatmetric" />
<check label="All syntax element metrics"
                   line="1"  column="2"
                   switch="--syntax-all"
                   tip="All syntax element metrics"
                   section="--targs:gnatmetric" />
<check label="Number of declarations"
                   line="1"  column="2"
                   switch="--declarations"
                   tip="Total number of declarations"
                   section="--targs:gnatmetric" />
<check label="Number of statements"
                   line="1"  column="2"
                   switch="--statements"
                   tip="Total number of statements"
                   section="--targs:gnatmetric" />
<check label="Number of public subprograms in a unit"
                   line="1"  column="2"
                   switch="--public-subprograms"
                   tip="Number of public subprograms in a compilation unit"
                   section="--targs:gnatmetric" />
<check label="Number of subprograms in a unit"
                   line="1"  column="2"
                   switch="--all-subprograms"
                   tip="Number of subprograms in a compilation unit"
                   section="--targs:gnatmetric" />
<check label="Number of public types in a unit"
                   line="1"  column="2"
                   switch="--public-types"
                   tip="Number of public types in a compilation unit"
                   section="--targs:gnatmetric" />
<check label="Number of types in a unit"
                   line="1"  column="2"
                   switch="--all-types"
                   tip="Number of types in a compilation unit"
                   section="--targs:gnatmetric" />
<check label="Maximal unit nesting level"
                   line="1"  column="2"
                   switch="--unit-nesting"
                   tip="Maximal unit nesting level"
                   section="--targs:gnatmetric" />
<check label="Maximal construct nesting level"
                   line="1"  column="2"
                   switch="--construct-nesting"
                   tip="Maximal construct nesting level"
                   section="--targs:gnatmetric" />
<check label="All coupling metrics"
                   line="2"  column="2"
                   switch="--coupling-all"
                   tip="All coupling metrics"
                   section="--targs:gnatmetric" />
<check label="Tagged (class) fan-out coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-out"
                   tip="Tagged (class) fan-out coupling"
                   section="--targs:gnatmetric" />
<check label="Tagged (class) fan-in coupling"
                   line="2"  column="2"
                   switch="--tagged-coupling-in"
                   tip="Tagged (class) fan-in coupling"
                   section="--targs:gnatmetric" />
<check label="Hierarchy (category) fan-out coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-out"
                   tip="Hierarchy (category) fan-out coupling"
                   section="--targs:gnatmetric" />
<check label="Hierarchy (category) fan-in coupling"
                   line="2"  column="2"
                   switch="--hierarchy-coupling-in"
                   tip="Hierarchy (category) fan-in coupling"
                   section="--targs:gnatmetric" />
<check label="Unit fan-out coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-out"
                   tip="Unit fan-out coupling"
                   section="--targs:gnatmetric" />
<check label="Unit fan-in coupling"
                   line="2"  column="2"
                   switch="--unit-coupling-in"
                   tip="Unit fan-in coupling"
                   section="--targs:gnatmetric" />
<check label="Control fan-out coupling"
                   line="2"  column="2"
                   switch="--control-coupling-out"
                   tip="Control fan-out coupling"
                   section="--targs:gnatmetric" />
<check label="Control fan-in coupling"
                   line="2"  column="2"
                   switch="--control-coupling-in"
                   tip="Control fan-in coupling"
                   section="--targs:gnatmetric" />
<check label="All contract metrics"
                   line="3"  column="1"
                   switch="--contract-all"
                   tip="All contract metrics"
                   section="--targs:gnatmetric" />
<check label="Subprograms with contracts"
                   line="3"  column="1"
                   switch="--contract"
                   tip="Subprograms with contracts"
                   section="--targs:gnatmetric" />
<check label="Subprograms with postconditions"
                   line="3"  column="1"
                   switch="--post"
                   tip="Subprograms with postconditions"
                   section="--targs:gnatmetric" />
<check label="Subprograms with complete contracts"
                   line="3"  column="1"
                   switch="--contract-complete"
                   tip="Subprograms with complete contracts"
                   section="--targs:gnatmetric" />
<check label="McCabe Cyclomatic Complexity of contracts"
                   line="3"  column="1"
                   switch="--contract-cyclomatic"
                   tip="McCabe Cyclomatic Complexity of contracts"
                   section="--targs:gnatmetric" />
<check label="Incremental mode" switch="-i" line="3" column="2"
                   tip="Append this run results to the previous runs" />
   </switches>
</target-model>

<target model="gnathub_gnatmetric" category="_File_"
    name="GNAT Metrics for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg>--plugins=gnatmetric</arg>
      <arg>--targs:gnatmetric</arg>
      <arg>%fp</arg>
    </command-line>
    <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
    </output-parsers>
</target>

<target model="gnathub_gnatmetric" category="_PROJECT_"
    name="GNAT Metrics for project">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg>--plugins=gnatmetric</arg>
      <!-- We need to add at least a switch after targs -->
      <arg>--targs:gnatmetric</arg>
      <arg>-P%PP</arg>
    </command-line>
    <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
    </output-parsers>
</target>

<target model="gnathub_gnatmetric" category="_PROJECT_"
    name="GNAT Metrics for project and subprojects">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%gnathub</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
      <arg>%subdirsarg</arg>
      <arg>--plugins=gnatmetric</arg>
      <arg>--targs:gnatmetric</arg>
      <arg>-U</arg>
    </command-line>
    <output-parsers>
      output_chopper
      utf8_converter
      progress_parser
      gnathub_parser
      console_writer
      end_of_build
    </output-parsers>
</target>
""")


@gps_utils.interactive(
    category=CATEGORY, name="gnat metric on current project")
def gnatmetric_on_project():
    target = GPS.BuildTarget("GNAT Metrics for project")
    target.execute(synchronous=False)


@gps_utils.interactive(
    category=CATEGORY, name="gnat metric on current project and subprojects")
def gnatmetric_on_all_project():
    target = GPS.BuildTarget("GNAT Metrics for project and subprojects")
    target.execute(synchronous=False)


@gps_utils.interactive(
    category=CATEGORY, name="gnat metric on current file", filter="File")
def gnatmetric_on_file():
    target = GPS.BuildTarget("GNAT Metrics for file")
    target.execute(synchronous=False)


GPS.parse_xml(XML_BASE)
