import re
import GPS

# Initialize the targets
xml_base = """
<target-model name="gnatmetrics" category="">
   <description>Generic launch of gnat metrics</description>
   <command-line>
      <arg>%attr(ide#gnat,gnat)</arg>
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
      @@SWITCHES@@
   </switches>
</target-model>

<target model="gnatmetrics" category="_File_" name="GNAT Metrics for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>
    <command-line>
      <arg>%attr(ide#gnat,gnat)</arg>
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
      <arg>%attr(ide#gnat,gnat)</arg>
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
      <arg>%attr(ide#gnat,gnat)</arg>
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
   <description>Launch GNAT metric on the current project</description>
   <shell lang="python" output="none"
   >GPS.BuildTarget("GNAT Metrics for project").execute(synchronous=False)</shell>
</action>

<action name="GNAT Metric on current project and subprojects">
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

<contextual action="GNAT metric on current file" >
   <title>Metrics/Compute metrics for %f</title>
</contextual>

<contextual action="GNAT metric on current project" >
   <title>Metrics/Compute metrics for project %p</title>
</contextual>

<contextual action="GNAT metric on current project and subprojects" >
   <title>Metrics/Compute metrics for project %p and subprojects</title>
</contextual>

"""

headers = {
  "Complexity metrics": (1, 1),
  "Line metrics": (1, 2),
  "Syntax element metrics": (2, 1),
  "Coupling metrics": (2, 2)
}

class Switch(object):
    def __init__(self, label, switch, tip, line_column):
        """ Initialize a switch """

        self.label = label
        self.switch = switch
        self.tip = tip
        self.line, self.column = line_column

    def __str__(self):
        return """<check label="%s"
                   line="%s"  column="%s"
                   switch="%s"
                   tip="%s" />""" % (
                       self.label, self.line, self.column,
                       self.switch,
                       self.tip)

class Parse_Output(object):
    """ Parses the output of "gnat metric" and extract the switches. """

    def __init__(self, output):
        self.header_regexp = re.compile("^ *(%s)([\\.:]).*" % (
           "|".join(headers.keys())))
        self.data_regexp = re.compile("^ +(-[^ ]+) +- (.+)$")
        self.current_header = None
        self.current_line_col = (1, 1)
        self.results = []

        for line in output.split("\n"):
            self.parse_one_line(line)

    def parse_one_line(self, line):
        if not line:
            self.current_header = None
        m = self.header_regexp.match(line)
        if m:
            # Header
            self.current_header = m.group(1)
            self.current_line_col = headers[self.current_header]
        else:
            if self.current_header:
                 m = self.data_regexp.match(line)
                 if m:
                     self.emit_result(label=m.group(2),
                         switch=m.group(1), tip=m.group(2))
                 else:
                     if self.current_result:
                         self.current_result.tip += line.strip()

    def emit_result(self, label, switch, tip):
        result = Switch(label, switch, tip, self.current_line_col)
        self.current_result = result
        self.results.append(result)

    def __str__(self):
        return "\n".join([r.__str__() for r in self.results])


def initialize():
    # parse the output of "gnat metric" to get a list of switches

    try:
        p = GPS.Process("gnat metric")
        output = p.get_result()
    except:
        GPS.Console().write(
            "gnatmetric.py: could not launch 'gnat metric', plugin disabled.")
        output = None

    if output:
        switches = Parse_Output(output).__str__()
        GPS.parse_xml(xml_base.replace("@@SWITCHES@@", switches))

def on_compilation_finished(hook, category,
    target_name="", mode_name="", status=""):

    if not target_name.startswith("GNAT Metric"):
        return

    if status:
        return

    v = GPS.XMLViewer.create_metric("Metrics")
    v.parse("metrix.xml")


initialize()
GPS.Hook("compilation_finished").add(on_compilation_finished)
