"""
This is the python format checker plugin for GPS.
Its aim is to check identatoin, style when:
 1 user stop editing
 2 before python script is saved
"""

from GPS import *
import tool_output
import GPS
import colorschemes

# This is an XML model for pep8
Pep8_Model = """
<target-model name="pep8" category="">
   <description>Run pep8</description>
   <is-run> True </is-run>
   <command-line>
      <arg>pep8</arg>
   </command-line>
   <icon>gps-build-all</icon>
   <output-parsers>
      pep8_parser
      end_of_build
   </output-parsers>
</target-model>

<target model="pep8" category="Pep8" name="Pep8">
    <in-toolbar>FALSE</in-toolbar>
    <icon>gps-build-all</icon>
    <launch-mode>MANUALLY</launch-mode>
    <read-only>TRUE</read-only>
    <target-type>pep8</target-type>
    <command-line>
       <arg>pep8</arg>
    </command-line>
    <output-parsers>
      pep8_parser
      end_of_build
    </output-parsers>
</target>
"""

parse_xml(Pep8_Model)


class Pep8_Parser(tool_output.OutputParser):
    """
       This class is pep8 parser for format checking in pep8 target.
    """
    def on_stdout(self, text, command):
        """
           Overriden Method.
           Called to browse pep8 format message on current file.
        """
        self.__messages = []
        if text is not None:
            for i in text.split("\n"):
                a = i.strip("\n").split(":")
                if a[0] == "/tmp/_gps_pep8.py":
                    m = GPS.Message(category="Pep8",
                                    file=GPS.EditorBuffer.get().file(),
                                    line=int(a[1]),
                                    column=int(a[2]),
                                    text=a[3].strip(" "),
                                    flags=2)

                    m.set_action("", "gps-build-warning", m.get_text())
                    m.set_style(colorschemes.STYLE_WARNING)
