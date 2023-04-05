""" Alire integration script """

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import os_utils
import os.path
import re
import tool_output

alr = os_utils.locate_exec_on_path("alr")
saved_env = {}  # all changed env variables and their values


def find_alire_root(path):
    """Return parent directory with "alire.toml" or None"""
    parent = os.path.dirname(path)

    if path == parent:
        return None
    elif os.path.exists(os.path.join(parent, "alire.toml")):
        return parent

    return find_alire_root(parent)


def on_project_changing(hook, file):
    global saved_env

    # restore saved environment
    for name in saved_env:
        value = saved_env[name]
        GPS.setenv(name, value)

        if value:
            os.environ[name] = value
        else:
            del os.environ[name]

    saved_env = {}

    root = find_alire_root(file.path)

    if root:
        # launch alr printenv and update environment and saved_env
        alire_target = GPS.BuildTarget("Alire")
        alire_target.execute(directory=root)


class Alire_Parser(tool_output.OutputParser):

    def __init__(self, child=None):
        tool_output.OutputParser.__init__(self, child)
        self.exp = re.compile(r"export (\S+)=(.*)")

    def on_stdout(self, text, command):
        global saved_env

        for line in text.splitlines():
            m = self.exp.fullmatch(line)

            if m:
                name = m.group(1)
                value = m.group(2)
                saved_env[name] = GPS.getenv(name)
                GPS.setenv(name, value)
                os.environ[name] = value


if alr:
    GPS.Hook("project_changing").add(on_project_changing)
    GPS.parse_xml("""<?xml version="1.0"?><ALIRE>
    <target-model name="Alire" category="">
       <description>Laubch Alire to print environment</description>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>-q</arg>
          <arg>printenv</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <output-parsers>
         output_chopper
         utf8_converter
         progress_parser
         alire_parser
         console_writer
         end_of_build
       </output-parsers>
    </target-model>

    <target model="Alire" category="Alire" name="Alire"
            messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>-q</arg>
          <arg>printenv</arg>
       </command-line>
       <output-parsers>
         output_chopper
         utf8_converter
         progress_parser
         alire_parser
         console_writer
         end_of_build
       </output-parsers>
    </target>

    </ALIRE>""")
