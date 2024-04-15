"""
Alire integration script.
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import os_utils
import os.path
import re
import tool_output
import shlex
from gi.repository import Gtk

alr = os_utils.locate_exec_on_path("alr")
saved_env = {}  # all changed env variables and their values
project_to_reload = None  # The project we should reload after finding an Alire manifest

def find_alire_root(path):
    """
    Return parent directory with "alire.toml" or None
    """
    parent = os.path.dirname(path)

    if path == parent:
        return None
    elif os.path.exists(os.path.join(parent, "alire.toml")):
        return parent

    return find_alire_root(parent)


def on_project_recomputed(hook):
    global progress_timeout

    if project_to_reload:
        file, root = project_to_reload

        def display_message(timeout):
            """
            Display a message in the Locations view warning the user that
            Alire is being ran.
            """
            # Safety net in order to make sure we remove this timeout function
            # in any case.
            if timeout.counter > 20:
                timeout.remove()
                return
            timeout.counter += 1

            # Make sure to give the focus to the Locations view first...
            if GPS.MDI.current().name() != "Locations":
                GPS.MDI.get("Locations").raise_window()
            else:
                # Once the Locations view has the focus, clear the possible errors in
                # the Messages view due to loading failures before 'alr printenv' and
                # add a message saying that we are configuring the project through Alire
                GPS.Console().clear()
                GPS.Locations.add(
                    "Alire", GPS.File(file), 1, 1,
                    "Alire project detected, setting the needed environment to reload it properly...",
                    importance=GPS.Message.Importance.MEDIUM)
                GPS.MDI.get("Locations").set_activity_progress_bar_visibility(True)
                timeout.remove()

        # Run Alire to setup the environment
        GPS.Logger("ALIRE").log("Running alire...")
        alire_target = GPS.BuildTarget("Alire")
        alire_target.execute(directory=root, synchronous=False)

        # Display a message in the Locations view to warn the user that
        # Alire is being ran
        timeout = GPS.Timeout(100, display_message)
        timeout.counter = 0

def on_compilation_finished(hook, category, target_name, mode_name, status, cmd):
    """
    Reload the project once Alire has been ran to setup the
    environment.
    """
    global project_to_reload
    if not target_name.startswith("Alire"):
        return

    if project_to_reload:
        file, root = project_to_reload
        GPS.Logger("ALIRE").log(
            "Alire configuration finished, reloading %s" % str(file))
        GPS.MDI.get("Locations").set_activity_progress_bar_visibility(False)
        # Set ALIRE env variable to True before loading the project in order
        # to not re-do 'alr printenv' on the ALS side
        GPS.setenv("ALIRE", "True")
        # Load the project
        GPS.Project.load(file)
        # Warn the user that everything is now setup
        GPS.Locations.add(
            "Alire", GPS.File(file), 1, 1,
            "Alire environment is now setup: project has been reloaded",
            importance=GPS.Message.Importance.INFORMATIONAL)
        GPS.MDI.information_popup(
                'Alire project is now setup', 'vcs-up-to-date')
        project_to_reload = None


def on_project_changing(hook, file):
    """
    Detect if we are dealing with an Alire project.
    If yes, save the project we are trying to load so we
    can launch Alire after failing to load it, in order to
    reload it once the needed environment is set.
    """
    global saved_env, project_to_reload

    if project_to_reload or "ALIRE" in os.environ:
        project_to_reload = None
        return

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
        GPS.Logger("ALIRE").log("Alire manifest detected!")
        project_to_reload = (file.path, root)


class Alire_Parser(tool_output.OutputParser):
    """
    Parse the Alire output in oder to set the needed environment,
    saving the original environment in order to restore it if needed.
    """

    def __init__(self, child=None):
        tool_output.OutputParser.__init__(self, child)
        self.exp = re.compile(r"export (\S+)=(.*)")

    def on_stdout(self, text, command):
        global saved_env

        for line in text.splitlines():
            m = self.exp.fullmatch(line)

            if m:
                # Parse the output of 'alr printenv'.
                # The paths might be quoted (e.g: export PATH="/home/something"): we use
                # shlex.split to make sure we unquote them if needed.
                name = m.group(1)
                value = shlex.split(m.group(2))[0]
                GPS.Logger("ALIRE").log("%s=%s" % (name, value))
                saved_env[name] = GPS.getenv(name)
                GPS.setenv(name, value)
                os.environ[name] = value


if alr:
    GPS.Hook("project_changing").add(on_project_changing)
    GPS.Hook("project_view_changed").add(on_project_recomputed)
    GPS.Hook("compilation_finished").add(on_compilation_finished)

    GPS.parse_xml(
        """<?xml version="1.0"?><ALIRE>
    <target-model name="Alire" category="">
       <description>Launch Alire to print environment</description>
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

    </ALIRE>"""
    )
