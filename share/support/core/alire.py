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

alr = os_utils.locate_exec_on_path("alr")
saved_env: dict[str, str] = {}  # all changed env variables and their values
project_to_reload = None  # The project we should reload after finding an Alire manifest
alire_manifest = None  # The alire.toml file we are trying to load
alire_project_files = []  # GPR project files listed in the known Alire manifest

# Setting up an Alire crate is done asynchronously, so that GNAT Studio stays
# responsive while Alire fetches the toolchain and the dependencies (which can
# take several minutes the first time). 'alire_state' tells the hooks where we
# are in that sequence: it is None when no setup is pending,
# ALIRE_STATE_DETECTED between the detection of a manifest and the start of the
# sequence, the name of the running build target while the sequence progresses,
# and ALIRE_STATE_RELOADING while we perform the final project load.
alire_state = None

# Incremented each time an Alire manifest is detected. A setup sequence
# captures its value when it starts and gives up as soon as it does not match
# anymore, which happens when the user loads another project meanwhile.
alire_generation = 0

ALIRE_STATE_DETECTED = "detected"
ALIRE_STATE_RELOADING = "reloading"

# The build targets to run, in order, to set up an Alire crate:
# 'Alire Sync' ('alr build --stop-after=generation') fetches the toolchain and
# the dependencies, 'Alire Show' tells us which GPR project should be loaded
# and 'Alire Printenv' gives us the environment needed to load it.
ALIRE_SETUP_TARGETS = ("Alire Sync", "Alire Show", "Alire Printenv")

ALIRE_SETUP_MESSAGE = """Alire crate detected: fetching the toolchain and the \
dependencies. This may take several minutes the first time. The project will be \
reloaded automatically once Alire is done."""

ALIRE_MODELS_XML = """
    <target-model name="Alire" category="">
       <description>Launch Alire to print environment</description>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>-q</arg>
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

    <target-model name="Alire Builder" category="">
       <description>Generic Alire builder</description>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>--</arg>
          <arg>-d</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-build-all-symbolic</iconname>
       <switches command="%(tool_name)s" columns="1" lines="2" sections="-- -largs">
         <title column="1" line="1" >Profiles</title>
         <title column="1" line="2" >Misc</title>
         <radio
            line="1"
            label="Build Profiles"
            tip="A build profile can be selected with the appropriate switch.
The profile is applied to the root release only, whereas dependencies are built
in release mode.">
            <radio-entry label="Development"
            switch = ""/>
            <radio-entry label="Release"
            switch="--release"/>
            <radio-entry label="Validation"
            switch="--validation"/>
         </radio>
         <check label="Display memory usage" switch="-Wl,-Map=map.txt"
            section="-largs"
            tip="Display the memory usage in the Memory usage view"
            filter="ld_supports_map_file"
            line="2"/>
       </switches>
       <output-parsers>
         output_chopper
         utf8_converter
         progress_parser
         console_writer
         end_of_build
       </output-parsers>
    </target-model>

    <target-model name="Alire Clean" category="">
       <description>Clean compilation artifacts with Alire</description>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>--</arg>
          <arg>%X</arg>
       </command-line>
       <iconname>gps-clean-symbolic</iconname>
       <switches command="%(tool_name)s" columns="2" lines="1" sections="--">
         <title column="1" line="1" >Options</title>
            <check label="Delete cache of releases" switch="--cache"
            tip="All downloaded dependencies will be deleted." />
            <check label="Delete dangling temporary files" switch="--temp"
            tip="All alr-???.tmp files in the subtree will be deleted. These files may
remain when alr is interrupted via Ctrl-C or other forceful means.s" />
       </switches>
       <output-parsers>
         output_chopper
         utf8_converter
         progress_parser
         console_writer
         end_of_build
       </output-parsers>
    </target-model>
"""

ALIRE_TARGETS_XML = """
    <target model="Alire" category="Alire" name="Alire Printenv"
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

    <target model="Alire" category="Alire" name="Alire Show"
            messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>show</arg>
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

   <target model="Alire" category="Alire" name="Alire Sync"
           messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-refresh-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>build</arg>
          <arg>=--stop-after=generation</arg>
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

    <target model="Alire Builder" category="Alire" name="Alire Build All"
            messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-all-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>build</arg>
          <arg>--</arg>
          <arg>-d</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="Alire Builder" category="Alire" name="Alire Build Main"
            messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-build-main-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <target-type>main</target-type>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>build</arg>
          <arg>--</arg>
          <arg>-d</arg>
          <arg>%X</arg>
       </command-line>
    </target>

    <target model="Alire Clean" category="Alire" name="Alire Clean All"
            messages_category="Alire">
       <in-toolbar>FALSE</in-toolbar>
       <in-menu>FALSE</in-menu>
       <iconname>gps-clean-symbolic</iconname>
       <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
       <read-only>TRUE</read-only>
       <command-line>
          <arg>alr</arg>
          <arg>--non-interactive</arg>
          <arg>--no-color</arg>
          <arg>--no-tty</arg>
          <arg>clean</arg>
          <arg>--</arg>
          <arg>%X</arg>
       </command-line>
    </target>
"""

ALIRE_TARGET_ALIASES = {
    "Build All": "Alire Build All",
    "Build Main": "Alire Build Main",
    "Clean All": "Alire Clean All",
}


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


def update_aliases_for_alire_targets(is_alire_project):
    """
    Set or unset depending on `is_alire_project` the aliases on the
    Alire build targets for the default ones (e.g: 'Build All').
    """
    for target, alias in ALIRE_TARGET_ALIASES.items():
        GPS.BuildTarget(target).set_as_alias(alias if is_alire_project else "")


def _start_setup_feedback():
    """
    Tell the user that Alire is setting up the crate. This can take several
    minutes the first time, so say so plainly rather than leave GNAT Studio
    looking idle.
    """
    file, _ = project_to_reload

    GPS.Locations.add(
        "Alire",
        GPS.File(file),
        1,
        1,
        ALIRE_SETUP_MESSAGE,
        importance=GPS.Message.Importance.MEDIUM,
    )

    # The Locations view might not be present at all (e.g. in a perspective
    # that does not display it): the message above is still recorded.
    locations = GPS.MDI.get("Locations")
    if locations:
        locations.raise_window()
        locations.set_activity_progress_bar_visibility(True)

    GPS.MDI.information_popup("Setting up the Alire crate...", "gps-refresh-symbolic")


def _stop_setup_feedback():
    """
    Remove the progress feedback displayed while Alire was running.
    """
    locations = GPS.MDI.get("Locations")
    if locations:
        locations.set_activity_progress_bar_visibility(False)


def _report_setup_failure(target, status):
    """
    Report that the Alire setup sequence failed while running `target`,
    pointing the user at the Messages view for the command's output.

    This also covers the case where the user interrupted the task from the
    Task Manager: the intermediate project stays loaded and nothing is
    retried behind the user's back.
    """
    global alire_state

    file, _ = project_to_reload
    GPS.Logger("ALIRE").log("'%s' failed with status %s" % (target, status))

    GPS.Locations.add(
        "Alire",
        GPS.File(file),
        1,
        1,
        "Alire setup failed: '%s' exited with status %s. "
        "See the Messages view for the complete output." % (target, status),
        importance=GPS.Message.Importance.HIGH,
    )

    # We are not going to reload the project after all: report the errors of
    # the next project load normally.
    GPS.Project.set_ignore_load_errors(False)
    alire_state = None


def _load_alire_project():
    """
    Reload the project once Alire has been run to setup the environment.
    """
    global alire_state

    file, root = project_to_reload
    GPS.Logger("ALIRE").log("Alire configuration finished, reloading %s" % str(file))

    # This load is the real one, as opposed to the intermediate one performed
    # before Alire had set the environment: report its errors.
    GPS.Project.set_ignore_load_errors(False)

    alire_state = ALIRE_STATE_RELOADING
    GPS.Project.load(file)

    # Warn the user that everything is now setup
    GPS.Locations.add(
        "Alire",
        GPS.File(file),
        1,
        1,
        "Alire environment is now setup: project has been reloaded",
        importance=GPS.Message.Importance.INFORMATIONAL,
    )

    update_aliases_for_alire_targets(is_alire_project=True)
    GPS.MDI.information_popup("Alire project is now setup", "vcs-up-to-date")

    # Change GS's current directory to Alire project's root directory.
    GPS.Logger("ALIRE").log("Changing current directory to: %s" % root)
    GPS.cd(root)
    GPS.Logger("ALIRE").log("Current directory is now: %s" % GPS.pwd())


def _setup_alire_crate(root, generation):
    """
    Run the Alire setup sequence asynchronously: synchronize the crate,
    determine which GPR project should be loaded and set the crate's
    environment, then reload that project.

    `generation` is the value of `alire_generation` when the manifest that
    started this sequence was detected: it allows abandoning the sequence when
    the user loads another project while Alire is still running.
    """
    global alire_state

    # 'workflows' lives in share/support/ui, which is added to sys.path after
    # share/support/core: import it here rather than at module level.
    from workflows.promises import TargetWrapper

    _start_setup_feedback()

    try:
        for target in ALIRE_SETUP_TARGETS:
            alire_state = target
            GPS.Logger("ALIRE").log("Running '%s'..." % target)
            status = yield TargetWrapper(target).wait_on_execute(directory=root)

            if generation != alire_generation:
                GPS.Logger("ALIRE").log(
                    "Another project is being loaded: abandoning the Alire setup"
                )
                return

            if status != 0:
                _report_setup_failure(target, status)
                return
    finally:
        # When a newer sequence has taken over, it owns the feedback now.
        if generation == alire_generation:
            _stop_setup_feedback()

    _load_alire_project()


def on_project_recomputed(hook):
    GPS.Logger("ALIRE").log(f"on_project_recomputed called. alire_state: {alire_state}")

    if alire_state == ALIRE_STATE_DETECTED:
        # Now that GNAT Studio has finished loading the intermediate project,
        # start the Alire setup sequence in the background.
        import workflows

        workflows.driver(_setup_alire_crate(project_to_reload[1], alire_generation))

    elif not GPS.getenv("ALIRE"):
        # We are not loading an Alire project: unset the aliases
        # on Alire build targets.
        update_aliases_for_alire_targets(is_alire_project=False)


def on_project_changing(hook, file):
    """
    Detect if we are dealing with an Alire project.
    If yes, save the project we are trying to load so that the Alire setup
    sequence can be started by on_project_recomputed, once GNAT Studio is done
    loading it: the project is then reloaded once the needed environment is
    set.
    """
    global saved_env, project_to_reload, alire_manifest, alire_project_files
    global alire_state, alire_generation

    if alire_state == ALIRE_STATE_RELOADING:
        GPS.Logger("ALIRE").log(f"Loading Alire project through: {file.path}")
        alire_state = None
        project_to_reload = None
        return

    # Unless we detect a new manifest below, the project about to be loaded is
    # a regular one: report its errors.
    GPS.Project.set_ignore_load_errors(False)

    if file.path in alire_project_files:
        GPS.Logger("ALIRE").log(
            f"{file.path} already known as Alire project file, "
            + "skipping Alire detection and synchronization."
        )
        return

    # Any setup sequence still running is for a project we are not loading
    # anymore: bumping the generation turns it into a no-op.
    alire_generation += 1
    alire_state = None
    project_to_reload = None

    # restore saved environment
    for name in saved_env:
        value = saved_env[name]
        GPS.setenv(name, value)

        if value:
            os.environ[name] = value
        else:
            del os.environ[name]

    saved_env = {}

    root = (
        os.path.dirname(file.path)
        if file.base_name() == "alire.toml"
        else find_alire_root(file.path)
    )

    if root:
        # TODO: if file is an alire.toml file, set project_to_reload
        # to <base_name>.gpr by default
        project_to_reload = (file.path, root)
        alire_manifest = os.path.join(root, "alire.toml")

        # Forget the project files of the previous manifest: 'alr show' fills
        # the list for this one. This can't be done in Alire_Parser.__init__:
        # a fresh parser is created for each build target launch, so the
        # 'Alire Printenv' one would wipe what the 'Alire Show' one has just
        # gathered.
        alire_project_files = []

        alire_state = ALIRE_STATE_DETECTED

        # Set the ALIRE env variable right away: this keeps the language
        # server from launching its own 'alr', which would contend with ours
        # for the crate's lock for as long as the synchronization lasts.
        GPS.setenv("ALIRE", "True")

        # The project about to be loaded is an intermediate one: it can't be
        # loaded successfully before Alire has set the environment, and we
        # reload it ourselves once that's done, so don't bother the user with
        # its errors.
        GPS.Project.set_ignore_load_errors(True)

        GPS.Logger("ALIRE").log("Alire manifest detected: %s" % alire_manifest)


class Alire_Parser(tool_output.OutputParser):
    """
    Parse the Alire output in order to set the needed environment,
    saving the original environment in order to restore it if needed.
    """

    def __init__(self, child=None):
        GPS.Logger("ALIRE").log("Initializing alire output parser...")
        tool_output.OutputParser.__init__(self, child)
        self.export_var_regexp = re.compile(r"export (\S+)=(.*)")
        self.project_file_regexp = re.compile(r" +Project_File: ([^\n]+)")
        self.crate_name_regexp = re.compile(r" +Name: (\S+)")

    def on_stdout(self, text, command):
        global saved_env, project_to_reload, alire_manifest, alire_project_files

        for line in text.splitlines():
            m = self.export_var_regexp.fullmatch(line)

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
            else:
                m = self.crate_name_regexp.fullmatch(line)
                if m:
                    root = os.path.dirname(alire_manifest)
                    project_file_basename = m.group(1) + ".gpr"
                    GPS.Logger("ALIRE").log(
                        "project_base_name: %s" % project_file_basename
                    )

                    project_to_reload = (
                        os.path.join(root, project_file_basename),
                        root,
                    )
                else:
                    m = self.project_file_regexp.fullmatch(line)
                    if m:
                        project_file_path = m.group(1)
                        GPS.Logger("ALIRE").log(
                            "Project file found through 'alr show': %s"
                            % project_file_path
                        )
                        root = os.path.dirname(alire_manifest)

                        # Get the absolute path of the project file, if not already
                        # absolute
                        projet_file_abs_path = (
                            os.path.join(root, project_file_path)
                            if not os.path.isabs(project_file_path)
                            else project_file_path
                        )

                        # Append the project file to the list of Alire manifest project
                        # files.
                        # This is needed in case the user wants to load a different
                        # project file from the same Alire manifest: we don't want to
                        # re-run Alire in that case.
                        alire_project_files.append(projet_file_abs_path)

                        # Set it as the project to reload
                        project_to_reload = (
                            projet_file_abs_path,
                            root,
                        )


if alr:
    GPS.Hook("project_changing").add(on_project_changing)
    GPS.Hook("project_view_changed").add(on_project_recomputed)

GPS.parse_xml(ALIRE_MODELS_XML + ALIRE_TARGETS_XML)
