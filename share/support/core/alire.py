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

# GPR project files of the crate that is set up: loading one of them needs no
# Alire run. Only filled once a sequence has succeeded, since that is when the
# crate's environment is actually in place.
alire_project_files = []

# What 'alr show' has reported for the sequence under way.
alire_pending_project_files = []

# Where we are in the setup sequence, which runs asynchronously so that GNAT
# Studio stays responsive while Alire fetches the toolchain and the
# dependencies: None when no setup is pending, ALIRE_STATE_DETECTED between the
# detection of a manifest and the start of the sequence, the name of the running
# build target while it progresses, ALIRE_STATE_RELOADING during the final load.
alire_state = None

# Incremented each time a manifest is detected. A sequence and its output
# parsers capture it and give up as soon as it changes, which happens when the
# user loads another project meanwhile.
alire_generation = 0

# Generation of the last sequence started, so that a second
# 'project_view_changed' for the same manifest does not start it twice.
alire_started_generation = None

ALIRE_STATE_DETECTED = "detected"
ALIRE_STATE_RELOADING = "reloading"

# The build targets to run, in order, to set up a crate: 'alr build
# --stop-after=generation' fetches the toolchain and the dependencies, 'alr show'
# tells us which GPR project to load and 'alr printenv' the environment to load
# it with.
ALIRE_SETUP_TARGETS = ("Alire Sync", "Alire Show", "Alire Printenv")

# Name of the single task monitoring the whole sequence: the setup is then one
# interruptible entry in the Task Manager, and waiting for GNAT Studio's tasks
# (as the testsuites do) waits for the setup as a whole rather than for the
# individual 'alr' runs, between which no task is running.
ALIRE_SETUP_TASK_NAME = "setting up the Alire crate"

# How often, in ms, we check that the task monitoring a sequence is still there.
ALIRE_INTERRUPTION_CHECK_PERIOD = 200

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
          <arg>--stop-after=generation</arg>
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


def _setup_task_name(generation):
    """
    Return the name of the task monitoring the setup sequence of `generation`.

    Two sequences must not share a task name: a GPS.Task is identified by it, so
    the second one would land in the first one's queue and, worse, take its place
    as the instance both are executed through.
    """
    return "%s (%d)" % (ALIRE_SETUP_TASK_NAME, generation)


def _interrupt_task(name):
    """
    Interrupt the task called `name`, if it is still running. Build target tasks
    are named after their target, followed by the build mode when it is not the
    default one.
    """
    for task in GPS.Task.list():
        if task.name() == name or task.name().startswith(name + " ("):
            GPS.Logger("ALIRE").log("Interrupting the '%s' task" % task.name())
            task.interrupt()


def _run_setup_target(target, root):
    """
    Run one of the ALIRE_SETUP_TARGETS in `root` and return a promise resolving
    to its exit status.

    Kept out of '_setup_alire_crate' so that 'alire.state_machine' can exercise
    the sequence with no 'alr' to run, which is the only way to test it where the
    testsuite cannot provide a fake one.
    """
    # share/support/ui is added to sys.path after share/support/core.
    from workflows.promises import TargetWrapper

    return TargetWrapper(target).wait_on_execute(directory=root)


def _restore_saved_env():
    """
    Give the environment variables we overrode for a crate their former values.
    """
    global saved_env

    for name, value in saved_env.items():
        GPS.setenv(name, value)

        if value:
            os.environ[name] = value
        else:
            # 'GPS.setenv' does not propagate to 'os.environ', so the variable
            # might never have been there.
            os.environ.pop(name, None)

    saved_env = {}


def _start_setup_feedback():
    """
    Tell the user that Alire is setting up the crate, which can take several
    minutes the first time, rather than leave GNAT Studio looking idle.
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

    # The Locations view might not be in the current perspective.
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


def _abandon_setup():
    """
    Give up on the pending Alire setup: report the next project load's errors
    again and give the environment back the values it had before we entered the
    crate (ALIRE, and whatever 'alr printenv' had already applied).

    Forget what Alire told us about the crate too: keeping the project files
    reported by 'alr show' would make loading one of them take the shortcut meant
    for a crate that is set up, so the setup would never be retried.
    """
    global alire_state, alire_manifest
    global alire_project_files, alire_pending_project_files

    GPS.Project.set_ignore_load_errors(False)
    _restore_saved_env()
    alire_state = None
    alire_manifest = None
    alire_project_files = []
    alire_pending_project_files = []


def _report_setup_problem(message):
    """
    Report `message` about the crate we are setting up in the Locations view.
    """
    if not project_to_reload:
        return

    file, _ = project_to_reload
    GPS.Locations.add(
        "Alire",
        GPS.File(file),
        1,
        1,
        message,
        importance=GPS.Message.Importance.HIGH,
    )


def _report_setup_failure(target, status):
    """
    Report that the setup failed while running `target` and give up on it. This
    also covers `target` being interrupted from the Task Manager.
    """
    GPS.Logger("ALIRE").log("'%s' failed with status %s" % (target, status))

    _report_setup_problem(
        "Alire setup failed: '%s' exited with status %s. "
        "See the Messages view for the complete output." % (target, status)
    )

    _abandon_setup()


def _watch_for_interruption(generation):
    """
    Watch the task monitoring the setup sequence of `generation` and clean up
    after it if the user interrupts it from the Task Manager.

    An interrupted task is simply never given control again: the generator
    driving the sequence is neither resumed nor closed, so it cannot notice by
    itself and its cleanup code would never run.
    """
    task_name = _setup_task_name(generation)

    def check_task(timeout):
        if generation != alire_generation or alire_state is None:
            # The sequence is over, or a newer one has superseded it.
            timeout.remove()
            return

        if alire_state == ALIRE_STATE_RELOADING:
            # The final project load is not interruptible.
            return

        if task_name in [task.name() for task in GPS.Task.list()]:
            return

        timeout.remove()
        GPS.Logger("ALIRE").log("The Alire setup has been interrupted")

        if alire_state != ALIRE_STATE_DETECTED:
            # 'alire_state' is then the name of the target still running, which
            # should neither go on fetching a toolchain nor have its output
            # parsed into a later sequence.
            _interrupt_task(alire_state)

        _stop_setup_feedback()
        _report_setup_problem(
            "Alire setup interrupted: the project has not been reloaded."
        )
        _abandon_setup()

    GPS.Timeout(ALIRE_INTERRUPTION_CHECK_PERIOD, check_task)


def _load_alire_project(generation):
    """
    Reload the project once Alire has set the environment, returning whether the
    reload actually happened: 'GPS.Project.load' can return without loading
    anything (a cancelled save of modified editors, a project file that is not
    there), in which case the 'project_changing' hook is not run and the caller
    must clean up rather than wait for a load that will never come.
    """
    global alire_state, alire_project_files

    file, root = project_to_reload
    GPS.Logger("ALIRE").log("Alire configuration finished, reloading %s" % str(file))

    # This load is the real one, as opposed to the intermediate one performed
    # before Alire had set the environment: report its errors.
    GPS.Project.set_ignore_load_errors(False)

    alire_state = ALIRE_STATE_RELOADING
    GPS.Project.load(file)

    if generation != alire_generation:
        # Loading a project runs the main loop, so another project may have been
        # asked for meanwhile: it owns the state now.
        GPS.Logger("ALIRE").log("Another project has been loaded during the reload")
        return False

    if alire_state == ALIRE_STATE_RELOADING:
        # 'on_project_changing' would have cleared the state: the load did not
        # happen, so give up rather than have the next one taken for ours.
        GPS.Logger("ALIRE").log("%s has not been loaded after all" % str(file))
        _report_setup_problem(
            "Alire setup failed: %s could not be loaded. "
            "See the Messages view for the complete output." % str(file)
        )
        _abandon_setup()
        return False

    # The crate is set up: loading another project of the same manifest does not
    # need Alire to be run again.
    alire_project_files = list(alire_pending_project_files)

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

    return True


def _setup_alire_crate(task, root, generation):
    """
    Run the Alire setup sequence asynchronously: synchronize the crate, determine
    which GPR project should be loaded and set the crate's environment, then
    reload that project.

    `task` monitors this sequence and reports its progress. `generation` is the
    value of `alire_generation` when the manifest was detected, which is how the
    sequence is abandoned when the user loads another project meanwhile.
    """
    global alire_state

    if generation != alire_generation:
        # The task manager can give us our first slice well after the detection.
        GPS.Logger("ALIRE").log(
            "Another project has been loaded: nothing left to set up"
        )
        return

    _start_setup_feedback()
    completed = False

    try:
        for index, target in enumerate(ALIRE_SETUP_TARGETS):
            alire_state = target
            task.set_progress(index, len(ALIRE_SETUP_TARGETS))
            GPS.Logger("ALIRE").log("Running '%s'..." % target)
            status = yield _run_setup_target(target, root)

            if generation != alire_generation:
                GPS.Logger("ALIRE").log(
                    "Another project is being loaded: abandoning the Alire setup"
                )
                return

            if status != 0:
                _report_setup_failure(target, status)
                return

        completed = _load_alire_project(generation)
    finally:
        # When a newer sequence has taken over, it owns the feedback now.
        if generation == alire_generation:
            _stop_setup_feedback()

            if not completed and alire_state is not None:
                # Neither completed nor already reported as failed. An
                # interruption from the Task Manager never gets here, which is
                # what '_watch_for_interruption' is for.
                GPS.Logger("ALIRE").log("The Alire setup has been abandoned")
                _abandon_setup()


def on_project_recomputed(hook):
    global alire_started_generation

    GPS.Logger("ALIRE").log(f"on_project_recomputed called. alire_state: {alire_state}")

    if (
        alire_state == ALIRE_STATE_DETECTED
        and alire_started_generation != alire_generation
    ):
        # The intermediate project is loaded: start the sequence in the
        # background, and only once, since the project view can be recomputed
        # again before the sequence has had a chance to run.
        from workflows import task_workflow

        generation = alire_generation
        alire_started_generation = generation
        task_workflow(
            _setup_task_name(generation),
            _setup_alire_crate,
            root=project_to_reload[1],
            generation=generation,
        )
        _watch_for_interruption(generation)

    elif not GPS.getenv("ALIRE"):
        # We are not loading an Alire project: unset the aliases
        # on Alire build targets.
        update_aliases_for_alire_targets(is_alire_project=False)


def on_project_changing(hook, file):
    """
    Detect whether we are dealing with an Alire project. If so, remember it so
    that 'on_project_recomputed' can start the setup sequence once GNAT Studio is
    done loading it; the project is reloaded once the environment is set.
    """
    global project_to_reload, alire_manifest
    global alire_project_files, alire_pending_project_files
    global alire_state, alire_generation

    if alire_state == ALIRE_STATE_RELOADING:
        GPS.Logger("ALIRE").log(f"Loading Alire project through: {file.path}")
        alire_state = None
        project_to_reload = None
        return

    # Unless we detect a new manifest below, the project about to be loaded is
    # a regular one: report its errors.
    GPS.Project.set_ignore_load_errors(False)

    if alire_state is None and file.path in alire_project_files:
        GPS.Logger("ALIRE").log(
            f"{file.path} already known as Alire project file, "
            + "skipping Alire detection and synchronization."
        )
        return

    # A sequence still running is for a project we are not loading anymore:
    # bumping the generation below turns it into a no-op, and interrupting the
    # 'alr' it waits on keeps its output out of the sequence superseding it.
    if alire_state not in (None, ALIRE_STATE_DETECTED):
        _interrupt_task(alire_state)

    alire_generation += 1
    alire_state = None
    project_to_reload = None

    _restore_saved_env()

    # The crate we were in has just had its environment restored, so loading one
    # of its project files does need Alire again. 'alr show' fills the list back
    # for the manifest detected below. This can't be done in
    # 'Alire_Parser.__init__': a parser is created per target launch, so the
    # 'Alire Printenv' one would wipe what the 'Alire Show' one gathered.
    alire_project_files = []
    alire_pending_project_files = []

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
        alire_state = ALIRE_STATE_DETECTED

        # Set ALIRE right away, so that the language server does not launch its
        # own 'alr' and contend with ours for the crate's lock. Its former value
        # is remembered next to the ones 'alr printenv' sets, and restored with
        # them when we are done with this crate.
        saved_env["ALIRE"] = GPS.getenv("ALIRE")
        GPS.setenv("ALIRE", "True")

        # The project about to be loaded is an intermediate one, which can't load
        # successfully before Alire has set the environment: don't report its
        # errors, we reload it ourselves once that is done.
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

        # The sequence this parser belongs to: an 'alr' we have given up on can
        # still deliver output afterwards, which must not reach the sequence that
        # superseded it.
        self.generation = alire_generation

    def on_stdout(self, text, command):
        global saved_env, project_to_reload, alire_manifest
        global alire_pending_project_files

        if self.generation != alire_generation:
            GPS.Logger("ALIRE").log("Ignoring the output of an abandoned 'alr' run")
            return

        for line in text.splitlines():
            m = self.export_var_regexp.fullmatch(line)

            if m:
                # Output of 'alr printenv'. Values might be quoted (e.g. export
                # PATH="/home/something"), hence 'shlex.split'.
                name = m.group(1)
                value = shlex.split(m.group(2))[0]
                GPS.Logger("ALIRE").log("%s=%s" % (name, value))

                # Only record the value the variable had before *we* touched it:
                # 'alr printenv' exports ALIRE itself, which we have already
                # overridden above, and restoring "True" would leave the crate's
                # environment behind for the rest of the session.
                saved_env.setdefault(name, GPS.getenv(name))

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
                        projet_file_abs_path = (
                            os.path.join(root, project_file_path)
                            if not os.path.isabs(project_file_path)
                            else project_file_path
                        )

                        # Remembered so that loading another project of the same
                        # manifest does not re-run Alire. Only becomes the list
                        # consulted for that once the setup has succeeded.
                        alire_pending_project_files.append(projet_file_abs_path)

                        project_to_reload = (
                            projet_file_abs_path,
                            root,
                        )


def register_hooks():
    """
    Connect to the hooks driving the Alire integration.

    Done at load time when 'alr' is on the PATH; a test standing in for Alire
    itself has no 'alr' to find and calls this explicitly.
    """
    GPS.Hook("project_changing").add(on_project_changing)
    GPS.Hook("project_view_changed").add(on_project_recomputed)


if alr:
    register_hooks()

GPS.parse_xml(ALIRE_MODELS_XML + ALIRE_TARGETS_XML)
