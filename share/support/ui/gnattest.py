"""
This file provides support for gnattest.
"""

#
# No user customization below this line
#

from functools import reduce
import json
import os.path
import re
import shutil
import tempfile

from gnatemulator import GNATemulator
import GPS
from gs_utils import hook, in_ada_file, interactive
from os_utils import locate_exec_on_path
import workflows
from workflows.promises import ProcessWrapper, TargetWrapper


last_gnattest = {
    'project':     None,  # project which gnattest run for
    'root':        None,  # root project opened before switching to harness
    'harness':     None,  # harness project name before switching to it
    'harness_dir': None   # Explicit specified directory to harness project
}


TOOL_VERSION_REGEXP = re.compile(r"[a-zA-Z\s]+ ([0-9]*)\.?([0-9]*w?)")


def run_test_list_in_emulator(main_name):
    """
    We run a test-drivers list in GNATemulator. If we have one executable,
    we have access to the Locations view. Otherwise, we run all the executables
    in consoles. This function does not build the executables, we can't
    get the main file associated with a given executable. It yields the promise
    that would be `returned` by run_gnatemu, so that we can launch multiple
    gnatemu instance in parallel.
    """
    fname = get_driver_list()
    with open(fname) as f:
        lines = [line.strip() for line in f]
    in_console = len(lines) > 1
    for exec_path in lines:
        yield GNATemulator.run_gnatemu([exec_path], in_console=in_console)


__targetsDef = [
    ["Run test driver with emulator", "run-test-driver-with-emulator",
     lambda x: GNATemulator.build_and_run(
         x, in_console=False), "gps-gnattest-run"],
    ["Run test-drivers list with emulator",
     "run-test-drivers-list-with-emulator",
     run_test_list_in_emulator, "gps-gnattest-run"]]


def run(project, target, extra_args="", synchronous=False, force=False):
    """ Run gnattest and switch to harness if success. """
    last_gnattest['project'] = project
    GPS.BuildTarget(target).execute(extra_args=extra_args,
                                    synchronous=synchronous,
                                    force=force)


def version(exe):
    """
    Return the tool version as (major version, minor version)
    """
    version_out = GPS.Process(exe + " --version").get_result()

    # Support gnattest built in dev mode
    if "GNATTEST Pro dev" in version_out:
        return 'dev'

    matches = TOOL_VERSION_REGEXP.findall(version_out.splitlines()[0])
    return matches[0]


def use_rts_and_target_options():
    """
    Return True if GNATtest should be launched with the --target and --RTS
    options.
    This is only available with 22+ GNATtest versions: older GNATtest versions
    should be prefixed by the project's target instead (e.g: arm-eabi-gnattest)
    """

    if locate_exec_on_path('gnattest'):
        v = version('gnattest')
        if v == 'dev':
            return True
        major, _ = v
        return int(major) >= 22

    return False


def get_gnattest_exe():
    """
    Return the GNATtest executable that should be used.
    Older versions of GNATtest used to be prefixed by the target
    (e.g: arm-eabi-gnattest) while it's not the case on newer GNATtest
    versions (22+).
    """
    should_use_prefix = not use_rts_and_target_options()

    if should_use_prefix:
        target = GPS.get_target()
        if target:
            return '{}-gnattest'.format(target)
        else:
            return 'gnattest'
    else:
        return 'gnattest'


def get_target_arg():
    """
    Return the --target attribute needed for GNATtest.
    This is only needed with newer versions of GNATtest, since older versions
    were prefixed by the target (e.g: arm-eabi-gnattest).
    """
    should_use_target = use_rts_and_target_options()

    if should_use_target:
        target = GPS.get_target()
        if target:
            return "--target=%s" % target
        else:
            return ""
    else:
        return ""


def get_runtime_arg():
    """
    Return the --RTS attribute needed for GNATtest.
    This is only needed with non-native targets, on newer GNATtest versions.
    """
    should_use_runtime = use_rts_and_target_options()

    if should_use_runtime:
        runtime = GPS.get_runtime()
        if runtime:
            return "--RTS=%s" % runtime
        else:
            return ""
    else:
        return ""


def get_driver_list():
    """ Check if root project has test_drivers.list file and return it. """
    root_project = GPS.Project.root().file().path
    (name, ext) = os.path.splitext(root_project)
    name = name + ".list"
    if os.path.exists(name):
        return name
    else:
        return ""


def get_harness_project_file(cur):
    """ Return name of harness project with last modification time """
    harness_dir = cur.get_attribute_as_string("Harness_Dir", "GNATtest")
    project_dir = cur.file().directory()
    object_dir = cur.get_attribute_as_string("Object_Dir")
    parent_dir = os.path.join(project_dir, object_dir)

    list = []

    if last_gnattest['harness_dir']:
        list.append(os.path.join(last_gnattest['harness_dir'],
                                 "test_driver.gpr"))
        list.append(os.path.join(last_gnattest['harness_dir'],
                                 "test_drivers.gpr"))

    if harness_dir == "":
        list.append(os.path.join(parent_dir,
                                 "gnattest", "harness",
                                 "test_driver.gpr"))
        list.append(os.path.join(parent_dir,
                                 "gnattest", "harness",
                                 "test_drivers.gpr"))
        list.append(os.path.join(parent_dir,
                                 "gnattest_stub", "harness",
                                 "test_drivers.gpr"))
    else:
        if os.path.isabs(harness_dir):
            list.append(os.path.join(harness_dir,
                                     "test_driver.gpr"))
            list.append(os.path.join(harness_dir,
                                     "test_drivers.gpr"))

        else:
            list.append(os.path.join(parent_dir,
                                     harness_dir,
                                     "test_driver.gpr"))
            list.append(os.path.join(parent_dir,
                                     harness_dir,
                                     "test_drivers.gpr"))

    def compare(a, b):
        if not os.path.exists(a):
            return b
        elif not os.path.exists(b) or \
                os.path.getmtime(a) > os.path.getmtime(b):

            return a
        else:
            return b

    return reduce(compare, list, "")


def get_user_project_file():
    root_project = GPS.Project.root()
    if last_gnattest['harness'] == root_project.file().path:
        user_project = last_gnattest['root']
    else:
        # Check if we are in the harness project
        if root_project.original_project():
            user_project = root_project.original_project().file().path

        # If we aren't, assume that this is the user project
        else:
            user_project = root_project.file().path
    return user_project


def open_harness_project(cur):
    """ Open harness project if it hasn't open yet."""
    if GPS.Project.root().is_harness_project():
        return

    prj = get_harness_project_file(cur)

    if os.path.exists(prj):
        last_gnattest['root'] = GPS.Project.root().file().path
        GPS.Project.load(prj, False, True)
        GPS.Console("Messages").write("Switched to harness project: " +
                                      GPS.Project.root().file().path + "\n")
        last_gnattest['harness'] = GPS.Project.root().file().path
    else:
        GPS.Console("Messages").write("No harness project found: %s!\n" %
                                      (prj))


def exit_harness_project():
    """ Leave harness project and open user's project. """
    GPS.Project.load(get_user_project_file(), False, True)
    GPS.Console("Messages").write("Exit harness project to: " +
                                  GPS.Project.root().file().path + "\n")


@hook('compilation_finished')
def __on_compilation_finished(category, target_name,
                              mode_name, status, cmd):

    if not target_name.startswith("GNATtest"):
        return

    if status:
        return

    hd = [arg[14:] for arg in cmd if arg.startswith("--harness-dir=")]
    last_gnattest['harness_dir'] = hd[0] if hd else ""

    open_harness_project(last_gnattest['project'])


def __update_build_targets_visibility():
    """
    Update the GNATtest/'Run Main' Build Targets visibility regarding
    the nature of the loaded project. Also check whether or not we need
    to display GNATtest emulator Build Targets.
    """
    try:
        test_run_target = GPS.BuildTarget("Run a test-driver")
        test_run_targets = GPS.BuildTarget("Run a test drivers list")
        run_main_target = GPS.BuildTarget("Run Main")
        test_run_emulator_target = GPS.BuildTarget(
            "Run test driver with emulator")
        test_run_emulator_targets = GPS.BuildTarget(
            "Run test-drivers list with emulator")
    except Exception:
        # In some rare cases GPS recompute project view before build targets
        # are actually created. We don't update targets in these cases.
        return

    if not GPS.Project.root().is_harness_project():
        run_main_target.show()
        test_run_target.hide()
        test_run_targets.hide()
        test_run_emulator_targets.hide()
        test_run_emulator_target.hide()
    elif get_driver_list() == "":
        """ We have a single test driver. """
        run_main_target.hide()
        test_run_targets.hide()
        test_run_emulator_targets.hide()
        if GNATemulator.gnatemu_on_path():
            test_run_emulator_target.show()
            test_run_target.hide()
        else:
            test_run_emulator_target.hide()
            test_run_target.show()
    else:
        """
        The file 'test_drivers.list' is present.
        We have a list of test drivers to execute.
        """
        run_main_target.hide()
        test_run_target.hide()
        test_run_emulator_target.hide()
        if GNATemulator.gnatemu_on_path():
            test_run_targets.hide()
            test_run_emulator_targets.show()
        else:
            test_run_targets.show()
            test_run_emulator_targets.hide()


def create_build_targets_gnatemu():
    """
    Creates the Build Targets used to call gnatemu when in a test harness.
    """
    global __targetsDef

    for target_def in __targetsDef:
        workflows.create_target_from_workflow(
            target_def[0], target_def[1], target_def[2], target_def[3],
            parent_menu='/Build/Emulator/%s/' % target_def[0])


@hook('gps_started')
def on_gps_start():
    """
    Make sure that the 'Run main' build target is not hidden when opening
    GPS with a non-harness project.
    """
    __update_build_targets_visibility()


@hook('project_view_changed')
def on_project_view_changed():
    """ Replace run target in harness project. """
    __update_build_targets_visibility()


def open_harness_filter(context):
    if GPS.Project.root().is_harness_project():
        return False

    if not isinstance(context, GPS.FileContext):
        return False

    project = context.project() or GPS.Project.root()

    return os.path.exists(get_harness_project_file(project))


@interactive("General", open_harness_filter, name="open harness",
             contextual="GNATtest/Open harness project")
def open_harness():
    """
    Open harness project for current project
    """
    project = GPS.current_context().project() or GPS.Project.root()

    open_harness_project(project)


def get_harness_dir():
    context = GPS.current_context()
    if not isinstance(context, GPS.FileContext):
        project = GPS.Project.root()
    else:
        project = context.project() or GPS.Project.root()
    harness_prj = get_harness_project_file(project)
    return os.path.dirname(harness_prj)


def gnattest_config():
    config = os.path.join(get_harness_dir(), ".gnattest-config.json")
    if os.path.exists(config):
        with open(config, "r") as f:
            return json.loads(f.read())
    else:
        return {"dump_test_inputs": False}


def is_harness_instr():
    if gnattest_config()["dump_test_inputs"]:
        return True
    return False


@workflows.run_as_workflow
def build_instr_harness_workflow():
    # Check if the generated harness was built with --dump-test-inputs. If not,
    # regenerate it.
    console = GPS.Console()
    if not is_harness_instr():
        cmd = [
            "gnattest",
            "-P", get_user_project_file(), "--dump-test-inputs"]
        console.write("Generating an instrumented test harness...")
        p = ProcessWrapper(cmd, spawn_console="")

        # Show the output in GPS's Messages window (use by default when an
        # empty string is passed to spawn_console)
        status, output = yield p.wait_until_terminate()

        # If it failed, exit early
        if status != 0:
            console.write(
                "Failed to generate an instrumented harness\n", mode="error"
            )
            yield status
    # Once done, we need to build the instrumented harness. For now, we will
    # only support the most basic gnattest usage: a single driver named
    # test_driver and use the test_driver-build-instr target of the generated
    # Makefile. TODO: support separate drivers and stub mode on.
    cmd = [
        "make", "-C", get_harness_dir(), "test_driver-build-inst"
    ]
    p = ProcessWrapper(cmd, spawn_console="")
    status, output = yield p.wait_until_terminate(show_if_error=True)
    console.write(output)

    if status != 0:
        console.write(
            "Failed to build the instrumented test harness\n", mode="error"
        )
    yield status


def start_fuzz(task, corpus_dir, fuzz_dir, force=False):
    """The 'gnatfuzz fuzz' workflow"""
    # Move away the previous fuzzing session dir

    fuzz_session_dir = os.path.join(fuzz_dir, "fuzz_testing", "session")
    if os.path.exists(fuzz_session_dir):
        shutil.rmtree(fuzz_session_dir)

    # TODO! account for scenario variables as done in the gnatfuzz plugin

    # Activate the GNATfuzz mode using tgen. TODO! active concolic execution by
    # default.
    os.environ["GNATFUZZ_TGEN"] = "1"
    args = [
        "-P",
        os.path.join(fuzz_dir, "fuzz_testing", "fuzz_test.gpr"),
        f"--corpus-path={corpus_dir}",
        f"--stop-criteria={fuzz_dir}"
        "/fuzz_testing/user_configuration/stop_criteria.xml",
    ]

    p = TargetWrapper("gnattest fuzz")
    yield p.wait_on_execute(extra_args=args, force=force)
    cmd = [
        "gnattest",
        "-P",
        get_user_project_file(),
    ]
    p = ProcessWrapper(cmd, spawn_console="")
    status, output = yield p.wait_until_terminate()
    if status != 0:
        GPS.console().write(
            "Failed to run: " + ' '.join(cmd) + "\n"
        )


def is_fuzz_running():
    """Return True if "gnatfuzz fuzz" is running"""
    tasks = [t for t in GPS.Task.list() if t.name() == "gnattest fuzz"]
    return len(tasks) > 0


def stop_fuzz():
    tasks = [t for t in GPS.Task.list() if t.name() == "gnattest fuzz"]
    if len(tasks) > 0:
        tasks[0].interrupt()


def has_gnatfuzz():
    if not locate_exec_on_path("gnatfuzz"):
        return False
    return True


def fuzz_subp_filter(context):
    """
    TODO! add filters to check that the cursor has selected a subprogram
    specification with associated testcases, and is supported by tgen and
    gnatfuzz.
    """
    return in_ada_file(context) and has_gnatfuzz()


@interactive("General", fuzz_subp_filter,
             contextual="GNATtest/Start\/Stop fuzzing subprogram",
             name="gnattest fuzz subprogram",
             description="Fuzz suprogram using test cases as initial seeds",
             contextual_group=GPS.Contextual.Group.EXTRA_INFORMATION)
def fuzz_subp_workflow():
    """
    Workflow to fuzz a subprogram using gnattest test-cases as a starting
    corpus.
    """

    # If the fuzzer is already running, stop it. TODO: make this specific to
    # the selected subprogram.
    if is_fuzz_running():
        stop_fuzz()
        return

    # We are going to compile: save everything that needs saving
    GPS.MDI.save_all()

    console = GPS.Console()
    context = GPS.current_context()
    local_file_basename = os.path.basename(context.file().path)
    local_file_fullname = context.file().path
    line = str(context.location().line())
    function_repr = local_file_basename + ":" + line
    function_hash = str(hash(function_repr))

    # Start by building the instrumented harness
    status = yield build_instr_harness_workflow()

    if status != 0:
        return

    # Then, execute the test runner with the adequate test filter. Run it in a
    # temporary directory: this is where the starting corpus will be generated.

    corpus_dir = tempfile.mkdtemp()
    cwd = os.getcwd()
    os.chdir(corpus_dir)
    console.write(
        "Generating starting corpus in " + corpus_dir + "\n"
    )
    cmd = [
        os.path.join(get_harness_dir(), "test_runner"),
        "--routines",
        local_file_basename + ":" + line,
    ]
    p = ProcessWrapper(cmd, spawn_console="")
    status, output = yield p.wait_until_terminate()
    if status != 0:
        console.write(
            "Failed to run: " + ' '.join(cmd) + "\n"
        )
        return status

    os.chdir(cwd)

    # We can now start fuzzing: execute the GNATfuzz generate workflow passing
    # the right subprogram. Ensure that the harness directory was cleaned
    # beforehand.

    user_project = GPS.Project(get_user_project_file())
    fuzz_dir = os.path.join(
        user_project.object_dirs()[0], "fuzz" + function_hash
    )

    if os.path.exists(fuzz_dir):
        shutil.rmtree(fuzz_dir)

    # To facilitate testing of the workflow, force the execution of the
    # BuildTargets if the environment variable GS_TESTSUITE_RUN is set. This
    # means that the BuildTargets will execute without waiting for a user
    # action (a click on the Execute button). TODO! there should be a cleaner
    # way to do that.
    force = bool(os.environ['GS_TESTSUITE_RUN'])
    p = TargetWrapper("gnatfuzz generate")
    yield p.wait_on_execute(
        extra_args=[
            "-S",
            local_file_fullname,
            "-L",
            line,
            "-o",
            fuzz_dir
        ],
        force=force,
    )

    # Now onto fuzzing: execute the GNATfuzz fuzz workflow. Run it in a
    # dedicated background task: the user will be able to stop it when clicking
    # back on it.

    workflows.task_workflow(
        "fuzz" + function_hash, start_fuzz, False,
        corpus_dir=corpus_dir, fuzz_dir=fuzz_dir,
        force=force,
    )
    shutil.rmtree(corpus_dir)


XML = r"""<?xml version="1.0" ?>
<gnattest>
  <project_attribute package="gnattest"
    name="harness_dir"
    editor_page="GNATtest"
    editor_section="Directories"
    label="Harness Directory"
    description="Used to specify the directory in which to """ \
    """place harness packages and project file for the test driver. If """ \
    """it's a relative path, it is considered relative to the """ \
    """object directory of the project file."
    hide_in="wizard library_wizard"
    >
    <string default=""/>
  </project_attribute>

  <project_attribute package="gnattest"
    name="tests_dir"
    editor_page="GNATtest"
    editor_section="Directories"
    disable_if_not_set="true"
    disable="gnattest.tests_root gnattest.subdir"
    label="Tests Directory"
    description="All test packages are placed in this directory. """ \
    """If it's a relative path, it is considered relative """ \
    """to the object directory of the project file. When all """ \
    """sources from all projects are taken recursively from """ \
    """all projects, directories with the given relative path are """ \
    """created for each project in their object directories and test """ \
    """packages are placed accordingly."
    >
    <string default="gnatest/tests"/>
  </project_attribute>

  <project_attribute package="gnattest"
    name="tests_root"
    editor_page="GNATtest"
    editor_section="Directories"
    disable_if_not_set="true"
    disable="gnattest.tests_dir gnattest.subdir"
    label="Tests Root"
    description="Test files are put in a same directory hierarchy """ \
    """as the sources with this directory as the root directory. If """ \
    """it's a relative path, it is considered relative to the """ \
    """object directory of the project file. When projects are """ \
    """considered recursively, directory hierarchies of tested """ \
    """sources are recreated for each project in their object """ \
    """directories and test packages are placed accordingly."
    hide_in="wizard library_wizard"
    />

  <project_attribute package="gnattest"
    name="subdir"
    editor_page="GNATtest"
    editor_section="Directories"
    disable_if_not_set="true"
    disable="gnattest.tests_dir gnattest.tests_root"
    label="Tests Subdir"
    description="Test packages are placed in a subdirectory of the """ \
    """corresponding source directory, with the specified name. Thus, """ \
    """each set of unit tests are placed in a subdirectory of the code """ \
    """under test. If the sources are in separate directories, each """ \
    """source directory will have a test subdirectory."
    hide_in="wizard library_wizard"
    >
  </project_attribute>

  <project_attribute package="gnattest"
    name="stubs_dir"
    editor_page="GNATtest"
    editor_section="Directories"
    disable_if_not_set="true"
    disable="gnattest.tests_dir gnattest.tests_root"
    label="Stubs Directories"
    description="The hierarchy of directories containing stubbed units """ \
    """is recreated in the specified directory, with stubs placed """ \
    """in directories corresponding to projects they are derived from. """ \
    """If it's a relative path, it is considered relative to the object """ \
    """directory of the project file. When projects are considered """ \
    """recursively, directory hierarchies of stubs are recreated for """ \
    """each project in their object directories and test packages """ \
    """are placed accordingly."
    >
  </project_attribute>

  <project_attribute package="gnattest"
    name="additional_tests"
    editor_page="GNATtest"
    label="Additional Tests"
    description="Sources described in given project are considered """ \
    """potential additional manual tests to be added to the test suite."
    hide_in="wizard library_wizard"
    >
    <string type="file" filter="project"/>
  </project_attribute>

  <project_attribute package="gnattest"
    name="skeletons_default"
    editor_page="GNATtest"
    label="Skeletons Default"
    description="Default behavior of generated skeletons."
    hide_in="wizard library_wizard"
    >
    <choice default="true">fail</choice>
    <choice>pass</choice>
  </project_attribute>

  <project_attribute package="gnattest"
    name="gnattest_switches"
    editor_page="GNATtest"
    list="true"
    label="GNATtest Switches"
    description="Custom switches for gnattest."
    hide_in="wizard library_wizard"
    >
    <string default=""/>
  </project_attribute>

  <project_attribute package="gnattest"
    name="gnattest_mapping_file"
    hide_in="all"
    editor_page="GNATtest"
    label="GNATtest Mapping File"
    description="Map tests to subprograms."
    />

  <action name="run gnattest">
    <filter_and>
      <filter id="Project only"/>
      <filter id="Non harness project"/>
    </filter_and>
    <description>Run gnattest on current project</description>
    <shell lang="python" output="none"
    >gnattest.run(GPS.current_context().project(), """ \
    """"GNATtest for project")</shell>
  </action>

  <action name="run gnattest on root">
    <filter_and>
      <filter id="Non harness project"/>
    </filter_and>
    <description>Run gnattest on root project</description>
    <shell lang="python" output="none"
    >gnattest.run(GPS.Project.root(), "GNATtest for root project")</shell>
  </action>

  <action name="exit harness" output="none">
    <filter id="Harness project"/>
    <description>Return to user project from current """ \
    """harness project</description>
    <shell lang="python">gnattest.exit_harness_project()</shell>
  </action>

  <action name="make single test" output="none">
    <filter_and>
      <filter id="Library package declaration"/>
      <filter id="Non harness project"/>
    </filter_and>
    <description>Run gnattest on single package</description>
    <shell lang="python" output="none"
    >gnattest.run(GPS.current_context().project(), "GNATtest for file")</shell>
  </action>

  <contextual action="run gnattest" after="GNATtest">
    <title>GNATtest/Generate unit test setup for %p</title>
  </contextual>

  <contextual action="exit harness" after="GNATtest">
    <title>GNATtest/Exit harness project</title>
  </contextual>

  <contextual action="make single test" after="GNATtest">
    <title>GNATtest/Generate unit test setup for %f</title>
  </contextual>

  <target-model name="gnattest" category="">
    <description>Generic launch of gnattest</description>
    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>-dd</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
    </command-line>
    <iconname>gps-build-all-symbolic</iconname>
    <switches command="gnattest" columns="2" lines="1">
      <title column="1" line="1">Files and directories</title>
      <title column="2" line="1">Other switches</title>
      <field label="harness directory"
             line="1"  column="1"
             switch="--harness-dir"
             separator="="
             as-directory="true"
             tip="is used to specify the directory in which to """ \
"""place harness packages and project file for the test driver." />
      <field label="separate tests root"
             line="1"  column="1"
             switch="--tests-root"
             separator="="
             as-directory="true"
             tip="The directory hierarchy of tested sources is """ \
    """recreated in this directory, and test packages are placed in """ \
    """corresponding directories." />
      <field label="tests subdir"
             line="1"  column="1"
             switch="--subdir"
             separator="="
             as-directory="true"
             tip="Test packages are placed in subdirectories" />
      <field label="tests directory"
             line="1"  column="1"
             switch="--tests-dir"
             separator="="
             as-directory="true"
             tip="All test packages are placed in this directory." />
      <field label="additional tests"
             line="1"  column="1"
             switch="--additional-tests"
             separator="="
             as-file="true"
             tip="Sources described in given project are considered """ \
    """potential additional manual tests to be added to the test suite." />
      <combo label="skeletons default"
             line="2"  column="1"
             switch="--skeleton-default"
             separator="="
             noswitch="fail"
             tip="Default behavior of generated skeletons.">
        <combo-entry label="Fail" value="fail"/>
        <combo-entry label="Pass" value="pass"/>
      </combo>
      <check label="automatic testcase generation (beta)"
             line="1"  column="2"
             switch="--gen-test-vectors"
             tip="Automatically generate testcases for all subprograms """ \
                 """ supporting automatic generation (beta). " />
      <check label="use stubbing"
             line="1"  column="2"
             switch="--stub"
             tip="Generates the testing framework that uses subsystem """ \
    """stubbing to isolate the code under test. " />
      <check label="generate harness only"
             line="1"  column="2"
             switch="--harness-only"
             tip="Create a harness for all sources, treating them as """ \
    """test packages." />
      <check label="no-subprojects"
             line="1"  column="2"
             switch="--no-subprojects"
             tip="Consider only the root project's source files." />
      <check label="silent"
             line="1"  column="2"
             switch="-q"
             tip="Suppresses noncritical output messages." />
      <check label="verbose"
             line="1"  column="2"
             switch="-v"
             tip="Verbose mode: generates version information." />
      <check label="validate type ext."
             line="1"  column="2"
             switch="--validate-type-extensions"
             tip="Enables substitution check: run all tests from all """ \
    """parents in order to check substitutability." />
    </switches>
  </target-model>

  <!-- This is model for run test-driver executables generated by GNATtest -->
  <target-model name="GNATtest run" category="">
    <description>Run a test-driver generated by GNATtest</description>
    <is-run>FALSE</is-run>
    <command-line>
      <arg>%E</arg>
      <arg>--passed-tests=hide</arg>
    </command-line>
    <server>Execution_Server</server>
    <iconname>gps-run-symbolic</iconname>

    <switches command="%(tool_name)s" columns="1" separator="=">
      <combo label="Default test behavior "
        switch="--skeleton-default="
        tip="How to count unimplemented tests in this run">
        <combo-entry label="Pass" value="pass"/>
        <combo-entry label="Fail" value="fail"/>
      </combo>

      <check label="Suppress passed tests output"
        switch="--passed-tests=hide"
        switch-off="--passed-tests=show"
        default="on"
        tip="hide passed tests from test river output"/>

    </switches>
  </target-model>

  <!-- This is model for run test-drivers.list generated by GNATtest -->
  <target-model name="GNATtest execution mode" category="">
    <description>Run a test-drivers.list generated by GNATtest</description>
    <is-run>FALSE</is-run>
    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>test_drivers.list</arg>
      <arg>--passed-tests=hide</arg>
    </command-line>
    <server>Execution_Server</server>
    <iconname>gps-run-symbolic</iconname>

    <switches command="%(tool_name)s" columns="1" separator="=">
      <spin label="Runs N tests in parallel"
        switch="--queues=" min="1" default="1"
        tip="Runs n tests in parallel (default is 1)." />

      <check label="Suppress passed tests output"
        switch="--passed-tests=hide"
        switch-off="--passed-tests=show"
        default="on"
        tip="hide passed tests from test river output"/>

    </switches>
  </target-model>

  <target model="gnattest" category="_Project_" name="GNATtest for project">
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>

    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>-dd</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="gnattest" category="_File_" name="GNATtest for file">
    <in-toolbar>FALSE</in-toolbar>
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>

    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>-dd</arg>
      <arg>-P%pp</arg>
      <arg>%X</arg>
      <arg>%F</arg>
    </command-line>
  </target>

  <target model="gnattest" category="_Project_"
          name="GNATtest for root project">
    <in-menu>FALSE</in-menu>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <read-only>TRUE</read-only>

    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>-dd</arg>
      <arg>-P%PP</arg>
      <arg>%X</arg>
    </command-line>
  </target>

  <target model="GNATtest run" category="Run"
          name="Run a test-driver"
          messages_category="test-driver">
    <visible>FALSE</visible>
    <in-menu>TRUE</in-menu>
    <in-toolbar>TRUE</in-toolbar>
    <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <target-type>executable</target-type>
    <command-line>
      <arg>%E</arg>
      <arg>--passed-tests=hide</arg>
    </command-line>
    <iconname>gps-gnattest-run</iconname>
  </target>

  <target model="GNATtest execution mode" category="Run"
          name="Run a test drivers list"
          messages_category="test-driver">
    <visible>FALSE</visible>
    <in-menu>TRUE</in-menu>
    <in-toolbar>TRUE</in-toolbar>
    <in-contextual-menus-for-projects>TRUE</in-contextual-menus-for-projects>
    <launch-mode>MANUALLY_WITH_DIALOG</launch-mode>
    <target-type>executable</target-type>
    <command-line>
      <arg>%python(gnattest.get_gnattest_exe())</arg>
      <arg>%python(gnattest.get_target_arg())</arg>
      <arg>%python(gnattest.get_runtime_arg())</arg>
      <arg>test_drivers.list</arg>
      <arg>--passed-tests=hide</arg>
    </command-line>
    <iconname>gps-gnattest-run</iconname>
  </target>

</gnattest>
"""

GPS.parse_xml(XML)

# We create the Build Targets related to gnatemu when GPS is launched.
# Afteward we only update the visibility of the affected Build Targets
create_build_targets_gnatemu()
