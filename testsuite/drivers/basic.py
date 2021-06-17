from e3.fs import mkdir, sync_tree, cp
from e3.os.process import Run, STDOUT
from e3.testsuite.result import TestStatus, Log
from drivers import GPSTestDriver
import os
import difflib
import glob
import shutil
import tempfile

GPS_DEV = "GPS_DEV"
# The name of the environment variable to position: if this is set, assume
# that tests are being run by a GPS developer, and capture the results in
# the directory pointed by this


class Xvfb(object):
    def __init__(self, num):
        """Start a xvfb X11 server

        PARAMETERS
          num: the display number
        """
        self.num = num

        with tempfile.NamedTemporaryFile(suffix="xvfb") as f:
            xvfb_file_name = f.name

        # unset TMPDIR around call to Xvfb, to workaround
        # bug in Ubuntu: see
        # bugs.launchpad.net/ubuntu/+source/xorg-server/+bug/972324
        old_tmpdir = None
        if "TMPDIR" in os.environ:
            old_tmpdir = os.environ["TMPDIR"]
            os.environ["TMPDIR"] = ""

        command = ["Xvfb", ":%s" % num, "-screen", "0", "1600x1200x24", "-ac"]

        self.xvfb_handle = Run(command, bg=True, output=xvfb_file_name, error=STDOUT)

        if old_tmpdir is not None:
            os.environ["TMPDIR"] = old_tmpdir

    def stop(self):
        # Send SIGTERM
        self.xvfb_handle.internal.terminate()


class XvfbRegistry(object):
    """ A class to hold the Xvfb registries """

    def __init__(self):
        self.xvfbs = []

    def start_displays(self, start_display, jobs):
        """ Initialize Xvfb servers """
        self.start_display = start_display
        for slot in range(jobs):
            self.xvfbs.append(Xvfb(start_display + slot))

    def get_env(self, slot):
        """ Return the environment snippet needed for the given slot. """
        # Useful to bypass display setting when launching tests via anod
        if "GNATSTUDIO_NO_XVFB" in os.environ:
            return {}

        if self.xvfbs:
            return {"DISPLAY": ":{}".format(self.xvfbs[slot - 1].num)}
        return {}

    def stop_displays(self):
        for x in self.xvfbs:
            x.stop()


Xvfbs = XvfbRegistry()


class BasicTestDriver(GPSTestDriver):
    """ Each test should have:
          - a test.yaml containing
                title: '<test name>'

          - a test driver: a test.cmd or a test.py. test.cmd has priority
                if it exists.

            If the execution returns code 100, it's an XFAIL.
    """

    def add_test(self, dag):
        self.add_fragment(dag, "prepare")
        self.add_fragment(dag, "run", after=["prepare"])

    def prepare(self, previous_values, slot):
        testsuite_dir = os.path.join(os.path.dirname(__file__), "..")
        mkdir(self.test_env["working_dir"])
        sync_tree(self.test_env["test_dir"], self.test_env["working_dir"])

        # Create .gnatstudio
        self.gps_home = os.path.join(self.test_env["working_dir"], ".gnatstudio")
        mkdir(self.gps_home)

        # Populate the .gnatstudio dir
        sync_tree(
            os.path.abspath(os.path.join(testsuite_dir, "gnatstudio_home")),
            self.gps_home,
            delete=False,
        )
        if self.env.options.pycov:
            cp(os.path.join(testsuite_dir, "pycov_data", "pycov_startup.xml"),
               os.path.join(self.gps_home, "startup.xml"),)
            # Copy the coverage preference
            cp(os.path.join(testsuite_dir, "pycov_data", ".coveragerc"),
               self.test_env["working_dir"],)
            py_name = ".coverage"
            py_dir = os.path.join(testsuite_dir, "pycov_data")
            mkdir(py_dir)
            self.test_env["pycov"] = os.path.abspath(os.path.join(py_dir, py_name))
        else:
            self.test_env["pycov"] = ""

    def _capture_for_developers(self):
        """Utility for GPS developers: if GPS_DEV is set, capture the
           logs in $GPS_DEV
        """
        printed = ""
        if GPS_DEV in os.environ:
            printed = "\n"
            tgt = os.environ[GPS_DEV]
            for g in glob.glob(os.path.join(self.gps_home, "log", "*")):
                cp(g, tgt)
                printed += "captured log: {}\n".format(
                    os.path.join(tgt, os.path.basename(g))
                )
        return printed

    def run(self, previous_values, slot):
        # Check whether the test should be skipped
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        # If there's a test.cmd, execute it with the shell;
        # otherwise execute test.py.
        wd = self.test_env["working_dir"]
        base = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))

        # In the development environment, run the development GPS,
        # otherwise use the GS found on the PATH
        devel_gs = os.path.join(base, "gnatstudio", "obj", "gnatstudio")
        test_cmd = os.path.join(wd, "test.cmd")

        if os.path.exists(devel_gs):
            # We are testing the development executable: we need to
            # pass the valgrind command ourselves.
            if os.path.exists(test_cmd):
                # run via a test.cmd
                GS = " ".join(self.env.valgrind_cmd + [devel_gs])
                cmd_line = ["bash", test_cmd]
            else:
                # run the executable directly
                GS = devel_gs
                cmd_line = self.env.valgrind_cmd + [devel_gs, "--load=python:test.py"]
        else:
            # We are testing the real 'gnatstudio' script.
            # In this case we rely on GPS_WRAPPER to carry the
            # valgrind command.
            GS = "gnatstudio"
            if os.path.exists(test_cmd):
                # run via a test.cmd
                cmd_line = ["bash", test_cmd]
            else:
                # run the script directly
                cmd_line = [GS, "--load=python:test.py"]

        env = {
            "GNATSTUDIO_HOME": self.test_env["working_dir"],
            "GNATINSPECT": shutil.which("gnatinspect") + " --exit",
            "GNATSTUDIO": GS,
            "GPS": GS,
            "GPS_WRAPPER": " ".join(self.env.valgrind_cmd),
            "GNATSTUDIO_PYTHON_COV": self.test_env["pycov"],
        }

        env.update(Xvfbs.get_env(slot))

        process = Run(
            cmd_line,
            cwd=wd,
            timeout=(
                None
                if "GPS_PREVENT_EXIT" in os.environ
                else (120 * self.env.wait_factor)
            ),
            env=env,
            ignore_environ=False,
        )
        output = process.out

        if output:
            # If there's an output, capture it
            self.result.log += output

        is_error = False
        if process.status:
            # Nonzero status?
            if process.status == 100:
                # This one is an xfail
                self.result.set_status(TestStatus.XFAIL)
            elif process.status == 99:
                # This is intentionally deactivated in this configuration
                self.result.set_status(TestStatus.SKIP)
            else:
                # Unknown status!
                self.result.set_status(TestStatus.ERROR)
                is_error = True
        else:
            # Status is 0...
            if output:
                # ... and there is an output: compare it to test.out
                # if it exists
                test_out = os.path.join(wd, "test.out")

                if os.path.exists(test_out):
                    with open(test_out, "r") as f:
                        expected = f.read()

                    res = "\n".join(
                        difflib.unified_diff(expected.splitlines(), output.splitlines())
                    )
                    if res == "":
                        self.result.set_status(TestStatus.PASS)
                    else:
                        self.result.out = Log(output)
                        self.result.expected = Log(expected)
                        self.result.diff = Log(res)
                        self.result.set_status(TestStatus.FAIL)
                        is_error = True

                else:
                    # ... if there's no test.out, that's a FAIL
                    self.result.set_status(TestStatus.FAIL)
                    is_error = True
            else:
                # ... and no output: that's a PASS
                self.result.set_status(TestStatus.PASS)

        if is_error:
            self.result.log += self._capture_for_developers()

        self.push_result()
