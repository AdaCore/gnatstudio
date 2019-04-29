from e3.fs import mkdir, sync_tree, echo_to_file
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
from drivers import GPSTestDriver
import os

PREFS = """<?xml version="1.0"?>
<GPS>
  <pref name="General-Splash-Screen">False</pref>
  <pref name="Smart-Completion-Mode" > 0</pref>
  <pref name="Default-VCS"></pref>
  <pref name="General/Display-Tip-Of-The-Day">FALSE</pref>
  <pref name="Documentation:GNATdoc/Doc-Spawn-Browser" >FALSE</pref>
  <pref name=":VCS/Traverse-Limit" > 1</pref>
</GPS>
"""


class BasicTestDriver(GPSTestDriver):
    """ Each test should have:
          - a test.yaml containing
                title: '<test name>'

          - a test driver: a test.cmd or a test.py. test.cmd has priority
                if it exists.

            If the execution returns code 100, it's an XFAIL.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, previous_values):
        mkdir(self.test_env['working_dir'])
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

        # Create .gps
        self.gps_home = os.path.join(self.test_env['working_dir'], '.gps')
        mkdir(self.gps_home)
        mkdir(os.path.join(self.gps_home, 'plug-ins'))
        mkdir(os.path.join(self.gps_home, 'log_files'))
        echo_to_file(os.path.join(self.gps_home, 'preferences.xml'), PREFS)
        echo_to_file(os.path.join(self.gps_home, "gnatinspect_traces.cfg"),
                     ">gnatinspect.log\n")

    def run(self, previous_values):
        # Check whether the test should be skipped
        skip = self.should_skip()
        if skip is not None:
            self.result.set_status(skip)
            self.push_result()
            return False

        # If there's a test.cmd, execute it with the shell;
        # otherwise execute test.py.
        wd = self.test_env['working_dir']
        base = os.path.abspath(os.path.join(os.path.dirname(__file__),
                                            "..", ".."))

        # In the development environment, run the development GPS,
        # otherwise use the GPS found on the PATH
        devel_gps = os.path.join(base, "gps", "obj", "gps")
        if os.path.exists(devel_gps):
            the_gps = devel_gps
        else:
            the_gps = "gps"

        test_cmd = os.path.join(wd, 'test.cmd')
        if os.path.exists(test_cmd):
            cmd_line = ['bash', test_cmd]
        else:
            cmd_line = [the_gps, "--load={}".format('test.py')]

        # TODO: add support for valgrind
        process = Run(
            cmd_line,
            cwd=wd,
            timeout=None if 'GPS_PREVENT_EXIT' in os.environ else 120,
            env={'GPS_HOME': self.gps_home,
                 'GPS': the_gps},
            ignore_environ=False)
        output = process.out

        if output:
            # If there's an output, capture it
            self.result.out = output

        if process.status:
            # Nonzero status?
            if process.status == 100:
                # This one is an xfail
                self.result.set_status(TestStatus.XFAIL)
            else:
                # Unknown status!
                self.result.set_status(TestStatus.ERROR)
        else:
            # Status is 0...
            if output:
                # ... and there is an output: that's a FAIL
                self.result.set_status(TestStatus.FAIL)
            else:
                # ... and no output: that's a PASS
                self.result.set_status(TestStatus.PASS)

        self.push_result()
