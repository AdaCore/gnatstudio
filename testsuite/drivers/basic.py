from e3.fs import mkdir, sync_tree, cp
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
from drivers import GPSTestDriver
import os
import glob

GPS_DEV = "GPS_DEV"
# The name of the environment variable to position: if this is set, assume
# that tests are being run by a GPS developer, and capture the results in
# the directory pointed by this


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

    def _capture_for_developers(self):
        """Utility for GPS developers: if GPS_DEV is set, capture the
           logs in $GPS_DEV
        """
        printed = ""
        if GPS_DEV in os.environ:
            printed = "\n"
            tgt = os.environ[GPS_DEV]
            for g in glob.glob(os.path.join(self.gps_home,
                                            ".gps", "log", '*')):
                cp(g, tgt)
                printed += "captured log: {}\n".format(
                               os.path.join(tgt, os.path.basename(g)))
        return printed

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
        devel_gps = os.path.join(base, "gps", "obj", "gnatstudio")
        if os.path.exists(devel_gps):
            the_gps = devel_gps
        else:
            the_gps = "gnatstudio"

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

        is_error = False
        if process.status:
            # Nonzero status?
            if process.status == 100:
                # This one is an xfail
                self.result.set_status(TestStatus.XFAIL)
            else:
                # Unknown status!
                self.result.set_status(TestStatus.ERROR)
                is_error = True
        else:
            # Status is 0...
            if output:
                # ... and there is an output: that's a FAIL
                self.result.set_status(TestStatus.FAIL)
                is_error = True
            else:
                # ... and no output: that's a PASS
                self.result.set_status(TestStatus.PASS)

        if is_error:
            self.result.out += self._capture_for_developers()

        self.push_result()
