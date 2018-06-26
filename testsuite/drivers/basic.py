from e3.fs import mkdir, sync_tree
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
import os


class BasicTestDriver(TestDriver):
    """ Each test should have:
          - a test.yaml containing
                title: '<test name>'

          - a test driver: a test.py

            If the execution returns code 100, it's an XFAIL.
    """

    def add_test(self, dag):
        self.add_fragment(dag, 'prepare')
        self.add_fragment(dag, 'run', after=['prepare'])

    def prepare(self, previous_values):
        mkdir(self.test_env['working_dir'])
        sync_tree(self.test_env['test_dir'],
                  self.test_env['working_dir'])

    def run(self, previous_values):
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

        # TODO: add support for valgrind
        process = Run([the_gps,
                       "--load={}".format(os.path.join(wd, 'test.py'))],
                      cwd=wd)
        output = process.out

        if output:
            # If there's an output, log it
            self.result.log.log = output

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
