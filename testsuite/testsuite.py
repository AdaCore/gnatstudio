#!/usr/bin/env python
from drivers.basic import BasicTestDriver, Xvfbs
from distutils.spawn import find_executable
from e3.testsuite import Testsuite
from e3.env import Env
import os

DEFAULT_XVFB_DISPLAY = 1001
# Where to launch Xvfb if nothing is otherwise specified

VALGRIND_OPTIONS = [
    "--quiet",                   # only print errors
    "--tool=memcheck",           # the standard tool
    # "--leak-check=full",         # report memory leaks
    # "--num-callers=100",       # more frames in call stacks
    # "--gen-suppressions=all",  # use this to generate suppression entries
    "--suppressions={base}/valgrind/valgrind-python.supp",
    "--suppressions={base}/valgrind/gps.supp",
    ]


class GSPublicTestsuite(Testsuite):

    def add_options(self, parser):
        parser.add_argument(
            "--noxvfb",
            default=False,
            action="store_true",
            help="disable Xvfb")
        parser.add_argument(
            "--build",
            default="",
            action="store",
            help="Ignored, here for compatibility purposes")
        parser.add_argument(
            "--valgrind_memcheck", action="store_true",
            help="Runs gnatstudio under valgrind, in memory"
                 " check mode. This requires valgrind on the PATH.")

    def set_up(self):

        base = os.path.dirname(__file__)
        # Set a gnatdebug common to all tests
        os.environ['ADA_DEBUG_FILE'] = os.path.join(
            self.test_dir, 'tests', 'gnatdebug')

        # The following are used by the internal testsuite
        os.environ['GNATSTUDIO_TESTSUITE_SCRIPTS'] = os.path.join(
            base, 'internal', 'scripts')
        os.environ['GNATSTUDIO_GVD_TESTSUITE'] = os.path.join(
            base, 'internal', 'gvd_testsuite')
        os.environ['GPS_SRC_DIR'] = os.path.join(base, '..')
        os.environ['PYTHONPATH'] = "{}{}{}".format(
            os.path.join(base, 'internal', 'tests'),
            os.path.pathsep,
            os.environ.get('PYTHONPATH', ''))
        os.environ['GPS_TEST_CONTEXT'] = 'nightly'
        os.environ['CODEPEER_DEFAULT_LEVEL'] = '3'

        # Prepare valgrind command line

        self.env.wait_factor = 1
        self.env.valgrind_cmd = []

        if self.env.options.valgrind_memcheck:
            self.env.valgrind_cmd = [find_executable("valgrind")
                                     ] + [opt.format(base=base)
                                          for opt in VALGRIND_OPTIONS]
            self.env.wait_factor = 40  # valgrind is slow

        # Launch Xvfb if needs be
        self.xvfb = None

        if (not self.main.args.noxvfb) and Env().platform.endswith('linux'):
            Xvfbs.start_displays(DEFAULT_XVFB_DISPLAY, self.main.args.jobs)

        # Export the WINDOWS_DESKTOP environment variable to
        # test GPS in a separate virtual desktop on Windows
        if Env().build.os.name == 'windows':
            os.environ['WINDOWS_DESKTOP'] = "gps_desktop"

    def tear_down(self):
        super(GSPublicTestsuite, self).tear_down()
        Xvfbs.stop_displays()

    @property
    def test_driver_map(self):
        return {'default': BasicTestDriver}

    @property
    def default_driver(self):
        return 'default'

    def test_name(self, test_dir):
        relative = os.path.relpath(test_dir, os.path.dirname(__file__))

        name = relative.replace(os.sep, '.').lstrip('tests.')

        # Special case to handle GAIA requirements
        if name == "Z999-999":
            return "regressions.Z999-999"
        return name
