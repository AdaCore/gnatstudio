#!/usr/bin/env python
from drivers.basic import BasicTestDriver, Xvfbs
from e3.testsuite import Testsuite
from e3.env import Env
import os

DEFAULT_XVFB_DISPLAY = 1001
# Where to launch Xvfb if nothing is otherwise specified


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

    def set_up(self):
        # Set a gnatdebug common to all tests
        os.environ['ADA_DEBUG_FILE'] = os.path.join(
            self.test_dir, 'tests', 'gnatdebug')

        # The following are used by the internal testsuite
        os.environ['GNATSTUDIO_TESTSUITE_SCRIPTS'] = os.path.join(
            os.path.dirname(__file__), 'internal', 'scripts')
        os.environ['GNATSTUDIO_GVD_TESTSUITE'] = os.path.join(
            os.path.dirname(__file__), 'internal', 'gvd_testsuite')
        os.environ['GPS_SRC_DIR'] = os.path.join(
            os.path.dirname(__file__), '..')
        os.environ['PYTHONPATH'] = "{}{}{}".format(
            os.path.join(os.path.dirname(__file__), 'internal', 'tests'),
            os.path.pathsep,
            os.environ.get('PYTHONPATH', ''))
        os.environ['GPS_TEST_CONTEXT'] = 'nightly'
        os.environ['CODEPEER_DEFAULT_LEVEL'] = '3'

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
