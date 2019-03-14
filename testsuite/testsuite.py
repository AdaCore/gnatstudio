#!/usr/bin/env python
from drivers.basic import BasicTestDriver
from e3.fs import ls
from e3.testsuite import Testsuite
from e3.os.process import Run, STDOUT
from e3.env import Env
import os
import logging
import tempfile


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
        if 'TMPDIR' in os.environ:
            old_tmpdir = os.environ['TMPDIR']
            os.environ['TMPDIR'] = ""

        command = ['Xvfb', ':%s' % num, '-screen', '0',
                   '1600x1200x24', '-ac']

        self.xvfb_handle = Run(command,
                               bg=True,
                               output=xvfb_file_name,
                               error=STDOUT)

        if old_tmpdir is not None:
            os.environ['TMPDIR'] = old_tmpdir

    def stop(self):
        # Send SIGTERM
        self.xvfb_handle.internal.terminate()


class GPSPublicTestsuite(Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {'default': BasicTestDriver}

    def add_options(self):
        self.main.argument_parser.add_argument(
            "--noxvfb",
            default=False,
            action="store_true",
            help="disable Xvfb")
        self.main.argument_parser.add_argument(
            "--build",
            default="",
            action="store",
            help="Ignored, here for compatibility purposes")

    def tear_up(self):
        # Set a gnatdebug common to all tests
        os.environ['ADA_DEBUG_FILE'] = os.path.join(self.test_dir,
                                                    'gnatdebug')

        # Launch Xvfb if needs be
        self.xvfb = None
        if (not self.main.args.noxvfb) and Env().platform.endswith('linux'):
            self.xvfb = Xvfb(1)
            os.environ['DISPLAY'] = ':1'

        # Export the WINDOWS_DESKTOP environment variable to
        # test GPS in a separate virtual desktop on Windows
        if Env().build.os.name == 'windows':
            os.environ['WINDOWS_DESKTOP'] = "gps_desktop"

    def tear_down(self):
        super(GPSPublicTestsuite, self).tear_down()
        if self.xvfb:
            self.xvfb.stop()

    def get_test_list(self, sublist):
        # The tests are one per subdir of "tests"
        if sublist:
            dirs = [os.path.abspath(os.path.join(self.test_dir, '..', s))
                    for s in sublist]
        else:
            dirs = ls(os.path.join(self.test_dir, '*'))
        results = []
        for d in dirs:
            if os.path.isdir(d):
                # Create the test.yamls if they don't exist!
                yaml = os.path.join(d, 'test.yaml')
                basename = os.path.basename(d)

                if not os.path.exists(yaml):
                    with open(yaml, 'wb') as f:
                        logging.info("creating {} for you :-)".format(yaml))
                        f.write("title: '{}'\n".format(basename))
                results.append(os.path.join(basename, 'test.yaml'))

        logging.info('Found %s tests %s', len(results), results)
        logging.debug("tests:\n  " + "\n  ".join(results))
        return results

    @property
    def default_driver(self):
        return 'default'
