from e3.testsuite.driver import TestDriver
from e3.testsuite.result import TestStatus
from e3.os.fs import df
import os
import logging
import traceback

TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))

PACKAGE_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)


class GPSTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    def should_skip(self):
        """Handle of 'skip' in test.yaml.
        :return: None if the test should not be skipped, a TestStatus
            otherwise.
        :rtype: None | TestStatus
        """
        if 'skip' in self.test_env:
            eval_env = {
                'env': self.env,
                'test_env': self.test_env,
                'disk_space': lambda: df(self.env.working_dir)}

            for status, expr in self.test_env['skip']:
                try:
                    if eval(expr, eval_env):
                        return TestStatus[status]
                except Exception:
                    logging.error(traceback.format_exc())
                    return TestStatus.ERROR
        return None
