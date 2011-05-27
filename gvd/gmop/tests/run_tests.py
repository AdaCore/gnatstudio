#! /usr/bin/env python

__authors__ = [
  '"Jean-Charles Delay" <delay@adacore.com>',
]


import os
import sys
import time

from getopt import getopt

from subprocess import PIPE
from subprocess import Popen

from signal import SIGINT
from signal import signal


# Timeout limit in second for each test.
TIMEOUT = 3
TEST_BIN = 'obj/mi_check'

# Some constants about test case files.
CASES_SUBPATH = 'cases'
CASES_EXTENTION = '.mi'

# Colors used in output.
BLUE = '\x1b[1;34m'
GREEN = '\x1b[0;32m'
NC = '\x1b[0m'
RED = '\x1b[0;31m'


# Global context map.
global _ctx
_ctx = {
  'verbose' : False,
  'categories' : [],
}


def sigint_handler(signal, frame):
  sys.stdout.write('\nSignal SIGINT caught, exiting...')
  sys.exit(1)


class CmdlineError(Exception):
  """Base exception for all exceptions raised in Cmdline."""
  pass


class Cmdline(object):
  def __init__(self, argv):
    self.__argv = argv

  def usage(self):
    return 'usage: python run_tests.py [-hv] [-c <category> [-c <category>]...]'

  def parse(self):
    try:
      (opts, args) = getopt(self.__argv, 'hvc:',
                            ['help', 'verbose', 'category='])
    except getopt.GetoptError as error:
      raise CmdlineError(str(error))
    global _ctx
    for (opt, arg) in opts:
      if opt in ('-h', '--help'):
        self.usage()
        sys.exit(0)
      elif opt in ('-v', '--verbose'):
        _ctx['verbose'] = True
      elif opt in ('-c', '--category'):
        _ctx['categories'].append(arg)


class TestDriverError(Exception):
  """Base exception for all exceptions raised in TestDriver."""
  pass


class TestDriver(object):

  """Main class to collect and run test cases.

  Attributes:
    testdir: TODO(jcd)
    categories: A list of test categories, corresponding to a leaf directory.
    cases: A dictionary associating list of mi files to their category.
  """

  def __init__(self, testdir):
    """Inits TestDriver with command line arguments (stripped of argv[0]).

    Args:
      TODO(jcd)
    """
    self.__testdir = testdir
    self.__testbin = os.path.join(testdir, TEST_BIN)
    self.__categories = []
    self.__cases = {}

  def _findLeafDirs(self, base_dir):
    """Protected method that returns a list of all leaf directories in the
    filesystem from the specified base_dir, e.g.

        base_dir
        |-- dir1
        |-- dir2
        |   |-- dir4
        |   |-- dir5
        |-- dir3

    Args:
      The root directory from where starting the search. Defaulted to the
      current working directory.

    Returns:
      A list of BASE_DIR leaves [ "dir1", "dir4", "dir5", "dir3" ]
    """

    # Tests whether the directory is a leaf or if it contains subdirectories.
    found  = False
    for item in os.listdir(base_dir):
      directory = os.path.join(base_dir, item)
      if os.path.isdir(directory) and item != '.svn':
        found = True
        break
    # Recursion stop condition, the directory is a leaf, so returns it.
    if not found:
      return [base_dir]

    # Directory has subdirectories. Collects them.
    leaves = []
    for item in os.listdir(base_dir):
      directory = os.path.join(base_dir, item)
      if os.path.isdir(directory) and item != '.svn':
        leaves = leaves + self._findLeafDirs(directory)
    return leaves

  def _findMiFiles(self, search_path, categories=None):
    """Protected method to find the mi tests file on filesystem.

    This function looks for the .mi tests files, eventually filtering depending
    in the category if specified.

    Returns a dict with a entry for each available category and a corresponding
    list of absolute path to the .mi files.
    """

    # Compute search path value.
    if not os.path.exists(search_path) or not os.path.isdir(search_path):
      raise TestDriverError('cannot find test cases directory')
    # Find leaf directories.
    leaves = self._findLeafDirs(search_path)
    if categories and len(categories):
      leaves = filter(lambda x: os.path.basename(x) in categories, leaves);
    # Find '.mi' files.
    files = {}
    for leaf in leaves:
      item = os.path.basename(leaf)
      files[item] = []
      for f in os.listdir(leaf):
        p = os.path.join(leaf, f)
        if os.path.isfile(p) and os.path.splitext(p)[1] == CASES_EXTENTION:
          files[item].append(p)
    return files

  def load(self):
    """Find and load test categories and cases.

    Find the list of available categories and their associated test cases, and
    store them in the class attributes.
    """

    if not os.path.exists(self.__testbin):
      print('test executable does not exists')
      sys.exit(1)
    if not os.access(self.__testbin, os.X_OK):
      print('cannot run test executable: permission denied')
      sys.exit(1)
    self.__testdir = os.path.join(self.__testdir, CASES_SUBPATH)
    self.__cases = self._findMiFiles(self.__testdir, _ctx['categories'])
    return 1

  def _invoke(self, argv):
    ecode = None
    (stdout, stderr) = (None, None)
    # Runs the process.
    process = Popen(argv, stdout=PIPE, stderr=PIPE)
    # Wait for completion or for timeout.
    start = time.time()
    while time.time() - start < TIMEOUT:
      ecode = process.poll()
      if ecode is not None:
        break
      time.sleep(0.05)
    if ecode is not None:
      (stdout, stderr) = process.communicate()
    else:
      # The process has timed out, so we kill it.
      process.terminate()
    # ECODE is None if the process timed out.
    return (ecode, stdout, stderr)

  def run(self):
    total = 0
    errors = 0
    for key in self.__cases:
      if self.__cases[key] is None or not len(self.__cases[key]):
        print('Warn: No test case for category %s' % key)
        continue
      idx = 0
      suberrors = 0
      subtotal = len(self.__cases[key])
      global _ctx
      for case in self.__cases[key]:
        progress = idx * 100 / subtotal
        if _ctx['verbose']:
          print('%s(%*d/%d)%s [%s%s%s] %s' % (BLUE, len(str(subtotal)), idx + 1,
                  subtotal, NC, GREEN, key, NC,
                  case.replace(self.__testdir, '')[1:]))
        else:
          sys.stdout.write('Running: %.1f%% (%d/%d) [%s]\r' % (progress, idx,
                                                               subtotal, key))
          sys.stdout.flush()
        idx += 1
        total += 1
        (ecode, stdout, stderr) = self._invoke([self.__testbin, case])
        if ecode is None or ecode != 0:
          print('%sFAIL%s: %s' % (RED, NC, case))
          errors = errors + 1
      assert(idx == subtotal)
      if not _ctx['verbose']:
        print('Running: 100.0%% (%d/%d) [%s]' % (idx, subtotal, key))
      else:
        if not suberrors:
          print('%sSuccessfully passed %d test(s)%s' % (GREEN, subtotal, NC))
        else:
          print('%sFailed %d test(s) among %d%s' % (RED, suberrors, subtotal, NC))
      errors = errors + suberrors
    if not errors:
      print('%sSuccessfully passed %d all test(s)%s' % (GREEN, total, NC))
    else:
      print('%sFailed %d test(s) among all %d%s' % (RED, errors, total, NC))
    return errors


if __name__ == '__main__':
  path = os.path.split(sys.argv[0])
  # Register SIGINT handler.
  signal(SIGINT, sigint_handler)
  # Compute test directory path.
  testdir = os.path.abspath(path[0])
  # Parse the command line.
  Cmdline(sys.argv[1:]).parse()
  driver = TestDriver(testdir)
  driver.load()
  # Run the main loop.
  sys.exit(driver.run())
