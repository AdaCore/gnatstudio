#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import GPS
import sys
from workflows import driver
from editor import click_in_text
from gi.repository import Gtk
from gps_utils.internal.utils import simple_error
from gps_utils.internal.asserts import SUCCESS, FAILURE, NOT_RUN, XFAIL

# Some of the imports here are necessary for some of the tests
from workflows.promises import hook, timeout, wait_tasks, wait_idle


def do_exit(timeout):
    """ Force an exit of GPS right now, logging as an error the contents
        of the Messages window. This is useful for capturing more traces
        for stalled tests that are about to get killed by rlimit.
    """
    timeout.remove()
    simple_error(GPS.Console("Messages").get_text())
    GPS.exit(force=1)


def run_test_driver(action_fn):
    """
    This function runs a test driver. A test driver is a workflow (see
    workflows and workflows.promises for more details) so you can use the yield
    construct to make asynchronous computations follow a synchronous control
    flow.

    The things that you yield needs to be either:
    - Instances of the Promise class that wraps an asynchronous callback
      computation
    - Other generators that will be called in a synchronous fashion

    You can use this function as a generator, so you can write a test like:

    @run_test_driver
    def test_driver():
        print "hello 1"
        yield timeout(500)
        print "hello 2"
        yield timeout(500)
        print "hello 3"
        if not gps_assert(...):
            return    # if you want to stop the test

        return XFAIL  # if you want the testsuite to mark the test as XFAIL

    """

    def workflow():
        _ = yield hook("gps_started")
        yield timeout(10)

        last_result = None

        try:
            action_gen = action_fn()
            if action_gen is not None:
                last_result = yield action_gen

        except Exception:
            import traceback
            GPS.Logger('TESTSUITE').log(
                "Driver workflow received an exception %s %s\n%s"
                % (sys.exc_info()[0],
                   sys.exc_info()[1],
                   traceback.format_exc()))

        finally:
            if "GPS_PREVENT_EXIT" not in os.environ:
                if last_result in (SUCCESS, FAILURE, NOT_RUN, XFAIL):
                    status = last_result
                else:
                    status = 0
                GPS.exit(force=True, status=status)

    # Install a timeout to catch the errors in GPS, if any, before rlimit
    # kills everything.

    # Exit GPS 10 seconds before the rlimit expires. If the rlimit
    # is not set, default to waiting 130 seconds.
    timeout_seconds = int(os.environ.get('GPS_RLIMIT_SECONDS', '130')) - 10
    GPS.Timeout(timeout_seconds * 1000, do_exit)

    # Run the workflow

    driver(workflow())


def editor_contextual(editor, name):
    """
    A generator that activates an editor contextual menu.
    Use it as::
        yield editor_contextual(editor, "Version Control/Commit")

    Unless you are indeed testing the contextual menu itself, you might
    want to directly call GPS.execute_action(...) instead, with the action
    that the contextual menu would execute. This makes the test more efficient
    and more reliable.
    """
    from gps_utils.internal.utils import activate_contextual, close_contextual
    windows = Gtk.Window.list_toplevels()
    click_in_text(editor.current_view().cursor(), button=3)
    yield wait_idle()  # wait for contextual menu to appear
    activate_contextual(windows, name)
    close_contextual(windows)
    yield wait_idle()  # wait for action to start executing
