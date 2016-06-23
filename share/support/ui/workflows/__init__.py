"""
This module defines a framework for writing workflows.

A workflow is a Python generator which can be used to execute
a chain of asynchronous actions, while retaining a sequential
structure.

This package is in most cases not directly useful on its own.  Instead,
consider workflows as an extension to standard synchronous python functions.
For instance, it is hard to write a GPS action that would run a build target
(e.g. "build all"), then when the compilation has finished would start
running the exectable.

The function would look something like:

    def my_asynchronous_function_1():
        GPS.BuildTarget("build main").execute("main.adb", synchronous=True)
        GPS.BuildTarget("run main").execute("main.adb")

with a major drawback: since the build is run synchronously, the whole of GPS
is frozen while the compiler is doing its job, including the progress bars.
So this solution is not suitable. This means we need to run the first command
asynchronously:

    def my_asynchronous_function_2():
        def on_exit(*args):
            GPS.BuildTarget("run main").execute("main.adb")
        GPS.BuildTarget("build main").execute("main.adb", on_exit=on_exit)

so that when the build finishes, it calls `on_exit`, which in turns starts
running the executable. Imagine what happens when you want to chain more
than two functions.

Instead, the functions is this module let you write python generator
functions, which start executing, then give the control back to GPS, which
resumes their execution when the current task has finished.

   from workflows.promises import TargetWrapper
   def build_and_run():
       yield TargetWrapper('build main').wait_on_execute('main.adb')
       yield TargetWrapper('run main').wait_on_execute('main.adb')

and so on if we want to chain more than two actions. The power of this
framework, though, is that it works for most GPS concepts: one could run
any external process, wait for its completion, then execute a GPS target
as we did above, then wait for a specific hook to be run by GPS, then
execute an asynchronous python function,... All the while, the `my_workflow`
remains sequential, and thus is easy to read and maintain.

Such workflows can be used like standard functions to create GPS actions.
For instance:

   import gps_utils

   @gps_utils.interactive('my action', menu='/Workflows/Build and Run')
   def build_and_run():
       # as above

and we have just defined a new menu which executes our workflow. Now that we
have a GPS action, users can also associate key bindings to it via the
preferences dialog.

Work can be split among several functions just as easily. Imagine we now
want to write a function that would generate some code (via an external
program), then build and run the main as we did above. This can be done
with:

   from workflows.promises import ProcessWrapper
   def generate_build_and_run():
       yield ProcessWrapper(['generate_code', '--full']).wait_until_terminate()
       yield build_and_run()    # Calling our previous function
"""

import inspect
import sys
import GPS
import workflows.promises as promises
import traceback
import types

# A table of all registered workflows
registered_workflows = {}

# A set of the BuildTarget names for every registered workflows. Used by
# BuildTarget to determine wether a buildtarget is implemented by a workflow or
# not
workflows_target_name_set = set()

# Table of exit handlers for build targets implemented via workflows
exit_handlers_table = {}


def run_registered_workflows(workflow_name, target_name, main_name):
    """ Find workflow and run it with the driver.
    """
    # The BuildTarget wrapper may have registered an on exit handler for us to
    # call at the end of the workflow execution. Check if there is one in the
    # exit_handlers_table
    on_exit_handler = exit_handlers_table.get((target_name, main_name), None)

    try:
        wf = registered_workflows[workflow_name](main_name)

        def wrapper():
            """
            This wrapper just wraps the workflow, and calls the on_exit handler
            at the end
            """
            exit_value = 0
            try:
                yield wf
            except Exception:
                # In case of problem in the workflow, register a non zero
                # return value for the exit handler
                exit_value = 1
            finally:
                # Call the on_exit_handler and remove it from the handlers
                # table
                if on_exit_handler:
                    on_exit_handler(exit_value)
                exit_handlers_table[(target_name, main_name)] = None

        driver(wrapper())
    except KeyError:
        GPS.Console("Messages").write(
            "\nError: Workflow name not registered.\n")


def peel_traceback_to(tb, frame):
    """
    Peel `tb` (a traceback object) until the top-level frame is `frame`.

    Return the resulting traceback object, or None if `tb` contains no such
    frame.
    """
    while tb and tb.tb_frame != frame:
        tb = tb.tb_next
    return tb


def driver(gen_inst):
    """
    This is the main driver for workflows. You can pass your worklow (which is
    a python generator instance) to it and it will execute it.

    From a worklow, you can yield two types of objects:

    - You can yield promises. Those will be chained so that when the promise
      resolves, the execution of the workflow is resumed, and any eventual
      result of the promise will be passed as result to the yield call.

    - You can yield other generators, in which case the driver will take care
      of consuming (executing) them, and then resume the execution of the
      current generator.

    Generators can throw exceptions: these will be propagated to the generator
    that spawned them.
    """

    # Stack of generators, similar to a call stack. The first one is the
    # original generator and the last one is the most recently spawned one.
    gen_stack = [gen_inst]

    def resume(return_val=None):
        """Resume execution for this workflow."""
        el = None
        exc_info = None

        while gen_stack:
            gen = gen_stack[-1]
            try:
                if exc_info is not None:
                    # If the previous round raised an exception, propagate it
                    # to this generator.
                    el = gen.throw(*exc_info)
                elif return_val is not None:
                    # If there's feedback from previous event, tell the
                    # generator.
                    el = gen.send(return_val)
                else:
                    # Otherwise just go to the next step.
                    el = gen.next()

            except StopIteration:
                # The current generator just done: discard it so we can resume
                # its parent generator.
                gen_stack.pop()

            except BaseException:
                # The current generator aborted because of an uncaught
                # exception: discard it and let the next round propagate the
                # exception to its "caller".
                gen_stack.pop()

                # For debugging purpose, keep exception information so that at
                # the end, the user can have a traceback that is focused on its
                # generators.
                exc_type, exc_value, exc_tb = sys.exc_info()
                # Strip the traceback to only keep the user part. Be careful
                # about currentframe: on some implementations it can return
                # None.
                frame = inspect.currentframe()
                if frame:
                    exc_tb = peel_traceback_to(exc_tb, frame)
                    # We want a traceback that do not contain this frame: peel
                    # one more level!
                    if exc_tb:
                        exc_tb = exc_tb.tb_next
                exc_info = (exc_type, exc_value, exc_tb)
                continue

            if isinstance(el, types.GeneratorType):
                # The last generator performed some kind of "call": schedule to
                # run the child generator for the next round.
                gen_stack.append(el)
                el = None
            elif isinstance(el, promises.Promise):
                # If the last generator yielded a promise, schedule to resume
                # its execution when the promise is ready.
                # ??? Should we connect to reject to cancel the whole workflow?
                el.then(resume)
                return

            # Clean state for the next round.
            return_val = el
            exc_info = None

        # If we reach this point, there's nothing to execute anymore: just log
        # any uncaught exception.
        if exc_info is not None:
            message = (
                'Uncaught exception in workflows:\n'
                '{}\n'.format(''.join(traceback.format_exception(*exc_info)))
            )
            # This one is for debugging/testing convenience.
            GPS.Console('Messages').write(message)
            # This one is for automatic issue detection in testsuites. This
            # should also ring a bell while analysis post-mortem GPS logs.
            GPS.Logger('TESTSUITE.EXCEPTIONS').log(message)

    # We just created a new execution state (gen_stack), so technically we are
    # resuming it below.
    resume()


def run_as_workflow(workflow):
    """
    Decorator used to run a function as a worfklow.

    :param workflow: the function to be run as a workflow.
    """
    def internal_run_as_wf(*args, **kwargs):
        return driver(workflow(*args, **kwargs))

    return internal_run_as_wf


def create_target_from_workflow(target_name, workflow_name, workflow,
                                icon_name="gps-print-symbolic",
                                in_toolbar=True):
    """
    Create a Target under the category Workflow from a given workflow.
    Executing this target runs the workflow.
    The `workflow` receives the `main_name` as argument.

    target_name and workflow_name are strings
    """

    # going to store the feeded workflow in a global variable
    global registered_workflows
    registered_workflows[workflow_name] = workflow

    # Add the target's name to the set of registered workflow BuildTargets
    workflows_target_name_set.add(target_name)

    xml1 = """
<target model="python" category="Workflow" name="%s">
<in-toolbar>%s</in-toolbar>
<iconname>%s</iconname>
<launch-mode>MANUALLY</launch-mode>
<read-only>FALSE</read-only>
<do-not-save>FALSE</do-not-save>
<target-type>main</target-type>
<command-line>
    <arg>workflows.run_registered_workflows("%s", "%s", "</arg>
    """ % (target_name,
           "TRUE" if in_toolbar else "FALSE",
           icon_name, workflow_name, target_name)

    xml2 = """
    <arg>%TT</arg>
    <arg>")</arg>
</command-line>
</target>"""

    XML = xml1 + xml2
    GPS.parse_xml(XML)
