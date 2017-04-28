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
execute an asynchronous python function,... All the while, the code in
`build_and_run` remains sequential, and thus is easy to read and maintain.

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

Workflows vs generators
-----------------------

Workflows are not exactly the same as python generator, because of the values
they can yield.

Given the following code, where `func_returning_promise` is a function
that returns an instance of `workflows.promises.Promise`, like a lot of
functions do in the workflows package::

    def foo():
        a = yield func_returning_promise()
        ... something else

    def main():
        yield foo()
        func1()

If you execute this function outside of the workflows driver, as in::

    main()    # NOT WHAT YOU WANT, most likely
    func2()

then the following occurs: `func_returning_promise`, when called, immediately
returns a promise, which `foo` yields. Then `main` also yields that promise,
and `func2` executes. When that promise is eventually resolved (because
some background processing done by `func_returning_promise` completes),
nothing more happens because nobody is waiting on the promise.

If on other hand you execute foo as part of the workflow driver, as in one of
the following two alternatives::

    workflows.driver(main)
    # or add a @run_as_workflow decorator to main

then the following occurs: `func_returning_promise` returns a promise, so
`foo` stops executing; control is returned to `main`, which in turn yields
that promise to the driver. The driver now waits (in the background) until
the promise is eventually resolved. When that happens, `foo` resumes, `a`
receives the value of the promise and the rest of `foo` is executed. When
`foo` eventually finishes, `main` in turn continues executing, and `func`
is executed.

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
      result of the promise will be passed as result to the yield call. For
      instance the call to `wait_idle` below returns a Promise

          def foo():
              ... do something
              yield wait_idle() # both foo and bar suspend until GPS has time
              ... do something now that GPS is available again

          def bar():
              ... do something before foo starts
              yield foo()
              ... do something after foo has finished

          driver(bar)

    - You can yield other generators, in which case the driver will take care
      of consuming (executing) them, and then resume the execution of the
      current generator. For instance, the call to `yield foo()` above returns
      a generator.

    Generators can throw exceptions: these will be propagated to the generator
    that spawned them.

    :return: a promise, that will be resolved when the workflow has finished
      executing. This can in general be ignored, since as described above
      `driver` will automatically chain things. In some contexts it might be
      useful to use this promise though.
    """

    promise = promises.Promise()

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

            except BaseException as e:
                # The current generator aborted because of an uncaught
                # exception: discard it and let the next round propagate the
                # exception to its "caller".
                gen_stack.pop()

                # For debugging purpose, keep exception information so that at
                # the end, the user can have a traceback that is focused on its
                # generators.
                exc_type, exc_value, exc_tb = sys.exc_info()
                GPS.Logger("WORKFLOW").log(
                    "Unexpected exception in workflow: %s %s" % (e, exc_tb))

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
            promise.reject(message)
        else:
            promise.resolve(return_val)

    # We just created a new execution state (gen_stack), so technically we are
    # resuming it below.
    resume()

    return promise


def run_as_workflow(workflow):
    """
    Decorator used to run a function as a worfklow.
    This is a way to make a function run in the background automatically.
    For instance::

        @run_as_workflow
        def my_function():
            p = ProcessWrapper([...])
            while True:
                # Here, code stops executing until p has some new output.
                # In the meantime, the code after the call to my_function()
                # keeps executing. When new data is available from p, the
                # following code will resume executing.
                line = yield p.wait_until_match('^.+$')
                if line is None:
                    break
                ...

        my_function()

    will start running an external process and parse all its lines. But the
    call to `my_function()` returns immediately, and thus GPS is not blocked.

    :param workflow: the function to be run as a workflow.
       The workflow has potentially not finished running when this function
       returns.
       This also works for standard functions
    :return: either the result of workflow, or a promise that will resolve
       to that result eventually.
    """
    def internal_run_as_wf(*args, **kwargs):
        r = workflow(*args, **kwargs)
        if isinstance(r, types.GeneratorType):
            return driver(r)
        else:
            return r

    return internal_run_as_wf


def task_workflow(task_name, workflow, **kwargs):
    """Run a workflow monitored by a task.

    The workflow is launched as the same time as the task, and runs for
    as long as the task is running - if the task is interrupted, the workflow
    is never resumed.

    Unless it is interrupted, the task returns with success once the workflow
    completes.

    For instance:

        # this is the definition of the workflow
        def my_function(task):
            # let's pretend this is a process which returns 100 lines
            p = ProcessWrapper([...])
            counter = 1

            while True:
                # use this to set the progress counter on the task
                task.set_progress(counter, 100)

                counter += 1
                line = yield p.wait_until_match('^.+$')
                if line is None:
                    break

        # this creates the task and launches the workflow
        GPS.workflows.task_workflow("my_task_name", my_function)

    :param task_name: the name to give to the task
    :param workflow: the workflow to launch: this is a function which
        receives the task as a parameter.
    :return: the task which monitors the workflow.
    """

    def execute(t):
        """ The execute function for our task """

        # The task manager might try to run "execute" right after the creation
        # of the task, ie before it has been given the necessary attributes
        # to manage the workflow. In this case, simply wait.
        if not hasattr(t, 'gen_stack'):
            return GPS.Task.EXECUTE_AGAIN

        # If there are no generators left, nothing to do, leave the task.
        if not t.gen_stack:
            return GPS.Task.SUCCESS

        # If this is set, this means that a promise is running. Simply wait
        # for this promise to return.
        if t.wait:
            return GPS.Task.EXECUTE_AGAIN

        # Act on the first generator in the stack
        gen = t.gen_stack[-1]

        try:
            if t.return_val is not None:
                # If we previously had a value to return, send it to the
                # generator...
                el = gen.send(t.return_val)
                t.return_val = None
            else:
                # ... otherwise simply next the generator
                el = gen.next()
        except StopIteration:
            # We reached the end of the generator: pop the stack and continue.
            t.gen_stack.pop()
            return GPS.Task.EXECUTE_AGAIN

        if isinstance(el, types.GeneratorType):
            # The last generator performed some kind of "call": schedule to
            # run the child generator for the next round.
            t.gen_stack.append(el)
            el = None
            return GPS.Task.EXECUTE_AGAIN

        elif isinstance(el, promises.Promise):
            # If the last generator yielded a promise, schedule to resume
            # its execution when the promise is ready.

            def resume(rv):
                t.return_val = rv
                t.wait = False

            # We set 'wait' to True, this will be unset when the promise
            # resolves.
            t.wait = True
            el.then(resume)
            return GPS.Task.EXECUTE_AGAIN
        else:
            # The generator returned something which is neither a promise
            # nor another generator: treat this as a yielded result.
            t.return_val = el
            return GPS.Task.EXECUTE_AGAIN

    # Create a task with our execute function
    t = GPS.Task(task_name, execute)

    # We have created a task object: here are the fields that are going
    # to be used for handling the workflow for it.
    t.gen_stack = [workflow(t, **kwargs)]  # The stack of generators
    t.return_val = None    # the value returned by the last generator call
    t.wait = False         # A promise is running and the task should wait
    return t


def create_target_from_workflow(target_name, workflow_name, workflow,
                                icon_name="gps-print-symbolic",
                                in_toolbar=True,
                                main_arg="%TT",
                                category='_Workflow_',
                                parent_menu='/Build/Workflow/'):
    """
    Create a Target under the category Workflow from a given workflow.
    Executing this target runs the workflow.
    By default, the `workflow` receives the `main_name` as argument.
    You can modify `main_arg` to receive the argument of your choice, by
    specifying a GPS macro.

    :param str target_name: The name of the target. Also the name of the
        menu item.
    :param str workflow_name:
    :param str main_arg:
    :param str category: The category, as seen in the target editing dialog
    :param str parent_menu: path to the menu item. The menu item itself
        is `category`, unless the category starts and ends with
        an underscore character. It should end with a slash.
    """

    def xml_quote(str):
        """return an XML safe version of str"""
        return str.replace('&', '&amp;').replace(
            '<', '&lt;').replace('>', '&gt;')

    # going to store the feeded workflow in a global variable
    global registered_workflows
    registered_workflows[workflow_name] = workflow

    # Add the target's name to the set of registered workflow BuildTargets
    workflows_target_name_set.add(target_name)

    xml1 = """
<target model="python" category="%s" name="%s" menu='%s'>
<in-toolbar>%s</in-toolbar>
<iconname>%s</iconname>
<launch-mode>MANUALLY_WITH_NO_DIALOG</launch-mode>
<read-only>FALSE</read-only>
<target-type>%s</target-type>
<do-not-save>FALSE</do-not-save>
<command-line>
    <arg>workflows.run_registered_workflows("%s", "%s", "</arg>
    """ % (xml_quote(category),
           xml_quote(target_name),
           xml_quote(parent_menu),
           "TRUE" if in_toolbar else "FALSE",
           icon_name,
           "main" if main_arg == "%TT" else "",
           xml_quote(workflow_name),
           xml_quote(target_name))

    xml2 = """
    <arg>%s</arg>
    <arg>")</arg>
</command-line>
</target>""" % (main_arg)

    XML = xml1 + xml2
    GPS.parse_xml(XML)
