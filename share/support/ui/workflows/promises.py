"""
This package provides a number of classes and functions to help write
workflows. See the full description of workflows in workflows/__init__.py
"""

import time
from time_utils import TimeDisplay

import GPS
import re
import types
from pygps import process_all_events
from gi.repository import GLib
import workflows


class Promise(object):
    """
    A promise is a wrapper object around an asynchronous computation.
    - The client of the promise calls promise.then(use_callback) on the promise
      to register the action that he wants executed when the promise's result
      is available.
    - The creator of the promise calls resolve on the promise when the result
      is ready, which will notify the client via the callback.

    Promises are one of the two low-level support concepts for workflows
    (along with python's yield keyword). They can be used to implement your
    own asynchronous programming functions, if the other functions in this
    module are not sufficient.

    For instance, let's assume you want to query data from a web server.
    Since such a query could take a while, we do not want to block GPS during
    that time. So we need to do this query in the background.

        class WebQuery(Thread):
            def __init__(self, url, promise):
                self.promise = promise
                self.url = url
            def run(self):
                # Running in a separate thread: connect to the server and
                # retrieve the document, for instance with urllib. Since this
                # is a separate thread, it doesn't block GPS
                doc = urllib.urlopen(self.url)

                # Let the promise no that we now have the info
                self.promise.resolve(doc)

        def query_from_server(*args):
            p = Promise()
            # run the query in the background
            w = WebQuery(url='...', promise=p)
            return p

    One could now use the `query_from_server` directly from a GPS workflow,
    but just to show the low-level of using promises:

        # We receive the document, as per the call to resolve() above
        def when_query_is_done(doc):
            ...

        # The following call is not blocking at all
        query_from_server(...).then(when_query_is_done)

    """

    PENDING = -1
    RESOLVED = 0
    REJECTED = 1

    def __init__(self):
        self.__success = []  # Called when the promise is resolved
        self.__failure = []  # Called when the promise is rejected
        self.__result = None   # The result of the promise
        self._state = Promise.PENDING

    def then(self, success=None, failure=None):
        """
        Register user defined callback to be called when the promise is
        resolved or rejected.

        :param success: a function called when the promise is resolved
           (or immediately if the promise has already been resolved).
           It receives as parameter the value passed when calling
           `self.resolve()` (or, if that was also a promise, the value of
           that promise once it has been resolved).
           `success` is called at most once (and never if the promise is
           rejected).
           If this function returns a promise, that promise will be
           resolved before calling the next promise in the chain (see the
           description on the return value below).

        :param failure: a function called when the promise is rejected
           (or immediately if the promise has already been rejected).
           It receives as a parameter the value passed when calling
           `self.reject()`
           `failure` is called at most once (and never if the promise is
           resolved).

        :return: a new promise, which will be resolved with the return
           value of `success`, or rejected with the return value of
           `failure`. This way, promises can be chained more easily:

               Promise().then(on_promise1_done).then(on_promise2_done)

           where on_promise1_done is called with the return value of the
           first promise, and on_promise2_done is called with the
           return value of on_promise1_done.
           If `success` also return a promise, the value of that promise will
           be used to resolve the result of `then`.
        """
        ret = Promise()

        def __fullfill(value):
            ret.resolve(success(value) if success else value)

        def __reject(reason):
            ret.reject(failure(reason) if failure else reason)

        if self._state == Promise.RESOLVED:
            __fullfill(self.__result)
        elif self._state == Promise.REJECTED:
            __reject(self.__result)
        else:
            self.__success.append(__fullfill)
            self.__failure.append(__reject)
        return ret

    def resolve(self, result=None):
        """
        Set the value of the promise.
        This automatically calls the callbacks set by the user.
        :param result: any type
           This is the result of the promise, and is passed to the callback.
        """
        if self._state == Promise.PENDING:
            if isinstance(result, Promise):
                result.then(self.resolve, self.reject)
            else:
                self._state = Promise.RESOLVED
                self.__result = result  # in case we call then() later
                for s in self.__success:
                    s(result)

                # Release all listeners, for garbage collecting, since the
                # state of the promise can't change anymore, and listeners are
                # only called once.
                self.__success = None
                self.__failure = None

    def reject(self, reason=None):
        """
        The promise cannot be fullfilled after all, so call the appropriate
        callbacks. `reason` should not be a Promise.
        """
        if self._state == Promise.PENDING:
            self._state = Promise.REJECTED
            self.__result = reason
            for s in self.__failure:
                s(reason)

            self.__success = None
            self.__failure = None


class Stream(Promise):
    """
    A stream is a promise which emits zero or more events during its lifetime,
    in addition to the usual `resolve` and `reject` behavior.
    Such a stream should be used when a function can return multiple values
    in its lifetime. At the same time, it acts like a promise so that it is
    compatible with the workflow framework.
    """

    def __init__(self):
        super(Stream, self).__init__()
        self._onnext = []

    def subscribe(self, onnext=None, onerror=None, oncompleted=None):
        """
        Subscribe to events emitted by this stream.

        :param onnext: a function that receives one value in argument
        :param onerror: a function called when the stream has been
           rejected. Receives a string, the reason for the failure.
        :param oncompleted: a function called when the stream has been
           resolved. Receives the exit value.
        """
        if self._state == Promise.PENDING:
            if onnext is not None:
                self._onnext.append(onnext)

        self.then(success=oncompleted, failure=onerror)

        return self   # for chaining

    def emit(self, value):
        """
        Emit one more event
        """
        for cb in self._onnext:
            cb(value)

    def resolve(self, result=None):
        self._onnext = []
        super(Stream, self).resolve(result)

    def reject(self, reason=None):
        self._onnext = []
        super(Stream, self).reject(reason)

    def map(self, transform):
        """
        A function that transforms a stream into another stream.

        :param transform: a function that converts a value emitted
           by `self` into another value emitted by the result stream.
           This does not apply to the result of `resolve` or `reject`
        :returntype: a Stream
        """
        out = Stream()
        self.subscribe(
            onnext=lambda value: out.emit(transform(value)),
            oncompleted=lambda value: out.resolve(value),
            onerror=lambda reason: out.reject(reason))
        return out

    def flatMap(self, transform):
        """
        A function that aggregates one or more items emitted by `self`,
        and emits them (or another set of items) on another observable.

        :param transform: a function that receives the output observable
           and a value emitted by self, and optionally calls `out.emit`.
           Alternatively, this can be a class instance, where __call__ is
           executed for each value from `self`, and `oncompleted` is executed
           when `self` terminates.
        :returntype: a Stream
        """
        out = Stream()

        def oncompleted(value):
            if hasattr(transform, "oncompleted"):
                transform.oncompleted(out, value)
            out.resolve(value)

        self.subscribe(
            onnext=lambda value: transform(out, value),
            oncompleted=oncompleted,
            onerror=lambda reason: out.reject(reason))
        return out


def join(*args):
    """
    Return a promise that is resolved when all the promises given in argument
    are also resolved. The returned promise is resolved with a list of the
    values of all parameter promises::

        a = Promise()   # or a function returning a promise
        b = Promise()   # or a function returning a promise
        p = join(a, b)
        p.then(lambda(a1, b1): pass)  # a1 is the value of a, b1 of b

    or perhaps, when using workflows:

        @workflows.run_as_workflow
        def func1():
            yield 1
            yield 2

        def func2():    # directly a python generator
            yield 3
            yield 4

        @workflows.run_as_workflow
        def func3():
            a = yield join(func1(), func2())
            pass
            # executed when func1 and func2 have both terminated.
            # a == (2, 4)

    :param List(Promise) *args: promises to wait on
    """
    p = Promise()

    class _Resolver:
        _count = 0
        _result = [None] * len(args)

        def __init__(self, idx):
            self.idx = idx

        def __call__(self, x):
            """Called when the promise is resolved"""
            self._result[self.idx] = x
            _Resolver._count += 1
            if _Resolver._count == len(args):
                p.resolve(self._result)

    for idx, a in enumerate(args):
        if isinstance(a, types.GeneratorType):
            a = workflows.driver(a)
        a.then(_Resolver(idx))

    return p


def timeout(msecs):
    """
    This primitive allows the user to delay execution of the rest of a workflow
    for msecs milliseconds.

       def my_func():
           bar()
           yield timeout(200)   # wait 200ms
           bar()

    When possible, it is better to use `wait_idle` to wait until GPS is not
    busy doing anything else, rather than wait an explicit delay, which might
    depend on the CPU load for instance.
    """
    p = Promise()

    def timeout_handler():
        p.resolve()
        return False

    GLib.timeout_add(msecs, timeout_handler)
    return p


def wait_idle():
    """
    This primitive allows the writer of a workflow to wait until all event have
    been handled, and resume execution of the workflow in an idle callback
    """
    p = Promise()
    process_all_events()
    GLib.idle_add(lambda: p.resolve())
    return p


known_tasks = ["debugger output monitor 1", "refreshing Runtime menu"]
# List of background tasks that are known to be running in the background


def wait_tasks(other_than=None):
    """
    This primitive allows the user to delay the execution of the rest of a
    workflow until all active tasks are terminated. If you are waiting on
    tasks that you spawned yourself, it is better to use ProcessWrapper
    or TargetWrapper below to spawn the task.

    If other_than is specified, wait for the tasks that have a name different
    than the ones in other_than.
    """

    p = Promise()
    filt = other_than or []

    def timeout_handler():
        if not filter(lambda x: x.name() not in filt, GPS.Task.list()):
            process_all_events()
            GLib.idle_add(lambda: p.resolve())
            return False
        return True   # will try again

    GLib.timeout_add(200, timeout_handler)
    return p


def wait_specific_tasks(names):
    """
    Allows to delay the execution of the rest of a workflow until given tasks
    are terminated.
    """
    p = Promise()

    def timeout_handler():
        if not filter(lambda x: x.name() in names, GPS.Task.list()):
            GLib.idle_add(lambda: p.resolve())
            return False
        return True   # will try again

    GLib.timeout_add(200, timeout_handler)
    return p


def modal_dialog(action_fn, msecs=300):
    """
    This primitive executes a blocking function in the context of a workflow.
    This should rarely be used, but is sometimes needed for some modal dialogs
    that block GPS without executing the python script.
    Example:
        yield modal_dialog(
            300, lambda: GPS.execute_action('open project properties'))
    """
    p = Promise()

    def __on_timeout():
        p.resolve()
        return False

    def __start_action():
        # Use Glib's timeout_add, since GPS.Timeout doesn't seem to be run
        # correctly when running python's Gtk.Dialog.run (for instance for the
        # coding standard editor).
        GLib.timeout_add(msecs, __on_timeout)
        action_fn()
        return False

    # Since action is blocking, and we want modal_dialog to return the
    # promise, we need to start the action in a timeout.
    GLib.timeout_add(10, __start_action)
    return p


def idle_modal_dialog(action_fn):
    """
    Similar to `modal_dialog()`, but waits until GPS is finished processing
    events, instead of a specific timeout.
    """
    p = Promise()

    def __on_idle():
        GLib.idle_add(lambda: p.resolve())
        action_fn()

    GLib.idle_add(__on_idle)
    return p


def hook(hook_name):
    """
    This primitive allows the writer of a workflow to connect to a hook once,
    as if it were a function, and get the parameters of the hook as return
    values. For example:

        file = yield hook("buffer_edited")

    This will wait until the "buffer_edited" hook is triggered, and the file
    will be stored in the file variable. Result is a list if hook returns
    several values.
    """
    p = Promise()

    def hook_handler(hook, *args):
        GPS.Hook(hook_name).remove(hook_handler)
        # resolve accepts only one argument, so pass list of args if it longer
        if len(args) <= 1:
            p.resolve(*args)
        else:
            p.resolve(args)

    GPS.Hook(hook_name).add(hook_handler)
    return p


class ProcessWrapper(object):
    """
    ProcessWrapper is an advanced process manager
    It makes a promise (yield object of the promise class) when user:
        1 - want to wait for match in output
        2 - want to wait until process finish
    and the corresponding promises are answered with user defined
    handler (functions) when:
        1 - the pattern matches or timeout.
        2 - the process is terminated by GPS.

    Example of use:

        def my_func():
            p = ProcessWrapper(['ls'])
            status, output = yield p.wait_until_terminate()

    """

    def __init__(self, cmdargs=[], spawn_console=False,
                 directory=None, regexp='.+',
                 single_line_regexp=True, block_exit=True,
                 give_focus_on_create=False):
        """
        Initialize and run a process with no promises,
        no user-defined pattern to match,
        but a omnipotent regexp that catches everything.
        The process has empty output and two flags saying that
        the process is unfinished and no pattern has matched yet.

        If spawn_console is True, a console is spawned to display the
        process output. This console also allows the user to relaunch
        the associated process with a "Relaunch" button in the console
        toolbar.

        :param bool|str spawn_console: whether to display the process and
           its output to a console. If this is a boolean, then a new console
           named like the process is opened. If it is a string, it is the
           name of the console to use (the empty string reused GPS's
           Messages window).
        :param bool single_line_regexp: if True, then '.' in the regexp
           will also match '\n'. This is useful to capture larger parts of
           the output at once.
        :param bool block_exit: whether the user should be asked when GPS
           exits and this process is still running.
        :param bool give_focus_on_create: set it to True to give the focus
           to the spawned console, if any.
        """

        # __current_promise = about on waiting wish for match something
        self.__current_promise = None

        # the stream that includes all output from the process
        self.__stream = None

        # __current_pattern = regexp that user waiting for in the output
        self.__current_pattern = None

        # __output = a buffer for current output of self.__process
        self.__output = ""

        # __whether process has finished
        self.finished = False

        # Used to know when the attached process is being relaunched via
        # th relaunch button.
        self.__relaunched = False

        # handler of process will be created -> start running
        # Remove empty command line arguments
        self.__command = [c for c in cmdargs if c]

        # The console associated with the process.
        # Created only if spawn_console is set to True.
        self.__console = None

        # Launch the command
        try:
            self.__process = GPS.Process(
                command=self.__command,
                directory=directory,
                regexp=regexp,
                single_line_regexp=single_line_regexp,
                block_exit=block_exit,
                on_match=self.__on_match,
                on_exit=self.__on_exit)
        except Exception:
            GPS.Logger("PROMISES").log(
                "Failed to spawn %s" % (self.__command, ))
            self.__process = None
            return

        # Save the start time
        self.__start_time = time.time()

        # If requested, spawn a console to display the process output
        if spawn_console is not False:
            if isinstance(spawn_console, str):
                console_name = spawn_console
            else:
                console_name = cmdargs[0]

            toolbar_name = cmdargs[0] + '_toolbar'
            self.__console = GPS.Console(
                name=console_name,
                accept_input=False,
                on_destroy=self.__on_console_destroy,
                toolbar_name=toolbar_name,
                give_focus_on_create=give_focus_on_create)
            self.__action = GPS.Action('launch ' + cmdargs[0])

            self.__console.write("%s\n" % ' '.join(self.__command))

            # Create the associated action and relaunch button if it
            # does not exist yet.
            if not self.__action.exists():
                self.__action.create(
                    on_activate=self.__relaunch,
                    description='relaunch the spawned process',
                    icon='gps-refresh-symbolic')
                self.__action.button(
                    toolbar=toolbar_name,
                    label='Relaunch')

            def __show_console_on_exit(status):
                end_time = time.time()
                output = "\n" + TimeDisplay.get_timestamp(end_time)
                if not status:
                    output += " process terminated successfully"
                else:
                    output += " process exited with status " + str(status)
                output += ", elapsed time: " + TimeDisplay.get_elapsed(
                    self.__start_time, end_time) + "\n"
                if self.__console:
                    self.__console.write(output)

            def __display_output(out):
                if self.__console:
                    self.__console.write("%s\n" % out)

            self.stream.subscribe(
                __display_output,
                oncompleted=__show_console_on_exit)

    def __on_match(self, process, match, unmatch):
        """
        Called by GPS everytime there's output coming
        """
        if self.__current_promise is not None:
            self.__output += unmatch
            self.__output += match
            self.__check_pattern_and_resolve()
        if self.__stream is not None:
            self.__stream.emit(unmatch)
            self.__stream.emit(match)

    def __resolve_promise(self, value):
        """
        Resolve the current promise with the given value.
        """
        p = self.__current_promise
        if p:
            self.__current_promise = None  # garbage collect
            p.resolve(value)

    def __check_pattern_and_resolve(self):
        """
        Check whether the current pattern matches the already known output
        of the tool, and resolve the promise if possible.
        """
        if self.__current_promise is not None:
            p = self.__current_pattern.search(self.__output)
            if p:
                self.__output = self.__output[p.end(0):]
                self.__resolve_promise(p.group(0))
            elif self.finished:
                # We will never be able to match anyway
                self.__resolve_promise(None)

    def __on_exit(self, process, status, remaining_output):
        """
           Call by GPS when the process is finished.
           Final_promise will be solved with status
           Current_promise will be solved with False
        """
        self.finished = True
        if self.__current_promise is not None:
            self.__output += remaining_output
            self.__check_pattern_and_resolve()

        if self.__stream is not None:
            self.__stream.emit(remaining_output)

            # Don't resolve the stream when the process is being
            # relaunched: we don't want to lose the process output
            # in this case.
            if not self.__relaunched:
                self.__stream.resolve(status)

        # Don't consider the process as finished when it's being
        # relaunched.
        self.finished = not self.__relaunched
        self.__relaunched = False

    def wait_until_match(self, pattern, timeout=0):
        """
        Called by user. Make a promise to them that:
        I'll let you know when the pattern is matched/never matches and return
        the remaining output if the pattern matched.
        Matching is only attempted of the text output since the last call to
        wait_until_match.
        Promise made here will be resolved with either a string (the current
        output) or None (when the process has terminated.

        :param str|re.Pattern pattern: the regular expression to match on
           the output of `self`.
        :param int timeout: give up matching pattern after this many
           milliseconds, or wait for ever if 0.
        """
        # process has already terminated, return nothing
        if self.finished:
            return None

        if isinstance(pattern, str):
            self.__current_pattern = re.compile(pattern, re.MULTILINE)
        else:
            self.__current_pattern = pattern

        p = self.__current_promise = Promise()

        # Can we resolve immediately ?
        self.__check_pattern_and_resolve()
        if self.__current_promise:
            # if user defines a timeout, set up to
            # close output check after that timeout
            if timeout > 0:
                GLib.timeout_add(timeout, self.__on_timeout)

        return p

    def wait_line(self):
        """
        Wait for the next line to be available, and return it. The line
        does not include the trailing \n
        See documentation for `wait_until_match`.

        :return: a promise
        """
        p = Promise()

        s = self.wait_until_match("^.*\n")
        if s is None:
            p.resolve(None)   # already finished
        else:
            s.then(lambda line: p.resolve(line[:-1] if line else None))

        return p

    @property
    def stream(self):
        """
        Return the internal stream of output.
        Events are emitted every time some output becomes available. This
        output is not grouped into lines or other pattern matching a
        regular expression.
        The result stream is resolved when the process terminates, and
        resolves with the exit status of the process::

            def on_output(output):
                pass   # do something with the output

            @run_as_workflow
            def run_bg():
                p = ProcessWrapper(...)
                yield p.stream.subscribe(on_output)
        """
        if self.__stream is None:
            self.__stream = Stream()
        return self.__stream

    @property
    def lines(self):
        """
        A stream that emits one event for each line in the output::

            def online(line):
                pass   # do something with the line

            @run_as_workflow
            def execute():
                p = ProcessWrapper(...)
                yield p.lines.subscribe(online)  # wait until p terminates
                pass  # executes when p has terminated

        A similar pattern can be used to aggregate the lines into higher-level
        blocks.

            def transform(out_stream, line):
                # aggregate one or more lines into a block, then call
                out_stream.emit(line)

            def blocks():
                p = ProcessWrapper(...)
                return p.lines.flatMap(transform)

            def onblock(block):
                pass  # do something with the block

            blocks.subscribe(onblock)

        :returntype: a stream. If you yield it from a function with a
           @run_as_workflow decorator, the function will suspend until
           the process terminates.
           You can also subscribe to this stream to receive each line of
           the output.
        """

        class map_to_line:
            def __init__(self):
                self.buffer = ""
                self.__re = re.compile('^.*\n')

            def __call__(self, out_stream, output):
                self.buffer += output
                while True:
                    p = self.__re.search(self.buffer)
                    if not p:
                        break
                    out_stream.emit(p.group(0)[:-1])
                    self.buffer = self.buffer[p.end(0):]

            def oncompleted(self, out_stream, status):
                if self.buffer:
                    out_stream.emit(self.buffer)

        return self.stream.flatMap(map_to_line())

    def wait_until_terminate(self, show_if_error=False):
        """
        Called by user. Make a promise to them that:
        I'll let you know when the process is finished
        Promise made here will be resolved with a tuple:
            (exit_status, full output since call to wait_until_terminate)

        :param bool show_if_error: if true and the process exits with a
           non zero status, then print the full output of the process to the
           Messages window.
        """
        p = Promise()
        output = []

        def on_terminate(status):
            out = "".join(output)
            if show_if_error and status != 0:
                GPS.Console().write("%s\n" % (" ".join(self.__command, )))
                GPS.Console().write(out)
            p.resolve((status, "".join(output)))

        self.stream.subscribe(
            onnext=lambda out: output.append(out),
            oncompleted=on_terminate)
        return p

    def __on_timeout(self):
        """
        Called by GPS when it's timeout for a pattern to appear in output.
        """
        self.__resolve_promise(None)
        self.__current_pattern = None
        return False

    def terminate(self):
        """
        Called by the user to force the process to end and resolve
        the associated promises.
        Interrupt the attached process.
        """

        # get end timestamp
        end_time = time.time()
        # Interrupt the process, if any
        if not self.finished:
            self.__process.interrupt()
            if self.__console:
                self.__console.write(
                    "\n<^C> process interrupted (elapsed time: %s)\n" %
                    TimeDisplay.get_elapsed(self.__start_time, end_time))

    def __on_console_destroy(self, console):
        """
        Called when the console is being destroyed.
        Interrupt the attached process.
        """
        self.__process.interrupt()
        self.finished = True
        self.__console = None

    def __relaunch(self):
        """
        Called when clicking on the "Relaunch" button of the
        console toolbar.
        Terminate the process and relaunch it again.
        """
        self.__relaunched = True
        self.terminate()
        self.__process = GPS.Process(
            command=self.__command,
            regexp=".+",
            on_match=self.__on_match,
            on_exit=self.__on_exit)
        self.__start_time = time.time()


class DebuggerWrapper(object):
    """
       DebuggerWrapper is a debbuger (essentially a process in GPS) manager
       It make a promise (yield object of the promise class) when user:
           want to send a command to debugger

       and the corresponding promises are answered after
           1 the debugger is not busy, execute the command required
           2 timeout

      Instantiating a DebuggerWrapper instance raises an exception when
      the underlying debugger fails to start.
    """

    # static variable for interval that the manager checks whether
    # the debugger is busy, in milliseconds
    __query_interval = 200

    def __init__(self, f, reuse_existing=True,
                 remote_target='', remote_protocol=''):
        """
           Initialize a manager, begin a debugger on the given file
           with no timers, no promises, no command.

           The optional ``remote_target`` and ``remote_protocol`` parameters
           are used to initialize a remote debugging session when spawning the
           debugger. When not specified, the ``IDE'Program_Host`` and
           ``IDE'Communication_Protocol`` are used if present in the .gpr
            project file.
        """

        if reuse_existing:
            try:
                # handler for debugger
                self.__debugger = GPS.Debugger.get()

                # Raise the debugger's console if we are reusing an existing
                # one.
                GPS.MDI.get_by_child(
                    self.__debugger.get_console()).raise_window()

                # if we reach this, a debugger is running: interrupt it
                GPS.execute_action("debug interrupt")

                # Try to reconnect to the previous remote connection, if any
                GPS.execute_action("debug connect to board")
            except Exception:
                self.__debugger = GPS.Debugger.spawn(
                    executable=f,
                    remote_target=remote_target,
                    remote_protocol=remote_protocol)
                if self.__debugger:
                    pass
                else:
                    raise Exception("Could not launch the debugger")
        else:
            self.__debugger = GPS.Debugger.spawn(f)
            self.__debugger = GPS.Debugger.spawn(
                executable=f,
                remote_target=remote_target,
                remote_protocol=remote_protocol)
            if not self.__debugger:
                raise Exception("Could not launch the debugger")

        # current on waiting promise
        self.__this_promise = None

        # the command to be sent
        self.__next_cmd = None

        # the output returned after send __next_cmd
        self.__output = None

        # regular checker that checks if debugger is busy
        self.__timer = None

        # deadline for __next_cmd and __this_promise
        self.__deadline = None

    def __is_busy(self, timeout):
        """
           Called by GPS at each interval.
        """

        # if the debugger is not busy
        if not self.__debugger.is_busy():

            # remove all timers
            self.__remove_timers()

            # and if there's cmd to run, send it
            if self.__next_cmd is not None:

                if self.__next_cmd is not "":
                    self.__output = self.__debugger.send(
                        cmd=self.__next_cmd,
                        show_in_console=True)
                    self.__next_cmd = None
                    self.__remove_timers()
                    self.__this_promise.resolve(self.__output)

                # "" cmd are default value when making promise,
                # it's also a maker for pure checker
                else:
                    self.__this_promise.resolve(True)

    def __on_cmd_timeout(self, timeout):
        """
           Called by GPS at when the deadline defined by user is reached
        """

        # remove all timers
        self.__remove_timers()

        # answer the promise with the output
        if self.__this_promise:
            self.__next_cmd = None
            self.__this_promise.resolve(self.__output)

    def __remove_timers(self):
        """
           Called in timers to remove both: prepare for new timer registration
        """
        if self.__deadline:
            try:
                self.__deadline.remove()
            except Exception:
                pass
            self.__deadline = None

        if self.__timer:
            try:
                self.__timer.remove()
            except Exception:
                pass
            self.__timer = None

    def wait_and_send(self, cmd="", timeout=0, block=False):
        """
           Called by user on request for command within deadline (time)
           Promise returned here will be answered with: output

           This method may also function as a pure block-debugger-and-wait-
           until-not-busy call, when block=True.
           Promise returned for this purpose will be answered with: True/False
        """

        self.__this_promise = Promise()
        self.__next_cmd = cmd
        self.__output = None

        self.__timer = GPS.Timeout(self.__query_interval, self.__is_busy)

        # only register deadline for real command waiting
        if not block:
            if timeout > 0:
                self.__deadline = GPS.Timeout(timeout, self.__on_cmd_timeout)
        return self.__this_promise

    def get(self):
        """
           Accessible interface for my debugger
        """
        return self.__debugger


class TargetWrapper():
    """
    TargetWrapper is a manager that build target and return
    a thenable promise before execute the target
    The promise will be answered with the exit status when
    the target finishes
    """
    def __init__(self, target_name):
        """
           Build the target and initialize promise to None
        """

        # handler for my target
        self.__target = GPS.BuildTarget(target_name)

        # promise about building this target
        self.__promise = None

    def wait_on_execute(self, main_name="", file=None, extra_args=None):
        """
        Called by the user. Will execute the target and return a promise.
        Promises made here will be answered with: exit status of the build.
        """

        self.__promise = Promise()
        self.__target.execute(main_name=main_name,
                              synchronous=False,
                              file=file,
                              extra_args=extra_args,
                              on_exit=self.__on_exit)
        return self.__promise

    def __timeout_after_exit(self):
        self.__promise.resolve(self.__status)
        return False

    def __on_exit(self, status):
        """
           Called by GPS when target finishes executing.
           Will answer the promise with exiting status.
        """

        self.__status = status
        GLib.timeout_add(200, self.__timeout_after_exit)
