"""
   The 3 classes here:
   class Promise
     - provides thenable promises for any method, process and procedures

   class ProcessWrapper
         DebuggerWrapper
         TargetWrapper
     - examples as well as utils that controls process
       (or process-like execution) with promises
"""

import GPS
import re


class Promise:
    """
       A promise cam be registered with a resolve function which will be called
       whenever the promise is answered. User define and provide the function.
       A promise is thenable: it can execute the resolve function and return a
       new promise.
    """
    # the function that is called to fullfill the promise
    answer = None

    def then(self, answer=None):
        """
           Register user defined handler
        """
        self.answer = answer
        return Promise()

    def resolve(self, result=None):
        """
           If there is a handler call it, parameterized by the result
        """
        if self.answer is not None:
            self.answer(result)


class ProcessWrapper():
    """
       ProcessWrapper is a advanced process manager
       It make a promise (yield object of the promise class) when user:
           1 want to wait for match in output
           2 want to wait until process finish
       and the corresponding promises are answered with user defined
       handler (functions) when:
           1 the pattern matches/timeout
           2 the process is terminated by GPS.
    """
    def __init__(self, cmdargs=[]):
        """
           Initialize and run a process with no promises,
           no user-defined pattern to match,
           but a omnipotent regexp that catches everything.
           The process has empty output and two flags saying that
           the process is unfinished and no pattern has matched yet.
        """

        # input for constructor is a list of commands
        # concatenate them into a string
        cmd = ""
        for i in cmdargs:
            cmd += (i+" ")
        cmd = cmd.rstrip(" ")

        self.__final_promise = None
        self.__current_promise = None
        self.__current_pattern = None
        self.__output = ""
        self.finished = False
        self.__process = GPS.Process(cmd, ".+",
                                     on_match=self.__on_match,
                                     on_exit=self.__on_exit)

    def __on_match(self, process, match, unmatch):
        """
           Called by GPS everytime there's output comming
        """
        # Update all output returned by the process
        # and store it as a private buffer
        self.__output += match + unmatch

        # check if user has issued some pattern to match
        if self.__current_pattern is not None:
            p = re.search(self.__current_pattern, self.__output)
            # if the pattern is found, update the output to remaining and
            # answer the promise with True->found it
            if p is not None:
                self.__output = self.__output[p.span()[1]::]
                self.__current_promise.resolve(True)

    def __on_exit(self, process, status, remaining_output):
        """
           Call by GPS when the process is finished.
           Final_promise will be solved with status
           Current_promise will be solved with False
        """
        # check if there's unanswered match promises
        # if there is --> the pattern is never found, answer it with False
        if self.__current_promise is not None:
            self.__current_promise.resolve(False)

        # check if I had made a promise to finish the process
        # if there is, answer that promise with whatever the result is
        if self.__final_promise is not None:
            self.__final_promise.resolve(status)

        # mark my process as finished
        self.finished = True

    def wait_until_match(self, pattern=None, timeout=0):
        """
           Called by user. Make a promise to them that:
           I'll let you know when the pattern is matched/never matches
        """

        # keep the pattern info and return my promise
        self.__current_pattern = pattern
        self.__current_promise = Promise()

        # if user set up a timeout, set up: close output check after timeout
        if timeout > 0:
            x = GPS.Timeout(timeout, self.__on_timeout)
        return self.__current_promise

    def wait_until_terminate(self):
        """
           Called by impatient user. Make a promise to them that:
           I'll let you know when the process is finished
        """
        # process has already terminated, return nothing
        if self.finished:
            return None

        # process is still running, return my promise
        self.__final_promise = Promise()
        return self.__final_promise

    def __on_timeout(self, timeout):
        """
           Called by GPS when it's timeout for a pattern to appear in output.
        """
        if self.__current_promise is not None:
            # if the pattern is not found, answer the promise with False
            self.__current_promise.resolve(False)
        self.__current_pattern = None
        self.__current_promise = None
        timeout.remove()

    def get(self):
        return self.__process


class DebuggerWrapper():
    """
       DebuggerWrapper is a debbuger (eseentially a process in GPS) manager
       It make a promise (yield object of the promise class) when user:
           want to send a command to debugger

       and the corresponding promises are answered after
           1 the debugger is not busy, execute the command required
           2 timeout
    """

    # static variable for interval that the manager checks whether
    # the debugger is busy, in milliseconds
    __query_interval = 200

    def __init__(self, f):
        """
           Initialize a manager, begin a debugger on the given file
           with no timers, no promises, no command
        """
        self.__debugger = GPS.Debugger.spawn(f)
        self.__this_promise = None
        self.__next_cmd = None
        self.__output = None
        self.__timer = None
        self.__deadline = None

    def __is_busy(self, timeout):
        """
           Called by GPS at each interval.
        """
        # if the debugger is not busy
        if not self.__debugger.is_busy():

            # and if there's cmd to run, send it
            # remove the timer and deadline
            # and answer the promise with the output
            if self.__next_cmd is not None:
                if self.__next_cmd is not "":
                    self.__output = self.__debugger.send(self.__next_cmd)
                    self.__next_cmd = None
                    self.__remove_timers()
                    self.__this_promise.resolve(self.__output)
                else:
                    self.__this_promise.resolve(True)

    def __on_cmd_timeout(self, timeout):
        """
           Called by GPS at when the deadline defined by user is reached
        """
        # remove all timers
        self.__remove_timers()

        # answer the promise with the output
        if self.__this_promise is not None:
            self.__next_cmd = None
            self.__this_promise.resolve(self.__output)

    def __remove_timers(self):
        """
           Called in timers to remove both: prepare for new timer registration
        """
        if self.__deadline is not None:
            self.__deadline.remove()
            self.__deadline = None

        if self.__timer is not None:
            self.__timer.remove()
            self.__timer = None

    def wait_and_send(self, cmd="", timeout=0, block=False):
        """
           Called by user on request for command within deadline (time)
        """
        self.__remove_timers()
        self.__this_promise = Promise()
        self.__next_cmd = cmd
        self.__output = None

        self.__timer = GPS.Timeout(self.__query_interval, self.__is_busy)
        if not block:
            if timeout > 0:
                self.__deadline = GPS.Timeout(timeout, self.__on_cmd_timeout)

        return self.__this_promise

    def get(self):
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
        self.__target = GPS.BuildTarget(target_name)
        self.__promise = None

    def wait_on_execute(self):
        """
           Called by the user. Will execute the target and return a promise.
        """
        self.__promise = Promise()
        self.__target.execute(synchronous=False, on_exit=self.__on_exit)
        return self.__promise

    def __on_exit(self, status):
        """
           Called by GPS when target finishes executing.
           Will answer the promise with exiting status.
        """
        self.__promise.resolve(status)
