"""
   The two classes here:
   class Promise
     - provides thenable promises for any method, process and procedures
   class ProcessWrapper
     - an example as well as a util that controls GPS.Process with promises
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
       and the corresponding promises are answered by
           either success or fail, which are user defined handlers(functions)
       when the pattern matches, or when the process is terminated by GPS.
    """
    def __init__(self, cmdargs):
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


class DebuggerWrapper():

    def __init__(self, f):
        self.__debugger = GPS.Debugger.spawn(f)
        self.__this_promise = None
        self.__next_cmd = None
        self.__output = ""

    def __is_busy(self, timeout):
        """
           Called by GPS at each interval.
        """
        # if the debugger is not busy
        if not self.__debugger.is_busy():
            timeout.remove()
            # if there's cmd to run, send it
            if self.__next_cmd is not None:
                self.__output = self.__debugger.send(self.__next_cmd)
                print "output will be: " + self.__output
                self.__next_cmd = None
            # if there's unanswered promise, answer it
            if self.__this_promise is not None:
                self.__this_promise.resolve(self.__output)
                self.__this_promise = None

    def __on_cmd_timeout(self, timeout):
        print self.__next_cmd, "is timeout"
        """
           Called by GPS at each interval
        """
        # check if promise unanswered
        if self.__this_promise is not None:
            self.__this_promise.resolve()
        self.__this_promise = None
        self.__next_cmd = None
        timeout.remove()

    def wait_and_send(self, cmd, timeout=0, interval=500):
        self.__this_promise = Promise()
        self.__next_cmd = cmd
        self.__output = None

        GPS.Timeout(interval, self.__is_busy)

        if timeout > 0:
            GPS.Timeout(timeout, self.__on_cmd_timeout)

        return self.__this_promise
