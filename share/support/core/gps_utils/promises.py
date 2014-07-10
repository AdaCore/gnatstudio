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

        self._final_promise = None
        self._current_promise = None
        self._current_pattern = None
        self._output = ""
        self.finished = False
        self.matched = False
        self._process = GPS.Process(cmd, ".+",
                                    on_match=self.__on_match,
                                    on_exit=self.__on_exit)

    def __on_match(self, process, match, unmatch):
        """
           Called by GPS everytime there's output comming
        """
        # Update all output returned by the process
        # and store it as a private buffer
        self._output += match + unmatch

        # check if user has issued some pattern to match
        if self._current_pattern is not None:
            p = re.search(self._current_pattern, self._output)
            # if the pattern is found, update the output to remaining and
            # answer the promise with True->found it
            if p is not None:
                self.matched = True
                self._output = self._output[p.span()[1]::]
                self._current_promise.resolve(self.matched)

    def __on_exit(self, process, status, remaining_output):
        """
           Call by GPS when the process is finished.
        """
        # check if there's unanswered match promises
        # if there is, that means the pattern is never found,
        # so answer it with fail
        if self._current_promise is not None:
            self._current_promise.resolve(False)

        # check if I had made a promise to finish the process
        # if there is, answer that promise with whatever the result is
        if self._final_promise is not None:
            self._final_promise.resolve(status)

        # mark my process as finished
        self.finished = True

    def wait_until_match(self, pattern=None, timeout=0):
        """
           Called by user. Make a promise to them that:
           I'll let you know when the pattern is matched/never matches
        """

        # keep the pattern info and return my promise
        self._current_pattern = pattern
        self._current_promise = Promise()
        self.matched = False

        # if user set up a timeout, set up: close output check after timeout
        if timeout > 0:
            x = GPS.Timeout(timeout, self.__on_timeout)
        return self._current_promise

    def wait_until_terminate(self):
        """
           Called by impatient user. Make a promise to them that:
           I'll let you know when the process is finished
        """
        # process has already terminated, return nothing
        if self.finished:
            return None

        # process is still running, return my promise
        self._final_promise = Promise()
        return self._final_promise

    def __on_timeout(self, timeout):
        """
           Called by GPS when time for a match desire is out.
        """
        if self._current_promise is not None:
            # if the pattern is not found, answer the promise with False
            if not self.matched:
                self._current_promise.resolve(False)
        self._current_pattern = None
        self._current_promise = None
        timeout.remove()
