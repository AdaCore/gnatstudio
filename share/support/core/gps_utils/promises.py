"""
   The 4 classes here:
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

    # answer = a function/handler that is called to fullfill the promise
    answer = None

    def then(self, answer=None):
        """
           Register user defined handler
        """
        self.answer = answer
        return Promise()

    def resolve(self, result=None):
        """
           If there is a handler, then call it -> parameterized by the result
        """
        if self.answer is not None:
            self.answer(result)


class ProcessWrapper():
    """
       ProcessWrapper is a advanced process manager
       It makes a promise (yield object of the promise class) when user:
           1 want to wait for match in output
           2 want to wait until process finish
       and the corresponding promises are answered with user defined
       handler (functions) when:
           1 the pattern matches or timeout.
           2 the process is terminated by GPS.
    """

    def __init__(self, cmdargs=[], messages=True):
        """
           Initialize and run a process with no promises,
           no user-defined pattern to match,
           but a omnipotent regexp that catches everything.
           The process has empty output and two flags saying that
           the process is unfinished and no pattern has matched yet.
        """

        # input for constructor is a list for command and args
        # concatenate them into a string
        cmd = ""
        for i in cmdargs:
            cmd += (i+" ")
        cmd = cmd.rstrip(" ")

        # __final_promise = about termination
        self.__final_promise = None

        # __current_promise = about on waiting wish for match something
        self.__current_promise = None

        # __current_pattern = regexp that user waiting for in the output
        self.__current_pattern = None

        # __whether the __current_promise is answered
        self.__current_answered = False

        # __output = a buffer for current output of self.__process
        self.__output = ""

        # __whether process has finished
        self.finished = False

        # handler of process will be created -> start running
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
            # answer the promise with True-->found it

            if p is not None:
                self.__current_answered = True
                self.__output = self.__output[p.span()[1]::]
                self.__current_promise.resolve(True)

    def __on_exit(self, process, status, remaining_output):
        """
           Call by GPS when the process is finished.
           Final_promise will be solved with status
           Current_promise will be solved with False
        """

        # mark my process as finished
        self.finished = True

        # check if there's unanswered match promises
        # if there is --> pattern has never been found, answer with False
        if self.__current_promise is not None and \
           not self.__current_answered:
            self.__current_promise.resolve(False)

        # check if I had made a promise to finish the process
        # if there is, answer with whatever the exit status is
        if self.__final_promise is not None:
            self.__final_promise.resolve(status)

    def wait_until_match(self, pattern=None, timeout=0):
        """
           Called by user. Make a promise to them that:
           I'll let you know when the pattern is matched/never matches
           * Promise made here will be answered with: True/False
        """

        # keep the pattern info and return my promise
        self.__current_pattern = pattern
        self.__current_promise = Promise()

        # if user defines a timeout, set up to
        # close output check after that timeout

        if timeout > 0:
            x = GPS.Timeout(timeout, self.__on_timeout)

        return self.__current_promise

    def wait_until_terminate(self):
        """
           Called by user. Make a promise to them that:
           I'll let you know when the process is finished
           * Promise made here will be answered with: exit status
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

        timeout.remove()
        # current promise unanswered --> too late, it fails
        if not self.__current_answered:
            self.__current_pattern = None
            self.__current_answered = True
            # answer the promise with False
            self.__current_promise.resolve(False)

    def get(self):
        return self.__process

    def kill(self):
        self.__process.kill()


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

    def __init__(self, f, reuse_existing=True):
        """
           Initialize a manager, begin a debugger on the given file
           with no timers, no promises, no command
        """

        if reuse_existing:
            try:
                # handler for debugger
                self.__debugger = GPS.Debugger.get()

                # if we reach this, a debugger is running: interrupt it
                GPS.execute_action("/Debug/Interrupt")
            except:
                self.__debugger = GPS.Debugger.spawn(f)
                pass
        else:
            self.__debugger = GPS.Debugger.spawn(f)

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
                    self.__output = self.__debugger.send(self.__next_cmd)
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
        if self.__this_promise is not None:
            self.__next_cmd = None
            self.__this_promise.resolve(self.__output)

    def __remove_timers(self):
        """
           Called in timers to remove both: prepare for new timer registration
        """
        if self.__deadline is not None:
            try:
                self.__deadline.remove()
            except:
                pass
            self.__deadline = None

        if self.__timer is not None:
            try:
                self.__timer.remove()
            except:
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

    def wait_on_execute(self, main_name=""):
        """
           Called by the user. Will execute the target and return a promise.
           Promises made here will be answered with: exit status of building
        """

        self.__promise = Promise()
        self.__target.execute(main_name=main_name,
                              synchronous=False,
                              on_exit=self.__on_exit)

        return self.__promise

    def __timeout_after_exit(self, timeout):
        timeout.remove()
        self.__promise.resolve(self.__status)

    def __on_exit(self, status):
        """
           Called by GPS when target finishes executing.
           Will answer the promise with exiting status.
        """
        self.__status = status
        GPS.Timeout(200, self.__timeout_after_exit)
