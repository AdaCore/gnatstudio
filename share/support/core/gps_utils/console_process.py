#!/usr/bin/python
# -*- coding: utf-8 -*-
import GPS
import sys

Shift_Mask = 1
Lock_Mask = 2
Control_Mask = 4
Mod1_Mask = 8
Mod2_Mask = 16

Key_Return = 65293
Key_Backspace = 65288
Key_Tab = 65289
Key_Left = 65361
Key_Up = 65362
Key_Right = 65363
Key_Down = 65364
Key_Escape = 65307


class Console_Process(GPS.Console, GPS.Process):

    """This class provides a way to spawn an interactive process and
     do its input/output in a dedicated console in GPS.
     The process is created so that it does not appear in the task
     manager, and therefore the user can exit GPS without being
     asked whether or not to kill the process.

     You can of course derive from this class easily. Things are
     slightly more complicated if you want in fact to derive from
     a child of GPS.Console (for instance a class that would handle
     ANSI escape sequences). The code would then look like::

        class ANSI_Console (GPS.Console):
           def write (self, txt): ...

        class My_Process (ANSI_Console, Console_Process):
           def __init__ (self, command):
             Console_Process.__init__ (self, command)

     In the list of base classes for My_Process, you must put
     ANSI_Console before Console_Process. This is because python
     resolves overridden methods by looking depth-first search from
     left to right. This way, it will see ANSI_Console.write before
     Console_Process.write and therefore use the former.

     However, because of that the __init__ method that would be called
     when calling My_Process (...) is also that of ANSI_Console.
     Therefore you must define your own __init__ method locally.

     See also the class ANSI_Console_Process if you need your process
     to execute within a terminal that understands ANSI escape sequences.

     :param boolean force: If True, a new console is opened, otherwise an
        existing one will be reused (although you should take care in this
        case if you have multiple processes attached to the same console).

     :param boolean manage_prompt: If True, then GPS will do some higher level
        handling of prompts: when some output is done by the process, GPS
        will temporarily hide what the user was typing, insert the output,
        and append what the user was typing. This is in general suitable but
        might interfer with external programs that do their own screen
        management through ANSI commands (like a Unix shell for instance).

     :param boolean task_manager: If True, the process will be visible in the
          GPS tasks view and can be interrupted or paused by users.
          Otherwise, it is running in the background and never visible to the
          user.
     """

    def __init__(self, command, close_on_exit=True, force=False,
                 ansi=False, manage_prompt=True, task_manager=False):
        self.close_on_exit = close_on_exit
        try:
            GPS.Console.__init__(
                self,
                command[0],
                manage_prompt=manage_prompt,
                on_input=self.on_input,
                on_destroy=Console_Process.on_destroy,
                on_resize=self.on_resize,
                on_interrupt=Console_Process.on_interrupt,
                on_completion=self.on_completion,
                on_key=self.on_key,
                ansi=ansi,
                force=force)
            GPS.Process.__init__(
                self,
                command=command,
                regexp='.+',
                single_line_regexp=True,  # For efficiency
                strip_cr=not ansi,        # if ANSI terminal, CR is irrelevant
                task_manager=task_manager,
                on_exit=self.on_exit,
                on_match=self.on_output)
            GPS.MDI.get_by_child(self).raise_window()
        except:
            GPS.Console().write(str(sys.exc_info()[1]) + '\n')
            try:
                self.destroy()
                self.kill()
            except:
                pass
            GPS.Console().write('Could not spawn: %s\n' % (' '.join(command)))

    def on_output(self, matched, unmatched):
        """This method is called when the process has emitted some output.
           The output is then printed to the console
        """
        self.write(unmatched + matched)

    def on_exit(self, status, remaining_output):
        """This method is called when the process terminates.
           As a result, we close the console automatically, although we could
           decide to keep it open as well
        """
        try:
            if self.close_on_exit:
                self.destroy()  # Close console
            else:
                self.write(remaining_output)
                self.write('\nexit status: %s' % status)
        except:
            pass  # Might have already been destroyed if that's what
            # resulted in the call to on_exit

    def on_input(self, input):
        """This method is called when the user has pressed <enter> in the
           console. The corresponding command is then sent to the process
        """
        self.send(input)

    def on_destroy(self):
        """This method is called when the console is being closed.
           As a result, we terminate the process (this also results in a
           call to on_exit
        """
        self.kill()

    def on_resize(self, console, rows, columns=None):
        """This method is called when the console is being resized. We then
           let the process know about the size of its terminal, so that it
           can adapt its output accordingly. This is especially useful with
           processes like gdb or unix shells
        """
        # ??? There is an issue here, since this subprogram seems to be called
        # sometimes with 3 parameters, sometimes with 4: for sure, Ada calls it
        # with 3 parameters, but since it was passed to the GPS.Console
        # constructor as "self.on_resize", there is one extra arg for self.

        if isinstance(console, int):
            columns = rows
            rows = console
        self.set_size(rows, columns)

    def on_interrupt(self):
        """This method is called when the user presses control-c in the
           console. This interrupts the command we are currently processing
        """
        self.interrupt()

    def on_completion(self, input):
        """The user has pressed <tab> in the console. The default is just to
           insert the \t character, but if you are driving a process that knows
           about completion, such as an OS shell for instance, you could have
           a different implementation. input is the full input till, but not
           including, the tab character
        """
        self.write('\t')

    def on_key(self, keycode, key, modifier):
        """The user has pressed a key in the console (any key). This is called
           before any of the higher level on_completion or on_input callbacks.
           If this subprogram returns True, GPS will consider that the key has
           already been handled and will not do its standard processing with
           it. By default, we simply let the key through and let GPS handle it.

           :param key: the unicode character (numeric value) that was entered
              by the user. _modifier_ is a mask of the control and shift keys
              that were pressed at the same time. See the Mask constants above.
              keycode is the code of the key, which is useful for non-printable
              characters. It is set to 0 in some cases if the input is
              simulated after the user has copied some text into the console

           This function is also called for each character pasted by the user
           in the console. If it returns True, then the selection will not be
           inserted in the console.
        """
        return False


class ANSI_Console_Process(Console_Process):

    """This class has a purpose similar to Console_Process.
      However, this class does not attempt to do any of the high-level
      processing of prompt and input that Console_Process does, and instead
      forward immediately any of the key strokes within the console directly
      to the external process.
      It also provides an ANSI terminal to the external process. The latter
      can thus send escape sequences to change colors, cursor position,...
    """

    def __init__(self, command):
        Console_Process.__init__(self, command, force=True, ansi=True,
                                 manage_prompt=False)

    def on_input(self, input):
        # Do nothing, this was already handled when each key was pressed
        pass

    def on_completion(self, input):
        # Do nothing, this was already handled when each key was pressed
        pass

    def __get_key_str(self, keycode, key):
        """Convert a key/keycode into a string that can be sent to an external
           process"""

        if keycode == Key_Return:
            return '\r'
        elif keycode == Key_Tab:
            return '\t'
        elif keycode == Key_Backspace:
            return chr(8)
        elif key != 0:
            return unichr(key).encode('utf8')
        elif keycode == Key_Escape:
            return "\033"
        elif keycode == Key_Left:
            return "\033[D"
        elif keycode == Key_Right:
            return "\033[C"
        elif keycode == Key_Up:
            return "\033[A"
        elif keycode == Key_Down:
            return "\033[B"
        else:
            GPS.Logger('CONSOLE').log('keycode=%s key=%s' % (keycode, key))
            return ''

    def on_key(self, keycode, key, modifier):
        if modifier == 0 or modifier == Shift_Mask:
            self.send(self.__get_key_str(keycode, key), add_lf=False)
        elif modifier == Control_Mask:
            if key in range(ord('A'), ord('_') + 1):
                self.send(chr(key - ord('A') + 1), add_lf=False)
            elif key in range(ord('a'), ord('z') + 1):
                # Same as pressing the upper-case equivalent
                self.send(chr(key - ord('a') + 1), add_lf=False)
            elif keycode == Key_Return or keycode == Key_Escape:
                # Same as key without return
                self.send('\n', add_lf=False)
            else:
                # Seems like most terminals just send ESC in such a case
                self.send("\033", add_lf=False)
        else:
            GPS.Logger('CONSOLE').log('keycode=%s key=%s modifier=%s'
                                      % (keycode, key, modifier))

        return True
