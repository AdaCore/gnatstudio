import GPS, sys

class Console_Process (GPS.Console, GPS.Process):
  """This class provides a way to spawn an interactive process and
     do its input/output in a dedicated console in GPS.
     You can of course derive from this class easily. Things are
     slightly more complicated if you want in fact to derive from
     a child of GPS.Console (for instance a class that would handle
     ANSI escape sequences). The code would then look like:
        class ANSI_Console (GPS.Console):
           def write (self, txt): ...

        class My_Process (ANSI_Console, Console_Process):
           def __init__ (self, process, args=""):
             Console_Process.__init__ (self, process, args)

     In the list of base classes for My_Process, you must put
     ANSI_Console before Console_Process. This is because python
     resolves overridden methods by looking depth-first search from
     left to right. This way, it will see ANSI_Console.write before
     Console_Process.write and therefore use the former.

     However, because of that the __init__ method that would be called
     when calling My_Process (...) is also that of ANSI_Console.
     Therefore you must define your own __init__ method locally.
  """

  def on_output (self, matched, unmatched):
    """This method is called when the process has emitted some output.
       The output is then printed to the console"""
    self.write (unmatched + matched)

  def on_exit (self, status, remaining_output):
    """This method is called when the process terminates.
       As a result, we close the console automatically, although we could
       decide to keep it open as well"""
    try:
       if self.close_on_exit:
          self.destroy ()  # Close console
       else:
          self.write (remaining_output)
          self.write ("exit status: " + `status`)
    except: pass  # Might have already been destroyed if that's what
                  # resulted in the call to on_exit

  def on_input (self, input):
    """This method is called when the user has pressed <enter> in the
       console. The corresponding command is then sent to the process"""
    self.send (input)

  def on_destroy (self):
    """This method is called when the console is being closed.
       As a result, we terminate the process (this also results in a
       call to on_exit"""
    self.kill ()

  def on_resize (self, rows, columns):
    """This method is called when the console is being resized. We then
       let the process know about the size of its terminal, so that it
       can adapt its output accordingly. This is especially useful with
       processes like gdb or unix shells"""
    self.set_size (rows, columns)

  def on_interrupt (self):
    """This method is called when the user presses control-c in the
       console. This interrupts the command we are currently processing"""
    self.interrupt()

  def on_completion (self, input):
    """The user has pressed <tab> in the console. The default is just to
       insert the \t character, but if you are driving a process that knows
       about completion, such as an OS shell for instance, you could have
       a different implementation. input is the full input till, but not
       including, the tab character"""
    self.write ("\t")

  Shift_Mask   = 1
  Lock_Mask    = 2
  Control_Mask = 4
  Mod1_Mask    = 8

  Key_Return    = 65293
  Key_Backspace = 65288
  Key_Tab       = 65289
  Key_Left      = 65361
  Key_Up        = 65362
  Key_Right     = 65363
  Key_Down      = 65364
  Key_Escape    = 65307

  def on_key (self, keycode, key, modifier):
    """The user has pressed a key in the console (any key). This is called
       before any of the higher level on_completion or on_input callbacks.
       If this subprogram returns True, GPS will consider that the key has
       already been handled and will not do its standard processing with it.
       By default, we simply let the key through and let GPS handle it.

       _key_ is the unicode character (numeric value) that was entered by
       the user. _modifier_ is a mask of the control and shift keys that
       were pressed at the same time. See the *_Mask constants above.
       keycode is the code of the key, which is useful for non-printable
       characters. It is set to 0 in some cases if the input is simulated
       after the user has copied some text into the console

       This function is also called for each character pasted by the user
       in the console. If it returns True, then the selection will not be
       inserted in the console.
    """
    return False

  def __init__ (self, process, args="", close_on_exit=True, force = False,
                ansi = False, manage_prompt = True):
    """Spawn a new interactive process and show its input/output in a
       new GPS console. The process is created so that it does not
       appear in the task manager, and therefore the user can exit GPS
       without being asked whether or not to kill the process.

       If _force_ is set to True, a new console is opened, otherwise an
       existing one will be reused (although you should take care in this
       case if you have multiple processes attached to the same console).

       If _manage_prompt_ is True, then GPS will do some higher level
       handling of prompts: when some output is done by the process, GPS
       will temporarily hide what the user was typing, insert the output,
       and append what the user was typing. This is in general suitable but
       might interfer with external programs that do their own screen
       management through ANSI commands (like a Unix shell for instance)."""
    self.close_on_exit = close_on_exit
    try:
      GPS.Console.__init__ (
        self, process.split()[0],
        manage_prompt = manage_prompt,
        on_input      = self.on_input,
        on_destroy    = self.on_destroy,
        on_resize     = self.on_resize,
        on_interrupt  = self.on_interrupt,
        on_completion = self.on_completion,
        on_key        = self.on_key,
        ansi          = ansi,
        force         = force)
      GPS.Process.__init__ (
        self, process + " " + args, ".+",
        single_line_regexp=True,  # For efficiency
        strip_cr = not ansi,  # If ANSI terminal, CR is relevant
        task_manager=False,
        on_exit = self.on_exit,
        on_match = self.on_output)
      GPS.MDI.get_by_child (self).raise_window ()
    except:
      GPS.Console().write (str (sys.exc_info()[1]) + "\n")
      try:
         self.destroy()
         self.kill()
      except:
         pass
      GPS.Console().write ("Could not spawn: " + process + " " + args + "\n")

