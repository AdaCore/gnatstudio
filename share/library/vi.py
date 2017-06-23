"""This plugin provides a basic emulation of the vi editor's command line.

It is not meant as a vi emulation, ie it will not emulate the insertion
and command modes for instance.
Instead, it provides a way to open a command line (the default key
shortcut is control-:, by analogy with vi's ":" prompt.
In this command line, you can type some of vi's command, to modify the
current editor. The format of the command line is

  [from[,to]]cmd[params]

where "from,to" defines a range of lines other that the command should
act on. If none of "from" and "to" are specified, the command only applies
to the current line.  If only "from" is specified, the command applies to
the line specified by "from". If both are specified, the command applies to
the range of lines defined by those, including those two lines.

"from" and "to" can both be one of:
   - ".": indicates the line on which the cursor currently is
   - "$": indicates the last line in the buffer
   - "<": indicates the start of the current selection
   - ">": indicates the end of the current selection
   - a number: indicates a specific line in the buffer
   - "+" and a number: indicates a line relative to the current line, or
     to the line specified by "from"
   - "-" and a number: indicates a line relative to the current line, or
     to the line specified by "from"
In addition, "from,to" can be replaced by "%" to indicate the whole
buffer.

The supported commands are:
   - s/str/repl/
     replaces the first occurrence of the regular expression "str" by
     "repl". The latter can contain \1, \2,... to reference parenthesis
     groups in "str".
     If a range of lines was specified, the replacement is done for all
     occurrences within that range. If a single line was specified (or
     none, which defaults to the current line), only the first occurrence
     on that line is replaced. If an extra "g" is put after the closing "/",
     all occurrences on that line are replaced.
     For convenience, the separator can be any character, not just "/".
     An extra "i" on the command line will make the search case-insensitive.
     You can set the option "ignorecase" below to be in case-insensitive
     mode by default.

   - d
     deletes the lines in the specied range

Here are a few examples:
   %s/foo/bar
      Replaces all instances of "foo" by "bar" in the whole buffer
   .,$s/foo/bar
      Replaces all instances of "foo" by "bar", in all the lines after the
      current one
   67,120s/\d+/DIGIT:\1
      Replaces all numbers from line 67 to 120 inclusive by adding a
      prefix "DIGIT:" to them

The command window that pops up when you press <control-:> has a history: if
you use the <up> and <down> keys, previous commands can be executed again,
or modified and then executed.
In addition, pression <control-.> will execute the previous command without
displaying the command window. This is by analogy with vi's <.> command,
although the scope is less ambitious here.
"""

#############################################################################
# No user customization below this line
#############################################################################

from GPS import Preference, CommandWindow, EditorBuffer, Hook, parse_xml
import re

Preference("Plugins/vi/bgcolor").create(
    "Background color", "color",
    """Color to use for the command line window""",
    "red")

Preference("Plugins/vi/ignorecase").create(
    "Ignore case", "boolean",
    """If enabled, searching will ignore casing by default""",
    False)


def on_gps_started(hook_name):
    parse_xml("""
  <action name='vi_command_line' category="Editor" output="none">
     <description />
     <filter id="Source editor" />
     <shell lang="python">vi.CmdLine()</shell>
  </action>
  <action name='vi_repeat_cmd' category="Editor" output="none">
     <description />
     <filter id="Source editor" />
     <shell lang="python">vi.CmdLine.repeat_command()</shell>
  </action>
  <menu action='vi_command_line' after="Refill">
    <title>/Edit/Vi command line</title>
  </menu>
  <key action="vi_command_line">control-colon</key>
  <key action="vi_repeat_cmd">control-period</key>
""")


class CmdLine(CommandWindow):
    history = []

    def __init__(self):
        try:
            self.loc = EditorBuffer.get().current_view().cursor()
            CommandWindow.__init__(self,
                                   prompt=self.prompt(),
                                   on_cancel=self.on_cancel,
                                   on_key=self.on_key,
                                   on_activate=self.on_activate)
            self.set_background(Preference("Plugins/vi/bgcolor").get())

            self.current_in_history = -1
            self.current_cmd_line = ""  # Before moving in the history

        except:
            pass

    def prompt(self):
        """Return the prompt to use for the command window"""
        return "Cmd:"

    def on_key(self, input, key, cursor_pos):
        if key.lower() == "up":
            if self.current_in_history == 0:
                self.current_cmd_line = input
            if self.current_in_history < len(CmdLine.history) - 1:
                self.current_in_history = self.current_in_history + 1
                self.write(CmdLine.history[self.current_in_history])

        elif key.lower() == "down":
            if self.current_in_history > 0:
                self.current_in_history = self.current_in_history - 1
                self.write(CmdLine.history[self.current_in_history])
            elif self.current_cmd_line != "":
                self.current_in_history = -1
                self.write(CmdLine.current_cmd_line)
                self.current_cmd_line = ""

    def on_activate(self, input):
        """The user has pressed enter"""
        if input != "":
            CmdLine.history.insert(0, input)
            CmdLine.repeat_command(self.loc)

    @staticmethod
    def get_loc(cmd, loc, buffer):
        """Parse the first location described in ref, and returns it and
           the remaining of the command. LOC is used when the location is
           "." or relative"""
        if cmd[0] == ".":
            return (loc, cmd[1:])
        elif cmd[0] == "$":
            return (buffer.end_of_buffer(), cmd[1:])
        elif cmd[0] == "<":
            return (buffer.selection_start(), cmd[1:])
        elif cmd[0] == ">":
            return (buffer.selection_end(), cmd[1:])
        else:
            match = re.search("([-+]?\d+)(.*)", cmd)
            if match:
                line = int(match.group(1))
                if cmd[0] == "+" or cmd[0] == "-":
                    return (loc.forward_line(line), match.group(2))
                else:
                    return (buffer.at(line, 1), match.group(2))
            else:
                return (loc, cmd)

    @staticmethod
    def get_scope(cmd, current, buffer):
        """return a tuple: (cmd_to_execute, FROM, TO).
           TO is set to FROM if the command should be executed only once"""
        if cmd[0] == "%":
            return (cmd[1:],
                    buffer.beginning_of_buffer(),
                    buffer.end_of_buffer())

        comma = cmd.find(",")
        if comma < 0:
            l, c = CmdLine.get_loc(cmd, current, buffer)
            return (c, l, l)  # Execute only once
        else:
            l, c = CmdLine.get_loc(cmd[:comma], current, buffer)
            e, c = CmdLine.get_loc(cmd[comma + 1:], l,   buffer)
            return (c, l, e)

    @staticmethod
    def do_replace(arg, loc, maxloc):
        buffer = loc.buffer()
        params = arg[1:].split(arg[0])
        if len(params) == 3:
            pattern, replace, options = params
        elif len(params) == 2:
            pattern, replace = params
            options = ""

        count = 1
        icase = Preference(
            "Plugins/vi/ignorecase").get() or (options.find("i") < 0)
        if loc == maxloc:
            maxloc = loc.end_of_line()  # On whole line by default
        else:
            count = 10000000

        if options.find("g") >= 0:
            count = 100000000  # as many times as needed

        while count > 0:
            result = loc.search(
                pattern, regexp=True,
                dialog_on_failure=False, case_sensitive=icase)
            if not result:
                return
            else:
                start, last = result
                if start > maxloc:
                    return

                # Add support for \1,.. in the replacement string
                found = buffer.get_chars(start, last - 1)
                if icase:
                    r = re.compile(pattern)
                else:
                    r = re.compile(pattern, re.IGNORECASE)
                repl = r.sub(replace, found)
                buffer.delete(start, last - 1)
                buffer.insert(start, repl)
                loc = start + len(repl)
                buffer.current_view().goto(loc)

            count = count - 1

    @staticmethod
    def do_delete_line(arg, loc, maxloc):
        loc = loc.beginning_of_line()
        maxloc = maxloc.end_of_line()
        loc.buffer().delete(loc, maxloc)
        loc.buffer().current_view().goto(loc)

    @staticmethod
    def repeat_command(loc=None):
        """Repeat the last command that was executed, in Editor"""
        if not loc:
            loc = EditorBuffer.get().current_view().cursor()
        buffer = loc.buffer()
        (cmd, loc, maxloc) = CmdLine.get_scope(CmdLine.history[0], loc, buffer)

        with buffer.new_undo_group():
            if cmd[0] == "s":
                CmdLine.do_replace(cmd[1:], loc, maxloc)
            elif cmd[0] == "d":
                CmdLine.do_delete_line(cmd[1:], loc, maxloc)

    def on_cancel(self, input):
        """The user has cancelled the search"""
        pass

Hook("gps_started").add(on_gps_started)
