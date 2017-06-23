"""This plugin provides an interface to addr2line, to convert addresses into
 file names and line numbers

This is in particular useful when analyzing backtraces output by GNAT when
the application was bound with the -E switch:
    gnatmake ... -bargs -E
addr2line is used to process the output and display a symbolic backtrace with
hyper links on which the user can click to jump to the actual source location.

One console is opened per executable (that is several addr2line consoles can
exist at the same time, each associated with a different executable).
While the console exists, you can enter additional backtraces to be processes
for the same executable, so that you do not have to enter the name of the
executable each time.

A convenience menu is added to open the console:
   /Navigate/Open Addr2line Console
It asks you for the location of the executable
"""

#############################################################################
# No user customization below this line
#############################################################################

from GPS import Console, EditorBuffer, File, MDI, Preference, Process
from gps_utils import interactive
import re
import os.path

Preference("Plugins/addr2line/args").create(
    "Arguments", "string",
    """Additional arguments to pass to addr2line""",
    "--functions --demangle=gnat")

file_line_re = "(([-_\w./\\\\]+):(\d+)(:(\d+))?)"


class Addr2line (Console):

    def __init__(self, executable):
        self.executable = executable
        self.name = "addr2line -e " + os.path.basename(self.executable)
        Console.__init__(self, self.name,
                         on_input=Addr2line.on_input)
        self.create_link(file_line_re, self.onclick)
        self.clear()
        self.write("Backtrace ?")
        self.enable_input(True)
        MDI.get(self.name).raise_window()

    def backtrace(self, bt):
        self.clear()
        cmd = "addr2line -e " + self.executable + " " + \
            Preference("Plugins/addr2line/args").get()
        self.write(cmd + "\n")
        Process([cmd, bt], ".+",
                on_exit=self.on_exit,
                on_match=self.on_output)

    def on_input(self, input):
        self.backtrace(input.replace("\n", " "))

    def on_output(self, process, matched, unmatched):
        self.write_with_links(unmatched + matched)

    def on_exit(self, process, status, full_output):
        MDI.get(self.name).raise_window()
        self.write("\n\nBacktrace ?")
        self.enable_input(True)

    def onclick(self, text):
        matched = re.match(file_line_re, text)
        buffer = EditorBuffer.get(File(matched.group(2)))
        MDI.get_by_child(buffer.current_view()).raise_window()
        line = int(matched.group(3))
        column = matched.group(5)
        if column is not None:
            buffer.current_view().goto(buffer.at(line, int(column)))
        else:
            buffer.current_view().goto(buffer.at(line, 1))


@interactive(name='open addr2line console',
             menu='/Navigate/Open Addr2line Console')
def open_addr2line_console():
    executable = MDI.input_dialog("Location of the executable ?", "Exec")[0]
    Addr2line(executable)
