"""
Processes a text selection through an external shell command, and
substitutes it with the output of that command.
If no text is selected, simply insert the output of the external command.

This is similar to vi's ! command. For instance, you can use this
script to run a select chunk of text through the following shell
commands:
   - "fmt"  => Reformat each paragraph of the selection, using
               advanced algorithms that try not to break after the first
               word of a sentence, or before the last. Also try to
               balance line lengths.
               See the function fmt_selection() below, which automatically
               sets a number of parameters when calling this function.

   - "sort" => Sort the selected lines

   - "ls"   => If you have no current selection, this will simply insert
               the contents of the current directory in the file

   - "date" => Insert the current date in the file
"""

############################################################################
# No user customization below this line
############################################################################

from GPS import Preference, EditorBuffer, Process, CommandWindow
from gps_utils import interactive


def sel_pipe(command, buffer=None):
    """Process the current selection in BUFFER through COMMAND,
       and replace that selection with the output of the command"""
    if not buffer:
        buffer = EditorBuffer.get()
    start = buffer.selection_start()
    end = buffer.selection_end()

    # Ignore white spaces and newlines at end, to preserve the rest
    # of the text
    if start != end:
        while end.get_char() == ' ' or end.get_char() == '\n':
            end = end - 1

        text = buffer.get_chars(start, end)
    else:
        text = ""

    proc = Process(command)
    proc.send(text)
    proc.send(chr(4))  # Close input
    output = proc.get_result()
    with buffer.new_undo_group():
        if start != end:
            buffer.delete(start, end)
        buffer.insert(start, output.rstrip())


@interactive(name="Fmt selection")
def fmt_selection():
    """Process the current selection
       through the "fmt" command to reformat paragraphs
    """
    width = Preference("Src-Editor-Highlight-Column").get()
    buffer = EditorBuffer.get()
    prefix = None

    if buffer.file().language() == "ada":
        prefix = "--"

    loc = buffer.selection_start().beginning_of_line()
    while loc.get_char() == ' ':
        loc = loc + 1

    prefix = '-p """' + (' ' * (loc.column() - 1)) + prefix + '"""'
    sel_pipe("fmt " + prefix + " -w " + repr(width), buffer)


class ShellProcess (CommandWindow):

    """Send the current selection to an external process,
       and replace it with the output of that process"""

    def __init__(self):
        CommandWindow.__init__(self, global_window=True,
                               prompt="Shell command:",
                               on_activate=self.on_activate)

    def on_activate(self, shell_command):
        sel_pipe(shell_command)


@interactive(filter="Source editor")
def pipe():
    """
 Process the current selection through a shell command,
 and replace it with the output of that command.
    """
    ShellProcess()
