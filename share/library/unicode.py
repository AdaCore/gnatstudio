"""
Enter Unicode characters in editors

This module provides a GPS action that, when executed, opens a
small command window. In this window, the user can either type the
numeric value or the name of a unicode character to insert at the
cursor position in the current editor.

For instance, select the menu /Edit/Insert Unicode, then type the
following decimal value:
   928
This will insert a PI character. You could also insert it by typing
   greek capital letter pi
or (in hexadecimal):
   x3A0

Another GPS action is provided that describes the unicode character
currently under the cursor, including its numeric value and its name.
This action is called "Describe unicode char". It can be bound to a key,
assigned to a menu, or more easily executed through the startup script
execute_extended.py
"""
from GPS import CommandWindow, Console, EditorBuffer
from gps_utils import interactive
import unicodedata


background_color = "yellow"
# Background color to use for the command window

prefixes = ["greek capital letter ",
            "greek small letter ",
            "latin capital letter ",
            "latin small letter "]
words = ["acute", "grave", "circumflex", "diaresis", "title",
         "with"]
# Possible completions: prefixes is checked from the beginning of the
# command line, whereas words is checked only for the previous word.
# You can add to these in the initialization strings by using a python
# command similar to
# #    prefix = prefix + ["vertical", "opening"]
# Only lower case letters should be used

############################################################################
# No user customization below this line
############################################################################


def findcommonstart(strlist):
    return strlist[0][:([min([x[0] == elem for elem in x])
                         for x in zip(*strlist)] + [0]).index(0)]


@interactive(name="Insert unicode", category="Editor", filter="Source editor",
             menu="/Edit/Insert Unicode", before="Insert File...")
def insert():
    """Insert any unicode character given its decimal or
       hexadecimal value, or its name"""
    Unicode()


class Unicode (CommandWindow):

    def __init__(self):
        CommandWindow.__init__(self, prompt="Character code:",
                               on_key=self.on_key,
                               on_activate=self.on_activate)
        self.set_background(background_color)

    def on_key(self, input, key, cursor_pos):
        if key.lower() == "tab":
            input = " ".join(input.split())  # Only one space between words
            input = input.lower()
            compl = [p for p in prefixes if p.startswith(input)]
            if compl:
                self.write(findcommonstart(compl))
            else:
                split = input.split()
                compl = [p for p in words if p.startswith(split[-1])]
                if compl:
                    self.write(
                        " ".join(split[:-1]) + " " +
                        findcommonstart(compl) + " ")
            return True

    def on_activate(self, input):
        if input != "":
            buffer = EditorBuffer.get()
            input = input.strip()
            if input[0] in "0123456789":
                chr = unichr(int(input))
            elif input[0] == 'x' and input[1] in "0123456789":
                chr = unichr(int(input[1:], 16))
            else:
                chr = unicodedata.lookup(input)
            buffer.insert(buffer.current_view().cursor(), chr.encode('utf-8'))


@interactive(name="Describe unicode char", category="Editor",
             filter="Source editor")
def describe_char(char=None):
    """Describe the unicode character under the cursor (name, value,...)"""
    if not char:
        char = EditorBuffer.get().current_view().cursor().get_char()
    uni = char.decode("utf-8")
    Console().write("Character:  " + char + "\n")
    Console().write("      Name: " + unicodedata.name(uni) + "\n")
    Console().write("   Unicode: " + repr(ord(uni)) +
                    " (U+" + hex(ord(uni))[2:] + ")\n")
    Console().write("  Category: " + unicodedata.category(uni) + "\n")
