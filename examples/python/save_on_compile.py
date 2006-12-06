"""Automatically saves the contents of the Messages window when a
   compilation has finished.
   The output is saved in a file called "output" in the current directory.
   Such a script should be enhanced to save the file in a specific directory,
   for instance the root project's object dir, to make sure we do not ovewrite
   an existing file,..."""

from GPS import *

def on_compilation_finished (hook, category):
   f = file ("output", "w")
   f.write (Console().get_text())
   f.close()

Hook ("compilation_finished").add (on_compilation_finished)
