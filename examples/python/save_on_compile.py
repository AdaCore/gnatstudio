"""Automatically saves the contents of the Messages window when a
   compilation has finished.
   The output is saved in a file called "messages.txt" in the root project's
   object directory.
"""


from GPS import *

def on_compilation_finished (hook, category):
   f = file (Project.root().object_dirs (False)[0] + "/messages.txt", "w")
   f.write (Console().get_text())
   f.close()

Hook ("compilation_finished").add (on_compilation_finished)
