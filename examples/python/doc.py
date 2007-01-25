## This is an example of how documentation can be generated in batch mode.
## Assuming you want to generate documentation for project My_Project located
## in /foo/bar, just issue the following command:
##
##    $ gps -P /foo/bar/my_project.gpr --load=python:doc.py

from GPS import *

# If GPS is done with its processing, exit.
def delayed_exit (t):
   if len (Command.list()) == 0:
      exit()

# Set the preferences.
Preference ("Doc-Process-Body").set (True)
Preference ("Doc-Xref-All").set (True)
Preference ("Doc-Tagged").set (True)
Preference ("Doc-Show-Private").set (True)
Preference ("Doc-References").set (True)

# Generate documentation for the root projects and all subprojects.
execute_action ("/Tools/Documentation/Generate project & subprojects")

# Try to exit every 10 seconds.
Timeout (10000, delayed_exit)
