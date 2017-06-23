# This is an example of how documentation can be generated in batch mode.
# Assuming you want to generate documentation for project My_Project located
# in /foo/bar, just issue the following command:
##
# $ gps -P /foo/bar/my_project.gpr --load=python:doc.py

from GPS import Preference, Hook, Project
from pygps import delayed_exit


def on_gps_started(hook):
    # Set the preferences. You can adjust them at your convenience.
    Preference("Doc-Process-Body").set(True)
    Preference("Doc-Show-Private").set(True)
    Preference("Doc-References").set(True)
    Preference("Doc-Up-To-Date-Only").set(False)

    # Generate documentation for the root projects and all subprojects.
    Project.root().generate_doc(recursive=True)

    # Try to exit every 10 seconds.
    delayed_exit(10000)


Hook("gps_started").add(on_gps_started)
