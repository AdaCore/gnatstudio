"""This plug-in removes the menus that allow graphical edition
of project files.

This is useful when working with project files that have been
formatted manually, and graphical edition should be avoided,
so as to preserve the formatting.
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS


def on_gps_started(hook):
    GPS.Action('open Project Properties').disable()
    GPS.Action('open Switches editor').disable()

    # Remove the contextual menus that may cause modifications in the
    # projects
    GPS.Contextual("Edit project properties").hide()
    GPS.Contextual("Project dependencies").hide()
    GPS.Contextual("Add scenario variable").hide()
    GPS.Contextual("Save project").hide()


GPS.Hook("gps_started").add(on_gps_started)
