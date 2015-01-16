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
    GPS.Contextual("edit project properties").hide()
    GPS.Contextual("Project dependencies").hide()
    GPS.Contextual("Add scenario variable").hide()
    GPS.Contextual("Save project").hide()

    # Remove the global menus
    GPS.Menu.get("/Project/Properties...").hide()
    GPS.Menu.get("/Project/Edit file switches...").hide()
    GPS.Menu.get("/Project/Save All").hide()
    GPS.Menu.get("/Project/New...").hide()
    GPS.Menu.get("/Project/New from template...").hide()

GPS.Hook("gps_started").add(on_gps_started)
