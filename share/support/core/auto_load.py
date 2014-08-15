"""
This plugin load <project>.ide.py when <project>.gpr is open.
User provides initialize_project_plugin and finalize_project_plugin functions
to initialize/finalize per project plugin properly.

Trivial example of <project>.ide.py:

import GPS

def initialize_project_plugin():
    GPS.MDI.dialog ("initialize_project_plugin")

def finalize_project_plugin():
    GPS.MDI.dialog ("finalize_project_plugin")

"""

import GPS
import os.path

# functions to initialize/finalize current per project plugin
# to be changed in per project plugin:
initialize_project_plugin = (lambda: None)
finalize_project_plugin = (lambda: None)


def on_project_changed(hook):
    global initialize_project_plugin
    global finalize_project_plugin
    # finalize previous plugin
    finalize_project_plugin()
    finalize_project_plugin = (lambda: None)

    project = GPS.Project.root().file()
    plugin_name = project.name()[0:-4] + ".ide.py"
    if os.path.exists(plugin_name):
        execfile(plugin_name, globals())
        initialize_project_plugin()
        initialize_project_plugin = (lambda: None)

GPS.Hook("project_changed").add(on_project_changed)
