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
from gs_utils import hook

# functions to initialize/finalize current per project plugin
# to be changed in per project plugin:
initialize_project_plugin = lambda: None
finalize_project_plugin = lambda: None
suffix = ".ide.py"


@hook("project_changed")
def __on_project_changed():
    global initialize_project_plugin
    global finalize_project_plugin
    # finalize previous plugin
    finalize_project_plugin()
    finalize_project_plugin = lambda: None

    plugin_name = []
    project = GPS.Project.root()
    while project is not None:
        plugin_name.append(project.file().name()[0:-4] + suffix)
        project = project.get_extended_project()

    for name in plugin_name:
        if os.path.exists(name):
            exec(compile(open(name, "rb").read(), name, "exec"), globals())
            initialize_project_plugin()
            initialize_project_plugin = lambda: None
            break
