"""
This file provides a 'Restart build all' action which interrupts
the current build, if any, and starts a new build via the 
'Build all' action.
"""

from GPS import Action, Task, Console
from gs_utils import interactive


@interactive("Build", name="Restart build all")
def on_activate():
    for task in Task.list():
        if "Build" in task.name():
            task.interrupt()
    Action("Build All").execute_if_possible()
