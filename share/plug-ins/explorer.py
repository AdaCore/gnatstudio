"""Contextual menu for Explorer

This script provides a contextual menu for
file or directory to open
explorer in this directory.

"""

import os
import GPS
import os_utils
from gs_utils import interactive
import subprocess


def can_execute(context):
    """A filter to only allow this action for project or files view"""
    return (
            context.file() is not None or context.directory() is not None
    ) and context.module_name in ["Files_View", "Project_Explorer_Project"]


@interactive(
    name="Locate in Explorer",
    contextual="Open explorer",
    filter=can_execute
)
def on_activate():
    gps = os.environ.copy()
    sys = os.environ.copy()
    for k in gps.keys():
        if k.startswith("GPS_STARTUP_"):
            if gps[k] != "_ABSENT_VARIABLE_":
                sys[k[12:]] = gps[k]

    path = GPS.current_context().directory()

    if path is None:
        if GPS.current_context().file() is not None:
            path = GPS.current_context().file().path
        else:
            path = '.'

    if os.name == "nt":
        e = 'explorer.exe'
        path = '"' + path + '"'
    else:
        e = 'xdg-open'

    try:
        subprocess.call([e, path], env=sys)
    except Exception as ex:
        GPS.Console().write(print (ex))
