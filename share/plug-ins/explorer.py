"""Contextual menu: 'Open in system file explorer'

This plugin adds a contextual menu on files and directories in
the Project View and File View, to reveal the object in the system
file explorer.

"""

import os
import sys
import GPS
from gs_utils import interactive
import subprocess


def can_execute(context):
    """A filter to only allow this action for project or files view"""
    return (
            context.file() is not None or context.directory() is not None
    ) and context.module_name in ["Files_View", "Project_Explorer_Project"]


@interactive(
    name="Open in system file explorer",
    contextual="Open explorer",
    filter=can_execute
)
def on_activate():
    gps = os.environ.copy()
    restored = os.environ.copy()
    for k in gps.keys():
        if k.startswith("GPS_STARTUP_"):
            if gps[k] != "_ABSENT_VARIABLE_":
                restored[k[12:]] = gps[k]

    path = GPS.current_context().directory()

    if path is None:
        if GPS.current_context().file() is not None:
            path = GPS.current_context().file().path
        else:
            path = '.'

    if sys.platform == "win32":
        e = 'explorer.exe'
        path = '"' + path + '"'
    else:
        e = 'xdg-open'

    try:
        subprocess.run([e, path], env=restored)
    except Exception as ex:
        GPS.Console().write(str(ex))
