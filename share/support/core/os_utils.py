"""Utility library used by other plugins"""
###########################################################################
# No user customization below this line
###########################################################################

import os
import os.path
import string


def locate_exec_on_path(prog):
    """Utility function to locate an executable on path."""

    if os.name == 'nt':
        pathext = os.getenv('PATHEXT')
        if pathext:
            extensions = string.split(pathext, os.pathsep)
        else:
            extensions = [".exe", ".cmd", ".bat"]
    else:
        extensions = [""]

    alldirs = string.split(os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir, prog) for dir in alldirs]:
        for ext in extensions:
            if os.path.isfile(file + ext):
                return file
    return ""


def display_name(filename):
    if os.name == 'nt' and os.getenv("GNAT_CODE_PAGE") == "CP_ACP":
        return unicode(filename, "ISO-8859-1").encode("UTF-8")
    else:
        return filename
