"""Utility library used by other plugins"""
###########################################################################
# No user customization below this line
###########################################################################

import os
import os.path


def locate_exec_on_path(prog):
    """Utility function to locate an executable on path."""

    if os.name == "nt":
        pathext = os.getenv("PATHEXT")
        if pathext:
            extensions = str.split(pathext, os.pathsep)
        else:
            extensions = [".exe", ".cmd", ".bat"]
    else:
        extensions = [""]

    alldirs = str.split(os.getenv("PATH"), os.pathsep)
    for file in [os.path.join(dir, prog) for dir in alldirs]:
        for ext in extensions:
            if os.path.isfile(file + ext.lower()):
                return file + ext.lower()
    return ""


def locate_file(file, path=os.getenv("GPR_PROJECT_PATH")):
    """Utility function to locate a file on a path."""

    alldirs = str.split(path, os.pathsep)
    for item in [os.path.join(dir, file) for dir in alldirs]:
        if os.path.isfile(item):
            return item
    return ""


def display_name(filename):
    if os.name == "nt" and os.getenv("GNAT_CODE_PAGE") == "CP_ACP":
        return str(filename, "ISO-8859-1").encode("UTF-8")
    else:
        return filename
