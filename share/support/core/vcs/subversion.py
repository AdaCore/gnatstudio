"""
Provides support for the Subversion configuration management system.

It integrates into GPS's VCS support, and uses the same menus
as all other VCS systems supported by GPS.
You can easily edit this file if you would like to customize
the svn commands that are sent for each of the menus.
"""


import GPS
import os
import string
from vcs import register_vcs_actions, ACTION, LABEL, SEPARATOR


actions = [

    SEPARATOR,

    {ACTION: "Status", LABEL: "Query _status"},
    {ACTION: "Update", LABEL: "_Update"},
    {ACTION: "Commit", LABEL: "_Commit"},
    {ACTION: "Commit (via revision log)",
     LABEL: "_Commit (via revision log)"},
    {ACTION: "Commit (from revision log)", LABEL: "Commit file"},

    SEPARATOR,

    {ACTION: "Open",   LABEL: "_Open"},
    {ACTION: "History (as text)",
        LABEL: "View _entire revision history (as text)"},
    {ACTION: "History",
     LABEL: "View _entire revision history"},
    {ACTION: "History for revision",
     LABEL: "View specific revision _history"},

    SEPARATOR,

    {ACTION: "Diff against head",
     LABEL: "Compare against head revision"},
    {ACTION: "Diff against base",
     LABEL: "Compare against base revision"},
    {ACTION: "Diff against revision",
     LABEL: "Compare against specific revision"},
    {ACTION: "Diff between two revisions", LABEL: "Compare two revisions"},
    {ACTION: "Diff base against head",     LABEL: "Compare base against head"},
    {ACTION: "Diff against tag",                LABEL: "Compare against tag"},
    {ACTION: "Diff against selected revision",
     LABEL: "Compare against selected revision"},

    SEPARATOR,

    {ACTION: "Annotate",                LABEL: "Add annotations"},
    {ACTION: "Remove Annotate",         LABEL: "Remove annotations"},
    {ACTION: "Edit revision log",       LABEL: "Edit revision log"},
    {ACTION: "Edit global ChangeLog",   LABEL: "Edit global ChangeLog"},
    {ACTION: "Remove revision log",     LABEL: "Remove revision log"},

    SEPARATOR,

    {ACTION: "Add",                       LABEL: "Add"},
    {ACTION: "Add (via revision log)",    LABEL: "Add (via revision log)"},
    {ACTION: "Add no commit",             LABEL: "Add, no commit"},
    {ACTION: "Remove",                    LABEL: "Remove"},
    {ACTION: "Remove (via revision log)", LABEL: "Remove (via revision log)"},
    {ACTION: "Remove no commit",          LABEL: "Remove, no commit"},
    {ACTION: "Revert",                    LABEL: "Revert"},
    {ACTION: "Resolved",                  LABEL: "Reso_lved"},

    SEPARATOR,

    {ACTION: "Create tag",    LABEL: "Create _tag..."},
    {ACTION: "Switch tag",    LABEL: "S_witch tag..."},
    {ACTION: "Merge",         LABEL: "Merge"},
    {ACTION: "View revision", LABEL: "View revision"},

    SEPARATOR,

    {ACTION: "Add directory, no commit",
     LABEL: "Directory/Add directory, no commit"},
    {ACTION: "Remove directory, no commit",
     LABEL: "Directory/Remove directory, no commit"},
    {ACTION: "Commit directory",
        LABEL: "Directory/Commit directory"},
    {ACTION: "Status dir",
     LABEL: "Directory/Query status for directory"},
    {ACTION: "Update dir",
        LABEL: "Directory/Update directory"},
    {ACTION: "Status dir (recursively)",     LABEL:
        "Directory/Query status for directory recursively"},
    {ACTION: "Update dir (recursively)",
        LABEL: "Directory/Update directory recursively"},

    {ACTION: "List project",
     LABEL: "Project/List all files in project"},
    {ACTION: "Status project",
     LABEL: "Project/Query status for project"},
    {ACTION: "Update project",               LABEL: "Project/Update project"},
    {ACTION: "List project (recursively)",   LABEL:
        "Project/List all files in project (recursively)"},
    {ACTION: "Status project (recursively)", LABEL:
        "Project/Query status for project (recursively)"},
    {ACTION: "Update project (recursively)",
        LABEL: "Project/Update project (recursively)"},

]

# Parse commit output to set corresponding file statuses. The commit output is:
#
#    Adding         proj.gpr
#    Sending        file1.adb
#    Sending        file3.adb
#    Transmitting file data ..
#    Committed revision 50.
#
# This output is parsed and transformed to a format compatible with the output
# of the status command:
#
#                 50   50   nobody   proj.gpr
#                 50   50   nobody   file1.adb
#                 50   50   nobody   file3.adb
#
# These data are sent to the status parser to update the file statuses. This
# way the status are up-to-date in GPS interface and we avoid a query status
# which is somewhat slow.
#


def status_from_commit(filename):
    file = open(filename)
    lines = file.readlines()
    file.close()

    files = []
    version = ""
    for l in lines:
        if len(l) > 18 and l[0:18] == 'Committed revision':
            version = string.split(l)[2].replace('.', '')
        elif len(l) > 7 and l[0:7] == 'Sending':
            files.append(string.split(l)[1])
        elif len(l) > 6 and l[0:6] == 'Adding':
            files.append(string.split(l)[1])

    status = ""
    for f in files:
        line = "          " + version + "    " + \
            version + " nobody " + f + "\n"
        status = status + line

    GPS.VCS.status_parse("Subversion", status, True, True, GPS.pwd())

# XML data is stored at the same location as this file .pyxml as extension
with open(os.path.splitext(__file__)[0] + '.pyxml', 'rb') as fd:
        XML = fd.read()

GPS.parse_xml(XML)
register_vcs_actions("Subversion", actions)
