"""
Provides support for the CVS configuration management system.

This file integrates into GPS's VCS support, and uses the same
menus as all other VCS systems supported by GPS.
You can easily edit this file to configurat the CVS commands that
are executed by each of the menus
"""


import GPS
import os
from vcs import register_vcs_actions, ACTION, LABEL, SEPARATOR


actions = [
    SEPARATOR,

    {ACTION: "Status", LABEL: "Query _status"},
    {ACTION: "Update", LABEL: "_Update"},
    {ACTION: "Commit", LABEL: "_Commit"},
    {ACTION: "Commit (via revision log)", LABEL: "_Commit (via revision log)"},
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
    {ACTION: "Diff against revision",
     LABEL: "Compare against other revision"},
    {ACTION: "Diff between two revisions",
        LABEL: "Compare two revisions"},
    {ACTION: "Diff base against head",
     LABEL: "Compare base against head"},
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

# XML data is stored at the same location as this file .pyxml as extension
with open(os.path.splitext(__file__)[0] + '.pyxml', 'rb') as fd:
    XML = fd.read()

GPS.parse_xml(XML)
register_vcs_actions("CVS", actions)
