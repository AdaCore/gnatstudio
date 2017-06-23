"""
Mercurial CVS support plugin

Provides support for the Mercurial configuration management system.

It integrates into GPS's VCS support, and uses the same menus
as all other VCS systems supported by GPS.
You can easily edit this file if you would like to customize
the mercurial commands that are sent for each of the menus.
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
import os
from vcs import register_vcs_actions, ACTION, LABEL, SEPARATOR


#  Mercurial VCS Menu
actions = [

    SEPARATOR,

    {ACTION: "Status",                       LABEL: "Query _status"},
    {ACTION: "Commit",                       LABEL: "_Commit"},
    {ACTION: "Commit (via revision log)",
     LABEL: "_Commit (via revision log)"},
    {ACTION: "Commit (from revision log)",   LABEL: "Commit file"},

    SEPARATOR,

    {ACTION: "History (as text)",
        LABEL: "View _entire revision history (as text)"},
    {ACTION: "History",
     LABEL: "View _entire revision history"},
    {ACTION: "History for revision",
     LABEL: "View specific revision _history"},

    SEPARATOR,

    {ACTION: "Diff against head",
     LABEL: "Compare against tip revision"},
    {ACTION: "Diff against revision",
        LABEL: "Compare against other revision"},

    SEPARATOR,

    {ACTION: "Annotate",                     LABEL: "Add annotations"},
    {ACTION: "Remove Annotate",              LABEL: "Remove annotations"},
    {ACTION: "Edit revision log",            LABEL: "Edit revision log"},
    {ACTION: "Edit global ChangeLog",        LABEL: "Edit global ChangeLog"},
    {ACTION: "Remove revision log",          LABEL: "Remove revision log"},

    SEPARATOR,

    {ACTION: "Add no commit",                LABEL: "Add, no commit"},

    SEPARATOR,

    {ACTION: "Status dir (recursively)",     LABEL:
        "Directory/Query status for directory recursively"},

    {ACTION: "List project",
     LABEL: "Project/List all files in project"},
    {ACTION: "Status project",
     LABEL: "Project/Query status for project"},
    {ACTION: "List project (recursively)",   LABEL:
        "Project/List all files in project (recursively)"},
    {ACTION: "Status project (recursively)", LABEL:
        "Project/Query status for project (recursively)"},

]

#  Mercurial VCS Menu registration

register_vcs_actions("Mercurial", actions)

#  Mercurial VCS plugin configuration

# XML data is stored at the same location as this file .pyxml as extension
with open(os.path.splitext(__file__)[0] + '.pyxml', 'rb') as fd:
        MERCURIAL_CONFIG = fd.read()
GPS.parse_xml(MERCURIAL_CONFIG)
