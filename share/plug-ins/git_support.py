"""git support plug-in
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS
import os.path
from vcs import *

def from_git_root (filename):
    "Given a filename it returns the pathname relative to the Git root"
    dir=os.getcwd()
    full=""
    while not os.path.exists(dir + '/.git'):
        full = os.path.basename (dir) + "/" + full
        dir = os.path.dirname (dir)
    full = full + filename
    return full

#  Git VCS Menu

actions = [

 SEPARATOR,

 { ACTION: "Status", LABEL: "Query _status"  },
 { ACTION: "Commit", LABEL: "_Commit"  },
 { ACTION: "Commit (via revision log)",  LABEL: "_Commit (via revision log)"  },
 { ACTION: "Commit (from revision log)", LABEL: "Commit file"  },

 SEPARATOR,

 { ACTION: "History (as text)",          LABEL: "View _entire revision history (as text)" },
 { ACTION: "History",                    LABEL: "View _entire revision history" },
 { ACTION: "History for revision",       LABEL: "View specific revision _history"  },

 SEPARATOR,

 { ACTION: "Diff against head",          LABEL: "Compare against head revision"  },

 SEPARATOR,

 { ACTION: "Annotate",                LABEL: "Add annotations"  },
 { ACTION: "Remove Annotate",         LABEL: "Remove annotations"  },
 { ACTION: "Edit revision log",       LABEL: "Edit revision log"  },
 { ACTION: "Edit global ChangeLog",   LABEL: "Edit global ChangeLog"  },
 { ACTION: "Remove revision log",     LABEL: "Remove revision log"  },

 SEPARATOR,

 { ACTION: "Add no commit",             LABEL: "Add, no commit"  },

 { ACTION: "View revision", LABEL: "View revision"  },

 SEPARATOR,

 { ACTION: "Status dir",                   LABEL: "Directory/Query status for directory"  },
 { ACTION: "Status dir (recursively)",     LABEL: "Directory/Query status for directory recursively"  },
 { ACTION: "Update dir (recursively)",     LABEL: "Directory/Update directory recursively"  },

 { ACTION: "List project",                 LABEL: "Project/List all files in project"  },
 { ACTION: "Status project",               LABEL: "Project/Query status for project"  },
 { ACTION: "Update project",               LABEL: "Project/Update project"  },
 { ACTION: "List project (recursively)",   LABEL: "Project/List all files in project (recursively)"  },
 { ACTION: "Status project (recursively)", LABEL: "Project/Query status for project (recursively)"  },
 { ACTION: "Update project (recursively)", LABEL: "Project/Update project (recursively)"  },

]

register_vcs_actions ("Git", actions)
