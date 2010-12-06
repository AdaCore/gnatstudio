"""

"""



import GPS

from vcs import *

actions = [

 SEPARATOR,

 { ACTION: "Status", LABEL: "Describe"  },
 { ACTION: "Update", LABEL: "Update"  },
 { ACTION: "Commit", LABEL: "Check in"  },
 { ACTION: "Commit (via revision log)", LABEL: "Check in (via revision log)"  },

 SEPARATOR,

 { ACTION: "Open",   LABEL: "Check out" },
 { ACTION: "History (as text)",          LABEL: "View _entire revision history (as text)" },
 { ACTION: "History",                    LABEL: "View _entire revision history" },
 { ACTION: "History for revision",       LABEL: "View specific revision _history"  },

 SEPARATOR,

 { ACTION: "Diff against head",          LABEL: "Compare against head revision"  },
 { ACTION: "Diff against revision",      LABEL: "Compare against specific revision" },
 { ACTION: "Diff between two revisions", LABEL: "Compare two revisions" },
 { ACTION: "Diff base against head",     LABEL: "Compare base against head" },

 SEPARATOR,

 { ACTION: "Annotate",                LABEL: "Annotations"  },
 { ACTION: "Remove Annotate",         LABEL: "Remove annotations"  },
 { ACTION: "Edit revision log",       LABEL: "Edit revision log"  },
 { ACTION: "Edit global ChangeLog",   LABEL: "Edit global ChangeLog"  },
 { ACTION: "Remove revision log",     LABEL: "Remove revision log"  },

 SEPARATOR,

 { ACTION: "Add",               LABEL: "Add"  },
 { ACTION: "Add (via revision log)",               LABEL: "Add (via revision log)"  },
 { ACTION: "Add no commit",     LABEL: "Add, no commit"  },
 { ACTION: "Remove",            LABEL: "Remove"  },
 { ACTION: "Remove (via revision log)",            LABEL: "Remove (via revision log)"  },
 { ACTION: "Remove no commit",  LABEL: "Remove, no commit"  },
 { ACTION: "Revert",            LABEL: "Cancel checkout"  },

 SEPARATOR,

 { ACTION: "Add directory, no commit",     LABEL: "Directory/Add directory, no commit"  },
 { ACTION: "Remove directory, no commit",  LABEL: "Directory/Remove directory, no commit"  },
 { ACTION: "Commit directory",             LABEL: "Directory/Commit directory"  },
 { ACTION: "Status dir",                   LABEL: "Directory/Query status for directory"  },
 { ACTION: "Update dir",                   LABEL: "Directory/Update directory"  },
 { ACTION: "Status dir (recursively)",     LABEL: "Directory/Query status for directory recursively"  },
 { ACTION: "Update dir (recursively)",     LABEL: "Directory/Update directory recursively"  },

 { ACTION: "List project",                 LABEL: "Project/List files in project"  },
 { ACTION: "Status project",               LABEL: "Project/Query status for project"  },
 { ACTION: "Update project",               LABEL: "Project/Update project"  },
 { ACTION: "List project (recursively)",   LABEL: "Project/List files in project (recursively)"  },
 { ACTION: "Status project (recursively)", LABEL: "Project/Query status for project (recursively)"  },
 { ACTION: "Update project (recursively)", LABEL: "Project/Update project (recursively)"  },

]

register_vcs_actions ("ClearCase Native", actions)
