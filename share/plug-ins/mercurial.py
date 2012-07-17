"""Mercurial CVS support plug-in

   Provides support for the Mercurial configuration management system.

   It integrates into GPS's VCS support, and uses the same menus
   as all other VCS systems supported by GPS.
   You can easily edit this file if you would like to customize
   the mercurial commands that are sent for each of the menus.
"""

###########################################################################
## No user customization below this line
###########################################################################

import GPS
import os.path
from vcs import *

#  Mercurial VCS Menu

actions = [

 SEPARATOR,

 { ACTION: "Status",                       LABEL: "Query _status"  },
 { ACTION: "Commit",                       LABEL: "_Commit"  },
 { ACTION: "Commit (via revision log)",    LABEL: "_Commit (via revision log)"  },
 { ACTION: "Commit (from revision log)",   LABEL: "Commit file"  },

 SEPARATOR,

 { ACTION: "History (as text)",            LABEL: "View _entire revision history (as text)" },
 { ACTION: "History",                      LABEL: "View _entire revision history" },
 { ACTION: "History for revision",         LABEL: "View specific revision _history"  },

 SEPARATOR,

 { ACTION: "Diff against head",            LABEL: "Compare against tip revision"  },
{  ACTION: "Diff against revision",        LABEL: "Compare against other revision" },

 SEPARATOR,

 { ACTION: "Annotate",                     LABEL: "Add annotations"  },
 { ACTION: "Remove Annotate",              LABEL: "Remove annotations"  },
 { ACTION: "Edit revision log",            LABEL: "Edit revision log"  },
 { ACTION: "Edit global ChangeLog",        LABEL: "Edit global ChangeLog"  },
 { ACTION: "Remove revision log",          LABEL: "Remove revision log"  },

 SEPARATOR,

 { ACTION: "Add no commit",                LABEL: "Add, no commit"  },

 SEPARATOR,

 { ACTION: "Status dir (recursively)",     LABEL: "Directory/Query status for directory recursively"  },

 { ACTION: "List project",                 LABEL: "Project/List all files in project"  },
 { ACTION: "Status project",               LABEL: "Project/Query status for project"  },
 { ACTION: "List project (recursively)",   LABEL: "Project/List all files in project (recursively)"  },
 { ACTION: "Status project (recursively)", LABEL: "Project/Query status for project (recursively)"  },

]

#  Mercurial VCS Menu registration

register_vcs_actions ("Mercurial", actions)

#  Mercurial VCS plugin configuration

MERCURIAL_CONFIG = u'''<?xml version="1.0"?>

<GPS>

   <!-- Mercurial status -->

   <action name="generic_hg_local_status" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <external>hg --noninteractive status --all $2-</external>
      <shell>VCS.status_parse "Mercurial" "%1" FALSE FALSE "%2"</shell>
   </action>

   <action name="generic_hg_status" show-command="false" output="none" category="">
      <shell output="">echo "Querying status for $2-"</shell>
      <shell>pwd</shell>
      <external>hg --noninteractive status --all $2-</external>
      <shell>VCS.status_parse "Mercurial" "%1" FALSE FALSE "%2"</shell>
   </action>

   <action name="generic_hg_status_dir" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <shell output="">echo "Querying status for %1"</shell>
      <shell>pwd</shell>
      <external>hg --noninteractive status --all %1</external>
      <shell>VCS.status_parse "Mercurial" "%1" FALSE FALSE "%2"</shell>
   </action>

   <action name="generic_hg_status_dir_recursive" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <shell output="">echo "Querying status for %1"</shell>
      <shell>pwd</shell>
      <external>hg --noninteractive status --all</external>
      <shell>VCS.status_parse "Mercurial" "%1" FALSE FALSE "%2"</shell>
   </action>

   <action name="generic_hg_diff_head" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for $1 ..."</shell>
      <external>hg cat -r tip "$1"</external>
      <shell>dump "%1" FALSE</shell>
      <external>gnudiff %1 "$1"</external>
      <on-failure>
         <shell>base_name "$1"</shell>
         <shell>dump "%2" TRUE</shell>
         <shell>File "%1"</shell>
         <shell>File "$1"</shell>
         <shell>Hook "diff_action_hook"</shell>
         <shell>Hook.run %1 "$1" null %2 %3 "%5 [tip]"</shell>
         <shell>delete "%5"</shell>
      </on-failure>
   </action>

   <action name="generic_hg_diff" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for revision $1 of $2 ..."</shell>
      <external>hg cat -r $1 "$2"</external>
      <shell>dump "%1" FALSE</shell>
      <external>gnudiff %1 "$2"</external>
      <on-failure>
         <shell>base_name "$2"</shell>
         <shell>dump "%2" TRUE</shell>
         <shell>File "%1"</shell>
         <shell>File "$2"</shell>
         <shell>Hook "diff_action_hook"</shell>
         <shell>Hook.run %1 "$2" null %2 %3 "%5 [$1]"</shell>
         <shell>delete "%5"</shell>
      </on-failure>
   </action>

   <!-- Mercurial add (no commit) -->

   <action name="generic_hg_add_no_commit" show-command="false" output="none" category="">
      <shell output="">echo "Adding file(s) $2-"</shell>
      <external>hg --noninteractive add "$2-"</external>
      <on-failure>
         <shell output="">echo "Mercurial error:"</shell>
         <shell output="">echo "%2"</shell>
      </on-failure>
   </action>

   <!-- Mercurial commit -->

   <action name="generic_hg_commit" show-command="false" output="none" category="">
      <shell output="">echo "Committing file(s) $2-"</shell>
      <shell>dump "$1 " TRUE</shell>
      <external>hg --noninteractive commit -l "%1" $2-</external>
      <on-failure>
         <shell output="">echo "Mercurial error:"</shell>
         <shell output="">echo "%2"</shell>
      </on-failure>
      <shell>delete "%2"</shell>
      <shell>Hook "file_changed_on_disk"</shell>
      <shell>Hook.run %1 null</shell>
   </action>

   <!-- Mercurial annotate -->

   <action name="generic_hg_annotate" show-command="false" output="none" category="">
      <shell output="">echo "Querying annotations for $1"</shell>
      <external>hg --noninteractive annotate -udnl "$1"</external>
      <shell>VCS.annotations_parse "Mercurial" "$1" "%1"</shell>
   </action>

   <!-- Mercurial history -->

   <action name="generic_hg_history" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $1"</shell>
      <external>hg --noninteractive log --template "[{rev};{author};{date|isodate};{desc}\\n;]\\n" "$1"</external>
      <shell>VCS.log_parse "Mercurial" "$1" "%1"</shell>
   </action>

   <action name="generic_hg_history_text" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $1"</shell>
      <external>hg --noninteractive log "$1"</external>
      <shell>base_name "$1"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>Editor.edit "%1"</shell>
      <shell>Editor.set_title "%2" "Log for %3"</shell>
      <shell>Editor.set_writable "%3" FALSE</shell>
      <shell>MDI.split_vertically TRUE</shell>
      <shell>delete "%5"</shell>
   </action>

   <action name="generic_hg_history_rev" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $2"</shell>
      <external>hg --noninteractive log --rev $1 --template "[{rev};{author};{date|isodate};{desc}\\n;]\\n" "$2"</external>
      <shell>VCS.log_parse "Mercurial" "$2" "%1"</shell>
   </action>

   <!-- Mercurial revision -->

   <action name="generic_hg_revision" show-command="false" output="none" category="">
      <shell output="">echo "Getting $2 at revision $1"</shell>
      <external>hg --noninteractive cat --rev "$2" "$1"</external>
      <shell>base_name "$2"</shell>
      <shell>dump "%2" FALSE</shell>
      <shell>Editor.edit "%1"</shell>
      <shell>Editor.set_title "%2" "%3 [$1]" "%3 [$1]"</shell>
      <shell>Editor.set_writable "%3" FALSE</shell>
      <shell>delete "%4"</shell>
   </action>

   <!-- Mercurial -->

   <vcs name="Mercurial"
        path_style="System_Default"
        group_queries_by_directory="TRUE"
        absolute_names="FALSE" atomic_commands="TRUE"
        commit_directory="FALSE"
        administrative_directory=".hg">
      <status_files         action="generic_hg_status"/>
      <status_dir           action="generic_hg_status_dir"/>
      <status_dir_recursive action="generic_hg_status_dir_recursive"/>
      <local_status_files   action="generic_hg_local_status"/>
      <diff_head            action="generic_hg_diff_head"/>
      <diff                 action="generic_hg_diff"/>
      <commit               action="generic_hg_commit"/>
      <add_no_commit        action="generic_hg_add_no_commit"/>
      <annotate             action="generic_hg_annotate"/>
      <history              action="generic_hg_history"/>
      <history_text         action="generic_hg_history_text"/>
      <history_revision     action="generic_hg_history_rev"/>
      <revision             action="generic_hg_revision"/>

      <status label="Up to date" stock="gps-vcs-up-to-date" />
      <status label="Locally modified" stock="gps-vcs-modified" />
      <status label="Property modified" stock="gps-vcs-modified" />
      <status label="Needs merge" stock="gps-vcs-needs-merge" />
      <status label="Needs update" stock="gps-vcs-needs-update" />
      <status label="Contains merge conflicts" stock="gps-vcs-has-conflicts" />
      <status label="Removed" stock="gps-vcs-removed" />
      <status label="Added" stock="gps-vcs-added" />

      <status_parser>
         <regexp>(^|\\n)(.)( |\\t)([^\\\\/\\n]*[/|\\\\])*([^\\\\ \\n]+)</regexp>

         <status_index>2</status_index>
         <file_index>5</file_index>

         <status_matcher label="Added">A</status_matcher>
         <status_matcher label="Locally modified">M</status_matcher>
         <status_matcher label="Up to date">C</status_matcher>
         <status_matcher label="Removed">R</status_matcher>
      </status_parser>

      <local_status_parser>
         <regexp>(^|\\n)(.)( |\\t)([^/\\n]+/)*([^ \\n]+)</regexp>

         <status_index>2</status_index>
         <file_index>5</file_index>

         <status_matcher label="Added">A</status_matcher>
         <status_matcher label="Locally modified">C</status_matcher>
         <status_matcher label="Up to date">H</status_matcher>
      </local_status_parser>

      <annotations_parser>
         <regexp>[ \\t]*([^ \\t\\n]+)[ \\t]+([0-9]+) (... ... .. ..:..:.. ....) ([\+\-]....):( *)([0-9]+):(.*)(\\n|$)</regexp>

         <repository_revision_index>2</repository_revision_index>
         <author_index>1</author_index>
         <date_index>3</date_index>
         <file_index>7</file_index>
         <tooltip_pattern>Revision \\2 on \\3 \\4, Author \\1</tooltip_pattern>
      </annotations_parser>

      <log_parser>
         <regexp>(^|\\n)\[([^;\\n]+);([^;\\n]*);([^;\\n]*);([^;]*);\]($|\\n)</regexp>

         <repository_revision_index>2</repository_revision_index>
         <author_index>3</author_index>
         <date_index>4</date_index>
         <log_index>5</log_index>
      </log_parser>

   </vcs>
</GPS>
'''

GPS.parse_xml(MERCURIAL_CONFIG)
