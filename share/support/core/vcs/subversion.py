"""
Provides support for the Subversion configuration management system.

It integrates into GPS's VCS support, and uses the same menus
as all other VCS systems supported by GPS.
You can easily edit this file if you would like to customize
the svn commands that are sent for each of the menus.
"""


import GPS
import string

from vcs import *

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


XML = r"""<?xml version="1.0"?>
<GPS>
   <!-- SVN status -->

   <action name="generic_svn_status" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <shell output="">echo "Querying status for files in %1"</shell>
      <external check-password="true">svn --non-interactive status -N -u -v $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.status_parse "Subversion" "%1" "$1" FALSE "%3"</shell>
   </action>

   <!-- SVN status dir -->

   <action name="generic_svn_status_dir" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <shell output="">echo "Querying status for %1"</shell>
      <external check-password="true">svn --non-interactive status --non-recursive -u -v .</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.status_parse "Subversion" "%1" "$1" FALSE "%3"</shell>
   </action>

   <!-- SVN status dir recursive -->

   <action name="generic_svn_status_dir_recursive" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <shell output="">echo "Querying status for %1"</shell>
      <external check-password="true">svn --non-interactive status -u -v .</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.status_parse "Subversion" "%1" "$1" FALSE "%3"</shell>
   </action>

   <!-- SVN tag/branch -->

   <action name="generic_svn_create_tag" show-command="false" output="" category="">
      <shell output="none">VCS.repository_dir /tags/$2</shell>
      <shell output="none">VCS.repository_dir /tags/$2/$1</shell>
      <shell output="">echo "Create tag on %1"</shell>
      <external check-password="true" output="none">svn mkdir -m "New tag directory" "%3"</external>
      <external check-password="true" output="none">svn copy -m "New tag" "../$1" "%3"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <action name="generic_svn_create_branch" show-command="false" output="" category="">
      <shell output="none">VCS.repository_dir /branches/$2</shell>
      <shell output="none">VCS.repository_dir /branches/$2/$1</shell>
      <shell>echo "Create branch on %1"</shell>
      <external check-password="true" output="none">svn mkdir -m "New branch directory" "%3"</external>
      <external check-password="true" output="none">svn copy -m "New branch" "../$1" "%3"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <!-- SVN switch -->

   <action name="generic_svn_switch" show-command="false" output="" category="">
      <shell output="none">VCS.repository_dir $2</shell>
      <shell output="">echo "Switch to %1"</shell>
      <external check-password="true" output="none">svn switch %2</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <!-- SVN annotate -->

   <action name="generic_svn_annotate" show-command="false" output="" category="">
      <shell>echo "Querying annotations for $1"</shell>
      <external check-password="true" output="none">svn --non-interactive annotate -v "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell output="none">VCS.annotations_parse "Subversion" "$1" "%1"</shell>
   </action>

   <!-- SVN local status -->

   <action name="generic_svn_local_status" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <external check-password="true">svn --non-interactive status --non-recursive -v .</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.status_parse "Subversion" "%1" FALSE TRUE "%2"</shell>
   </action>

   <!-- SVN commit -->

   <action name="generic_svn_commit" show-command="false" output="none" category="">
      <shell output="">echo "Committing file(s) $2-"</shell>
      <shell>dump "$1" TRUE</shell>
      <external check-password="true">svn --non-interactive commit --non-recursive -F "%1" $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>delete "%2"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell lang="python" output="">import vcs.subversion; vcs.subversion.status_from_commit("%1")</shell>
      <shell>delete "%2"</shell>
      <shell>Hook "file_changed_on_disk"</shell>
      <shell>Hook.run %1 null</shell>
   </action>

   <!-- SVN add -->

   <action name="generic_svn_add" show-command="false" output="none" category="">
      <shell output="">echo "adding file(s) $2-"</shell>
      <external check-password="true">svn add $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>dump "$1" TRUE</shell>
      <external check-password="true">svn commit -F "%1" $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>delete "%2"</shell>
   </action>

   <!-- SVN add (no commit) -->

   <action name="generic_svn_add_no_commit" show-command="false" output="none" category="">
      <shell output="">echo "adding (no commit) file(s) $2-"</shell>
      <external check-password="true">svn add --non-recursive $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <!-- SVN remove -->

   <action name="generic_svn_remove" show-command="false" output="none" category="">
      <shell output="">echo "removing file(s) $2-"</shell>
      <external check-password="true">svn --non-interactive remove $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>dump "$1" TRUE</shell>
      <external check-password="true">svn commit -F "%1" $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>delete "%2"</shell>
   </action>

   <!-- SVN remove (no commit) -->

   <action name="generic_svn_remove_no_commit" show-command="false" output="none" category="">
      <shell output="">echo "removing file(s) $2-"</shell>
      <external check-password="true">svn --non-interactive remove $2-</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <!-- SVN history -->

   <action name="generic_svn_history" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $1"</shell>
      <external check-password="true">svn --non-interactive -v log --revision HEAD:0 "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.revision_parse "Subversion" "$1" "%1"</shell>
      <shell>VCS.log_parse "Subversion" "$1" "%2"</shell>
   </action>

   <action name="generic_svn_history_text" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $1"</shell>
      <external check-password="true">svn --non-interactive log --revision HEAD:0 "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$1"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>Editor.edit "%1"</shell>
      <shell>Editor.set_title "%2" "Log for %3"</shell>
      <shell>Editor.set_writable "%3" FALSE</shell>
      <shell>MDI.split_vertically TRUE</shell>
      <shell>delete "%5"</shell>
   </action>

   <action name="generic_svn_history_rev" show-command="false" output="none" category="">
      <shell output="">echo "Querying history for $2"</shell>
      <external check-password="true">svn --non-interactive log -r$1 "$2"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>VCS.revision_parse "Subversion" "$2" "%1"</shell>
      <shell>VCS.log_parse "Subversion" "$2" "%2"</shell>
   </action>

   <!-- SVN update -->

   <action name="generic_svn_update" show-command="false" output="none" category="">
      <shell>pwd</shell>
      <external check-password="true" output="">svn --non-interactive update $*</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>Hook "file_changed_on_disk"</shell>
      <shell>Hook.run %1 null</shell>
      <shell>VCS.update_parse "Subversion" "%3" "%4"</shell>
   </action>

   <!-- SVN merge -->

   <action name="generic_svn_merge" show-command="false" output="none" category="">
      <shell>VCS.repository_path "$2"</shell>
      <shell>VCS.repository_path "$2" "$1"</shell>
      <external check-password="true" output="">svn --non-interactive merge "%2" "%1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>Hook "file_changed_on_disk"</shell>
      <shell>Hook.run %1 null</shell>
   </action>

   <!-- SVN resolved -->

   <action name="generic_svn_resolved" show-command="false" output="none" category="">
      <external check-password="true" output="">svn resolved $*</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
   </action>

   <!-- SVN diff -->

   <action name="generic_svn_diff_patch" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for $2 ..."</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x -c "$2"</external>
      <shell>dump_file "%1" "$1" FALSE</shell>
   </action>

   <action name="generic_svn_diff_head" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for $1 ..."</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal -rHEAD "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$1"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>File "$1"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$1" null %2 %3 "%5 [HEAD]"</shell>
      <shell>delete "%5"</shell>
   </action>

   <action name="generic_svn_diff_working" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for $1 ..."</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$1"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>File "$1"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$1" null %2 %3 "%5 [HEAD]"</shell>
      <shell>delete "%5"</shell>
   </action>

   <action name="generic_svn_diff" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for revision $1 of $2 ..."</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal -r $1 "$2"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$2"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>File "$2"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$2" null %2 %3 "%5 [r$1]"</shell>
      <shell>delete "%5"</shell>
   </action>

   <action name="generic_svn_diff2" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison between revisions $1:$2 of $3 ..."</shell>
      <external check-password="true">svn --non-interactive cat -r$2 "$3"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$3"</shell>
      <shell>dump_file "%2" "%1.[r$2]" FALSE</shell>
      <shell>File "%1"</shell>
      <shell>Editor.edit "%2"</shell>
      <shell>Editor.set_title %3 "%4 [r$2]" "%4 [r$2]"</shell>
      <shell>Editor.set_writable %4 FALSE</shell>
      <shell>VCS.set_reference %5 "$3"</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal -r$1:$2 "$3"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>dump "%1" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$3" null %9 %2 "%11 [r$1]"</shell>
      <shell>delete "%4"</shell>
      <shell>delete "%12"</shell>
   </action>

   <action name="generic_svn_diff_base_head" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison between revisions BASE:HEAD of $1 ..."</shell>
      <external check-password="true">svn --non-interactive cat -rHEAD "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$1"</shell>
      <shell>dump_file "%2" "%1.[rHEAD]" FALSE</shell>
      <shell>File "%1"</shell>
      <shell>Editor.edit "%2"</shell>
      <shell>Editor.set_title %3 "%4 [rHEAD]" "%4 [rHEAD]"</shell>
      <shell>VCS.set_reference %4 "$1"</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal -rBASE:HEAD "$1"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>dump "%1" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$1" null %8 %2 "%10 [rBASE]"</shell>
      <shell>delete "%4"</shell>
      <shell>delete "%11"</shell>
      <shell>Editor.set_writable %12 FALSE</shell>
   </action>

   <action name="generic_svn_diff_tag" show-command="false" output="none" category="">
      <shell output="">echo "Getting comparison for revision $1 of $2 ..."</shell>
      <shell output="">VCS.repository_path "$2"</shell>
      <shell output="">VCS.repository_path "$2" "$1"</shell>
      <external check-password="true">svn --non-interactive diff --diff-cmd diff -x --normal "%2" "%1"</external>

      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$2"</shell>
      <shell>dump "%2" TRUE</shell>
      <shell>File "%1"</shell>
      <shell>File "$2"</shell>
      <shell>Hook "diff_action_hook"</shell>
      <shell>Hook.run %1 "$2" null %2 %3 "%5 [$1]"</shell>
      <shell>delete "%5"</shell>
   </action>

   <!-- SVN revert -->

   <action name="generic_svn_revert" show-command="false" output="none" category="">
      <external check-password="true" output="">svn revert $*</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>Hook "file_changed_on_disk"</shell>
      <shell>Hook.run %1 null</shell>
   </action>

   <!-- SVN revision -->

   <action name="generic_svn_revision" show-command="false" output="none" category="">
      <shell output="">echo "Getting $2 at revision $1"</shell>
      <external check-password="true" output="none">svn cat -r "$1" "$2"</external>
      <on-failure>
         <shell output="">echo_error "SVN error:"</shell>
         <shell output="">echo_error "%2"</shell>
         <shell lang="python" output="">ignored=[m.remove() for m in GPS.Message.list(category="Subversion errors")]</shell>
         <shell output="">Locations.parse "%4" "Subversion errors"</shell>
      </on-failure>
      <shell>base_name "$2"</shell>
      <shell>dump "%2"</shell>
      <shell>Editor.edit "%1"</shell>
      <shell>Editor.set_title "%2" "%3 [r$1]" "%3 [r$1]"</shell>
      <shell>Editor.set_writable "%3" FALSE</shell>
      <shell>delete "%4"</shell>
   </action>

   <!-- SVN -->

   <vcs name="Subversion"
        path_style="System_Default"
        group_queries_by_directory="TRUE"
        absolute_names="TRUE" atomic_commands="TRUE"
        commit_directory="TRUE"
        prev_revision="PREV"
	head_revision="HEAD"
	administrative_directory=".svn">
      <status_files       action="generic_svn_status"           label="Query status"/>
      <status_dir         action="generic_svn_status_dir"       label="Query status for directory"/>
      <status_dir_recursive
                action="generic_svn_status_dir_recursive"
                label="Query status for directory recursively"/>
      <local_status_files action="generic_svn_local_status"     label="Local status"/>
      <create_tag         action="generic_svn_create_tag"       label="Create tag"/>
      <create_branch      action="generic_svn_create_branch"    label="Create branch"/>
      <switch             action="generic_svn_switch"           label="Switch tag/branch"/>
      <update             action="generic_svn_update"           label="Update"/>
      <merge              action="generic_svn_merge"            label="Merge"/>
      <resolved           action="generic_svn_resolved"         label="Resolved"/>
      <commit             action="generic_svn_commit"           label="Commit"/>
      <history            action="generic_svn_history"          label="View entire revision history"/>
      <history_text       action="generic_svn_history_text"     label="View entire revision history (as text)"/>
      <history_revision   action="generic_svn_history_rev"      label="View specific revision history"/>
      <annotate           action="generic_svn_annotate"         label="Annotations"/>
      <add                action="generic_svn_add"              label="Add"/>
      <add_no_commit      action="generic_svn_add_no_commit"    label="Add, no commit"/>
      <remove             action="generic_svn_remove"           label="Remove"/>
      <remove_no_commit   action="generic_svn_remove_no_commit" label="Remove, no commit"/>
      <revert             action="generic_svn_revert"           label="Revert"/>
      <revision           action="generic_svn_revision"         label="View revision"/>
      <diff_patch         action="generic_svn_diff_patch"       label="Compare against head revision for building a patch file"/>
      <diff_head          action="generic_svn_diff_head"        label="Compare against head revision"/>
      <diff_working       action="generic_svn_diff_working"     label="Compare against working revision"/>
      <diff_base_head     action="generic_svn_diff_base_head"   label="Compare base against head"/>
      <diff               action="generic_svn_diff"             label="Compare against other revision"/>
      <diff_tag           action="generic_svn_diff_tag"         label="Compare against a tag/branch"/>
      <diff2              action="generic_svn_diff2"            label="Compare two revisions"/>

      <status label="Up to date" stock="vcs-up-to-date" />
      <status label="Locally modified" stock="vcs-modified" />
      <status label="Property modified" stock="vcs-modified" />
      <status label="Needs merge" stock="vcs-needs-merge" />
      <status label="Needs update" stock="vcs-needs-update" />
      <status label="Contains merge conflicts" stock="vcs-has-conflicts" />
      <status label="Removed" stock="vcs-removed" />
      <status label="Added" stock="vcs-added" />

      <status_parser>
         <regexp>(^|\n)([^\?S]........)\s+([0-9-]+)\s+([0-9?]+)\s+([^\s]+)\s+([^\n]+)</regexp>

         <file_index>6</file_index>
         <status_index>2</status_index>
         <local_revision_index>3</local_revision_index>

         <status_matcher label="Up to date">(  .....[^\*][^\*])</status_matcher>
         <status_matcher label="Locally modified">(M......  )</status_matcher>
         <status_matcher label="Property modified">( M.......)</status_matcher>
         <status_matcher label="Needs update">([^M]......)(\*.|.\*)</status_matcher>
         <status_matcher label="Added">A........</status_matcher>
         <status_matcher label="Removed">D........</status_matcher>
         <status_matcher label="Needs merge">(M......)(\*.|.\*)</status_matcher>
         <status_matcher label="Contains merge conflicts">C........</status_matcher>
      </status_parser>

      <local_status_parser>
         <regexp>(^|\n)([^\?S]........)\s+([0-9]+)\s+([0-9?]+)\s+([^\s]+)\s+([^\n]+)</regexp>

         <file_index>6</file_index>
         <status_index>2</status_index>
         <local_revision_index>3</local_revision_index>

         <status_matcher label="Up to date">(  .....[^\*][^\*])</status_matcher>
         <status_matcher label="Locally modified">(M......  )</status_matcher>
         <status_matcher label="Property modified">( M.......)</status_matcher>
         <status_matcher label="Needs update">([^M]......)(\*.|.\*)</status_matcher>
         <status_matcher label="Added">A........</status_matcher>
         <status_matcher label="Removed">D........</status_matcher>
         <status_matcher label="Needs merge">(M......)(\*.|.\*)</status_matcher>
         <status_matcher label="Contains merge conflicts">C........</status_matcher>
      </local_status_parser>

      <update_parser>
         <regexp>(^|\n)([ADUCG])\s+([^\n]+)</regexp>

         <file_index>3</file_index>
         <status_index>2</status_index>

         <status_matcher label="Up to date">U</status_matcher>
         <status_matcher label="Locally modified">G</status_matcher>
         <status_matcher label="Added">A</status_matcher>
         <status_matcher label="Removed">D</status_matcher>
         <status_matcher label="Contains merge conflicts">C</status_matcher>
      </update_parser>

      <annotations_parser>
         <regexp>(\d[^\s]*)\s*([^ ]+)\s*([^ ]+)\s+([^ ]+)[^\(]+\(([^\)]+)\) (.*)(\n|$)</regexp>

         <repository_revision_index>1</repository_revision_index>
	 <author_index>2</author_index>
	 <date_index>3</date_index>
         <file_index>6</file_index>
	 <tooltip_pattern>Revision \1 on \5 \4, Author \2</tooltip_pattern>
      </annotations_parser>

      <log_parser>
         <regexp>----------\nr([^ ]+) \| ([^\|]+)\| (....-..-.. ..:..:..)[^\n]+\n((\n|[^-\n]+\n|.?.?.?.?.?[^-][^-][^-][^-][^-][^\n]*\n)*)</regexp>

         <repository_revision_index>1</repository_revision_index>
	 <author_index>2</author_index>
	 <date_index>3</date_index>
	 <log_index>4</log_index>
      </log_parser>

      <revision_parser>
         <regexp>----------\nr([^ ]+) [^\n]+\nChanged paths:\n   A ([^ ]+) \(from </regexp>
	 <sym_index>2</sym_index>
	 <repository_revision_index>1</repository_revision_index>
      </revision_parser>
   </vcs>
</GPS>
"""


GPS.parse_xml(XML)
register_vcs_actions("Subversion", actions)
