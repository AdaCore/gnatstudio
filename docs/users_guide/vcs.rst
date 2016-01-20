.. _Version_Control_System:

**********************
Version Control System
**********************

.. index:: version control

GPS provides integration with a version control system (VCS) so multiple
developers can work on the same project.  Each project can be associated
with a VCS, through the :guilabel:`VCS` tab in the Project properties
editor.  See :ref:`The_Project_Properties_Editor`.

GPS does not come with any version control system.  Instead, it provides a
high level user interface to a command-line system such as Subversion or
ClearCase that you may already have on your system.

GPS has built in support for the following VCS systems:

* :guilabel:`Auto`

  .. index:: VCS, auto

  You can set GPS to auto-detect the VCS to use for each project by
  selecting :guilabel:`Auto` in the :guilabel:`VCS` tab of the Project
  properties editor. (See :ref:`The_Project_Properties_Editor`.)  This is
  the default behavior when no VCS is specified in the project.

* :guilabel:`ClearCase`

  .. index:: VCS, ClearCase

  The standard ClearCase interface, which is built-in and uses generic GPS
  terminology for VCS operations. Only Snapshot Views are supported in the
  ClearCase integration; Dynamic Views are not supported.

* :guilabel:`ClearCase Native`

  .. index:: VCS, ClearCase Native

  A customizable interface to ClearCase using, by default, terminology
  specific to ClearCase.  Only Snapshot Views are supported in the ClearCase
  integration; Dynamic Views are not supported.

* :guilabel:`CVS`

  .. index:: VCS, CVS

  The Concurrent Version System.  To use this, you must have a
  :program:`patch` tool, which usually comes with CVS.

* :guilabel:`Git`

  .. index:: VCS, Git

  Distributed fast source code management. Git is only partially supported
  by GPS: basic commands are supported but the full power of Git (such as
  working with the index) is only available from the command line.  To use
  this, you must have a :program:`diff` tool, which usually comes with Git.

* :guilabel:`Mercurial`

  .. index:: VCS, Mercurial

  An experimental plugin for supporting Mercurial.

* :guilabel:`Subversion`

  .. index:: VCS, Subversion

  The Subversion version control system.  On Windows, this version is
  intended to be used with Cygwin/Subversion and fully supports the Cygwin
  path names.  To use this, you must have the :program:`patch` and
  :program:`diff` tools that usually come with Subversion.

* :guilabel:`Subversion Windows`

  .. index:: VCS, Subversion Windows

  The Windows native Subversion version control system. The external
  Subversion commands are expected to be built for the Win32
  subsystem. This version does not support Cygwin path names.  To use this,
  you must have the :program:`patch` and :program:`diff` tools that usually
  come with Subversion.

By default, GPS uses "Auto" for a VCS. You can change this default via the
preferences dialog, or better by editing your project.

You can also add support for other version control systems or modify one of
the existing interfaces, see
:ref:`Adding_support_for_new_Version_Control_Systems` for more information.

Before using it under GPS, you should first get familiar with the version
control system you intend to use, since many concepts used in GPS assume
basic knowledge of the underlying system.  Associating a VCS to a project
allows using basic VCS features on the source files contained in the
project. Those basic features typically include checking in and out files,
querying of file status and revision history, and comparison between
various revisions.

.. index:: password

Note: you must be sure VCS commands can be launched without needing to
enter a password.

.. _The_VCS_Explorer:

The VCS Explorer
================

.. index:: VCS explorer
.. index:: version control

The :guilabel:`VCS Explorer` provides an overview of source files and their
status. GPS automatically adds a file edited in GPS to the :guilabel:`VCS
Explorer` with a *Modified* status (see below).

.. index:: screen shot
.. image:: vcs-explorer.jpg

Start the :guilabel:`VCS Explorer` through the :menuselection:`VCS -->
Explorer` menu or using the contextual menu :menuselection:`Version Control
--> Query status` on files, directories and projects in the file and
project views, and in editors.  See
:ref:`The_Version_Control_Contextual_Menu`.

The :guilabel:`VCS Explorer` contains the following columns:

* :guilabel:`Project / File`

  This is a two-level tree. The first level contains the name of the
  project and the second the name of files inside the project. Next to the
  project name, the VCS name, if any, is displayed.  You can sort this
  column by clicking on its header.  For a project, this is the only
  information available: the columns described below are for files only.

* :guilabel:`Status`

  Shows the status of the file. You can sort this column by clicking on
  its header. The possible status for files are:

  * :command:`Unknown`
    .. image:: gps-vcs-unknown.jpg

    The status is not yet determined or the VCS repository is not able to
    give this information (for example, it is unavailable or locked).

  * :command:`Not registered`
    .. image:: gps-vcs-not-registered.jpg

    The file is not known to the VCS repository.

  * :command:`Up-to-date`
    .. image:: gps-vcs-up-to-date.jpg

    The file corresponds to the latest version in the corresponding branch
    on the repository.

  * :command:`Added`
    .. image:: gps-vcs-added.jpg

    The file has been added remotely but is not yet updated in the local
    view.

  * :command:`Removed`
    .. image:: gps-vcs-removed.jpg

    The file still exists locally but is known to have been removed from
    the VCS repository.

  * :command:`Modified`
    .. image:: gps-vcs-modified.jpg

    You modified the file or explicitly opened it for editing.

  * :command:`Needs merge`
    .. image:: gps-vcs-needs-merge.jpg

    You modified the file both locally and in the repository.

  * :command:`Needs update`
    .. image:: gps-vcs-needs-update.jpg

    The file has been modified in the repository but not locally.

  * :command:`Contains merge conflicts`
    .. image:: gps-vcs-has-conflicts.jpg

    The file contains conflicts from a previous update operation.

* :guilabel:`Log`

  Indicates whether a revision log exists for this file.

* :guilabel:`Activity`

  The name of the activity the file belongs to. See :ref:`VCS_Activities`
  for more details.

* :guilabel:`Working rev.`

  Indicates the version of the local file.

* :guilabel:`Head rev.`

  Indicates the most recent version of the file in the repository.

The :guilabel:`VCS Explorer` supports multiple selections. To select a
single file, left-click on it. To select a range of files, select the
first, then hold the :kbd:`Shift` key and select the last. To add or remove
single columns from the selection, hold the :kbd:`Control` key and
left-click on the columns you want to select or remove.  You can also
select files having the same status using the :guilabel:`Select files same
status` menu entry. See :ref:`The_Version_Control_Contextual_Menu`.

.. index:: interactive search

The explorer also provides an :ref:`interactive search
<Interactive_Search>` capability allowing you to quickly look for a given
file name. The default key to start an interactive search is :kbd:`Ctrl-i`.

You can bring up the VCS contextual menu from the :guilabel:`VCS Explorer`
by left-clicking on a selection or single line. See
:ref:`The_Version_Control_Contextual_Menu`.

.. _VCS_Activities:

VCS Activities
==============

.. index:: VCS activities
.. index:: version control

VCS Activities allow a group of files to be committed together.  The files
can be committed atomically if supported by the version control system.

.. index:: screen shot
.. image:: vcs-activities.jpg

Start the :guilabel:`VCS Activities` view through the :menuselection:`VCS
--> Activities` menu.  It contains the following columns:

*Activity / File*

  The name of the activity or files belonging to an activity. You can sort
  this column by clicking on its header.

*Status*

  Shows the status of each file. You can sort this column by clicking on its
  header. See :ref:`The_VCS_Explorer` for a full description.

*Log*

  Indicates whether a revision log exists for the file.

*Working rev.*

  Indicates the version of the local file.

*Head rev.*

  Indicates the most recent version of the file in the repository.

The :guilabel:`VCS Explorer` supports multiple selections. To select a
single file, left-click on it. To select a range of files, select the
first, hold the :kbd:`Shift` key, and select the last. To add or remove
single lines from the selection, hold down the :kbd:`Control` key and
left-click what you want to select or remove.

There are different contextual menu entries depending on the position on the
screen.

* An empty area has only the following option:

  * :guilabel:`Create new activity`

    Create a new activity.  Edit the name by double clicking on it.

* On an activity line, the contextual menu contains:

  * :guilabel:`Group commit`

    This is a selectable menu entry, activated only if the VCS supports
    atomic commit and absolute filenames. See :ref:`The_VCS_node` for full
    details.

  * :guilabel:`Create new activity`

    Create a new activity.  Edit the name by double clicking on it.

  * :guilabel:`Re-open activity / Close activity`

    If the activity is closed, re-open it.  If open, close the activity.

  * :guilabel:`Delete activity`

    Remove the activity.

  * :guilabel:`Commit activity`

    Commit the activity. If group commit is activated then the commit log
    content is generated using a fully configurable template file.  See
    :ref:`Files`.  If group commit is not activated, the log content for each
    activity file is the file log concatenated with the activity log. After
    this operation the file's log are removed but the activity log is
    retained as documentation.

  * :guilabel:`Query status`

    Query the status for all source files contained in the activity.

  * :guilabel:`Update`

    Update all source files contained in the activity.

  * :guilabel:`Compare against head revision`

    Show a visual comparison between the local activity files and the most
    recent version of those files in the repository.

  * :guilabel:`Build patch file`

    Create a patch file (in text format) for the activity. The patch file
    contains a header (the activity log and file's logs) and the diff of
    each file. The header format is fully configurable using a template
    file. See :ref:`Files`.

  * :guilabel:`Edit revision log`

    Edit the current revision log for activity. This log is shared with all
    the activity files.

  * :guilabel:`Remove revision log`

    Remove the current revision log for activity. This menu is present
    only if the activity revision log exists.

* On a line containing a filename, the contextual menu contains:

  * :guilabel:`Create new activity`

    Create a new activity. Edit the name by double clicking on it.

  * :guilabel:`Remove from activity`

    Remove the selected file from the activity and delete the activity log.

  * :guilabel:`Edit revision log`

    Edit the current revision log for the selected file.

.. _The_VCS_Menu:

The VCS Menu
============

.. index:: version control
.. index:: menu

Access basic VCS operations through the VCS menu. Most of these functions
act on the current selection: the selected items in the :guilabel:`VCS
Explorer` if present, the currently selected file editor, or the currently
selected item in the :menuselection:`Tools --> Views --> Files` menu.  In
most cases, the VCS contextual menu offers more control of VCS operations.
See :ref:`The_Version_Control_Contextual_Menu`.

* :guilabel:`Explorer`

  Open or raise the :guilabel:`VCS Explorer`. See :ref:`The_VCS_Explorer`.

* :guilabel:`Update all projects`

  Update the source files in the current project and all imported
  subprojects.

* :guilabel:`Query status for all projects`

  Query the status of all files in the project and all imported subprojects.

* :guilabel:`Create tag...`

  Create a tag or branch tag starting from a specific root
  directory. The name of the tag is a simple name.

* :guilabel:`Switch tag...`

  Switch the local copy to a specific tag. The name of the tag depends on
  the external VCS used. For CVS this is a simple tag name and for
  Subversion the tag must conform to the default repository layout, which,
  for a branch tag, is :command:`/branches/<tag_name>/<root_dir>`.

For a description of the other entries in the VCS menu, see
:ref:`The_Version_Control_Contextual_Menu` below.

.. _The_Version_Control_Contextual_Menu:

The Version Control Contextual Menu
===================================

This section describes the version control contextual menu displayed when
you right-click on an entity (a file, a directory, or a project) from
various parts of GPS, including the project view, the source editor and the
:guilabel:`VCS Explorer`.

Depending on the context, some of the items described in this section
are not shown because they are not relevant to that context.

* :guilabel:`Remove project`

  Only displayed for a project. Remove the selected project from the
  :guilabel:`VCS Explorer`.

* :guilabel:`Expand all`

  Expand all :guilabel:`VCS Explorer` project nodes.

* :guilabel:`Collapse all`

  Collapse all :guilabel:`VCS Explorer` project nodes.

* :guilabel:`Clear View`

  Clear the :guilabel:`VCS Explorer`.

* :guilabel:`Query status`

  Query the status of the selected item. Starts the :guilabel:`VCS Explorer`.

.. _Update:

* :guilabel:`Update`


  Update the currently selected item (file, directory or project).

.. _Commit:

* :guilabel:`Commit`

  Submits the changes made to the file to the repository and queries the
  file's status file once the change is made.

  Tell GPS to check the file before the actual commit occurs by specifying
  a :guilabel:`File checker` in the :guilabel:`VCS` tab of the project
  properties dialog.  This :guilabel:`File checker` is a script or
  executable that takes an absolute file name as argument and displays any
  error message on its standard output. The VCS commit operation occurs
  only if nothing was written to the standard output.  You can also check
  the changelog of a file before commit by specifying a :guilabel:`Log
  checker` in the project properties dialog. This works on changelog files
  in the same way as the :guilabel:`File checker` works on source files.

.. _Open:

* :guilabel:`Open`

  Open the currently selected file for writing.  With some VCS systems,
  this is a necessary operation, but not on all.

.. _View_revision_history:

* :guilabel:`View entire revision history`

  Show the revision logs for all previous revisions of this file.

* :guilabel:`View specific revision history`

  Show the revision logs for one previous revision of this file.

.. _Compare_against_head:

* :guilabel:`Compare against head revision`

  .. index:: compare

  Display a visual comparison between the local file and the most recent
  version of that file in the repository.

.. _Compare_against_working:

* :guilabel:`Compare against other revision`


  Display a visual comparison between the local file and a specified
  version of that file in the repository.

.. _Compare_against_revision:

* :guilabel:`Compare two revisions`

  Display a visual comparison between two specified revisions of the file
  in the repository.

.. _Compare_base_against_head:

* :guilabel:`Compare base against head`

  Display a visual comparison between the current version of the file in
  the repository and the most recent version of that file.

.. _Compare_base_against_tag/branch:

* :guilabel:`Compare against tag/branch`

  Only available on a Revision View and over a tag or branch. Display a
  visual comparison between the corresponding version of the file in the
  repository and the version of that file in the tag or branch.

.. _Annotate:

* :guilabel:`Annotate`

  Display the annotations for the file, i.e. the information for each line
  of the file showing the revision corresponding to that line.  This may
  also display additional information on some VCS systems.

  When using CVS or Subversion, click the annotations to display the
  changelog associated with the specific revision of that line.

* :guilabel:`Remove Annotate`

  Remove annotations from the selected file.

* :guilabel:`Edit revision log`

  Edit the current revision log for the selected file.

* :guilabel:`Edit global ChangeLog`

  Edit the global ChangeLog entry for the selected file.
  See :ref:`Working_with_global_ChangeLog_file`.

* :guilabel:`Remove revision log`

  Clear the current revision associated with the selected file.

* :guilabel:`Add`

  Add a file to the repository, using the current revision log for the
  current file. If no revision log exists, create one. The file is
  committed in the repository.

* :guilabel:`Add/No commit`

  Likewise, but do not commit the file.

* :guilabel:`Remove`

  Remove a file from the repository, using the current revision log for the
  current file. If no revision log exists, create one. The modification is
  committed in the repository.

* :guilabel:`Remove/No commit`

  Likewise, but do not commit.

* :guilabel:`Revert`

  Revert a file to the repository revision, discarding all local
  changes.

* :guilabel:`Resolved`

  Mark a file's merge conflics as resolved. Some version control systems
  (e.g., Subversion) block any commit until this action is performed.

* :guilabel:`Switch tag/bracnh`

  Only available on a Revision View and over a tag or branch name
  Switch the tree starting from a selected root to this specific tag or
  branch.

* :guilabel:`Merge`

  Only available on a Revision View and over a tag or branch name. Merge
  file changes made on this specific tag or branch.

* :guilabel:`View revision`

  Only available on a Revision View and over a revision.

* :guilabel:`Commit as new Activity`

  Prepare a group-commit in just one-click. This action:

  * creates an anonymous activity

  * adds all files selected into the newly created anonymous activity

  * opens the activity log

  * commits the anonymous activity.

* :guilabel:`Add to Activity`

  A menu containing all the current activities. Selecting one adds the
  current file to this activity. This menu is present only if the file is
  not already part of an activity.

* :guilabel:`Remove from Activity`

  Remove file from the given activity. This menu is present only if the
  file is already part of an activity.

* :guilabel:`Directory`

  Only available when the current context contains directory information.
  This menu contains the following options, each applying to the directory:

  * :guilabel:`Add/No commit`

    Add the selected directory into the VCS.

  * :guilabel:`Remove/No commit`

    Removes the selected directory from the VCS.

  * :guilabel:`Commit`

    Commit the selected directory into the VCS. This action is available
    only if the VCS supports commit on directories.  See :ref:`The_VCS_node`.

  * :guilabel:`Add to Activity`

    Add the selected directory to the VCS. This action is available
    only if the VCS supports commit on directories.  See :ref:`The_VCS_node`.

  * :guilabel:`Query status for directory`

    Query the status for files contained in the selected directory.

  * :guilabel:`Update directory`

    Update the files in the selected directory.

  * :guilabel:`Query status for directory recursively`

    Query status for files in the selected directory and all subdirectories
    recursively. Links and hidden directories are not included.

  * :guilabel:`Update directory recursively`

    Update the files (bring them up to date with the repository) in the
    selected directory and all subdirectories recursively. Links and hidden
    directories not included..

* :guilabel:`Project`

  Only available when the current context contains project information

  * :guilabel:`List all files in project`

    Start the :guilabel:`VCS Explorer` with all source files contained
    in the project.

  * :guilabel:`Query status for project`

    Query the status for all source files contained in the project.

  * :guilabel:`Update project`

    Update all source files in the project.

  * :guilabel:`List all files in project and sub-projects`

    Start the :guilabel:`VCS Explorer` with all source files contained in
    the project and all imported subprojects.

  * :guilabel:`Query status for project and sub-projects`

    Query the status for all source files contained in the project and all
    imported subprojects.

  * :guilabel:`Update project and subprojects`

    Update all source files in the project and all imported subprojects.

* :guilabel:`Select files same status`

  Select the files with the same status as the current file.

* :guilabel:`Filters`

  Only available from the :guilabel:`VCS Explorer`. This menu controls the
  filtering of the items displayed in the list.

  * :guilabel:`Show all status`

    Do not exclude any file from the list.

  * :guilabel:`Hide all status`
  
    Remove all files from the list.

  * :guilabel:`Show <status>`

    When disabled, filter the files with the given status from the list.

.. _Working_with_global_ChangeLog_file:

Working with global ChangeLog file
==================================

.. index:: global ChangeLog
.. index:: ChangeLog file

A global :file:`ChangeLog` file contains revision logs for all files in a
directory. Its format is::

     **ISO-DATE  *name  <e-mail>***

     <HT>* **filename**[, **filename**]:
     <HT>revision history

where:

*ISO-DATE*

  A date with the ISO format: YYYY-MM-DD.

*name*

  A name, generally the developer's name.

*<e-mail>*

  The e-mail address of the developer surrounded with '<' and '>' characters.

*HT*

  Horizontal tab (or 8 spaces)

You can supply the *name* and *<e-mail>* items automatically by setting the
:command:`GPS_CHANGELOG_USER` environment variable. There are two spaces
between the *name* and the *<e-mail>*:

On :program:`sh` or :program:`bash` shell::

     export GPS_CHANGELOG_USER="John Doe  <john.doe@home.com>"

On Windows shell::

     set GPS_CHANGELOG_USER="John Doe  <john.doe@home.com>"
  
The menu entry :guilabel:`Edit global ChangeLog` opens the file
:file:`ChangeLog` in the directory containing the current file and creates
the corresponding :file:`ChangeLog` entry.  The ISO date and filename
headers are created if not yet present. You need to enter your name and
e-mail address unless the :command:`GPS_CHANGELOG_USER` environment
variable is present.

This :file:`ChangeLog` file serves as a location for revision logs.  When
you are ready to check-in a file, use the :guilabel:`Edit revision log` menu
command in the standard revision log buffer with the content filled from
the global :file:`ChangeLog` file.

.. _The_Revision_View:

The Revision View
=================

GPS uses the revision view to display a revision tree for a given
file. Each node contains information about a specific revision of the file.

.. index:: screen shot
.. image:: revision-view.jpg

*the revision number*

  This corresponds to the external VCS revision number.

*author*

  The author of this revision.

*date / log*

  For root nodes, this column contains the check-in date and any
  list of tags and branches associated with this revision. For
  child nodes, this contains the log for the corresponding revision.
