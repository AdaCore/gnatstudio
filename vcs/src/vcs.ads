-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with GPS.Kernel;                use GPS.Kernel;
with String_List_Utils;         use String_List_Utils;
with Generic_List;
with OS_Utils;                  use OS_Utils;
with VFS;
with GNAT.Strings;

package VCS is

   --  This package provides utilities for communicating with a VCS
   --  repository. It provides a general architecture which is common
   --  to all VCS systems.
   --
   --  Depending on what the underlying VCS is like, some operations might
   --  be more costly than others,
   --  Therefore we provide two kinds of access to a VCS repository :
   --  either by reading multiple entries at a time,
   --  or by reading one entry at a time.
   --
   --  All queries are configurable, ie the amount of information
   --  that we communicate can be decided by the user.

   type VCS_Record is abstract tagged limited private;
   --  A value used to reference a VCS repository

   type VCS_Access is access all VCS_Record'Class;

   type VCS_Action is
     (None,               --  Do nothing
      Status_Files,       --  Queries the status of one or more files
      Status_Dir,         --  Queries the status for one directory
      Status_Dir_Recursive, --  Queries the status for a directory recursively
      Local_Status_Files, --  Queries the local status for one or more files
      Local_Status_Dir,   --  Queries the local status for one directory
      Create_Tag,         --  Create a new tag
      Create_Branch,      --  Create a new branch
      Switch,             --  Switch to a specific branch/tag
      Merge,              --  Merge changes from a branch
      Open,               --  Open one or more file for writing
      Revision,           --  Get a specific file revision
      Update,             --  Update one or more files
      Resolved,           --  Change file status after conflict resolution
      Commit,             --  Commits one or more files
      History,            --  Get the entire revision history for one file
      History_Text,       --  As above but result as plain text information
      History_Revision,   --  Get the revision history for specified revision
      Annotate,           --  Get the annotations for one file
      Diff_Patch,         --  Diff current gainst head for building patch file
      Diff_Head,          --  Diff current against head revision
      Diff_Working,       --  Diff current against working revision
      Diff_Base_Head,     --  Diff base against head revision
      Diff,               --  Diff current against specified revision
      Diff2,              --  Diff between two specified revisions
      Diff_Tag,           --  Diff current against a specific branch/tag
      Add,                --  Add and commit one file or dir to the repository
      Add_No_Commit,      --  Add one file or dir to the repository
      Remove,             --  Remove and commit one file or dir from repository
      Remove_No_Commit,   --  Remove one file or dir from repository
      Revert);            --  Revert files or dirs to repository revision

   type Action_Array is array (VCS_Action) of GNAT.Strings.String_Access;

   function Get_VCS_From_Id (Id : String) return VCS_Access;
   --  Browse through all VCS identifiers that are registered and return
   --  a VCS reference to an appropriate system, if any.
   --  If no satisfying system was found, Null is returned.
   --  VCS identifiers are registered using Register_VCS_Identifier.

   type VCS_Id_Identifier is access
     function (Id : String) return VCS_Access;

   procedure Register_VCS_Identifier (Identifier : VCS_Id_Identifier);
   --  Add an identifier to the list of known identifiers.
   --  See Get_VCS_From_Id above.

   procedure Unregister_VCS_Identifier (Identifier : VCS_Id_Identifier);
   --  Remove from the list of known identifiers the first one that matches
   --  Identifier.

   function Name (Ref : access VCS_Record) return String is abstract;
   --  The name of the VCS system

   procedure Free (Ref : in out VCS_Record);
   --  Free memory associated with Ref

   procedure Free (Ref : in out VCS_Access);
   --  Free the VCS pointed to by Ref, and Ref itself

   function Group_Query_Status_By_Dir (Ref : access VCS_Record) return Boolean;
   --  Return whether global query operations should be grouped by directory,
   --  for efficiency purposes.

   function Absolute_Filenames_Supported
     (Ref : access VCS_Record) return Boolean;
   --  Returns True if the external VCS supports absolute filenames

   function Atomic_Commands_Supported (Ref : access VCS_Record) return Boolean;
   --  Returns True if the external VCS handles atomic commands

   function Commit_Directory (Ref : access VCS_Record) return Boolean;
   --  Returns True if an added/removed directories needs to be committed

   function Ignore_Filename (Ref : access VCS_Record) return String;
   --  Returns the file containing a list of file to ignore. Returns the empty
   --  string if no such file is set for this VCS.

   type File_Status is record
      Label    : GNAT.Strings.String_Access;
      --  The label corresponding to the status

      Stock_Id : GNAT.Strings.String_Access;
      --  Associated stock icon
   end record;

   procedure Free (X : in out File_Status);
   --  Free memory associated to X.
   --  Note that file status are intended to be static objects, they should not
   --  be freed until the end of the program.

   ---------------------
   -- Standard status --
   ---------------------

   Unknown_Label : aliased String := "Unknown";
   Unknown_Stock : aliased String := "gps-vcs-unknown";

   Unknown : constant File_Status :=
     (Unknown_Label'Access, Unknown_Stock'Access);
   --  The status is not yet determined or the VCS repository is not able to
   --  tell (disconnected, locked, broken, etc)

   type Status_Id
     is (Unknown_Id, Up_To_Date_Id, Modified_Id, Removed_Id, Added_Id,
         Needs_Merge_Id, Needs_Update_Id, Not_Registered_Id);

   function Get_File_Status
     (Ref    : access VCS_Record'Class;
      Status : Status_Id) return File_Status;
   --  Return the File_Status given one of standard status. This routine uses
   --  the File_Status Stock_Id as the key. Return the Unknown status if the
   --  key is not found for the given VCS.

   function Get_File_Status_Id (Status : File_Status) return Status_Id;
   --  Return the Status_Id given a File_Status . This routine uses the
   --  File_Status Stock_Id as the key. Return the Unknown status if the key is
   --  not found for the given VCS.

   type Status_Array is array (Natural range <>) of File_Status;
   type Status_Array_Access is access Status_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Status_Array, Status_Array_Access);

   type File_Status_Record is record
      --  Contains all the repository information concerning one file

      File                : VFS.Virtual_File := VFS.No_File;
      --  The corresponding file

      Status              : File_Status := Unknown;
      --  The status of the file

      Working_Revision    : String_List.List := String_List.Null_List;
      --  The current local version of the file

      Repository_Revision : String_List.List := String_List.Null_List;
      --  The latest version of the file in the repository

      Tags                : String_List.List := String_List.Null_List;
      --  Other versions of this file that exist in the repository

      Users               : String_List.List := String_List.Null_List;
      --  A list of users currently working on the file

      --  ???  We need to put additional info here: date, etc.
   end record;

   function Copy_File_Status
     (F : in File_Status_Record) return File_Status_Record;
   --  Return a deep copy of F

   procedure Free (F : in out File_Status_Record);
   package File_Status_List is new Generic_List (File_Status_Record);

   procedure Get_Status
     (Rep        : access VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) is abstract;
   --  Return the status of a list of files.
   --  The returned File_Status_Record is to be filled only with information
   --  that have the corresponding parameter set to True, all the other fields
   --  are empty lists.
   --
   --  Filenames must be a list of absolute file names or absolute
   --  directory names.
   --  If the list contains file names then the returned list must correspond
   --  to the order in Filenames.
   --
   --  Implementations for this procedure should document which fields are
   --  costly to obtain and which are easy.
   --
   --  The user must free Filenames.
   --  ??? The behaviour is undetermined when the Filenames list
   --  has duplicate entries, or empty/invalid entries.
   --
   --  If Clear_Logs is False, then the implementation should clear the
   --  logs corresponding to files that are Up_To_Date.
   --
   --  Ideally, Get_Status should call VCS_View_Pkg.Display_File_Status
   --  in order to display the status in the explorer.

   procedure Get_Status_Dirs
     (Rep        : access VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) is abstract;
   --  Same as above, but work on directories instead of files

   procedure Get_Status_Dirs_Recursive
     (Rep        : access VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) is abstract;
   --  Same as above, and works on all subdirectories recursively.

   function Local_Get_Status
     (Rep       : access VCS_Record;
      Filenames : String_List.List) return File_Status_List.List is abstract;
   --  Return the status of a list of files.
   --  This function only attempts to read information from local data, and
   --  does not connect to the repository.
   --  It is therefore faster than Get_Status but cannot find all information.
   --  Filenames can simply be a list of directories, in which case
   --  the function will return information about all files in those
   --  directories.
   --  The user must free Filenames.

   procedure Create_Tag
     (Rep       : access VCS_Record;
      Dir       : VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean) is abstract;
   --  Create a new tag/branch starting from dir as root. If As_Branch is set
   --  branch tag is created instead. Repository_Root can be set to point to
   --  the repository root directory path.

   procedure Open
     (Rep       : access VCS_Record;
      Filenames : String_List.List;
      User_Name : String := "") is abstract;
   --  Open the a file for modification.
   --  This is a necessary step for some systems but not all of them.
   --  If User_Name is not empty, then add it to the list of users
   --  editing the file, whenever possible.
   --  The user must free Filenames.

   procedure Commit
     (Rep       : access VCS_Record;
      Filenames : String_List.List;
      Log       : String) is abstract;
   --  Commit Filenames using Log.
   --  The user must free Filenames.

   procedure Update
     (Rep       : access VCS_Record;
      Filenames : String_List.List) is abstract;
   --  Synchronize the local files or directories.
   --  The user must free Filenames.

   procedure Switch
     (Rep : access VCS_Record;
      Dir : VFS.Virtual_File;
      Tag : String) is abstract;
   --  Swicth to the specified tag/branch starting from dir as root

   procedure Resolved
     (Rep       : access VCS_Record;
      Filenames : String_List.List) is abstract;
   --  Change file status to resolved after conflic resolution.
   --  The user must free Filenames.

   procedure Merge
     (Rep       : access VCS_Record;
      Filenames : String_List.List;
      Tag       : String) is abstract;
   --  Merge the files from the specified repository with the local files.
   --  The merge is done locally, a commit is required to pass the
   --  resulting changes to the repository.
   --  The user must free Filenames.

   procedure Add
     (Rep       : access VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True) is abstract;
   --  Add files to the specified VCS repository, commit if Commit is True.
   --  The user must free Filenames.

   procedure Remove
     (Rep       : access VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True) is abstract;
   --  Remove a given file/directory name from the specified VCS repository
   --  Commit the change if Commit is True.
   --  The user must free Filenames.

   procedure Revert
     (Rep       : access VCS_Record;
      Filenames : String_List.List) is abstract;
   --  Obtain the files in Filenames from their respective checked-in versions.
   --  The user must free Filenames.

   procedure File_Revision
     (Rep      : access VCS_Record;
      File     : VFS.Virtual_File;
      Revision : String) is abstract;
   --  Retreive a specific revision of a given file

   procedure Diff
     (Rep       : access VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "") is abstract;
   --  Return a diff between two versions of one file
   --  If Version_1 is empty, then the local file is taken.
   --  If Version_2 is empty, then the HEAD revision is taken.

   procedure Diff_Patch
     (Rep    : access VCS_Record;
      File   : VFS.Virtual_File;
      Output : VFS.Virtual_File) is abstract;

   procedure Diff_Working
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File) is abstract;
   --  Compare local against working revison of File

   procedure Diff_Base_Head
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File) is abstract;
   --  Compare base against head revision of File

   procedure Diff_Tag
     (Rep      : access VCS_Record;
      File     : VFS.Virtual_File;
      Tag_Name : String) is abstract;
   --  Compare local against the specified tag/branch

   procedure Log
     (Rep     : access VCS_Record;
      File    : VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True) is abstract;
   --  Display the changelog for the corresponding file.
   --  Rev is the requested revision. If Rev is empty, return the log for all
   --  revisions on the current branch.

   Annotation_Id : constant String := "Annotate";
   procedure Annotate
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File) is abstract;
   --  Return annotations for the corresponding file.
   --  The result String_List.List with one element for each line.
   --
   --  This procedure should call GPS.Kernel.Modules.Add_Line_Information
   --  in order to display the annotations in the GPS editors.
   --  The Line_Information ID that must be used is Annotation_Id.

   procedure Check_Files
     (Rep       : access VCS_Record;
      Filenames : String_List_Utils.String_List.List);
   --  Enqueue an action that will check that the open buffers correspond
   --  to files on disk, and reload the buffers if necessary, asking the user
   --  when the buffer has been modified.

   procedure Set_Error
     (Rep     : access VCS_Record;
      Message : String;
      Add_LF  : Boolean := True);
   --  Display message in the console

   --  ??? missing:
   --  init
   --  tag
   --  other version

   procedure Parse_Status
     (Rep        : access VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String);
   --  Parse Text and return the list of status obtained.
   --  Local indicates whether we are parsing local status.
   --  Dir is the directory which contains the status, if relevant.

   procedure Parse_Annotations
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String);
   --  Parse the annotations and fill the editor if needed

   procedure Parse_Log
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String);
   --  Parse the log and fill the Revision Browser if needed

   procedure Parse_Revision
     (Rep  : access VCS_Record;
      File : VFS.Virtual_File;
      Text : String);
   --  Parse the log to retreive the revision tag and branches information and
   --  fill the Revision Browser if needed.

   function Get_Identified_Actions
     (Rep : access VCS_Record) return Action_Array;
   --  Return the labels of the defined actions. User must not free the result.

   function Get_Registered_Status
     (Rep : access VCS_Record) return Status_Array;
   --  Return the registered status for Rep.
   --  The result should always have in first position the Unknown status,
   --  which is common to all VCS, and then the status considered as
   --  "up-to-date".

private

   Command_Line_Limit : constant Natural := 64;
   --  The maximum number of files transmitted to a command.
   --  This is used to avoid breaking the command-line limit on some systems.
   --  Should this be a (hidden) preference ???

   --------------------------
   -- Standard file status --
   --------------------------

   Up_To_Date_Label : aliased String := "Up to date";
   Up_To_Date_Stock : aliased String := "gps-vcs-up-to-date";
   Up_To_Date : File_Status :=
     (Up_To_Date_Label'Access, Up_To_Date_Stock'Access);
   --  The file corresponds to the latest version in the corresponding
   --  branch on the repository.

   Added_Label : aliased String := "Added";
   Added_Stock : aliased String := "gps-vcs-added";
   Added : File_Status :=
     (Added_Label'Access, Added_Stock'Access);
   --  The file has been locally added to the VCS repository but the change
   --  has not been committed.

   Removed_Label : aliased String := "Removed";
   Removed_Stock : aliased String := "gps-vcs-removed";
   Removed : File_Status :=
     (Removed_Label'Access, Removed_Stock'Access);
   --  The file still exists locally but is known to have been removed from
   --  the VCS repository.

   Modified_Label : aliased String := "Modified";
   Modified_Stock : aliased String := "gps-vcs-modified";
   Modified : File_Status :=
     (Modified_Label'Access, Modified_Stock'Access);
   --  The file has been modified by the user or has been explicitly opened
   --  for editing.

   Needs_Merge_Label : aliased String := "Needs merge";
   Needs_Merge_Stock : aliased String := "gps-vcs-needs-merge";
   Needs_Merge : File_Status :=
     (Needs_Merge_Label'Access, Needs_Merge_Stock'Access);
   --  The file has been modified locally and on the repository.

   Needs_Update_Label : aliased String := "Needs update";
   Needs_Update_Stock : aliased String := "gps-vcs-needs-update";
   Needs_Update : File_Status :=
     (Needs_Update_Label'Access, Needs_Update_Stock'Access);
   --  The file has been modified in the repository but not locally.

   Not_Registered_Label : aliased String := "Not registered";
   Not_Registered_Stock : aliased String := "gps-vcs-not-registered";
   Not_Registered : File_Status :=
     (Not_Registered_Label'Access, Not_Registered_Stock'Access);
   --  The file is unknown of the VCS repository.

   type VCS_Record is abstract tagged limited record
      Kernel              : GPS.Kernel.Kernel_Handle;
      Absolute_Names      : Boolean    := False;
      Query_Status_By_Dir : Boolean    := False;
      Atomic_Commands     : Boolean    := False;
      Commit_Directory    : Boolean    := False;
      Path_Style          : OS_Utils.Path_Style := System_Default;
      Ignore_Filename     : GNAT.Strings.String_Access;
   end record;

end VCS;
