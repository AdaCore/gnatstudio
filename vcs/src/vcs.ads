-----------------------------------------------------------------------
--                           GLIDE II                                --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Generic_List;

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
   --  A value used to reference a VCS repository.

   type VCS_Access is access all VCS_Record'Class;

   package String_List is new Generic_List (String);

   type File_Status is
     (Unknown,
      --  The status is not yet determined or the VCS repository is not able to
      --  tell (disconnected, locked, broken, etc)

      Not_Registered,
      --  The file is unknown of the VCS repository.

      Up_To_Date,
      --  The file corresponds to the latest version in the corresponding
      --  branch on the repository.

      Removed,
      --  The file still exists locally but is known to have been removed from
      --  the VCS repository.

      Modified,
      --  The file has been modified by the user or has been explicitly opened
      --  for editing.

      Needs_Merge,
      --  The file has been modified locally and on the repository.

      Needs_Update
      --  The file has been modified in the repository but not locally.
     );

   type File_Status_Record is record
      --  Contains all the repository information concerning one file.

      File_Name : String_List.List;
      --  The absolute local name of the file.

      Status : File_Status := Unknown;
      --  The status of the file.

      Working_Revision : String_List.List;
      --  The current local version of the file.

      Repository_Revision : String_List.List;
      --  The latest version of the file in the repository.

      Tags    : String_List.List;
      --  Other versions of this file that exist in the repository.

      Users : String_List.List;
      --  A list of users currently working on the file.

      --  ???  We need to put additional info here : date, etc.
   end record;

   package File_Status_List is new Generic_List (File_Status_Record);

   function Get_Status
     (Rep         : access VCS_Record;
      Filenames   :        String_List.List;
      Get_Status  :        Boolean    := True;
      Get_Version :        Boolean    := True;
      Get_Tags    :        Boolean    := False;
      Get_Users   :        Boolean    := False)
     return File_Status_List.List is abstract;
   --  Return the status of a list of files.
   --  The returned File_Status_Record is to be filled only with information
   --  that have the corresponding parameter set to True, all the other fields
   --  are empty lists.
   --
   --  Filenames must be a list of absolute file names.
   --
   --  The returned list must correspond to the order in Filenames.
   --
   --  Implementations for this procedure should document which fields are
   --  costly to obtain and which are easy.
   --
   --  ??? The behaviour is undetermined when the Filenames list
   --  has duplicate entries, or empty/invalid entries.

   function Local_Get_Status
     (Rep         : access VCS_Record;
      Filenames   :        String_List.List)
     return File_Status_List.List is abstract;
   --  Return the status of a list of files.
   --  This function only attempts to read information from local data, and
   --  does not connect to the repository.
   --  It is therefore faster than Get_Status but cannot find all information.
   --  Filenames can simply be a list of directories, in which case
   --  the function will return information about all files in those
   --  directories.

   procedure Open
     (Rep       : access VCS_Record;
      Name      : String;
      User_Name : String := "")
      is abstract;
   --  Open the a file for modification.
   --  This is a necessary step for some systems but not all of them.
   --  If User_Name is not empty, then add it to the list of users
   --  editing the file, whenever possible.

   procedure Commit
     (Rep  : access VCS_Record;
      Name : String;
      Log  : String) is abstract;
   --  Check a file Name in the specified repository.
   --  Log is used as the revision history.

   procedure Update (Rep : access VCS_Record; Name : String) is abstract;
   --  Check a file out the specified repository.
   --  Name is the name of the local file.

   procedure Merge (Rep : access VCS_Record; Name : String) is abstract;
   --  Merge the file from the specified repository with the local file.
   --  Name is the name of the local file.

   procedure Add (Rep : access VCS_Record; Name : String) is abstract;
   --  Add a given file/directory name in the specified VCS repository

   procedure Remove (Rep : access VCS_Record; Name : String) is abstract;
   --  Remove a given file/directory name from the specified VCS repository

   function Diff
     (Rep       : access VCS_Record;
      File_Name : String;
      Version_1 : String := "";
      Version_2 : String)
     return String_List.List is abstract;
   --  Return a diff between two versions of one file.
   --  The result is a String_List.List with one element for each line,
   --  in the standard basic diff format.
   --  If Version_1 is empty, then the local file is taken.

   function Log
      (Rep       : access VCS_Record;
       File_Name : String)
      return String_List.List is abstract;
   --  Return a changelog for the corresponding file.
   --  The result String_List.List with one element for each line.


   --  ??? The following two functions make this package thread-unsafe.

   function Success (Rep : access VCS_Record) return Boolean is abstract;
   --  Return True iff the last operation was successful.

   function Get_Message (Rep : access VCS_Record) return String is abstract;
   --  Return the last message given by the VCS software (useful for
   --  transmitting error messages back to the user).
   --
   --  This function must return an empty string whenever an operation succeeds
   --  and a non-empty string whenever an operation fails.

   --  missing:
   --  annotate
   --  init
   --  tag

private
   type VCS_Record is abstract tagged limited null record;
end VCS;
