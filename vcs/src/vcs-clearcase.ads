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

--  This package provides a ClearCase object implementating the VCS
--  abstract specification.
--
--  See package VCS for a complete spec of this package.

package VCS.ClearCase is

   type ClearCase_Record is new VCS_Record with private;
   --  A value used to reference a ClearCase repository.

   type ClearCase_Access is access all ClearCase_Record'Class;

   function Name (Ref : access ClearCase_Record) return String;

   procedure Get_Status
     (Rep         : access ClearCase_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False;
      Local       : Boolean := False);

   procedure Get_Status_Dirs
     (Rep         : access ClearCase_Record;
      Dirs        : String_List.List;
      Clear_Logs  : Boolean := False;
      Local       : Boolean := False) renames Get_Status;

   procedure Get_Status_Dirs_Recursive
     (Rep         : access ClearCase_Record;
      Dirs        : String_List.List;
      Clear_Logs  : Boolean := False;
      Local       : Boolean := False) renames Get_Status;

   function Local_Get_Status
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

   procedure Create_Tag
     (Rep       : access ClearCase_Record;
      Dir       : VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   procedure Open
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   procedure Commit
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String);

   procedure Update
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   procedure Switch
     (Rep : access ClearCase_Record;
      Dir : VFS.Virtual_File;
      Tag : String);

   procedure Resolved
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   procedure Merge
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Tag       : String);

   procedure Add
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Remove
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Revert
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   procedure File_Revision
     (Rep      : access ClearCase_Record;
      File     : VFS.Virtual_File;
      Revision : String);

   procedure Diff
     (Rep       : access ClearCase_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Diff_Patch
     (Rep    : access ClearCase_Record;
      File   : VFS.Virtual_File;
      Output : VFS.Virtual_File);

   procedure Diff_Base_Head
     (Rep  : access ClearCase_Record;
      File : VFS.Virtual_File);

   procedure Diff_Working
     (Rep  : access ClearCase_Record;
      File : VFS.Virtual_File);

   procedure Diff_Tag
     (Rep      : access ClearCase_Record;
      File     : VFS.Virtual_File;
      Tag_Name : String);

   procedure Log
     (Rep     : access ClearCase_Record;
      File    : VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   procedure Annotate
     (Rep  : access ClearCase_Record;
      File : VFS.Virtual_File);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.ClearCase module

   function Get_Identified_Actions
     (Rep : access ClearCase_Record) return Action_Array;

private
   type ClearCase_Record is new VCS_Record with null record;

end VCS.ClearCase;
