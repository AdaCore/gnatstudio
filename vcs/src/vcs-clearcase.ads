-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2008, AdaCore                --
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

   overriding function Name (Ref : access ClearCase_Record) return String;

   overriding procedure Get_Status
     (Rep        : access ClearCase_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   overriding procedure Get_Status_Dirs
     (Rep        : access ClearCase_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   overriding procedure Get_Status_Dirs_Recursive
     (Rep        : access ClearCase_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   overriding function Local_Get_Status
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

   overriding procedure Create_Tag
     (Rep       : access ClearCase_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   overriding procedure Open
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   overriding procedure Commit
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String);

   overriding procedure Update
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   overriding procedure Switch
     (Rep : access ClearCase_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String);

   overriding procedure Resolved
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   overriding procedure Merge
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Tag       : String);

   overriding procedure Add
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Remove
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Revert
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List);

   overriding procedure File_Revision
     (Rep      : access ClearCase_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Revision : String);

   overriding procedure Diff
     (Rep       : access ClearCase_Record;
      File      : GNATCOLL.VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   overriding procedure Diff_Patch
     (Rep    : access ClearCase_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Base_Head
     (Rep  : access ClearCase_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Working
     (Rep  : access ClearCase_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Tag
     (Rep      : access ClearCase_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String);

   overriding procedure Log
     (Rep     : access ClearCase_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   overriding procedure Annotate
     (Rep  : access ClearCase_Record;
      File : GNATCOLL.VFS.Virtual_File);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.ClearCase module

   overriding function Get_Identified_Actions
     (Rep : access ClearCase_Record) return Action_Array;

private
   type ClearCase_Record is new VCS_Record with null record;

end VCS.ClearCase;
