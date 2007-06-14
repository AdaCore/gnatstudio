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

--  This package provides an empty derivation of VCS_Record.
--
--  See package VCS for a complete spec of this package.

package VCS.Unknown_VCS is

   type Unknown_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Unknown_VCS repository.

   type Unknown_VCS_Access is access all Unknown_VCS_Record'Class;

   Unknown_VCS_Reference : VCS_Access;
   --  ??? Could this global variable be in some module specific information

   function Name (Ref : access Unknown_VCS_Record) return String;

   procedure Free (Ref : access Unknown_VCS_Record);

   procedure Get_Status
     (Rep        : access Unknown_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   procedure Get_Status_Dirs
     (Rep        : access Unknown_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   procedure Get_Status_Dirs_Recursive
     (Rep        : access Unknown_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   function Local_Get_Status
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

   procedure Create_Tag
     (Rep       : access Unknown_VCS_Record;
      Dir       : VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   procedure Open
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   procedure Commit
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String);

   procedure Update
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List);

   procedure Switch
     (Rep : access Unknown_VCS_Record;
      Dir : VFS.Virtual_File;
      Tag : String);

   procedure Resolved
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List);

   procedure Merge
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Tag       : String);

   procedure Add
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Remove
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Revert
     (Rep       : access Unknown_VCS_Record;
      Filenames : String_List.List);

   procedure File_Revision
     (Rep      : access Unknown_VCS_Record;
      File     : VFS.Virtual_File;
      Revision : String);

   procedure Diff
     (Rep       : access Unknown_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Diff_Patch
     (Rep    : access Unknown_VCS_Record;
      File   : VFS.Virtual_File;
      Output : VFS.Virtual_File);

   procedure Diff_Base_Head
     (Rep  : access Unknown_VCS_Record;
      File : VFS.Virtual_File);

   procedure Diff_Working
     (Rep  : access Unknown_VCS_Record;
      File : VFS.Virtual_File);

   procedure Diff_Tag
     (Rep      : access Unknown_VCS_Record;
      File     : VFS.Virtual_File;
      Tag_Name : String);

   procedure Log
     (Rep     : access Unknown_VCS_Record;
      File    : VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   procedure Annotate
     (Rep  : access Unknown_VCS_Record;
      File : VFS.Virtual_File);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.Unknown_VCS module

private
   type  Unknown_VCS_Record is new VCS_Record with null record;

end VCS.Unknown_VCS;
