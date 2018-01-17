------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides an empty derivation of VCS_Record.
--
--  See package VCS for a complete spec of this package.

package VCS.Unknown_VCS is

   type Unknown_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Unknown_VCS repository

   type Unknown_VCS_Access is access all Unknown_VCS_Record'Class;

   Unknown_VCS_Reference : VCS_Access;
   --  ??? Could this global variable be in some module specific information

   overriding function Name (Ref : access Unknown_VCS_Record) return String;

   overriding function Require_Log
     (Ref : access Unknown_VCS_Record) return Boolean;

   procedure Free (Ref : access Unknown_VCS_Record);

   overriding procedure Get_Status
     (Rep        : access Unknown_VCS_Record;
      Filenames  : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   overriding procedure Get_Status_Dirs
     (Rep        : access Unknown_VCS_Record;
      Filenames  : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   overriding procedure Get_Status_Dirs_Recursive
     (Rep        : access Unknown_VCS_Record;
      Filenames  : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False) renames Get_Status;

   overriding function Local_Get_Status
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array)
     return File_Status_List.Vector;

   overriding procedure Create_Tag
     (Rep       : access Unknown_VCS_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   overriding procedure Open
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      User_Name : String := "");

   overriding procedure Commit
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String);

   overriding procedure Update
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure Switch
     (Rep : access Unknown_VCS_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String);

   overriding procedure Resolved
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure Merge
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Tag       : String);

   overriding procedure Add
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Remove
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Revert
     (Rep       : access Unknown_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure File_Revision
     (Rep      : access Unknown_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Revision : String);

   overriding procedure Diff
     (Rep       : access Unknown_VCS_Record;
      File      : GNATCOLL.VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   overriding procedure Diff_Patch
     (Rep    : access Unknown_VCS_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Base_Head
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Working
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Tag
     (Rep      : access Unknown_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String);

   overriding procedure Log
     (Rep     : access Unknown_VCS_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   overriding procedure Annotate
     (Rep  : access Unknown_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.Unknown_VCS module

private
   type  Unknown_VCS_Record is new VCS_Record with null record;

end VCS.Unknown_VCS;
