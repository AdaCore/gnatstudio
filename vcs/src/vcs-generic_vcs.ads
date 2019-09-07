------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  This package provides an generic derivation of VCS_Record.
--
--  See package VCS for a complete spec of this package.

with GNAT.Expect; use GNAT.Expect;
with GNAT.Strings;

package VCS.Generic_VCS is

   type Generic_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Generic_VCS repository

   type Generic_VCS_Access is access all Generic_VCS_Record'Class;

   overriding procedure Free (Ref : in out Generic_VCS_Record);

   overriding function Name (Ref : access Generic_VCS_Record) return String;

   overriding function Administrative_Directory
     (Ref : access Generic_VCS_Record) return Filesystem_String;

   overriding procedure Get_Status
     (Rep        : access Generic_VCS_Record;
      Filenames  : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   overriding procedure Get_Status_Dirs
     (Rep        : access Generic_VCS_Record;
      Dirs       : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   overriding procedure Get_Status_Dirs_Recursive
     (Rep        : access Generic_VCS_Record;
      Dirs       : GNATCOLL.VFS.File_Array;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   overriding function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array) return File_Status_List.Vector;

   overriding procedure Create_Tag
     (Rep       : access Generic_VCS_Record;
      Dir       : GNATCOLL.VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   overriding procedure Open
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      User_Name : String := "");

   overriding procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String);

   overriding procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure Switch
     (Rep : access Generic_VCS_Record;
      Dir : GNATCOLL.VFS.Virtual_File;
      Tag : String);

   overriding procedure Resolved
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Tag       : String);

   overriding procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array;
      Log       : String;
      Commit    : Boolean := True);

   overriding procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : GNATCOLL.VFS.File_Array);

   overriding procedure File_Revision
     (Rep      : access Generic_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Revision : String);

   overriding procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : GNATCOLL.VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   overriding procedure Diff_Patch
     (Rep    : access Generic_VCS_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Output : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Base_Head
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Working
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Diff_Tag
     (Rep      : access Generic_VCS_Record;
      File     : GNATCOLL.VFS.Virtual_File;
      Tag_Name : String);

   overriding procedure Log
     (Rep     : access Generic_VCS_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   overriding procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Parse_Status
     (Rep        : access Generic_VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String);

   overriding procedure Parse_Update
     (Rep  : access Generic_VCS_Record;
      Text : String;
      Dir  : String);

   overriding procedure Parse_Annotations
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String);

   overriding procedure Parse_Log
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String);

   overriding procedure Parse_Revision
     (Rep  : access Generic_VCS_Record;
      File : GNATCOLL.VFS.Virtual_File;
      Text : String);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.Generic_VCS module

   overriding function Get_Registered_Status
     (Rep : access Generic_VCS_Record) return Status_Array;

   overriding function Create_From_VCS
     (Ref  : access Generic_VCS_Record;
      Name : String) return GNATCOLL.VFS.Virtual_File;

   overriding function Get_Default_Revision
     (Ref      : access Generic_VCS_Record;
      Revision : Revision_Type) return String;

private

   type Regexp_Status_Record is record
      Regexp : GNAT.Expect.Pattern_Matcher_Access;
      Index  : Natural;
   end record;

   procedure Free (X : in out Regexp_Status_Record);
   --  Free memory associated to X

   package Status_Parser is new GPS_Vectors (Regexp_Status_Record);

   type Status_Parser_Record is record
      Regexp               : GNAT.Expect.Pattern_Matcher_Access;
      Matches_Num          : Natural := 0;
      Status_Identifiers   : Status_Parser.Vector;

      File_Index           : Natural := 0;
      Status_Index         : Natural := 0;
      Local_Rev_Index      : Natural := 0;
      Repository_Rev_Index : Natural := 0;
      Author_Index         : Natural := 0;
      Date_Index           : Natural := 0;
      Log_Index            : Natural := 0;
      Sym_Index            : Natural := 0;
      Pattern              : GNAT.Strings.String_Access;
   end record;

   procedure Free (S : in out Status_Parser_Record);
   --  Free memory associated to X

   type Generic_VCS_Record is new VCS_Record with record
      Id                  : GNAT.Strings.String_Access;
      Administrative_Dir  : Filesystem_String_Access;
      --  Name of the directory where the external VCS keeps information
      Commands            : Action_Array;

      Current_Query_Files         : GNATCOLL.VFS.File_Array_Access;
      --  The files transmitted to the current "query status" command.
      --  It is only needed to store these when the status parser for the
      --  current query cannot locate files (ie File_Index = 0).

      Status                      : Status_Array_Access;

      Status_Parser               : Status_Parser_Record;
      Local_Status_Parser         : Status_Parser_Record;
      Update_Parser               : Status_Parser_Record;
      Annotations_Parser          : Status_Parser_Record;
      Log_Parser                  : Status_Parser_Record;
      Revision_Parser             : Status_Parser_Record;
      Parent_Revision_Regexp      : Pattern_Matcher_Access;
      Branch_Root_Revision_Regexp : Pattern_Matcher_Access;
   end record;

end VCS.Generic_VCS;
