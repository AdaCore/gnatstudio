-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007                      --
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

--  This package provides an generic derivation of VCS_Record.
--
--  See package VCS for a complete spec of this package.

with Ada.Unchecked_Deallocation;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Strings;

package VCS.Generic_VCS is

   type Generic_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Generic_VCS repository

   type Generic_VCS_Access is access all Generic_VCS_Record'Class;

   function Name (Ref : access Generic_VCS_Record) return String;

   procedure Free (Ref : access Generic_VCS_Record);

   procedure Get_Status
     (Rep        : access Generic_VCS_Record;
      Filenames  : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   procedure Get_Status_Dirs
     (Rep        : access Generic_VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   procedure Get_Status_Dirs_Recursive
     (Rep        : access Generic_VCS_Record;
      Dirs       : String_List.List;
      Clear_Logs : Boolean := False;
      Local      : Boolean := False);

   function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List) return File_Status_List.List;

   procedure Create_Tag
     (Rep       : access Generic_VCS_Record;
      Dir       : VFS.Virtual_File;
      Tag       : String;
      As_Branch : Boolean);

   procedure Open
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      User_Name : String := "");

   procedure Commit
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String);

   procedure Update
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Switch
     (Rep : access Generic_VCS_Record;
      Dir : VFS.Virtual_File;
      Tag : String);

   procedure Resolved
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Tag       : String);

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String;
      Commit    : Boolean := True);

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure File_Revision
     (Rep      : access Generic_VCS_Record;
      File     : VFS.Virtual_File;
      Revision : String);

   procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Diff_Patch
     (Rep    : access Generic_VCS_Record;
      File   : VFS.Virtual_File;
      Output : VFS.Virtual_File);

   procedure Diff_Base_Head
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Diff_Working
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Diff_Tag
     (Rep      : access Generic_VCS_Record;
      File     : VFS.Virtual_File;
      Tag_Name : String);

   procedure Log
     (Rep     : access Generic_VCS_Record;
      File    : VFS.Virtual_File;
      Rev     : String;
      As_Text : Boolean := True);

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Parse_Status
     (Rep        : access Generic_VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean;
      Dir        : String);

   procedure Parse_Annotations
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Text : String);

   procedure Parse_Log
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Text : String);

   procedure Parse_Revision
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Text : String);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VCS.Generic_VCS module

   function Get_Identified_Actions
     (Rep : access Generic_VCS_Record) return Action_Array;

   function Get_Registered_Status
     (Rep : access Generic_VCS_Record) return Status_Array;

private

   type Pattern_Matcher_Access is access Pattern_Matcher;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type Regexp_Status_Record is record
      Regexp : Pattern_Matcher_Access;
      Index  : Natural;
   end record;

   procedure Free (X : in out Regexp_Status_Record);
   --  Free memory associated to X

   package Status_Parser is new Generic_List (Regexp_Status_Record);

   type Status_Parser_Record is record
      Regexp               : Pattern_Matcher_Access;
      Matches_Num          : Natural := 0;
      Status_Identifiers   : Status_Parser.List;

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

   type Generic_VCS_Record is new VCS_Record with record
      Id       : GNAT.Strings.String_Access;
      Commands : Action_Array;
      Labels   : Action_Array;

      Current_Query_Files         : String_List_Utils.String_List.List;
      --  The files transmitted to the current "query status" command.
      --  It is only needed to store these when the status parser for the
      --  current query cannot locate files (ie File_Index = 0).

      Status                      : Status_Array_Access;

      Status_Parser               : Status_Parser_Record;
      Local_Status_Parser         : Status_Parser_Record;
      Annotations_Parser          : Status_Parser_Record;
      Log_Parser                  : Status_Parser_Record;
      Revision_Parser             : Status_Parser_Record;
      Parent_Revision_Regexp      : Pattern_Matcher_Access;
      Branch_Root_Revision_Regexp : Pattern_Matcher_Access;
   end record;

end VCS.Generic_VCS;
