-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003 - 2004                     --
--                            ACT-Europe                             --
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

with Basic_Types;               use Basic_Types;
with GNAT.Regpat;               use GNAT.Regpat;
with Ada.Unchecked_Deallocation;

package VCS.Generic_VCS is

   type Generic_VCS_Record is new VCS_Record with private;
   --  A value used to reference a Generic_VCS repository.

   type Generic_VCS_Access is access all Generic_VCS_Record'Class;

   function Name (Ref : access Generic_VCS_Record) return String;

   procedure Free (Ref : access Generic_VCS_Record);

   procedure Get_Status
     (Rep         : access Generic_VCS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False;
      Local       : Boolean := False);

   procedure Get_Status_Dirs
     (Rep         : access Generic_VCS_Record;
      Dirs        : String_List.List;
      Clear_Logs  : Boolean := False;
      Local       : Boolean := False);

   function Local_Get_Status
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List)
     return File_Status_List.List;

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

   procedure Merge
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Add
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String);

   procedure Remove
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List;
      Log       : String);

   procedure Revert
     (Rep       : access Generic_VCS_Record;
      Filenames : String_List.List);

   procedure Diff
     (Rep       : access Generic_VCS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "");

   procedure Diff_Base_Head
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Log
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File;
      Rev  : String);

   procedure Annotate
     (Rep  : access Generic_VCS_Record;
      File : VFS.Virtual_File);

   procedure Parse_Status
     (Rep        : access Generic_VCS_Record;
      Text       : String;
      Local      : Boolean;
      Clear_Logs : Boolean);

   procedure Parse_Annotations
     (Rep   : access Generic_VCS_Record;
      File  : VFS.Virtual_File;
      Text  : String);

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
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
   --  Free memory associated to X.

   package Status_Parser is new Generic_List
     (Regexp_Status_Record);

   type Status_Parser_Record is record
      Regexp             : Pattern_Matcher_Access;
      Matches_Num        : Natural := 0;
      Status_Identifiers : Status_Parser.List;

      File_Index           : Natural := 0;
      Status_Index         : Natural := 0;
      Local_Rev_Index      : Natural := 0;
      Repository_Rev_Index : Natural := 0;
   end record;

   type Generic_VCS_Record is new VCS_Record with record
      Id       : String_Access;
      Commands : Action_Array;
      Labels   : Action_Array;

      Current_Query_Files : String_List_Utils.String_List.List;
      --  The files transmitted to the current "query status" command.
      --  It is only needed to store these when the status parser for the
      --  current query cannot locate files (ie File_Index = 0).

      Status              : Status_Array_Access;

      Status_Parser       : Status_Parser_Record;
      Local_Status_Parser : Status_Parser_Record;
      Annotations_Parser  : Status_Parser_Record;

      Absolute_Names       : Boolean := True;
   end record;

end VCS.Generic_VCS;
