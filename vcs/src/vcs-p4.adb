-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

pragma Warnings (Off);

package body VCS.P4 is

   ---------
   -- Add --
   ---------

   procedure Add (Rep : access P4_Record; Filenames : String_List.List) is
   begin
      null;
   end Add;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access P4_Record;
      Filenames : String_List.List;
      Logs      : String_List.List) is
   begin
      null;
   end Commit;

   ----------
   -- Diff --
   ----------

   function Diff
     (Rep       : access P4_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "") return String_List.List is
   begin
      return Diff (Rep, File, Version_1, Version_2);
   end Diff;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Rep : access P4_Record) return String is
   begin
      return Get_Message (Rep);
   end Get_Message;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Rep         : access P4_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean := True;
      Get_Version : Boolean := True;
      Get_Tags    : Boolean := False;
      Get_Users   : Boolean := False) return File_Status_List.List
   is
      Result : File_Status_List.List;
   begin
      return Result;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep       : access P4_Record;
      Filenames : String_List.List) return File_Status_List.List is
   begin
      return Local_Get_Status (Rep, Filenames);
   end Local_Get_Status;

   ---------
   -- Log --
   ---------

   function Log
     (Rep  : access P4_Record;
      File : String) return String_List.List is
   begin
      return Log (Rep, File);
   end Log;

   --------------
   -- Annotate --
   --------------

   function Annotate
     (Rep  : access P4_Record;
      File : String) return String_List.List is
   begin
      return Annotate (Rep, File);
   end Annotate;

   -----------
   -- Merge --
   -----------

   procedure Merge (Rep : access P4_Record; Filenames : String_List.List) is
   begin
      null;
   end Merge;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access P4_Record;
      Filenames : String_List.List;
      User_Name : String := "") is
   begin
      null;
   end Open;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access P4_Record;
      Filenames : String_List.List) is
   begin
      null;
   end Remove;

   -------------
   -- Success --
   -------------

   function Success (Rep : access P4_Record) return Boolean is
   begin
      return Success (Rep);
   end Success;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access P4_Record;
      Filenames : String_List.List) is
   begin
      null;
   end Update;

   ----------------------------
   -- Register_Idle_Function --
   ----------------------------

   procedure Register_Idle_Function
     (Rep     : access P4_Record;
      Func    : Idle_Function;
      Timeout : Integer := 100) is
   begin
      null;
   end Register_Idle_Function;

end VCS.P4;

