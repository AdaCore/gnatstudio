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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect; use GNAT.Expect;
package body VCS.P4 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Set_Message
     (Rep : access P4_Record;
      M   : String);
   --  Sets the internal error message to M.

   -----------------
   -- Set_Message --
   -----------------

   procedure Set_Message
     (Rep : access P4_Record;
      M   : String)
   is
   begin
      if Rep.Message /= null then
         Free (Rep.Message);
      end if;
      Rep.Message := new String'(M);
   end Set_Message;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Rep         : access P4_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean          := True;
      Get_Version : Boolean          := True;
      Get_Tags    : Boolean          := False;
      Get_Users   : Boolean          := False)
     return File_Status_List.List
   is
      Result : File_Status_List.List;
   begin
      return Result;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep         : access P4_Record;
      Filenames   :        String_List.List)
     return File_Status_List.List
   is
      Result : File_Status_List.List;
   begin
      return Result;
   end Local_Get_Status;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access P4_Record;
      Name      : String;
      User_Name : String := "")
   is
   begin
      null;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep  : access P4_Record;
      Name : String;
      Log  : String) is
   begin
      null;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update (Rep : access P4_Record; Name : String) is
   begin
      null;
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge (Rep : access P4_Record; Name : String)
   is
   begin
      null;
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add (Rep : access P4_Record; Name : String)
   is
   begin
      null;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (Rep : access P4_Record; Name : String)
   is
   begin
      null;
   end Remove;

   ----------
   -- Diff --
   ----------

   function Diff
     (Rep       : access P4_Record;
      File_Name : String;
      Version_1 : String := "";
      Version_2 : String)
     return String_List.List
   is
      Result : String_List.List;
   begin
      return Result;
   end Diff;

   ---------
   -- Log --
   ---------

   function Log
      (Rep       : access P4_Record;
       File_Name : String)
      return String_List.List
   is
      Result : String_List.List;
   begin
      return Result;
   end Log;

   -------------
   -- Success --
   -------------

   function Success (Rep : access P4_Record) return Boolean
   is
   begin
      return False;
   end Success;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Rep : access P4_Record) return String
   is
   begin
      return "";
   end Get_Message;

end VCS.P4;
