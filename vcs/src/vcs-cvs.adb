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

package body VCS.CVS is

   -----------
   -- Close --
   -----------

   procedure Close (Rep : access CVS_Record) is
   begin
      Ada.Text_IO.Close (Ada.Text_IO.File_Type (Rep.Dir));
   end Close;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Rep : access CVS_Record) return Boolean is
   begin
      return Ada.Text_IO.Is_Open (Ada.Text_IO.File_Type (Rep.Dir));
   end Is_Open;

   ----------
   -- Open --
   ----------

   procedure Open (Rep : access CVS_Record; Dir_Name : in String) is
      File_Name : String := Dir_Name & "/CVS/Entries";
   begin
      Ada.Text_IO.Open
        (Ada.Text_IO.File_Type (Rep.Dir), Ada.Text_IO.In_File, File_Name);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Rep : access CVS_Record;
      Ent : out VCS_Entry)
   is
      Buffer : String (1 .. 2048);
      Len    : Natural;
      Ind    : Natural;
      Ind2   : Natural;

   begin
      Ada.Text_IO.Get_Line (Ada.Text_IO.File_Type (Rep.Dir), Buffer, Len);
      Ind := Index (Buffer (1 .. Len), "/");
      Ent.Is_Dir := Buffer (1 .. Ind - 1) = "D";
      Ind2 := Index (Buffer (Ind + 1 .. Len), "/");
      Ent.Filename_Len := Ind2 - Ind - 1;
      Ent.Filename (1 .. Ent.Filename_Len) := Buffer (Ind + 1 .. Ind2 - 1);
      Ind := Ind2;
      Ind2 := Index (Buffer (Ind + 1 .. Len), "/");
      Ent.Version_Len := Ind2 - Ind - 1;
      Ent.Version (1 .. Ent.Version_Len) := Buffer (Ind + 1 .. Ind2 - 1);
      Ind := Ind2;
      Ind2 := Index (Buffer (Ind + 1 .. Len), "/");
      Ent.Date_Len := Ind2 - Ind - 1;
      Ent.Date (1 .. Ent.Date_Len) := Buffer (Ind + 1 .. Ind2 - 1);
   exception
      when others =>
         Ent.Filename_Len := 0;
   end Read;

   ---------
   -- Add --
   ---------

   procedure Add (Rep : access CVS_Record; Name : String) is
      Args : Argument_List :=
        (new String' ("add"),
         new String' (Name));
      Success : Boolean;

   begin
      Spawn ("/usr/bin/cvs", Args, Success);

      for J in Args'Range loop
         Free (Args (J));
      end loop;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (Rep : access CVS_Record; Name : String) is
   begin
      raise Program_Error;
   end Remove;

   ------------
   -- Commit --
   ------------

   procedure Commit 
     (Rep  : access CVS_Record;
      Name : String; 
      Log  : String) is
   begin
      raise Program_Error;
   end Commit;

   --------------
   -- Checkout --
   --------------

   procedure Checkout (Rep : access CVS_Record; Name : String) is
   begin
      raise Program_Error;
   end Checkout;

end VCS.CVS;
