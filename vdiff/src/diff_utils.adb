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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect; use GNAT.Expect;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Diff_Utils is

   Diff_Cmd  : constant String := "diff";
   Patch_Cmd : constant String := "patch";
   --  <preferences>

   procedure Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Process_Descriptor'Class, Process_Descriptor_Access);

   procedure Compute_Occurrence
     (Ret        : in out Diff_Occurrence_Link;
      Occurrence : in out Diff_Occurrence_Link;
      S          : String;
      Matches    : Match_Array);
   --  Given a string matching an occurrence, compute and append a
   --  Diff_Occurrence in Ret. Ret is the head of the list, Occurrence is
   --  the last item in the list.

   procedure Compute_Occurrence
     (Ret        : in out Diff_Occurrence_Link;
      Occurrence : in out Diff_Occurrence_Link;
      S          : String;
      Matches    : Match_Array) is
   begin
      if Ret = null then
         Ret := new Diff_Occurrence;
         Occurrence := Ret;
      else
         Occurrence.Next := new Diff_Occurrence;
         Occurrence := Occurrence.Next;
      end if;

      Occurrence.Range1.First :=
        Natural'Value (S (Matches (1).First .. Matches (1).Last));

      if Matches (2) = No_Match then
         Occurrence.Range1.Last := Occurrence.Range1.First;
      else
         Occurrence.Range1.Last := Natural'Value
           (S (Matches (2).First + 1 .. Matches (2).Last));
      end if;

      Occurrence.Range2.First :=
        Natural'Value (S (Matches (4).First .. Matches (4).Last));

      if Matches (5) = No_Match then
         Occurrence.Range2.Last := Occurrence.Range2.First;
      else
         Occurrence.Range2.Last := Natural'Value
           (S (Matches (5).First + 1 .. Matches (5).Last));
      end if;

      --  Fix the beginning and end numbers and set action appropriately

      case S (Matches (3).First) is
         when 'a' =>
            Occurrence.Action := Append;
            Occurrence.Range2.Last := Occurrence.Range2.Last + 1;
            Occurrence.Range1.Last := Occurrence.Range1.First;
            Occurrence.Range1.First := Occurrence.Range1.First + 1;

         when 'c' =>
            Occurrence.Action := Change;
            Occurrence.Range1.Last := Occurrence.Range1.Last + 1;
            Occurrence.Range2.Last := Occurrence.Range2.Last + 1;

         when 'd' =>
            Occurrence.Action := Delete;
            Occurrence.Range1.Last := Occurrence.Range1.Last + 1;
            Occurrence.Range2.Last := Occurrence.Range2.First;
            Occurrence.Range2.First := Occurrence.Range2.First + 1;

         when others =>
            raise Program_Error;
      end case;

   exception
      when Constraint_Error =>
         null;
   end Compute_Occurrence;

   ----------
   -- Diff --
   ----------

   function Diff (File1, File2 : String) return Diff_Occurrence_Link is
      Descriptor : Process_Descriptor;
      Pattern    : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?$",
          Multiple_Lines);
      Matches    : Match_Array (0 .. 5);
      Args       : Argument_List (1 .. 2);
      Result     : Expect_Match;
      Ret        : Diff_Occurrence_Link;
      Occurrence : Diff_Occurrence_Link;
      Cmd        : String_Access;

   begin
      Cmd := Locate_Exec_On_Path (Diff_Cmd);
      Args (1) := new String' (File1);
      Args (2) := new String' (File2);
      Non_Blocking_Spawn (Descriptor, Cmd.all, Args);
      Free (Cmd);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      loop
         --  loop for Gtk+ events ???
         --  while Gtk.Main.Events_Pending loop
         --     Dead := Main_Iteration;
         --  end loop;

         Expect (Descriptor, Result, Pattern, Matches, Timeout => -1);
         Compute_Occurrence
           (Ret, Occurrence, Expect_Out (Descriptor), Matches);
      end loop;

   exception
      when Process_Died =>
         Close (Descriptor);
         return Ret;

      when others =>
         --  unexpected exception
         Close (Descriptor);
         return Ret;
   end Diff;

   function Diff
     (Orig_File : String;
      New_File  : String;
      Diff_File : String;
      Revert    : Boolean := False) return Diff_Occurrence_Link
   is
      Args       : Argument_List (1 .. 7);
      Ret        : Diff_Occurrence_Link;
      Occurrence : Diff_Occurrence_Link;
      Cmd        : String_Access;
      Pattern    : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?");
      Matches    : Match_Array (0 .. 5);
      File       : File_Type;
      Success    : Boolean;
      Num_Args   : Natural;
      Buffer     : String (1 .. 8192);
      Last       : Natural;

   begin
      Cmd      := Locate_Exec_On_Path (Patch_Cmd);
      Args (1) := new String' ("-s");
      Args (2) := new String' ("-i");
      Args (3) := new String' (Diff_File);
      Args (4) := new String' ("-o");

      if Revert then
         Args (5) := new String' (Orig_File);
         Args (6) := new String' ("-R");
         Args (7) := new String' (New_File);
         Num_Args := 7;

      else
         Args (5) := new String' (New_File);
         Args (6) := new String' (Orig_File);
         Num_Args := 6;
      end if;

      Spawn (Cmd.all, Args (1 .. Num_Args), Success);
      Free (Cmd);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      Open (File, In_File, Diff_File);

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         Match (Pattern, Buffer (1 .. Last), Matches);

         if Matches (0) /= No_Match then
            Compute_Occurrence
              (Ret, Occurrence, Buffer (1 .. Last), Matches);
         end if;
      end loop;

      Close (File);
      return Ret;

   exception
      when others =>
         --  unexpected exception
         return Ret;
   end Diff;

   ---------------
   -- Fine_Diff --
   ---------------

   function Fine_Diff
     (Line1, Line2 : String) return Diff_Occurrence_Link
   is
      Result, Tmp : Diff_Occurrence_Link;
      First, Last : Natural := 0;

   begin
      for J in Line1'Range loop
         if Line1 (J) = Line2 (J) then
            if First /= 0 then
               Last := J - 1;
            end if;

            if Result = null then
               Result := new Diff_Occurrence;
               Tmp := Result;
            else
               Tmp.Next := new Diff_Occurrence;
               Tmp := Tmp.Next;
            end if;

            Tmp.Range1 := (First, Last);

         else
            if First = 0 then
               First := J;
            end if;
         end if;
      end loop;

      return Result;
   end Fine_Diff;

   ----------
   -- Free --
   ----------

   procedure Free (Link : in out Diff_Occurrence_Link) is
      First, Tmp : Diff_Occurrence_Link;

      procedure Internal_Free is new
        Ada.Unchecked_Deallocation (Diff_Occurrence, Diff_Occurrence_Link);

   begin
      First := Link;
      Link := null;

      loop
         exit when First = null;
         Tmp := First;
         First := First.Next;
         Internal_Free (Tmp);
      end loop;
   end Free;

end Diff_Utils;
