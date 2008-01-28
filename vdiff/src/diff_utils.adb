-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2008, AdaCore                 --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Ada.Text_IO;            use Ada.Text_IO;

with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;        use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;            use GNAT.Regpat;

with Basic_Types;
with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Console;     use GPS.Kernel.Console;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with String_Utils;           use String_Utils;
with Traces;                 use Traces;
with VFS;                    use VFS;

package body Diff_Utils is
   use Diff_Occurrence_List;
   Me : constant Debug_Handle := Create ("diff_utils");
   procedure Free_All (Link : in out Diff_List_Head);
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
         Occurrence.Next.Prev := Occurrence;
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

   function Diff
     (File1, File2 : VFS.Virtual_File) return Diff_Occurrence_Link
   is
      Descriptor   : TTY_Process_Descriptor;
      Pattern      : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?.*\n",
          Multiple_Lines);
      Matches      : Match_Array (0 .. 5);
      Args         : Argument_List (1 .. 2);
      Result       : Expect_Match;
      Ret          : Diff_Occurrence_Link;
      Occurrence   : Diff_Occurrence_Link;
      Diff_Command : constant String := Get_Pref (Diff_Cmd);
      Cmd          : String_Access;
      Cmd_Args     : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Diff_Command);
      Cmd := Locate_Exec_On_Path (Unquote (Cmd_Args (Cmd_Args'First).all));

      if Cmd.all = "" then
         Trace (Me, "command not found: " & Diff_Command);
         Free (Cmd);
         Free (Cmd_Args);
         return Ret;
      end if;

      Args (1) := new String'(Full_Name (File1).all);
      Args (2) := new String'(Full_Name (File2).all);

      Trace (Me, "spawn: " & Diff_Command & " "
             & Full_Name (File1).all & " " & Full_Name (File2).all);

      Non_Blocking_Spawn
        (Descriptor, Cmd.all,
         Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) & Args);
      Free (Cmd);
      Free (Cmd_Args);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      loop
         --  ??? Should register a timer instead of blocking the whole process

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

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Orig_File : VFS.Virtual_File;
      New_File  : VFS.Virtual_File;
      Diff_File : VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_Occurrence_Link
   is
      Patch_Command : constant String := Get_Pref (Patch_Cmd);
      Descriptor    : TTY_Process_Descriptor;
      Args          : Argument_List (1 .. 6);
      Ret           : Diff_Occurrence_Link;
      Occurrence    : Diff_Occurrence_Link;
      Cmd           : String_Access;
      Pattern_Any   : constant Pattern_Matcher := Compile (".+");
      Pattern       : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?");
      Matches       : Match_Array (0 .. 5);
      Result        : Expect_Match;
      File          : File_Type;
      Num_Args      : Natural;
      Buffer        : String (1 .. 8192);
      Last          : Natural;
      Cmd_Args      : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Patch_Command);
      Cmd      :=
        Locate_Exec_On_Path (Unquote (Cmd_Args (Cmd_Args'First).all));

      if Cmd = null or else Cmd.all = "" then
         Insert (Kernel,
                 -"Patch command not found: " & Patch_Command & ASCII.LF
                 & (-"See the preferences if you need to change the value"),
                 Mode => Error);
         Trace (Me, "command not found: " & Patch_Command);
         Free (Cmd);
         Free (Cmd_Args);
         return Ret;
      end if;

      Args (1) := new String'("-s");
      Args (2) := new String'("-o");

      if Revert then
         Args (3) := new String'(Full_Name (Orig_File).all);
         Args (4) := new String'("-R");
         Args (5) := new String'(Full_Name (New_File).all);
         Num_Args := 6;

      else
         Args (3) := new String'(Full_Name (New_File).all);
         Args (4) := new String'(Full_Name (Orig_File).all);
         Num_Args := 5;
      end if;

      Args (Num_Args) := new String'(Full_Name (Diff_File).all);

      Trace (Me, "spawn: " &
             Argument_List_To_String (Cmd_Args.all & Args (1 .. Num_Args)));

      begin
         Non_Blocking_Spawn
           (Descriptor, Cmd.all,
            Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) &
            Args (1 .. Num_Args));
         Free (Cmd);
         Free (Cmd_Args);
         Basic_Types.Free (Args);

         loop
            Expect (Descriptor, Result, Pattern_Any, Matches, Timeout => -1);
         end loop;

      exception
         when others =>
            Close (Descriptor);
      end;

      Open (File, In_File, Locale_Full_Name (Diff_File));

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

   function Fine_Diff (Line1, Line2 : String) return Diff_Occurrence_Link is
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
               Tmp.Next.Prev := Tmp;
               Tmp := Tmp.Next;
            end if;

            Tmp.Range1 := (First, Last, null);

         else
            if First = 0 then
               First := J;
            end if;
         end if;
      end loop;

      return Result;
   end Fine_Diff;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (File1, File2, File3 : VFS.Virtual_File) return Diff_Pair
   is
      Result : Diff_Pair;
   begin
      Result.List21 := Diff (File1 => File2, File2 => File1);
      Result.List23 := Diff (File2, File3);
      return Result;
   end Diff3;

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
         Free (Tmp.Range1.Mark);
         Free (Tmp.Range2.Mark);
         Internal_Free (Tmp);
      end loop;
   end Free;

   --------------
   -- Free_All --
   --------------

   procedure Free_All (Link : in out Diff_List_Head) is

   begin
      Free (Link.List);
      Free (Link.File1);
      Free (Link.File2);
      Free (Link.File3);
   end Free_All;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (List : in out Diff_Occurrence_List.List) is
      CurrNode : Diff_Occurrence_List.List_Node :=
        First (List);
      Diff : Diff_List_Head;
   begin

      while CurrNode /= Null_Node
      loop
         Diff := Data (CurrNode);
         Free_All (Diff);
         CurrNode := Next (CurrNode);
      end loop;
   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (Link : in out Diff_List_Head) is
   pragma Unreferenced (Link);

   begin
      null;
      --  ??? just for the moment
   end Free;

end Diff_Utils;
