-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This package provides low-level utilities to handle differences between
--  files.

with Ada.Text_IO;              use Ada.Text_IO;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNAT.Expect;              use GNAT.Expect;
with GNAT.Regpat;              use GNAT.Regpat;
pragma Warnings (Off);
with GNAT.Expect.TTY;          use GNAT.Expect.TTY;
pragma Warnings (On);

with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Basic_Types;
with Generic_List;
with String_Utils;             use String_Utils;
with Traces;                   use Traces;

with Vdiff2_Module;            use Vdiff2_Module;
with Ada.Exceptions; use Ada.Exceptions;

package body Diff_Utils2 is
   use Diff_Chunk_List;
   use Diff_Head_List;

   Me : constant Debug_Handle := Create ("diff_utils2");

   type Ptr_Match_Array is access all Match_Array;
   type String_Array is array (Natural range <>) of String_Access;
   type Match_Vector is array (Natural range <>) of Ptr_Match_Array;

   type Diff3_Block is record
      S_Chunk : String_Array (0 .. 3);
      Matches_Chunk : Match_Vector (0 .. 3);
   end record;

   procedure Free (Vect : in out Diff3_Block);
   --  Free all content of Vect.

   procedure Compute3_Occurrence
     (Ret        : in out Diff_List;
      Occurrence : out Diff_Chunk_Access;
      Chunk      : Diff3_Block);
   --  Given a string matching an occurrence, compute and append a
   --  Diff_Occurrence in Ret. Ret is the head of the list, Occurrence is
   --  the last item in the list

   procedure Compute_Occurrence
     (Ret        : in out Diff_List;
      Occurrence : out Diff_Chunk_Access;
      S          : String;
      Matches    : Match_Array);
   --  Given a string matching an occurrence, compute and append a
   --  Diff_Occurrence in Ret. Ret is the head of the list, Occurrence is
   --  the last item in the list.

   ------------------------
   -- Compute_Occurrence --
   ------------------------

   procedure Compute_Occurrence
     (Ret        : in out Diff_List;
      Occurrence : out Diff_Chunk_Access;
      S          : String;
      Matches    : Match_Array) is
   begin

      Occurrence := new Diff_Chunk;
      Occurrence.Location := 2;
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
            Occurrence.Range2.Action := Append;
            Occurrence.Range2.Last := Occurrence.Range2.Last + 1;
            Occurrence.Range1.Last := Occurrence.Range1.First;
            Occurrence.Range1.First := Occurrence.Range1.First + 1;

         when 'c' =>
            Occurrence.Range2.Action := Change;
            Occurrence.Range1.Last := Occurrence.Range1.Last + 1;
            Occurrence.Range2.Last := Occurrence.Range2.Last + 1;

         when 'd' =>
            Occurrence.Range2.Action := Delete;
            Occurrence.Range1.Last := Occurrence.Range1.Last + 1;
            Occurrence.Range2.Last := Occurrence.Range2.First;
            Occurrence.Range2.First := Occurrence.Range2.First + 1;

         when others =>
            raise Program_Error;
      end case;

      Append (Ret, Occurrence);

   exception
      when Constraint_Error =>
         null;
   end Compute_Occurrence;

   -------------------------
   -- Compute3_Occurrence --
   -------------------------

   procedure Compute3_Occurrence
     (Ret        : in out Diff_List;
      Occurrence : out Diff_Chunk_Access;
      Chunk      : Diff3_Block)
   is
      Block_Ordered : Diff3_Block := Chunk;
      Rang : T_Loc := 0;
      Loc_Str : String (1 .. 1);
      Location : T_Loc := 0;
      VRange : array (1 .. 3) of Diff_Range;

   begin
      Occurrence := new Diff_Chunk;

      for J in 1 .. 3 loop
         Rang := Natural'Value
           (Chunk.S_Chunk (J).all
              (Chunk.Matches_Chunk (J)(1).First ..
                 Chunk.Matches_Chunk (J)(1).Last));
         Block_Ordered.S_Chunk (Rang) := Chunk.S_Chunk (J);
         Block_Ordered.Matches_Chunk (Rang) := Chunk.Matches_Chunk (J);
      end loop;

      if Chunk.Matches_Chunk (0)(1) = No_Match then
         Loc_Str := "0";
      else
         Loc_Str := Chunk.S_Chunk (0).all
           (Chunk.Matches_Chunk (0)(1).First ..
              Chunk.Matches_Chunk (0)(1).Last);
      end if;

      Location := Natural'Value (Loc_Str);
      Occurrence.Location := Location;

      for K in VRange'Range loop
         VRange (K).First :=
           Natural'Value
             (Block_Ordered.S_Chunk (K)
                (Block_Ordered.Matches_Chunk (K)(2).First ..
                   Block_Ordered.Matches_Chunk (K)(2).Last));

         if Block_Ordered.Matches_Chunk (K)(3) = No_Match then
            VRange (K).Last := VRange (K).First;
         else
            VRange (K).Last := Natural'Value
              (Block_Ordered.S_Chunk (K)
                 (Block_Ordered.Matches_Chunk (K)(4).First ..
                    Block_Ordered.Matches_Chunk (K)(4).Last));
         end if;

         case Block_Ordered.S_Chunk (K)
              (Block_Ordered.Matches_Chunk (K)(5).First) is
            when 'a' =>
               VRange (K).Action := Append;
               VRange (K).First := VRange (K).First + 1;
               VRange (K).Last := VRange (K).First;

            when 'c' =>
               VRange (K).Action := Change;
               VRange (K).Last := VRange (K).Last + 1;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Occurrence.Range1 := VRange (1);
      Occurrence.Range2 := VRange (2);
      Occurrence.Range3 := VRange (3);
      Append (Ret, Occurrence);

   exception
      when Constraint_Error =>
         null;
         --  parse error
         --  unexpected exception
   end Compute3_Occurrence;

   ----------
   -- Diff --
   ----------

   function Diff
     (Ref_File, New_File : String) return Diff_List
   is
      Diff_Command : constant String := "diff";
   begin

      return Diff (Diff_Command, Ref_File, New_File);
   end Diff;

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Ref_File, New_File : String)
      return Diff_List
   is
      Diff_Command : constant String := Get_Pref (Kernel, Diff_Cmd);
   begin

      return Diff (Diff_Command, Ref_File, New_File);
   end Diff;

   ----------
   -- Diff --
   ----------

   function Diff
     (Diff_Command  : String;
      Ref_File, New_File : String) return Diff_List
   is
      Descriptor   : TTY_Process_Descriptor;
      Pattern      : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?.*\n",
          Multiple_Lines);
      Matches      : Match_Array (0 .. 5);
      Args         : Argument_List (1 .. 2);
      Result       : Expect_Match;
      Ret          : Diff_List;
      Occurrence   : Diff_Chunk_Access;
      Cmd          : String_Access;
      Cmd_Args     : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Diff_Command);
      Cmd := Locate_Exec_On_Path (Cmd_Args (Cmd_Args'First).all);
      Args (1) := new String'(Ref_File);
      Args (2) := new String'(New_File);

      Trace (Me, "spawn: " & Diff_Command & " " & New_File & " " & Ref_File);

      Non_Blocking_Spawn
        (Descriptor, Cmd.all,
         Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) & Args);
      Free (Cmd);
      Free (Cmd_Args);
      Basic_Types.Free (Args);

      loop
         Expect (Descriptor, Result, Pattern, Matches, Timeout => -1);
         Compute_Occurrence
           (Ret, Occurrence, Expect_Out (Descriptor), Matches);
      end loop;

   exception

      when Process_Died =>
         Close (Descriptor);
         return Ret;

      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Close (Descriptor);
         return Diff_Chunk_List.Null_List;
   end Diff;

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Orig_File : String;
      New_File  : String;
      Diff_File : String;
      Revert    : Boolean := False) return Diff_List
   is
      Args       : Argument_List (1 .. 6);
      Ret        : Diff_List;
      Occurrence : Diff_Chunk_Access;
      Cmd        : String_Access;
      Pattern    : constant Pattern_Matcher :=
        Compile ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?");
      Matches    : Match_Array (0 .. 5);
      File       : File_Type;
      Success    : Boolean;
      Num_Args   : Natural;
      Buffer     : String (1 .. 8192);
      Last       : Natural;
      Cmd_Args   : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Get_Pref (Kernel, Patch_Cmd));
      Cmd      := Locate_Exec_On_Path (Cmd_Args (Cmd_Args'First).all);
      Args (1) := new String'("-s");
      Args (2) := new String'("-o");

      if Revert then
         Args (3) := new String'(Orig_File);
         Args (4) := new String'("-R");
         Args (5) := new String'(New_File);
         Num_Args := 6;
      else
         Args (3) := new String'(New_File);
         Args (4) := new String'(Orig_File);
         Num_Args := 5;
      end if;

      Args (Num_Args) := new String'(Diff_File);
      Trace (Me, "spawn: " &
             Argument_List_To_String (Cmd_Args.all & Args (1 .. Num_Args)));
      Spawn (Cmd.all, Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last)
             & Args (1 .. Num_Args), Success);
      Free (Cmd);
      Free (Cmd_Args);
      Basic_Types.Free (Args);
      Open (File, In_File, Diff_File);

      while not End_Of_File (File)
      loop
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
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Close (File);
         return Diff_Chunk_List.Null_List;
   end Diff;

   -----------
   -- Diff3 --
   -----------

   procedure Diff3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head) is
   begin
      Free_List (Item.List);
      if Item.File1 /= null and Item.File2 /= null then
         if Item.File3 /= null then
            Item.List := Diff3
              (Kernel, Item.File1.all, Item.File2.all, Item.File3.all);
         else
            Item.List := Diff
              (Kernel, Item.File1.all, Item.File2.all);
         end if;
      end if;
   end Diff3;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      My_Change, Old_File, Your_Change : String)
      return Diff_List
   is
      Diff3_Command  : constant String := Get_Pref (Kernel, Diff3_Cmd);
   begin

      return Diff3 (Diff3_Command, My_Change,
                              Old_File, Your_Change);
   end Diff3;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (My_Change, Old_File, Your_Change : String)
      return Diff_List
   is
      Diff3_Command : constant String := "diff3";
   begin

      return Diff3 (Diff3_Command, My_Change,
                              Old_File, Your_Change);
   end Diff3;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (Diff3_Command  : String;
      My_Change, Old_File, Your_Change : String)
      return Diff_List
   is
      Descriptor     : TTY_Process_Descriptor;
      Pattern_Bloc   : constant Pattern_Matcher :=
        Compile ("^====([1-3])?$",
                 Multiple_Lines);
      Pattern_Chunk  : constant Pattern_Matcher :=
        Compile ("^([1-3]):([0-9]+)(,([0-9]+))?([acd])$",
                 Multiple_Lines);
      Matches_Block  : Match_Array (0 .. 1);
      Matches_Chunk1,
      Matches_Chunk2,
      Matches_Chunk3 : Match_Array (0 .. 5);

      Chunk        : Diff3_Block;
      Args           : Argument_List (1 .. 3);
      Result         : Expect_Match;
      Ret            : Diff_List;
      Occurrence     : Diff_Chunk_Access;
      Cmd            : String_Access;
      Cmd_Args       : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Diff3_Command);
      Cmd := Locate_Exec_On_Path (Cmd_Args (Cmd_Args'First).all);

      Args (1) := new String'(My_Change);
      Args (2) := new String'(Old_File);
      Args (3) := new String'(Your_Change);

      Trace (Me, "spawn: " & Diff3_Command & " " &
             My_Change & " " & Old_File & " " & Your_Change);

      Non_Blocking_Spawn
        (Descriptor, Cmd.all,
         Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) & Args);
      Free (Cmd);
      Free (Cmd_Args);
      Basic_Types.Free (Args);

      loop
         Expect (Descriptor, Result, Pattern_Bloc,  Matches_Block,
                 Timeout => -1);
         Chunk.S_Chunk (0) := new String'(Expect_Out_Match (Descriptor));
         Chunk.Matches_Chunk (0) := new Match_Array'(Matches_Block);

         Expect (Descriptor, Result, Pattern_Chunk,
                 Matches_Chunk1, Timeout => -1);
         Chunk.S_Chunk (1) := new String'(Expect_Out_Match (Descriptor));
         Chunk.Matches_Chunk (1) := new Match_Array'(Matches_Chunk1);

         Expect (Descriptor, Result, Pattern_Chunk,
                 Matches_Chunk2, Timeout => -1);
         Chunk.S_Chunk (2) := new String'(Expect_Out_Match (Descriptor));
         Chunk.Matches_Chunk (2) := new Match_Array'(Matches_Chunk2);

         Expect (Descriptor, Result, Pattern_Chunk,
                 Matches_Chunk3, Timeout => -1);
         Chunk.S_Chunk (3) := new String'(Expect_Out_Match (Descriptor));
         Chunk.Matches_Chunk (3) := new Match_Array'(Matches_Chunk3);

         Compute3_Occurrence (Ret, Occurrence, Chunk);
         Free (Chunk);
      end loop;

   exception
      when Process_Died =>
         Close (Descriptor);
         return Ret;

      when others =>
         --  unexpected exception
         Close (Descriptor);
         return Diff_Chunk_List.Null_List;
   end Diff3;

   --------------
   -- Simplify --
   --------------

   function Simplify
     (Diff : Diff_List;
      Ref_File : T_Loc) return Diff_List
   is
      Ref        : constant T_Loc := Ref_File;
      Curr_Node  : Diff_List_Node;
      Curr_Chunk : Diff_Chunk_Access;
      VRange     : array (1 .. 3) of Diff_Range;
      Res        : Diff_List;

   begin
      Curr_Node := First (Diff);

      while Curr_Node /= Diff_Chunk_List.Null_Node
      loop
         Curr_Chunk := new Diff_Chunk'(Data (Curr_Node).all);
         VRange     :=
           (Curr_Chunk.Range1,
            Curr_Chunk.Range2,
            Curr_Chunk.Range3);

         if Curr_Chunk.Location = Ref then
            case VRange (Ref).Action is
               when Append =>
                  for J in VRange'Range loop
                     if J /= Ref then
                        VRange (J).Action := Append;
                     end if;
                  end loop;

               when Change =>
                  for I in VRange'Range loop
                     if I /= Ref then
                        if VRange (I).Action = Append then
                           VRange (I).Action := Delete;
                        end if;
                     end if;
                  end loop;
                  --  ???I think that not a conflict

               when others =>
                  null;
            end case;

            Curr_Chunk.Location := 0;

         elsif Curr_Chunk.Location = 0 then
            Curr_Chunk.Conflict := true;

            for I in VRange'Range loop

               if I /= Ref and then VRange (I).Action = Append then
                  VRange (I).Action := Delete;
               end if;

               if I /= Ref and then VRange (Ref).Action = Append then
                  VRange (I).Action := Append;
               end if;
            end loop;

         else
            if VRange (Curr_Chunk.Location).Action = Append then
               VRange (Curr_Chunk.Location).Action := Delete;
            elsif VRange (Ref).Action = Append then
               VRange (Curr_Chunk.Location).Action := Append;
            end if;

            for I in VRange'Range loop
               if I /= Ref and I /= Curr_Chunk.Location then
                  VRange (I).Action := Nothing;
               end if;
            end loop;

         end if;

         VRange (Ref).Action := Nothing;
         Curr_Chunk.Range1 := VRange (1);
         Curr_Chunk.Range2 := VRange (2);
         Curr_Chunk.Range3 := VRange (3);
         Append (Res, Curr_Chunk);
         Curr_Node  := Next (Curr_Node);
      end loop;
      return Res;
   end Simplify;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Link : in out Diff_List)
   is
      Curr_Node  : Diff_List_Node;
      Curr_Chunk : Diff_Chunk_Access;

   begin
      Curr_Node := First (Link);

      while Curr_Node /= Diff_Chunk_List.Null_Node
      loop
         Curr_Chunk := Data (Curr_Node);
         Free (Curr_Chunk.Range1.Mark);
         Free (Curr_Chunk.Range2.Mark);
         Free (Curr_Chunk.Range3.Mark);
         Free (Curr_Chunk.Range1.Blank_Lines);
         Free (Curr_Chunk.Range2.Blank_Lines);
         Free (Curr_Chunk.Range3.Blank_Lines);
         Curr_Node := Next (Curr_Node);
      end loop;

      Free (Link, True);
   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (Link : in out Diff_Head) is
   pragma Unreferenced (Link);
   begin
      null; -- ???
   end Free;

   --------------
   -- Free_All --
   --------------

   procedure Free_All (Link : in out Diff_Head) is
   begin
      Free_List (Link.List);
      Free (Link.File1);
      Free (Link.File2);
      Free (Link.File3);
   end Free_All;

   ----------
   -- Free --
   ----------

   procedure Free (Vect : in out Diff3_Block)
   is
      procedure Internal_Free is new
        Ada.Unchecked_Deallocation (Match_Array, Ptr_Match_Array);
   begin

      for J in Vect.Matches_Chunk'Range loop
         Internal_Free (Vect.Matches_Chunk (J));
      end loop;

      for J in Vect.S_Chunk'Range loop
         Free (Vect.S_Chunk (J));
      end loop;
   end Free;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (List : in out Diff_Head_List.List)
   is
      Curr_Node : Diff_Head_List.List_Node := First (List);
      Diff : Diff_Head;
   begin

      while Curr_Node /= Diff_Head_List.Null_Node
      loop
         Diff := Data (Curr_Node);
         Free_All (Diff);
         Curr_Node := Next (Curr_Node);
      end loop;

   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (V : in out T_VStr) is
   begin
      for J in V'Range loop
         Free (V (J));
      end loop;
   end Free;


end Diff_Utils2;





