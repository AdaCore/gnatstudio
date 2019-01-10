------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Text_IO;            use Ada.Text_IO;

with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;        use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Regpat;            use GNAT.Regpat;
with GNATCOLL.Arg_Lists;     use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;         use GNATCOLL.Utils;

with Toolchains_Old;         use Toolchains_Old;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with String_Utils;           use String_Utils;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with Vdiff2_Module;          use Vdiff2_Module;

with String_Diff;            use String_Diff;

package body Diff_Utils2 is

   use Diff_Chunk_List;
   use Diff_Head_List;

   Me : constant Trace_Handle := Create ("GPS.VDIFF.DIFF_UTILS2");

   type Ptr_Match_Array is access all Match_Array;
   type String_Array is array (Natural range <>) of String_Access;
   type Match_Vector is array (Natural range <>) of Ptr_Match_Array;

   type Diff3_Block is record
      S_Chunk       : String_Array (0 .. 3);
      Matches_Chunk : Match_Vector (0 .. 3);
   end record;

   procedure Free (Vect : in out Diff3_Block);
   --  Free all content of Vect

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
      Rang          : T_Loc := 0;
      Loc_Str       : String (1 .. 1);
      Location      : T_Loc := 0;
      VRange        : array (1 .. 3) of Diff_Range;

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
         --  ??? parse error
         --  ??? unexpected exception
   end Compute3_Occurrence;

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Ref_File, New_File : GNATCOLL.VFS.Virtual_File) return Diff_List
   is
      Diff_Command : constant String := Diff_Cmd.Get_Pref;
   begin
      return Diff (Kernel, Diff_Command, Ref_File, New_File);
   end Diff;

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Diff_Command       : String;
      Ref_File, New_File : GNATCOLL.VFS.Virtual_File) return Diff_List
   is
      Pattern    : constant Pattern_Matcher := Compile
        ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?.*\n", Multiple_Lines);
      Descriptor : TTY_Process_Descriptor;
      Matches    : Match_Array (0 .. 5);
      Args       : Argument_List (1 .. 2);
      Result     : Expect_Match;
      Ret        : Diff_List;
      Occurrence : Diff_Chunk_Access;
      Cmd        : Virtual_File;
      Cmd_Args   : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Diff_Command);
      Cmd := Locate_Tool_Executable (+Unquote (Cmd_Args (Cmd_Args'First).all));

      if Cmd = No_File then
         Kernel.Insert
           ("command not found: " & Diff_Command &
            ". You should modify the ""Visual Diff"" preferences",
            Mode => Error);
         Free (Cmd_Args);
         return Ret;
      end if;

      Args (1) := new String'(+Full_Name (Ref_File));
      Args (2) := new String'(+Full_Name (New_File));

      if Active (Me) then
         Trace (Me, "spawn: " & Diff_Command
                & " " & (+Full_Name (New_File))
                & " " & (+Full_Name (Ref_File)));
      end if;

      begin
         Non_Blocking_Spawn
           (Descriptor, +Cmd.Full_Name,
            Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) & Args);
         Free (Cmd_Args);
         Free (Args);

         loop
            Expect (Descriptor, Result, Pattern, Matches, Timeout => -1);
            Compute_Occurrence
              (Ret, Occurrence, Expect_Out (Descriptor), Matches);
         end loop;

      exception
         when Process_Died =>
            Close (Descriptor);
            return Ret;
      end;

   exception
      when E : others =>
         Trace (Me, E);
         Close (Descriptor);
         return Diff_Chunk_List.Empty_Vector;
   end Diff;

   ----------
   -- Diff --
   ----------

   function Diff
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Orig_File : GNATCOLL.VFS.Virtual_File;
      New_File  : GNATCOLL.VFS.Virtual_File;
      Diff_File : GNATCOLL.VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_List
   is
      Patch_Command : constant String := Patch_Cmd.Get_Pref;
      Pattern_Any   : constant Pattern_Matcher := Compile (".+");
      Pattern       : constant Pattern_Matcher := Compile
        ("^([0-9]+)(,[0-9]+)?([acd])([0-9]+)(,[0-9]+)?");
      Args          : Argument_List (1 .. 7);
      Descriptor    : TTY_Process_Descriptor;
      Ret           : Diff_List;
      Occurrence    : Diff_Chunk_Access;
      Cmd           : Virtual_File;
      Matches       : Match_Array (0 .. 5);
      Result        : Expect_Match;
      File          : File_Type;
      Num_Args      : Natural;
      Buffer        : String (1 .. 8192);
      Last          : Natural;
      Cmd_Args      : Argument_List_Access;

   begin
      --  ??? Why would we need to apply 'patch' when we already have the old
      --  and the new file ?

      if (Revert and then not Orig_File.Is_Regular_File)
        or else (not Revert and then not New_File.Is_Regular_File)
      then
         Cmd_Args := Argument_String_To_List (Patch_Command);
         Cmd      :=
           Locate_Tool_Executable (+Unquote (Cmd_Args (Cmd_Args'First).all));

         if Cmd = No_File then
            Kernel.Insert
              ("command not found: " & Patch_Command &
                 ". You should modify the ""Visual Diff"" preferences",
               Mode => Error);
            Free (Cmd_Args);
            return Ret;
         end if;

         Args (1) := new String'("-f");
         Args (2) := new String'("-s");
         Args (3) := new String'("-o");

         if Revert then
            Args (4) := new String'(+Full_Name (Orig_File));
            Args (5) := new String'("-R");
            Args (6) := new String'(+Full_Name (New_File));
            Num_Args := 7;
         else
            Args (4) := new String'(+Full_Name (New_File));
            Args (5) := new String'(+Full_Name (Orig_File));
            Num_Args := 6;
         end if;

         Args (Num_Args) := new String'(+Full_Name (Diff_File));

         if Active (Me) then
            Trace (Me, "spawn: " &
                     Argument_List_To_String
                     (Cmd_Args.all & Args (1 .. Num_Args)));
         end if;

         begin
            Non_Blocking_Spawn
              (Descriptor, +Cmd.Full_Name,
               Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) &
                 Args (1 .. Num_Args));
            Free (Cmd_Args);
            Free (Args);

            loop
               Expect
                 (Descriptor, Result, Pattern_Any, Matches, Timeout => -1);
            end loop;

         exception
            when others =>
               Close (Descriptor);
         end;
      end if;

      --  ??? Should use VFS.Read_File instead, more efficient
      Open (File, In_File, +Diff_File.Full_Name);

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         Match (Pattern, Buffer (1 .. Last), Matches);

         if Matches (0) /= No_Match then
            Compute_Occurrence (Ret, Occurrence, Buffer (1 .. Last), Matches);
         end if;
      end loop;

      Close (File);
      return Ret;

   exception
      when E : others =>
         Trace (Me, E);
         Close (File);
         return Diff_Chunk_List.Empty_Vector;
   end Diff;

   -----------
   -- Diff3 --
   -----------

   procedure Diff3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head) is
   begin
      Item.List.Clear;

      if Item.Files (1) /= No_File and Item.Files (2) /= No_File then
         if Item.Files (3) /= GNATCOLL.VFS.No_File then
            Item.List :=
              Diff3 (Kernel, Item.Files (1), Item.Files (2), Item.Files (3));
         else
            Item.List :=
              Diff (Kernel, Item.Files (1), Item.Files (2));
         end if;
      end if;
   end Diff3;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      My_Change, Old_File, Your_Change : Virtual_File) return Diff_List
   is
      Diff3_Command : constant String := Diff3_Cmd.Get_Pref;
   begin
      return Diff3 (Kernel, Diff3_Command, My_Change, Old_File, Your_Change);
   end Diff3;

   -----------
   -- Diff3 --
   -----------

   function Diff3
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Diff3_Command : String;
      My_Change, Old_File, Your_Change : Virtual_File) return Diff_List
   is
      Pattern_Bloc   : constant Pattern_Matcher :=
                         Compile ("^====?([1-3])?\r?$", Multiple_Lines);
      Pattern_Chunk  : constant Pattern_Matcher :=
                         Compile ("^([1-3]):([0-9]+)(,([0-9]+))?([acd])\r?$",
                                  Multiple_Lines);
      Descriptor     : TTY_Process_Descriptor;
      Matches_Block  : Match_Array (0 .. 1);
      Matches_Chunk1,
      Matches_Chunk2,
      Matches_Chunk3 : Match_Array (0 .. 5);

      Chunk          : Diff3_Block;
      Args           : Argument_List (1 .. 3);
      Result         : Expect_Match;
      Ret            : Diff_List;
      Occurrence     : Diff_Chunk_Access;
      Cmd            : Virtual_File;
      Cmd_Args       : Argument_List_Access;

   begin
      Cmd_Args := Argument_String_To_List (Diff3_Command);
      Cmd      := Locate_Tool_Executable (+Cmd_Args (Cmd_Args'First).all);

      if Cmd = No_File then
         Kernel.Insert
           ("command not found: " & Diff3_Command &
            ". You should modify the ""Visual Diff"" preferences",
            Mode => Error);
         Free (Cmd_Args);
         return Ret;
      end if;

      Args (1) := new String'(+Full_Name (My_Change));
      Args (2) := new String'(+Full_Name (Old_File));
      Args (3) := new String'(+Full_Name (Your_Change));

      if Active (Me) then
         Trace (Me, "spawn: " & Diff3_Command & " " &
                Display_Full_Name (My_Change)
                & " " & Display_Full_Name (Old_File)
                & " " & Display_Full_Name (Your_Change));
      end if;

      Non_Blocking_Spawn
        (Descriptor, +Cmd.Full_Name,
         Cmd_Args (Cmd_Args'First + 1 .. Cmd_Args'Last) & Args);
      Free (Cmd_Args);
      Free (Args);

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

      when E : others =>
         Trace (Me, E);
         Close (Descriptor);
         return Diff_Chunk_List.Empty_Vector;
   end Diff3;

   --------------
   -- Simplify --
   --------------

   function Simplify
     (Diff     : Diff_List;
      Ref_File : T_Loc) return Diff_List
   is
      Ref        : constant T_Loc := Ref_File;
      Curr_Chunk : Diff_Chunk_Access;
      VRange     : array (1 .. 3) of Diff_Range;
      Res        : Diff_List;

   begin
      for Item of Diff loop
         Curr_Chunk := new Diff_Chunk'(Item.all);
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
                  for J in VRange'Range loop
                     if J /= Ref then
                        if VRange (J).Action = Append then
                           VRange (J).Action := Delete;
                        end if;
                     end if;
                  end loop;
                  --  this case is not a conflict even if Location := 0

               when others =>
                  null;
            end case;

            Curr_Chunk.Location := 0;

         elsif Curr_Chunk.Location = 0 then
            Curr_Chunk.Conflict := True;

            for J in VRange'Range loop
               if J /= Ref and then VRange (J).Action = Append then
                  VRange (J).Action := Delete;
               end if;

               if J /= Ref and then VRange (Ref).Action = Append then
                  VRange (J).Action := Append;
               end if;
            end loop;

         else
            if VRange (Curr_Chunk.Location).Action = Append then
               VRange (Curr_Chunk.Location).Action := Delete;
            elsif VRange (Ref).Action = Append then
               VRange (Curr_Chunk.Location).Action := Append;
            end if;

            for J in VRange'Range loop
               if J /= Ref and then J /= Curr_Chunk.Location then
                  VRange (J).Action := Nothing;
               end if;
            end loop;
         end if;

         VRange (Ref).Action := Nothing;
         Curr_Chunk.Range1 := VRange (1);
         Curr_Chunk.Range2 := VRange (2);
         Curr_Chunk.Range3 := VRange (3);
         Append (Res, Curr_Chunk);
      end loop;

      return Res;
   end Simplify;

   ---------------------
   -- Horizontal_Diff --
   ---------------------

   function Horizontal_Diff
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Line1, Line2 : String) return Diff_List
   is
      pragma Unreferenced (Kernel);

      Result : Diff_List;

      Current_Chunk : Diff_Chunk_Access := null;

      Old_Range : Natural := 1;

      procedure Cb
        (Old_Obj, New_Obj : Character;
         State            : String_Diff.Diff_State);
      --  Callback for the diff procedure

      --------
      -- Cb --
      --------

      procedure Cb
        (Old_Obj, New_Obj : Character;
         State            : String_Diff.Diff_State)
      is
         pragma Unreferenced (Old_Obj, New_Obj);
         Action : Diff_Action;
      begin
         case State is
            when Added => Action := Append;
            when Removed => Action := Delete;
            when Equal => Action := Nothing;
         end case;

         if Current_Chunk /= null
           and then Action /= Current_Chunk.Range1.Action
         then
            Diff_Chunk_List.Append (Result, Current_Chunk);
            Current_Chunk := null;
         end if;

         case Action is
            when Nothing =>
               Old_Range := Old_Range + 1;

            when Append =>
               if Current_Chunk = null then
                  Current_Chunk := new Diff_Chunk'
                    (Range1   => (First              => Old_Range,
                                  Last               => Old_Range + 1,
                                  Action             => Action,
                                  Blank_Lines_Mark   => <>,
                                  Special_Lines_Mark => <>),
                     Range2   => Null_Range,
                     Range3   => Null_Range,
                     Location => 0,
                     Conflict => False);
               else
                  Current_Chunk.Range1.Last := Current_Chunk.Range1.Last + 1;
               end if;

               Old_Range := Old_Range + 1;

            when Delete =>
               if Current_Chunk = null then
                  Current_Chunk := new Diff_Chunk'
                    (Range1   => (First              => Old_Range - 1,
                                  Last               => Old_Range,
                                  Action             => Action,
                                  Blank_Lines_Mark   => <>,
                                  Special_Lines_Mark => <>),
                     Range2   => Null_Range,
                     Range3   => Null_Range,
                     Location => 0,
                     Conflict => False);
               end if;

            when Change =>
               null;
         end case;
      end Cb;

   begin
      String_Diff.Diff (Line2, Line1, Cb'Unrestricted_Access);
      return Result;
   end Horizontal_Diff;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (Link : in out Diff_List) is
   begin
      Link.Clear;
   end Free_List;

   ----------
   -- Free --
   ----------

   procedure Free (Link : in out Diff_Head) is
   begin
      Link.Instances.Free;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Vdiff : in out Diff_Head_Access) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Diff_Head, Diff_Head_Access);
   begin
      Free (Vdiff.all);
      Unchecked_Free (Vdiff);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Vect : in out Diff3_Block) is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Match_Array, Ptr_Match_Array);
   begin
      for J in Vect.Matches_Chunk'Range loop
         Unchecked_Free (Vect.Matches_Chunk (J));
      end loop;

      for J in Vect.S_Chunk'Range loop
         Free (Vect.S_Chunk (J));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Vdiff_List : in out Diff_Head_List_Access) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation
          (Diff_Head_List.Vector, Diff_Head_List_Access);
   begin
      Vdiff_List.Clear;
      Unchecked_Free (Vdiff_List);
   end Free;

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
