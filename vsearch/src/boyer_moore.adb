-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
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

--  The search is done right-to-left. In the best cases (the text doesn't
--  contain any character from the pattern), this results in
--   string_length / pattern_length, characters being examined, instead of
--   string_length characters.
--
--  Compiling the pattern generates two lookup-tables:
--
--  The Last Occurrence Function
--  ============================
--
--  The Last-Ocurrence-Function returns the right-most location for each
--  character in the alphabet in the pattern.
--  When a character is seen in the searched string, this array will suggest
--  the offset by which we should move the character:
--      string:   "revolution in the treatment of"
--      pattern:  "     reminiscence"
--                                ^ when we see the 'h', we can move to:
--                "                reminiscence"
--
--      string:   "written notice that"
--      pattern:  "  reminiscence"
--                            ^  when the see the 'i', we can move to:
--                "      reminiscence"
--
--      string:   "golden fleece of"
--      pattern:  " reminiscence"
--                           ^   when we see the 'e', no move can be suggested,
--                               since 'e' appears at the right-most position
--                               in the pattern.
--
--  The Good Suffix Function
--  ========================
--
--  This function reports the least amount that garantees that any pattern
--  characters that align with the good suffix previously found in the text
--  will match those suffix characters.
--  For instance:
--
--      string:   "written notice that"
--      pattern:  "  reminiscence"
--                            ^   The pattern would be moved so that the "ce"
--                             vv we have already found match some text.
--                "     reminiscence"
--
--  Combination
--  ===========
--
--  The two functions above can be computed statically based only on the
--  pattern, and without any knowledge of the text.
--  When we try to match a pattern with a text, these two functions are
--  combined, and the pattern is moved forward by the maximum amount reported
--  by the two functions.

with Unchecked_Deallocation;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;
with GNAT.Case_Util;          use GNAT.Case_Util;

package body Boyer_Moore is

   Debug     : constant Boolean := False;
   Debug_Run : constant Boolean := False;

   procedure Dump_Str (Str : String);
   --  Print string, replacing the newlines with spaces for clarity

   procedure Dump
      (M, Shift, J : Natural;
       Num_Comp  : in out Natural;
       Motif     : Pattern;
       In_String : String);
   --  Print the current state of the search.
   --  The parameters are the internal state in Search. We do not use a
   --  nested subprogram for efficiency reasons

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Motif          : in out Pattern;
      From_String    : String;
      Case_Sensitive : Boolean := True)
   is
      --  Prefix contains the following:
      --    Prefix (J) is the length of the longest prefix of Motif
      --    which is also a suffix of
      --    Motif (Motif'First .. Motif'First + J - 1)
      --    ie of the motif made of the j-th first characters of Motif
      --
      --  Reverse_Prefix is the Prefix function applied to the reverse of Motif
      --
      --  Motif.Last_Occurrence contains the index of the last occurrence of
      --    the character in the motif. This is in the range 1 .. Motif'Length
      --
      --  Good_Suffix at index J:
      --    If a mismatch occurs in the j-th character of the pattern, we
      --    can safely advance by good_suffix (j).
      --       m = Motif'Length
      --       GS(J) = m
      --          - Max (k; 0<=k<m
      --                    and (Motif (J+1 .. m) suffix of Motif (1 .. k)
      --                         or Motif (1 .. j) suffix of Motif (J+1 .. m))
      --
      --    For instance:  Motif="AABBA"
      --       GS(1) = 5 - 1 = 4
      --          since Motif (1 .. 1)="A" is a suffix "ABBA"
      --       GS(4) = 5 - 2 = 3
      --          since "A" is a suffix of Motif (1 .. 2) = "AA"
      --       GS(5) = 5 - 1 = 4
      --          since "" is a suffix of Motif (1 .. 4) = "AABB"

      Prefix          : Offset_Array (1 .. From_String'Length);
      Reverse_Prefix  : Offset_Array (1 .. From_String'Length);
      K, K2           : Natural := 0;
      Tmp             : Natural;

   begin
      Motif.Case_Sensitive  := Case_Sensitive;
      Motif.Last_Occurrence := (others => 0);
      Motif.Motif           := new String (1 .. From_String'Length);
      Motif.Motif.all       := From_String;
      if not Case_Sensitive then
         To_Lower (Motif.Motif.all);
      end if;

      Prefix (Prefix'First)                   := 0;
      Reverse_Prefix (Reverse_Prefix'First)   := 0;
      Motif.Last_Occurrence (Motif.Motif (1)) := 1;

      for Q in 2 .. Motif.Motif'Last loop
         --  Compute Last occurrence

         Motif.Last_Occurrence (Motif.Motif (Q)) := Q;

         --  Compute prefix function

         while K > 0
           and then Motif.Motif (K + 1) /= Motif.Motif (Q)
         loop
            K := Prefix (K);
         end loop;

         if Motif.Motif (K + 1) = Motif.Motif (Q) then
            K := K + 1;
         end if;

         Prefix (Q) := K;

         --  Compute the reverse prefix function

         while K2 > 0
           and then Motif.Motif (Motif.Motif'Last - K2) /=
           Motif.Motif (Motif.Motif'Last + 1 - Q)
         loop
            K2 := Reverse_Prefix (K2);
         end loop;

         if Motif.Motif (Motif.Motif'Last - K2) =
           Motif.Motif (Motif.Motif'Last + 1 - Q)
         then
            K2 := K2 + 1;
         end if;

         Reverse_Prefix (Q) := K2;
      end loop;

      --  Compute the good suffix function

      K := From_String'Length - Prefix (From_String'Length);
      Motif.Good_Suffix := new Offset_Array'(0 .. From_String'Length => K);

      for L in Motif.Motif'Range loop
         K   := From_String'Length - Reverse_Prefix (L);
         Tmp := L - Reverse_Prefix (L);

         if Motif.Good_Suffix (K) > Tmp then
            Motif.Good_Suffix (K) := Tmp;
         end if;
      end loop;

      if Debug then
         Put ("   i   =  ");
         for J in Motif.Motif'Range loop
            Put (Item => J, Width => 3);
         end loop;
         New_Line;

         Put ("  Pat[i]=  ");
         for J in Motif.Motif'Range loop
            Put ("  " & Motif.Motif (J));
         end loop;
         New_Line;

         Put ("  Pre[i]=  ");
         for J in Prefix'Range loop
            Put (Item => Prefix (J), Width => 3);
         end loop;
         New_Line;

         Put ("RevPre[i]=  ");
         for J in Reverse_Prefix'Range loop
            Put (Item => Reverse_Prefix (J), Width => 3);
         end loop;
         New_Line;

         Put ("GoodSu[i]=  ");
         for J in Motif.Good_Suffix'Range loop
            Put (Item => Motif.Good_Suffix (J), Width => 3);
         end loop;
         New_Line;
      end if;
   end Compile;

   ----------
   -- Free --
   ----------

   procedure Free (Motif : in out Pattern) is
      procedure Internal is new Unchecked_Deallocation
        (Offset_Array, Offset_Array_Access);
      procedure Internal is new Unchecked_Deallocation (String, String_Access);
   begin
      Internal (Motif.Good_Suffix);
      Internal (Motif.Motif);
   end Free;

   --------------
   -- Dump_Str --
   --------------

   procedure Dump_Str (Str : String) is
   begin
      for S in Str'Range loop
         if Str (S) = ASCII.LF then
            Put (' ');
         else
            Put (Str (S));
         end if;
      end loop;
      New_Line;
   end Dump_Str;

   ----------
   -- Dump --
   ----------

   procedure Dump
      (M, Shift, J : Natural;
       Num_Comp  : in out Natural;
       Motif     : Pattern;
       In_String : String) is
   begin
      --  Show current automaton state
      Num_Comp := Num_Comp + M - J + 1;

      if Debug_Run then
         New_Line;
         Put_Line ("Offset : Shift+j="
                   & Integer'Image (Shift + J)
                   & " J=" & J'Img
                   & " Last_Occ=" & In_String (Shift + J)
                   & " Max ("
                   & Motif.Good_Suffix (J)'Img
                   & ","
                   & Integer'Image
                   (J - Motif.Last_Occurrence (In_String (Shift + J)))
                   & ")");

         if In_String'Length < 400 then
            Dump_Str (In_String);
            Put ((1 .. Shift - In_String'First + 1 => ' '));
         end if;
         Dump_Str (Motif.Motif.all);

         if Shift + J - In_String'First < 400 then
            Put ((1 .. Shift + J - In_String'First => ' '));
            Put_Line ("^");
         end if;
      end if;

      if J = 0 then
         Put_Line ("Matched at position" & Natural'Image (Shift + 1)
                   & " after" & Num_Comp'Img & " comparisons");
      end if;
   end Dump;

   ------------
   -- Search --
   ------------

   function Search (Motif : Pattern; In_String : String) return Integer is
      M : constant Natural := Motif.Motif'Length; --  length of pattern
      Shift : Natural := In_String'First - 1;
      J : Natural;
      Num_Comp : Natural := 0;
   begin
      pragma Assert (Motif.Motif'First = 1);

      if not Motif.Case_Sensitive then
         while Shift <= In_String'Last - M loop
            J := M;
            while J > 0
              and then Motif.Motif (J) = To_Lower (In_String (Shift + J))
            loop
               J := J - 1;
            end loop;

            if J = 0 then
               return Shift + 1;
            elsif Debug then
               Dump (M, Shift, J, Num_Comp, Motif, In_String);
            end if;

            Shift := Shift +
              Natural'Max (Motif.Good_Suffix (J),
                           J - Motif.Last_Occurrence
                           (To_Lower (In_String (Shift + J))));
         end loop;

      else
         while Shift <= In_String'Last - M loop
            J := M;
            while J > 0 and then Motif.Motif (J) = In_String (Shift + J) loop
               J := J - 1;
            end loop;

            if J = 0 then
               return Shift + 1;
            elsif Debug then
               Dump (M, Shift, J, Num_Comp, Motif, In_String);
            end if;

            Shift := Shift +
              Natural'Max (Motif.Good_Suffix (J),
                           J - Motif.Last_Occurrence (In_String (Shift + J)));
         end loop;
      end if;

      return -1;
   end Search;

end Boyer_Moore;
