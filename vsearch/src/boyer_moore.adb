
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Boyer_Moore is

   Debug : constant Boolean := False;
   Debug_Run : constant Boolean := False;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Motif : in out Pattern;
      From_String : String;
      Case_Sensitive : Boolean := True)
   is
      Prefix_Func : Offset_Array (From_String'Range);
      Reverse_Prefix_Func : Offset_Array (From_String'Range);
      Good_Suffix : Offset_Array (From_String'Range);
      K, K2 : Natural;
      Max : constant Natural := From_String'Last + From_String'First;
      Motif_First : constant := 1; --  Index of Motif.Motif'First
   begin
      --  Compute the last occurence function
      --  Compute the two prefix functions (both from left-to-right and
      --  from right-to-left for the suffixes).
      --  We use a single loop over the pattern for these, for efficiency.

      pragma Assert (From_String'Length <= Max_Pattern_Length);

      Motif.Case_Sensitive := Case_Sensitive;
      Motif.Last_Occurence := (others => 0);
      Motif.Last_Occurence (From_String (From_String'First)) := 1;
      Motif.Motif := new String (Motif_First .. From_String'Length);
      if Case_Sensitive then
         Motif.Motif.all := From_String;
      else
         Motif.Motif.all := To_Lower (From_String);
      end if;

      K := Motif_First - 1;
      Prefix_Func (Motif_First) := K;
      Reverse_Prefix_Func (Motif_First) := K;
      K2 := K;

      for Q in Motif_First + 1 .. From_String'Length loop

         --  The last occurence function

         Motif.Last_Occurence (Motif.Motif (Q)) := Q - Motif_First + 1;

         --  The prefix function

         loop
            if K < Motif_First then
               if Motif.Motif (Motif_First) = Motif.Motif (Q) then
                  K := Motif_First;
               end if;
               exit;

            elsif Motif.Motif (K + 1) /= Motif.Motif (Q) then
               K := Prefix_Func (K);

            else
               K := K + 1;
               exit;
            end if;
         end loop;

         Prefix_Func (Q) := K;

         --  The suffix function
         loop
            if K2 < Motif_First then
               if From_String (From_String'Length) = Motif.Motif (Max - Q) then
                  K2 := From_String'First;
               end if;
               exit;

            elsif Motif.Motif (Max - K2 - 1) /= Motif.Motif (Max - Q) then
               K2 := Reverse_Prefix_Func (K2);

            else
               K2 := K2 + 1;
               exit;
            end if;
         end loop;

         Reverse_Prefix_Func (Q) := K2;
      end loop;

      K := From_String'Length - Prefix_Func (From_String'Length);
      Good_Suffix := (others => K);

      for L in Motif.Motif'Range loop
         K2 := Reverse_Prefix_Func (L);
         K := Motif.Motif'Last - K2;
         if Good_Suffix (K) > L - K2 then
            Good_Suffix (K) := L - K2;
         end if;
      end loop;

      Motif.Good_Suffix := new Offset_Array (0 .. Motif.Motif'Last);
      Motif.Good_Suffix (0) :=
        Motif.Motif'Last - Prefix_Func (Motif.Motif'Last);
      Motif.Good_Suffix (1 .. Good_Suffix'Length) := Good_Suffix;

      if Debug then
         Put_Line ("Pattern, and reverse-pattern: P and RP");
         Put_Line ("Prefix and Suffix functions (F and RF):");
         Put_Line ("   If fail when matching character i, move i-F[i] chars"
                   & " forward");
         Put_Line ("   and restart match at location F[i]+1 in pattern");
         Put_Line ("Good_Suffix_Function: SF");
         Put_Line ("Last_Occurence_Function: L");

         Put ("   i   =  ");
         for J in From_String'Range loop
            Put (Item => J, Width => 3);
         end loop;
         New_Line;

         Put ("   P[i]=  ");
         for J in Motif.Motif'Range loop
            Put ("  " & Motif.Motif (J));
         end loop;
         New_Line;

         Put ("   L   =  ");
         for J in From_String'Range loop
            Put (Item => Motif.Last_Occurence (From_String (J)), Width => 3);
         end loop;
         New_Line;

         Put ("   F[i]=  ");
         for J in Prefix_Func'Range loop
            Put (Item => Prefix_Func (J), Width => 3);
         end loop;
         New_Line;

         Put ("   RP[i]= ");
         for J in reverse Motif.Motif'Range loop
            Put ("  " & Motif.Motif (J));
         end loop;
         New_Line;
         Put ("   RF[i]= ");
         for J in Reverse_Prefix_Func'Range loop
            Put (Item => Reverse_Prefix_Func (J), Width => 3);
         end loop;
         New_Line;

         Put ("   GS[i]= ");
         for J in From_String'Range loop
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

   ------------
   -- Search --
   ------------

   function Search (Motif : Pattern; In_String : String) return Integer is
      M : constant Natural := Motif.Motif'Length; --  length of pattern
      Shift : Natural := In_String'First - 1;
      J : Natural;
      Num_Comp : Natural := 0;

      procedure Dump;
      --  Print the current state of the search

      procedure Dump is
      begin
         --  Show current automaton state
         if Debug then
            Num_Comp := Num_Comp + M - J + 1;

            if Debug_Run then
               Put_Line ("Offset : J=" & J'Img
                         & " Last_Occ=" & In_String (Shift + J)
                         & " Max ("
                         & Motif.Good_Suffix (J)'Img
                         & ","
                         & Natural'Image
                         (J - Motif.Last_Occurence (In_String (Shift + J)))
                         & ")");
               Put_Line (In_String);
               Put ((1 .. Shift - In_String'First + 1 => ' '));
               Put_Line (Motif.Motif.all);
               Put ((1 .. Shift + J - In_String'First => ' '));
               Put_Line ("^");
            end if;

            if J = 0 then
               Put_Line ("Matched at position" & Natural'Image (Shift + 1)
                         & " after" & Num_Comp'Img & " comparisons");
            end if;
         end if;
      end Dump;

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

            if Debug then
               Dump;
            end if;

            if J = 0 then
               return Shift + 1;
               --  Shift := Shift + Motif.Good_Suffix (0)   to find next
            end if;

            Shift := Shift +
              Natural'Max (Motif.Good_Suffix (J),
                           J - Motif.Last_Occurence
                           (To_Lower (In_String (Shift + J))));
         end loop;

      else
         while Shift <= In_String'Last - M loop
            J := M;
            while J > 0 and then Motif.Motif (J) = In_String (Shift + J) loop
               J := J - 1;
            end loop;

            if Debug then
               Dump;
            end if;

            if J = 0 then
               return Shift + 1;
               --  Shift := Shift + Motif.Good_Suffix (0)   to find next
            end if;

            Shift := Shift +
              Natural'Max (Motif.Good_Suffix (J),
                           J - Motif.Last_Occurence (In_String (Shift + J)));
         end loop;
      end if;

      return -1;
   end Search;

end Boyer_Moore;
