------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Boyer_Moore;        use GNATCOLL.Boyer_Moore;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNAT.Expect;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNAT.Strings;                use GNAT.Strings;
with Glib.Convert;
with Gtkada.Style;                use Gtkada.Style;
with Interfaces;                  use Interfaces;
with Unicode.CES.Utf8;            use Unicode, Unicode.CES.Utf8;

with Default_Preferences;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body GPS.Search is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.SEARCH");
   Memcheck_Handle : constant Trace_Handle := Create
     ("TESTSUITE.MEM", Off);

   type Boyer_Moore_Pattern_Access is access all GNATCOLL.Boyer_Moore.Pattern;

   type Full_Text_Search is new Search_Pattern with record
      Pattern : Boyer_Moore_Pattern_Access;
      Length  : Natural;
   end record;

   type Regexp_Search is new Search_Pattern with record
      Pattern : GNAT.Expect.Pattern_Matcher_Access;
   end record;

   type Fuzzy_Search is new Search_Pattern with null record;

   Approximate_Max_Errors        : constant := 2;
   Approximate_Insertion_Cost    : constant := 1;
   Approximate_Substitution_Cost : constant := 1;
   Approximate_Deletion_Cost     : constant := 1;
   --  The cost for character insertion, substitution or deletion. Set any of
   --  these to Integer'Last to disable this type of errors.

   Approximate_Max_Cost : constant :=
     Integer'Max
       (Integer'Max
            (Integer'Max
                 (Approximate_Insertion_Cost,
                  Approximate_Substitution_Cost),
             Approximate_Deletion_Cost),
        Approximate_Max_Errors);

   subtype Mask is Interfaces.Unsigned_64;
   --  We only consider the 1..Pattern'Length

   type Character_Mask_Array is array (Unicode_Char range <>) of Mask;
   type Character_Masks is access all Character_Mask_Array;
   type Approximate_Status is
     array (-Approximate_Max_Cost .. Approximate_Max_Errors) of Mask;
   type Approximate_Status_Access is access all Approximate_Status;

   type Approximate_Search is new Search_Pattern with record
      Pattern : Character_Masks;
      --  Precomputed info about the pattern
      --  ??? We only need entries for the characters in the Pattern, so we
      --  are wasting space here. This would also allow working with UTF8
      --  characters.

      Max_Errors : Natural := Approximate_Max_Errors;

      Result : Approximate_Status_Access;
      --  ??? This would be better part of the search context

      Matched : Mask;
      --  Value in Result that indicates when the character matches
   end record;
   type Approximate_Search_Access is access all Approximate_Search'Class;
   --  An approximate matcher. The algorithm is from:
   --    http://en.wikipedia.org/wiki/Bitap_algorithm
   --    from Wu and Manber "Fast Text Searching With Errors"

   function Compile_Approximate
     (Pattern         : String;
      Case_Sensitive  : Boolean;
      Whole_Word      : Boolean;
      Allow_Highlight : Boolean;
      Negate          : Boolean;
      Max_Errors      : Integer := Approximate_Max_Errors)
     return Approximate_Search_Access;
   --  Compile the pattern

   overriding function Start
     (Self        : Full_Text_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context;
   overriding function Start
     (Self        : Regexp_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context;
   overriding function Start
     (Self        : Fuzzy_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context;
   overriding function Start
     (Self        : Approximate_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context;
   overriding procedure Next
     (Self    : Full_Text_Search;
      Buffer  : String;
      Context : in out Search_Context);
   overriding procedure Next
     (Self    : Regexp_Search;
      Buffer  : String;
      Context : in out Search_Context);
   overriding procedure Next
     (Self    : Fuzzy_Search;
      Buffer  : String;
      Context : in out Search_Context);
   overriding procedure Next
     (Self    : Approximate_Search;
      Buffer  : String;
      Context : in out Search_Context);
   overriding procedure Free (Self : in out Full_Text_Search);
   overriding procedure Free (Self : in out Approximate_Search);
   overriding procedure Free (Self : in out Regexp_Search);
   overriding function Highlight_Match
      (Self    : Fuzzy_Search;
       Buffer  : String;
       Context : Search_Context) return String;

   procedure Update_Location
     (Context : in out Search_Context;
      Buffer  : String);
   --  Compute the (line, column) location for the match, based on previous
   --  knowledge in Context.

   function Is_Word_Delimiter (C : Character) return Boolean;
   --  Whether this character is not part of a word

   function "<" (P1, P2 : Provider_Info) return Boolean;
   function "<" (P1, P2 : Provider_Info) return Boolean is
   begin
      return P1.Provider.Rank < P2.Provider.Rank;
   end "<";

   package Sorting_By_Rank is new Provider_Lists.Generic_Sorting ("<");

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Kind : Search_Kind) return String is
   begin
      case Kind is
         when Full_Text =>
            return "Full text";
         when Regexp =>
            return "Regular expression";
         when Fuzzy =>
            return "Fuzzy";
         when Approximate =>
            return "Approximate";
      end case;
   end Get_Label;

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (Context : in out Search_Context;
      Buffer  : String)
   is
      Tab_Width : constant Visible_Column_Type := 8;
      --  Visible_Column_Type (Vsearch.Get_Tab_Width);

      type Unicode_Char is mod 2 ** 32;

      C : Character;
      C2 : Unicode_Char;
      Len : Integer;

      M : Positive := Index_After_Match (Context);
   begin
      if Is_Empty_Match (Context) then
         M := M + 1;
      end if;

      --  Ref should be before Start to update Start, Finish correctly
      if Context.Ref = Unknown_Position
        or else Context.Ref.Index > Context.Start.Index
      then
         --  Assume beginning of buffer is first line and column
         Context.Ref := (Buffer'First, 1, 1, 1);
      end if;

      while Context.Ref.Index < M
        and then Context.Ref.Index <= Context.Buffer_End
      loop
         --  UTF-8 decoding for the current character.

         C := Buffer (Context.Ref.Index);
         C2 := Character'Pos (C);
         if C2 < 128 then
            Len := 1;
         elsif (C2 and 16#E0#) = 16#C0# then
            Len := 2;
         elsif (C2 and 16#F0#) = 16#E0# then
            Len := 3;
         elsif (C2 and 16#F8#) = 16#F0# then
            Len := 4;
         elsif (C2 and 16#FC#) = 16#F8# then
            Len := 5;
         elsif (C2 and 16#FE#) = 16#FC# then
            Len := 6;
         else
            Len := 1;   --  not valid utf8
         end if;

         if Context.Ref.Index <= Context.Start.Index
           and then Context.Start.Index < Context.Ref.Index + Len
         then
            Context.Start := Context.Ref;
         end if;

         --  We are unable to calculate Finish of empty match, because it could
         --  be before Buffer'First and before Context.Ref. Keep it as is.
         if not Is_Empty_Match (Context)
           and then Context.Ref.Index <= Context.Finish.Index
           and then Context.Finish.Index < Context.Ref.Index + Len
         then
            Context.Finish := Context.Ref;
            Context.Finish.Index := Context.Ref.Index + Len - 1;
         end if;

         if C = ASCII.LF
           or else (C = ASCII.CR
                    and then Context.Ref.Index < Context.Buffer_End
                    and then Buffer (Context.Ref.Index + 1) /= ASCII.LF)
         then
            Context.Ref.Line := Context.Ref.Line + 1;
            Context.Ref.Column := 1;
            Context.Ref.Visible_Column := 1;
            Context.Ref.Index := Context.Ref.Index + 1;

         elsif C = ASCII.HT then
            Context.Ref.Column := Context.Ref.Column + 1;
            Context.Ref.Visible_Column := Context.Ref.Visible_Column
              + Tab_Width - (Context.Ref.Visible_Column mod Tab_Width) + 1;
            Context.Ref.Index := Context.Ref.Index + 1;

         else
            Context.Ref.Index := Context.Ref.Index + Len;
            Context.Ref.Column := Context.Ref.Column + 1;
            Context.Ref.Visible_Column := Context.Ref.Visible_Column + 1;
         end if;

      end loop;
   end Update_Location;

   -----------------------
   -- Is_Word_Delimiter --
   -----------------------

   function Is_Word_Delimiter (C : Character) return Boolean is
   begin
      return not (Is_Alphanumeric (C) or else C = '_');
   end Is_Word_Delimiter;

   -----------------------
   -- Search_Best_Match --
   -----------------------

   function Search_Best_Match
     (Self    : not null access Search_Pattern'Class;
      Buffer  : String) return Search_Context
   is
      Context : Search_Context := Self.Start (Buffer);
   begin
      --  Searching for the best match only makes sense when searching in
      --  Approximate mode.
      if Self.Kind /= Approximate then
         return Context;
      end if;

      declare
         Max_Score_Context : Search_Context := Context;
      begin
         while Context /= No_Match loop
            if Context.Score > Max_Score_Context.Score then
               Max_Score_Context := Context;
            end if;

            Self.Next (Buffer, Context);
         end loop;

         return Max_Score_Context;
      end;
   end Search_Best_Match;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self        : Full_Text_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context
   is
      Index : Integer;
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Buffer_Position :=
        (if Ref.Index = -1 then (Buffer'First, 1, 1, 1) else Ref);
      Context : Search_Context;
      S2      : Integer := S;
   begin
      loop
         Index := GNATCOLL.Boyer_Moore.Search
           (Self.Pattern.all, Buffer (S2 .. F));

         exit when not Self.Whole_Word
           or else Index = -1
           or else Index > Buffer'Last
           or else
               --  Check we have word delimiters on either sides
           ((Index = Buffer'First
             or else Is_Word_Delimiter (Buffer (Index - 1)))
              and then
                (Index = Buffer'Last - Self.Length + 1
                 or else Is_Word_Delimiter (Buffer (Index + Self.Length))));
         S2 := Index + 1;
      end loop;

      if Index = -1 then
         if Self.Negate then
            Context := Search_Context'
              (Start        => (S, 1, 1, 1),  --  line/col updated below
               Finish       => (F, 1, 1, 1),  --  line/col updated below
               Score        => 50,
               Groups       => (others => GNAT.Regpat.No_Match),
               Color_String => Get_Default_Fg,
               Buffer_Start => S,
               Buffer_End   => F,
               Ref          => R);
            Update_Location (Context, Buffer);
         else
            Context := No_Match;
         end if;
      elsif Self.Negate then
         Context := No_Match;
      else
         Context := Search_Context'
           (Start        => (Index, 1, 1, 1),  --  line/col updated below
            Finish       => (Index + Self.Length - 1, 1, 1, 1),
            Score        => 100,
            Groups       => (others => GNAT.Regpat.No_Match),
            Color_String => Get_Default_Fg,
            Buffer_Start => S,
            Buffer_End   => F,
            Ref          => R);
         Update_Location (Context, Buffer);
      end if;

      return Context;
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self        : Regexp_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context
   is
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Buffer_Position :=
        (if Ref.Index = -1 then (Buffer'First, 1, 1, 1) else Ref);
      Context : Search_Context :=
          (Start              => <>,
           Finish             => <>,
           Score              => 100,
           Groups             => <>,
           Color_String       => Get_Default_Fg,
           Buffer_Start       => S,
           Buffer_End         => (if F = 0 then Positive'Last else F),
           Ref                => R);
   begin
      Match
        (Self.Pattern.all, Buffer, Context.Groups,
         Data_First => Context.Buffer_Start,
         Data_Last  => Context.Buffer_End);

      --  The second test below works around an apparent bug in GNAT.Regpat

      if Context.Groups (0) = GNAT.Regpat.No_Match
        or else Context.Groups (0).First > Buffer'Last
      then
         if Self.Negate then
            Context.Start  := (Context.Buffer_Start, 1, 1, 1);
            Context.Finish := (Context.Buffer_End, 1, 1, 1);
            Update_Location (Context, Buffer);
            return Context;
         else
            return No_Match;
         end if;
      elsif Self.Negate then
         return No_Match;
      end if;

      --  If we are matching an empty string set Finish as Unknown_Position
      if Context.Groups (0).Last < Context.Groups (0).First then
         Context.Finish := Unknown_Position;
      else
         Context.Finish := (Context.Groups (0).Last, 1, 1, 1);
      end if;

      Context.Start  := (Context.Groups (0).First, 1, 1, 1);
      Update_Location (Context, Buffer);
      return Context;
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self        : Fuzzy_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context
   is
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Buffer_Position :=
        (if Ref.Index = -1 then (Buffer'First, 1, 1, 1) else Ref);
      Start : Natural := Natural'Last;
      Score : Integer := 0;

      T : Natural := Self.Text'First;
      Context : Search_Context;

      B : Natural := S;
      C, C2 : Unicode_Char;
      B1 : Natural;

   begin
      if Self.Text.all = "" then
         Context := Search_Context'
           (Start              => (S, 1, 1, 1),
            Finish             => (F, 1, 1, 1),
            Score              => Score,
            Groups             => (others => GNAT.Regpat.No_Match),
            Color_String       => Get_Default_Fg,
            Buffer_Start       => S,
            Buffer_End         => F,
            Ref                => R);
         Update_Location (Context, Buffer);
         return Context;
      end if;

      Utf8_Get_Char (Self.Text.all, T, C2);  --  also moves T to next char

      if not Self.Case_Sensitive then
         C2 := To_Lower (C2);
      end if;

      while B <= F loop
         B1 := B;
         Utf8_Get_Char (Buffer, B, C); --  also moves B to next char
         if not Self.Case_Sensitive then
            C := To_Lower (C);
         end if;

         if C = C2 then
            if Start = Natural'Last then
               Start := B1;
            end if;

            if T > Self.Text'Last then
               --  The score should be higher when the characters are closer
               --  together
               Score := Integer'Max (101 - (B - Start), 0);

               if Self.Negate then
                  return GPS.Search.No_Match;
               else
                  Context := Search_Context'
                    (Start              => (Start, 1, 1, 1),
                     Finish             => (B - 1, 1, 1, 1),
                     Score              => Score,
                     Groups             => (others => GNAT.Regpat.No_Match),
                     Color_String       => Get_Default_Fg,
                     Buffer_Start       => S,
                     Buffer_End         => F,
                     Ref                => R);
                  Update_Location (Context, Buffer);
                  return Context;
               end if;
            end if;

            Utf8_Get_Char (Self.Text.all, T, C2);  --  moves T forward
            if not Self.Case_Sensitive then
               C2 := To_Lower (C2);
            end if;
         end if;
      end loop;

      if Self.Negate then
         Context := Search_Context'
           (Start              => (S, 1, 1, 1),
            Finish             => (F, 1, 1, 1),
            Score              => 100,
            Groups             => (others => GNAT.Regpat.No_Match),
            Color_String       => Get_Default_Fg,
            Buffer_Start       => S,
            Buffer_End         => F,
            Ref                => R);
         Update_Location (Context, Buffer);
         return Context;

      else
         return GPS.Search.No_Match;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return GPS.Search.No_Match;
   end Start;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self        : Approximate_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref         : Buffer_Position := Unknown_Position) return Search_Context
   is
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Buffer_Position :=
        (if Ref.Index = -1 then (Buffer'First, 1, 1, 1) else Ref);
      Context : Search_Context :=
        (Start        => (S, 1, 1, 1),  --  first byte matched
         Finish       => (S - 1, 1, 1, 1), --  last byte of last char read
         Score        => 100,
         Groups       => (others => GNAT.Regpat.No_Match),
         Color_String       => Get_Default_Fg,
         Buffer_Start => S,
         Buffer_End   => F,
         Ref          => R);
   begin
      --  Initialize the pattern with K ones
      Self.Result.all := (others => 0);
      for K in 1 .. Self.Max_Errors loop
         Self.Result (K) := Shift_Left (Self.Result (K - 1), 1) or 1;
      end loop;

      Next (Self, Buffer, Context);

      if Context = No_Match then
         if Self.Negate then
            Context.Start := (S, 1, 1, 1);
            Context.Finish := (F, 1, 1, 1);
            Update_Location (Context, Buffer);
         end if;
         return Context;
      elsif Self.Negate then
         return No_Match;
      else
         Update_Location (Context, Buffer);
         return Context;
      end if;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self    : Approximate_Search;
      Buffer  : String;
      Context : in out Search_Context)
   is
      C : Unicode_Char;
      P, P1 : Natural;
      Tmp_R : Approximate_Status;
      Offset : Mask;

   begin
      Context.Ref := (Buffer'First, 1, 1, 1);

      P := Context.Finish.Index + 1;  --  points to first byte of first char

      while P <= Context.Buffer_End loop
         P1 := P;
         Utf8_Get_Char (Buffer, P, C);
         if not Self.Case_Sensitive then
            C := To_Lower (C);
         end if;

         Tmp_R := Self.Result.all;

         if C in Self.Pattern'Range then
            Offset := Self.Pattern (C);
         else
            Offset := 0;
         end if;

         Self.Result (0) := (Shift_Left (Tmp_R (0), 1) or 1) and Offset;

         for K in 1 .. Self.Max_Errors loop
            Self.Result (K) :=
              ((Shift_Left (Tmp_R (K), 1) or 1) and Offset)
              or (Shift_Left (Tmp_R (K - Approximate_Substitution_Cost)
                              or Self.Result (K - Approximate_Deletion_Cost),
                              1) or 1)
              or Tmp_R (K - Approximate_Insertion_Cost);
         end loop;

         for K in Self.Result'First .. Self.Max_Errors loop
            if P - Self.Text'Length - K + 1 >= Buffer'First
              and then (Self.Result (K) and Self.Matched) /= 0
            then
               Context.Start.Index := P1;
               for N in 1 .. Utf8_Length (Self.Text.all) - 1 loop
                  Context.Start.Index :=
                    Utf8_Prev_Char (Buffer, Context.Start.Index);
               end loop;

               Context.Finish.Index := P - 1;
               --  last byte of last significant char

               Context.Score := 100 - K;
               Update_Location (Context, Buffer);
               return;
            end if;
         end loop;
      end loop;

      Context := No_Match;
   end Next;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Approximate_Search) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Approximate_Status, Approximate_Status_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Character_Mask_Array, Character_Masks);
   begin
      Unchecked_Free (Self.Pattern);
      Unchecked_Free (Self.Result);
   end Free;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self    : Fuzzy_Search;
      Buffer  : String;
      Context : in out Search_Context)
   is
      T : Natural := Self.Text'First;
      B : Natural := Context.Finish.Index + 1;
      B1 : Natural;
      C, C2 : Unicode_Char;
   begin
      Utf8_Get_Char (Self.Text.all, T, C2);  --  moves T forward
      if not Self.Case_Sensitive then
         C2 := To_Lower (C2);
      end if;

      Context.Start.Index := Natural'Last;

      while B <= Context.Buffer_End loop
         B1 := B;
         Utf8_Get_Char (Buffer, B, C);  --  moves B forward
         if not Self.Case_Sensitive then
            C := To_Lower (C);
         end if;

         if C = C2 then
            if Context.Start.Index = Natural'Last then
               Context.Start.Index := B1;
            end if;

            if T > Self.Text'Last then
               Context.Score := 101 - (B - Context.Start.Index);
               Context.Finish.Index := B - 1;
               Update_Location (Context, Buffer);
               return;
            end if;

            Utf8_Get_Char (Self.Text.all, T, C2);  --  moves T forward
            if not Self.Case_Sensitive then
               C2 := To_Lower (C2);
            end if;
         end if;
      end loop;
      Context := No_Match;
   end Next;

   ---------------------
   -- Highlight_Match --
   ---------------------

   overriding function Highlight_Match
      (Self    : Fuzzy_Search;
       Buffer  : String;
       Context : Search_Context) return String
   is
      T : Natural := Self.Text'First;
      Result : Unbounded_String;
      B : Natural := Context.Start.Index;
      B1 : Natural;
      C, C2 : Unicode_Char;
   begin
      if not Self.Allow_Highlight
        or else Self.Negate
        or else Self.Text'Length = 0
      then
         return Buffer;
      end if;

      Utf8_Get_Char (Self.Text.all, T, C2); --  moves T forward
      if not Self.Case_Sensitive then
         C2 := To_Lower (C2);
      end if;

      Result := To_Unbounded_String
         (Glib.Convert.Escape_Text (Buffer (Buffer'First .. B - 1)));

      while B <= Context.Finish.Index loop
         B1 := B;
         Utf8_Get_Char (Buffer, B, C);  --  moves B forward
         if not Self.Case_Sensitive then
            C := To_Lower (C);
         end if;

         if C2 /= Unicode_Char'Last and then C = C2 then
            Append (Result,
                    Tag (Context, Glib.Convert.Escape_Text
                      ("" & Buffer (B1 .. B - 1))));

            if T <= Self.Text'Last then
               Utf8_Get_Char (Self.Text.all, T, C2); --  moves T forward
               if not Self.Case_Sensitive then
                  C2 := To_Lower (C2);
               end if;
            else
               C2 := Unicode_Char'Last;
            end if;
         else
            Append (Result, Glib.Convert.Escape_Text (Buffer (B1 .. B - 1)));
         end if;
      end loop;

      Append (Result, Glib.Convert.Escape_Text (Buffer (B .. Buffer'Last)));
      return To_String (Result);
   end Highlight_Match;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self    : Full_Text_Search;
      Buffer  : String;
      Context : in out Search_Context)
   is
      Index : Integer;
   begin
      loop
         --  Perform a search from the character next to the last match's
         --  ending until the end of the buffer.
         Index := GNATCOLL.Boyer_Moore.Search
           (Self.Pattern.all,
            Buffer (Context.Finish.Index + 1 .. Context.Buffer_End));

         --  Check if we match a whole word. Exit in that case.
         exit when not Self.Whole_Word
           or else Index = -1
           or else Index > Buffer'Last
           or else
             ((Index = Buffer'First
               or else Is_Word_Delimiter (Buffer (Index - 1)))
              and then
                (Index = Buffer'Last - Self.Length + 1
                 or else Is_Word_Delimiter (Buffer (Index + Self.Length))));

         --  If we did not match a whole word, continue the search, starting
         --  from the character next to the match's ending.
         Context.Finish.Index := Index + Self.Length - 1;
      end loop;

      if Index = -1 then
         Context := No_Match;
      else
         Context.Start.Index := Index;
         Context.Finish.Index := Index + Self.Length - 1;
         Update_Location (Context, Buffer);
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self    : Regexp_Search;
      Buffer  : String;
      Context : in out Search_Context)
   is
      First : Positive := Index_After_Match (Context);
   begin
      --  We need to skip extra position in case of empty match to avoid
      --  endless search loop
      if Is_Empty_Match (Context) then
         First := First + 1;
      end if;

      Match
        (Self.Pattern.all, Buffer, Context.Groups, First, Context.Buffer_End);

      --  The second test below works around an apparent bug in GNAT.Regpat

      if Context.Groups (0) = GNAT.Regpat.No_Match
        or else Context.Groups (0).First > Buffer'Last
      then
         Context := No_Match;
      else
         --  If we are matching an empty string set Finish as Unknown_Position
         if Context.Groups (0).Last < Context.Groups (0).First then
            Context.Finish := Unknown_Position;
         else
            Context.Finish := (Context.Groups (0).Last, 1, 1, 1);
         end if;

         Context.Start.Index := Context.Groups (0).First;
         Update_Location (Context, Buffer);
      end if;
   end Next;

   ---------------------------
   -- Matched_Subexpression --
   ---------------------------

   procedure Matched_Subexpression
     (Result      : Search_Context;
      Index       : Natural;
      First       : out Natural;
      Last        : out Natural) is
   begin
      if Index in Result.Groups'Range then
         First := Result.Groups (Index).First;
         Last := Result.Groups (Index).Last;
      else
         First := 1;
         Last := 0;
      end if;
   end Matched_Subexpression;

   ---------------------
   -- Highlight_Match --
   ---------------------

   function Highlight_Match
      (Self    : Search_Pattern;
       Buffer  : String;
       Context : Search_Context) return String
   is
      B, F, S, E : Natural;
   begin
      if not Self.Allow_Highlight or else Self.Negate then
         return Buffer;
      end if;

      B := Integer'Max (Context.Buffer_Start, Buffer'First);
      F := Integer'Min (Context.Buffer_End, Buffer'Last);
      S := Integer'Max (Context.Start.Index, Buffer'First);
      E := Index_After_Match (Context) - 1;
      E := Integer'Min (E, Buffer'Last);

      return Glib.Convert.Escape_Text (Buffer (B .. S - 1))
        & Tag (Context, Glib.Convert.Escape_Text (Buffer (S .. E)))
        & Glib.Convert.Escape_Text (Buffer (E + 1 .. F));
   end Highlight_Match;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Result) is
   begin
      Trace (Memcheck_Handle, "Free Search_Result");
      if Self.Id /= Self.Short and then Self.Id /= Self.Long then
         Free (Self.Id);
      end if;

      Free (Self.Short);
      Free (Self.Long);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Result_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Result'Class, Search_Result_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Provider_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Provider'Class, Search_Provider_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   --------------
   -- Register --
   --------------

   procedure Register
     (Self     : in out Search_Provider_Registry;
      Template : not null access Search_Provider'Class)
   is
   begin
      Self.Providers.Append
         ((Provider => Search_Provider_Access (Template)));
      Search_Provider_Registry'Class (Self).Sort_Providers;
   end Register;

   --------------------
   -- Sort_Providers --
   --------------------

   procedure Sort_Providers (Self : in out Search_Provider_Registry) is
   begin
      Sorting_By_Rank.Sort (Self.Providers);
   end Sort_Providers;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Search_Provider_Registry;
      N    : Positive) return Search_Provider_Access
   is
      use Provider_Lists;
      C : Provider_Lists.Cursor;
      Count : Natural := 1;
   begin
      if N > Integer (Self.Providers.Length) then
         return null;
      else
         C := Self.Providers.First;
         while Has_Element (C)
            and then Count < N
         loop
            Count := Count + 1;
            Next (C);
         end loop;

         if Has_Element (C) then
            return Element (C).Provider;
         else
            return null;
         end if;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Search_Provider_Registry;
      Name : String) return Search_Provider_Access is
   begin
      for P of Self.Providers loop
         if P.Provider.Display_Name = Name then
            return P.Provider;
         end if;
      end loop;
      return null;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free
     (Self : in out Search_Provider_Registry_Access)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Provider_Registry'Class, Search_Provider_Registry_Access);
   begin
      if Self /= null then
         for Provider_Info of Self.Providers loop
            Free (Provider_Info.Provider);
         end loop;

         Self.Providers.Clear;
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Full_Text_Search) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNATCOLL.Boyer_Moore.Pattern, Boyer_Moore_Pattern_Access);
   begin
      GNATCOLL.Boyer_Moore.Free (Self.Pattern.all);
      Unchecked_Free (Self.Pattern);
      Free (Search_Pattern (Self));
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Regexp_Search) is
   begin
      Unchecked_Free (Self.Pattern);
      Free (Search_Pattern (Self));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Pattern_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Pattern'Class, Search_Pattern_Access);
   begin
      if Self /= null then
         Free (Self.all);
         Unchecked_Free (Self);
      end if;
   end Free;

   -------------------------
   -- Compile_Approximate --
   -------------------------

   function Compile_Approximate
     (Pattern         : String;
      Case_Sensitive  : Boolean;
      Whole_Word      : Boolean;
      Allow_Highlight : Boolean;
      Negate          : Boolean;
      Max_Errors      : Integer := Approximate_Max_Errors)
     return Approximate_Search_Access
   is
      Result : constant Approximate_Search_Access := new Approximate_Search'
        (Text            => new String'(Pattern),
         Case_Sensitive  => Case_Sensitive,
         Whole_Word      => Whole_Word,
         Kind            => Approximate,
         Negate          => Negate,
         Pattern         => null,
         Max_Errors      => Max_Errors,
         Result          => new Approximate_Status,
         Allow_Highlight => Allow_Highlight,
         Matched         =>  2 ** (Pattern'Length - 1));

      Min : Unicode_Char := Unicode_Char'Last;
      Max : Unicode_Char := 0;
      C : Unicode_Char;
      P : Natural := Pattern'First;
   begin
      while P <= Pattern'Last loop
         Utf8_Get_Char (Pattern, P, C);
         if not Case_Sensitive then
            C := To_Lower (C);
         end if;

         Min := Unicode_Char'Min (Min, C);
         Max := Unicode_Char'Max (Max, C);
      end loop;

      Result.Pattern := new Character_Mask_Array (Min .. Max);
      Result.Pattern.all := (others => 0);

      --  Compared to the paper, we revert the bit ordering in S
      P := Pattern'First;
      while P <= Pattern'Last loop
         Utf8_Get_Char (Pattern, P, C);
         if not Case_Sensitive then
            C := To_Lower (C);
         end if;

         Result.Pattern (C) :=
           Result.Pattern (C) or 2 ** (P - 1 - Pattern'First);
      end loop;

      return Result;
   end Compile_Approximate;

   -----------
   -- Build --
   -----------

   function Build
     (Pattern         : String;
      Case_Sensitive  : Boolean := False;
      Whole_Word      : Boolean := False;
      Negate          : Boolean := False;
      Kind            : Search_Kind := Full_Text;
      Allow_Highlight : Boolean := False)
      return Search_Pattern_Access
   is
      BM    : Boyer_Moore_Pattern_Access;
      Re    : GNAT.Expect.Pattern_Matcher_Access;
      Flags : Regexp_Flags := Multiple_Lines;
      WD    : constant String := "\b";  --  word delimiter
   begin
      case Kind is
         when Full_Text =>
            BM := new GNATCOLL.Boyer_Moore.Pattern;
            Compile (BM.all, Pattern, Case_Sensitive => Case_Sensitive);
            return new Full_Text_Search'
              (Pattern         => BM,
               Text            => new String'(Pattern),
               Case_Sensitive  => Case_Sensitive,
               Negate          => Negate,
               Allow_Highlight => Allow_Highlight,
               Whole_Word      => Whole_Word,
               Kind            => Kind,
               Length          => Pattern'Length);

         when Fuzzy =>
            return new Fuzzy_Search'
              (Text            => new String'(Pattern),
               Allow_Highlight => Allow_Highlight,
               Case_Sensitive  => Case_Sensitive,
               Whole_Word      => Whole_Word,
               Negate          => Negate,
               Kind            => Kind);

         when Approximate =>
            if Pattern'Length <= 4 or else Pattern'Length > 64 then
               --  Fallback to Full_Text, pattern is too long or too short
               BM := new GNATCOLL.Boyer_Moore.Pattern;
               Compile (BM.all, Pattern, Case_Sensitive => Case_Sensitive);
               return new Full_Text_Search'
                 (Pattern         => BM,
                  Text            => new String'(Pattern),
                  Allow_Highlight => Allow_Highlight,
                  Case_Sensitive  => Case_Sensitive,
                  Whole_Word      => Whole_Word,
                  Kind            => Kind,
                  Negate          => Negate,
                  Length          => Pattern'Length);

            else
               --  The maximum number of errors depends on the length of the
               --  patterns.
               --  For a very short pattern, allow no error, otherwise for
               --  instance "naa" would match "nmi", which is surprising to
               --  users). As the length of the pattern extends, allow more
               --  errors.
               return Search_Pattern_Access (Compile_Approximate
                 (Pattern,
                  Allow_Highlight => Allow_Highlight,
                  Case_Sensitive  => Case_Sensitive,
                  Negate          => Negate,
                  Whole_Word      => Whole_Word,
                  Max_Errors      =>
                     (if Pattern'Length <= 4 then 0
                      elsif Pattern'Length <= 10 then 1
                      else 2)));
            end if;

         when Regexp =>
            if not Case_Sensitive then
               Flags := Flags or Case_Insensitive;
            end if;

            begin
               if Whole_Word then
                  Re := new GNAT.Regpat.Pattern_Matcher'
                    (Compile (WD & Pattern & WD, Flags));
               else
                  Re := new GNAT.Regpat.Pattern_Matcher'
                    (Compile (Pattern, Flags));
               end if;

               return new Regexp_Search'
                 (Pattern        => Re,
                  Text           => new String'(Pattern),
                  Allow_Highlight => False,
                  Case_Sensitive => Case_Sensitive,
                  Whole_Word     => Whole_Word,
                  Kind           => Kind,
                  Negate         => Negate);

            exception
               when GNAT.Regpat.Expression_Error =>
                  return Build
                    (Pattern         => Pattern,
                     Case_Sensitive  => Case_Sensitive,
                     Whole_Word      => Whole_Word,
                     Kind            => Full_Text,
                     Negate          => Negate,
                     Allow_Highlight => Allow_Highlight);
            end;
      end case;
   end Build;

   -----------
   -- Build --
   -----------

   function Build
      (Pattern : not null access Search_Pattern'Class;
       Text    : String) return Search_Pattern_Access is
   begin
      return Build
         (Pattern         => Text,
          Case_Sensitive  => Pattern.Case_Sensitive,
          Allow_Highlight => Pattern.Allow_Highlight,
          Whole_Word      => Pattern.Whole_Word,
          Negate          => Pattern.Negate,
          Kind            => Pattern.Kind);
   end Build;

   -----------
   -- Build --
   -----------

   function Build
      (Pattern : not null access Search_Pattern'Class;
       Kind    : Search_Kind) return Search_Pattern_Access is
   begin
      return Build
         (Pattern         => Pattern.Text.all,
          Case_Sensitive  => Pattern.Case_Sensitive,
          Whole_Word      => Pattern.Whole_Word,
          Allow_Highlight => Pattern.Allow_Highlight,
          Negate          => Pattern.Negate,
          Kind            => Kind);
   end Build;

   ---------------------
   -- Build_If_Needed --
   ---------------------

   function Build_If_Needed
     (Pattern    : not null access Search_Pattern'Class;
      Kind       : Search_Kind;
      New_Kind   : Search_Kind;
      Built      : out Boolean) return Search_Pattern_Access is
   begin
      if Pattern.Kind = Kind then
         Built := True;
         return Pattern.Build (Kind => New_Kind);
      else
         Built := False;
         return Search_Pattern_Access (Pattern);
      end if;
   end Build_If_Needed;

   ----------------
   -- Get_Negate --
   ----------------

   function Get_Negate
     (Pattern : not null access Search_Pattern'Class) return Boolean is
   begin
      return Pattern.Negate;
   end Get_Negate;

   ------------------------
   -- Get_Case_Sensitive --
   ------------------------

   function Get_Case_Sensitive
     (Pattern    : not null access Search_Pattern'Class) return Boolean is
   begin
      return Pattern.Case_Sensitive;
   end Get_Case_Sensitive;

   --------------------
   -- Get_Whole_Word --
   --------------------

   function Get_Whole_Word
     (Pattern    : not null access Search_Pattern'Class) return Boolean is
   begin
      return Pattern.Whole_Word;
   end Get_Whole_Word;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind
      (Pattern : not null access Search_Pattern'Class) return Search_Kind is
   begin
      return Pattern.Kind;
   end Get_Kind;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Pattern : not null access Search_Pattern'Class) return String is
   begin
      return Pattern.Text.all;
   end Get_Text;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Pattern) is
   begin
      Free (Self.Text);
   end Free;

   ---------
   -- "=" --
   ---------

   overriding function "=" (P1, P2 : Search_Pattern) return Boolean is
   begin
      if P1.Text = null or else P2.Text = null then
         return False;
      end if;
      return P1.Text.all = P2.Text.all;
   end "=";

   ------------
   -- Equals --
   ------------

   function Equals
     (P1, P2 : Search_Pattern_Access) return Boolean is
   begin
      if P1 = null then
         return P2 = null;
      elsif P2 = null then
         return False;
      else
         return P1.all = P2.all;
      end if;
   end Equals;

   --------------------
   -- Compute_Suffix --
   --------------------

   procedure Compute_Suffix
     (Self        : Search_Pattern;
      Context     : Search_Context;
      Text        : String;
      Suffix      : in out Ada.Strings.Unbounded.Unbounded_String;
      Suffix_Last : in out Natural)
   is
      T : constant String := To_String (Suffix);
   begin
      if T = "" then
         Suffix := To_Unbounded_String
           (Text (Context.Finish.Index + 1 .. Text'Last));
         Suffix_Last := Length (Suffix);
      else
         for S in 1 .. Suffix_Last loop
            if Context.Finish.Index + S > Text'Last then
               Suffix_Last := S - 1;
               exit;
            else
               if (Self.Case_Sensitive
                   and then T (S) /= Text (Context.Finish.Index + S))
                 or else
                   (not Self.Case_Sensitive
                    and then To_Lower (T (S)) /=
                      To_Lower (Text (Context.Finish.Index + S)))
               then
                  Suffix_Last := S - 1;
                  exit;
               end if;
            end if;
         end loop;

         if Suffix_Last = 0 then
            Trace (Me, "No suffix completion, previous candidate was "
                   & T & " and new attempt was " & Text);
         end if;
      end if;
   end Compute_Suffix;

   --------------------------
   -- Get_Allow_Highlights --
   --------------------------

   function Get_Allow_Highlights
     (Self  : not null access Search_Pattern'Class) return Boolean is
   begin
      return Self.Allow_Highlight;
   end Get_Allow_Highlights;

   -----------
   -- Image --
   -----------

   function Image (Pos : Buffer_Position) return String is
   begin
      return '(' & Pos.Index'Img & "," & Pos.Line'Img
        & "," & Pos.Column'Img & "," & Pos.Visible_Column'Img & ')';
   end Image;

   --------------------
   -- Get_Default_Fg --
   --------------------

   function Get_Default_Fg return RGB_String is
      use Default_Preferences;
   begin
      --  By default, use the numbers style
      if Numbers_Style = null then
         return Blue;
      else
         return To_Hex (Numbers_Style.Get_Pref_Fg);
      end if;
   end Get_Default_Fg;

   -----------------------
   -- Index_After_Match --
   -----------------------

   function Index_After_Match (Self : Search_Context) return Positive is
   begin
      if Is_Empty_Match (Self) then
         return Self.Start.Index;
      else
         return Self.Finish.Index + 1;
      end if;
   end Index_After_Match;

   ---------
   -- Tag --
   ---------

   function Tag (Self : Search_Context; Text : String) return String is
   begin
      return "<span foreground=""" & Self.Color_String & """>"
        & Text & "</span>";
   end Tag;

   --------------------
   -- Reset_Progress --
   --------------------

   procedure Reset_Progress
     (Self : not null access Search_Provider) is
   begin
      Self.Searched_Count := 0;
   end Reset_Progress;

end GPS.Search;
