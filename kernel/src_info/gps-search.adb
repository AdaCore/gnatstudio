------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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
with GNAT.Expect;
with GNAT.Regpat;                 use GNAT.Regpat;
with GNAT.Strings;                use GNAT.Strings;
with Glib.Convert;

package body GPS.Search is

   type Boyer_Moore_Pattern_Access is access all GNATCOLL.Boyer_Moore.Pattern;
   type Match_Array_Access is access GNAT.Regpat.Match_Array;

   type Full_Text_Search is new Search_Pattern with record
      Pattern : Boyer_Moore_Pattern_Access;
      Length  : Natural;
   end record;

   type Regexp_Search is new Search_Pattern with record
      Pattern : GNAT.Expect.Pattern_Matcher_Access;
      Matches : Match_Array_Access;
   end record;

   type Fuzzy_Search is new Search_Pattern with record
      null;
   end record;

   overriding function Start
     (Self        : Full_Text_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context;
   overriding function Start
     (Self        : Regexp_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context;
   overriding function Start
     (Self        : Fuzzy_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context;
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
   overriding procedure Free (Self : in out Full_Text_Search);
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

   ---------------------
   -- Update_Location --
   ---------------------

   procedure Update_Location
     (Context : in out Search_Context;
      Buffer  : String)
   is
      Tab_Width : constant Visible_Column_Type := 8;
      --  Visible_Column_Type (Vsearch.Get_Tab_Width);

      C : Character;
   begin
      while Context.Ref_Index < Context.Finish
        and then Context.Ref_Index < Context.Buffer_End
      loop
         Context.Ref_Index := Context.Ref_Index + 1;

         C := Buffer (Context.Ref_Index);

         if C = ASCII.LF
           or else (C = ASCII.CR
                    and then Context.Ref_Index < Context.Buffer_End
                    and then Buffer (Context.Ref_Index + 1) /= ASCII.LF)
         then
            Context.Ref_Line := Context.Ref_Line + 1;
         elsif C = ASCII.HT then
            Context.Ref_Column := Context.Ref_Column + 1;
            Context.Ref_Visible_Column := Context.Ref_Visible_Column
              + Tab_Width - (Context.Ref_Visible_Column mod Tab_Width) + 1;
         else
            Context.Ref_Column := Context.Ref_Column + 1;
            Context.Ref_Visible_Column := Context.Ref_Visible_Column + 1;
         end if;

         if Context.Ref_Index = Context.Start then
            Context.Line_Start := Context.Ref_Line;
            Context.Col_Start := Context.Ref_Column;
            Context.Col_Visible_Start := Context.Ref_Visible_Column;
         end if;

         if Context.Ref_Index = Context.Finish then
            Context.Line_End := Context.Ref_Line;
            Context.Col_End := Context.Ref_Column;
            Context.Col_Visible_End := Context.Ref_Visible_Column;
         end if;
      end loop;
   end Update_Location;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Self        : Full_Text_Search;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Integer := -1;
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context
   is
      Index : Integer;
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Integer :=
        (if Ref_Index = -1 then Buffer'First else Ref_Index);
      Context : Search_Context;
   begin
      Index := GNATCOLL.Boyer_Moore.Search (Self.Pattern.all, Buffer (S .. F));
      if Index = -1 then
         return No_Match;
      end if;

      Context := Search_Context'
        (Start             => Index,
         Finish            => Index + Self.Length - 1,
         Line_Start        => 1,
         Line_End          => 1,
         Col_Start         => 1,
         Col_End           => 1,
         Col_Visible_Start => 1,
         Col_Visible_End   => 1,
         Score             => 100,
         Buffer_Start      => S,
         Buffer_End        => F,
         Ref_Index         => R,
         Ref_Line          => Ref_Line,
         Ref_Column        => Ref_Column,
         Ref_Visible_Column =>
           (if Ref_Visible_Column = -1
            then Visible_Column_Type (Ref_Column)
            else Ref_Visible_Column));

      Update_Location (Context, Buffer);
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
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context
   is
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Integer :=
        (if Ref_Index = -1 then Buffer'First else Ref_Index);
      Context : Search_Context;
   begin
      Match (Self.Pattern.all, Buffer, Self.Matches.all, S, F);

      --  The second test below works around an apparent bug in GNAT.Regpat

      if Self.Matches (0) = GNAT.Regpat.No_Match
        or else Self.Matches (0).First > Buffer'Last
      then
         return No_Match;
      end if;

      Context := Search_Context'
        (Start             => Self.Matches (0).First,
         Finish            => Self.Matches (0).Last,
         Line_Start        => 1,
         Line_End          => 1,
         Col_Start         => 1,
         Col_End           => 1,
         Col_Visible_Start => 1,
         Col_Visible_End   => 1,
         Score             => 100,
         Buffer_Start      => S,
         Buffer_End        => F,
         Ref_Index         => R,
         Ref_Line          => Ref_Line,
         Ref_Column        => Ref_Column,
         Ref_Visible_Column =>
           (if Ref_Visible_Column = -1
            then Visible_Column_Type (Ref_Column)
            else Ref_Visible_Column));

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
      Ref_Index   : Integer := -1;
      Ref_Line    : Natural := 1;
      Ref_Column  : Character_Offset_Type := 1;
      Ref_Visible_Column : Visible_Column_Type := -1) return Search_Context
   is
      S : constant Integer :=
        (if Start_Index = -1 then Buffer'First else Start_Index);
      F : constant Integer :=
        (if End_Index = -1 then Buffer'Last else End_Index);
      R : constant Integer :=
        (if Ref_Index = -1 then Buffer'First else Ref_Index);
      Start : Natural;
      Score : Natural;

      T : Natural := Self.Text'First;
      Context : Search_Context;

   begin
      for B in S .. F loop
         if (Self.Case_Sensitive and then Buffer (B) = Self.Text (T))
            or else (not Self.Case_Sensitive
                     and then To_Lower (Buffer (B)) = To_Lower (Self.Text (T)))
         then
            if T = Self.Text'First then
               Start := B;
            end if;

            if T = Self.Text'Last then
               --  The score should be higher when the characters are closer
               --  together
               Score := 100 - (B - Start);

               Context := Search_Context'
                 (Start             => Start,
                  Finish            => B,
                  Line_Start        => 1,
                  Line_End          => 1,
                  Col_Start         => 1,
                  Col_End           => 1,
                  Col_Visible_Start => 1,
                  Col_Visible_End   => 1,
                  Score             => Score,
                  Buffer_Start      => S,
                  Buffer_End        => F,
                  Ref_Index         => R,
                  Ref_Line          => Ref_Line,
                  Ref_Column        => Ref_Column,
                  Ref_Visible_Column =>
                    (if Ref_Visible_Column = -1
                     then Visible_Column_Type (Ref_Column)
                     else Ref_Visible_Column));

               Update_Location (Context, Buffer);
               return Context;
            end if;

            T := T + 1;
         end if;
      end loop;

      return GPS.Search.No_Match;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self    : Fuzzy_Search;
      Buffer  : String;
      Context : in out Search_Context)
   is
      T : Natural := Self.Text'First;
   begin
      for B in Context.Finish + 1 .. Context.Buffer_End loop
         if (Self.Case_Sensitive and then Buffer (B) = Self.Text (T))
            or else (not Self.Case_Sensitive
                     and then To_Lower (Buffer (B)) = To_Lower (Self.Text (T)))
         then
            if T = Self.Text'First then
               Context.Start := B;
            end if;

            if T = Self.Text'Last then
               Context.Score := 100 - (B - Context.Start);
               Context.Finish := B;
               Update_Location (Context, Buffer);
               return;
            end if;

            T := T + 1;
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
   begin
      Result := To_Unbounded_String
         (Glib.Convert.Escape_Text
            (Buffer (Buffer'First .. Context.Start - 1)));

      for B in Context.Start .. Context.Finish loop
         --   ??? Missing case sensitivity
         if T <= Self.Text'Last and then Buffer (B) = Self.Text (T) then
            Append (Result, "<b>"
               & Glib.Convert.Escape_Text ("" & Buffer (B)) & "</b>");
            T := T + 1;
         else
            Append (Result, Glib.Convert.Escape_Text ("" & Buffer (B)));
         end if;
      end loop;

      Append (Result, Glib.Convert.Escape_Text
         (Buffer (Context.Finish + 1 .. Buffer'Last)));
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
      Index := GNATCOLL.Boyer_Moore.Search
        (Self.Pattern.all, Buffer (Context.Start + 1 .. Context.Buffer_End));
      if Index = -1 then
         Context := No_Match;
      else
         Context.Start := Index;
         Context.Finish := Index + Self.Length - 1;
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
   begin
      Match (Self.Pattern.all, Buffer, Self.Matches.all,
             Context.Start + 1, Context.Buffer_End);

      --  The second test below works around an apparent bug in GNAT.Regpat

      if Self.Matches (0) = GNAT.Regpat.No_Match
        or else Self.Matches (0).First > Buffer'Last
      then
         Context := No_Match;
      else
         Context.Start := Self.Matches (0).First;
         Context.Finish := Self.Matches (0).Last;
         Update_Location (Context, Buffer);
      end if;
   end Next;

   ---------------------
   -- Highlight_Match --
   ---------------------

   function Highlight_Match
      (Self    : Search_Pattern;
       Buffer  : String;
       Context : Search_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return Buffer (Context.Buffer_Start .. Context.Start - 1)
         & "<b>"
         & Buffer (Context.Start .. Context.Finish)
         & "</b>"
         & Buffer (Context.Finish + 1 .. Context.Buffer_End);
   end Highlight_Match;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Search_Result) is
   begin
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
      Name     : String;
      Template : not null access Search_Provider'Class)
   is
   begin
      --  Use Include, since we want to allow overriding predefined
      Self.Map.Include (Name, Search_Provider_Access (Template));
   end Register;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Search_Provider_Registry;
      Name : String) return Search_Provider_Access
   is
      use Provider_Maps;
      C : constant Cursor := Self.Map.Find (Name);
   begin
      if Has_Element (C) then
         return new Search_Provider'Class'(Element (C).all);
      else
         return null;
      end if;
   end Get;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Full_Text_Search) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNATCOLL.Boyer_Moore.Pattern, Boyer_Moore_Pattern_Access);
   begin
      Free (Search_Pattern (Self));
      Unchecked_Free (Self.Pattern);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Regexp_Search) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Match_Array, Match_Array_Access);
   begin
      Free (Search_Pattern (Self));
      Unchecked_Free (Self.Pattern);
      Unchecked_Free (Self.Matches);
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

   -----------
   -- Build --
   -----------

   function Build
     (Pattern        : String;
      Case_Sensitive : Boolean := False;
      Whole_Word     : Boolean := False;
      Kind           : Search_Kind := Full_Text)
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
              (Pattern        => BM,
               Text           => new String'(Pattern),
               Case_Sensitive => Case_Sensitive,
               Whole_Word     => Whole_Word,
               Kind           => Kind,
               Length         => Pattern'Length);

         when Fuzzy =>
            return new Fuzzy_Search'
              (Text           => new String'(Pattern),
               Case_Sensitive => Case_Sensitive,
               Whole_Word     => Whole_Word,
               Kind           => Kind);

         when Regexp =>
            if not Case_Sensitive then
               Flags := Flags or Case_Insensitive;
            end if;

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
               Case_Sensitive => Case_Sensitive,
               Whole_Word     => Whole_Word,
               Kind           => Kind,
               Matches        => new Match_Array (0 .. Paren_Count (Re.all)));
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
         (Pattern        => Text,
          Case_Sensitive => Pattern.Case_Sensitive,
          Whole_Word     => Pattern.Whole_Word,
          Kind           => Pattern.Kind);
   end Build;

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

end GPS.Search;
