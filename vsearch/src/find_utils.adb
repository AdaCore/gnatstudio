-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Glib.Convert;              use Glib.Convert;
with Glib.Unicode;              use Glib.Unicode;
with Boyer_Moore;               use Boyer_Moore;
with GPS.Kernel;                use GPS.Kernel;
with Traces;                    use Traces;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtk.Widget;                use Gtk.Widget;
with Vsearch;                   use Vsearch;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;

package body Find_Utils is

   Me : constant Debug_Handle := Create ("Find_Utils");

   procedure Free_Pattern_Matcher is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   procedure Free_Match_Array is new Ada.Unchecked_Deallocation
     (Match_Array, Match_Array_Access);

   function Is_Word_Delimiter (C : Character) return Boolean;
   pragma Inline (Is_Word_Delimiter);
   --  Return True if C is a character which can't be in a word.

   function End_Of_Line (Buffer : String; Pos : Natural) return Integer;
   pragma Inline (End_Of_Line);
   --  Return the index for the end of the line containing Pos

   procedure Register_Search_Function
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : Search_Module_Data)
      renames Vsearch.Register_Search_Function;

   function Search_Context_From_Module
     (Id     : access GPS.Kernel.Abstract_Module_ID_Record'Class;
      Handle : access Kernel_Handle_Record'Class)
      return Find_Utils.Search_Module_Data
      renames Vsearch.Search_Context_From_Module;

   -----------------------
   -- Is_Word_Delimiter --
   -----------------------

   function Is_Word_Delimiter (C : Character) return Boolean is
   begin
      return not (Is_Alphanumeric (C) or else C = '_');
   end Is_Word_Delimiter;

   -----------------
   -- End_Of_Line --
   -----------------

   function End_Of_Line (Buffer : String; Pos : Natural) return Integer is
      J : Integer := Pos;
   begin
      while J < Buffer'Last loop
         if Buffer (J) = ASCII.LF
           or else Buffer (J) = ASCII.CR
         then
            return J - 1;
         end if;

         J := J + 1;
      end loop;

      return Buffer'Last;
   end End_Of_Line;

   -----------
   -- Match --
   -----------

   function Match
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Integer := -1;
      End_Index   : Positive := Positive'Last) return Integer
   is
      Result : Integer := -1;

      function Callback (Match : Match_Result) return Boolean;
      --  Simple callback for the search algorithm

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
      begin
         Result := Match.Index;
         return False;
      end Callback;

      Index       : Integer := Buffer'First;
      Line        : Natural := 0;
      Column      : Character_Offset_Type := 0;
      Was_Partial : Boolean;
      Start       : constant Integer :=
                      Integer'Max (Start_Index, Buffer'First);
      Last        : constant Integer := Integer'Min (End_Index, Buffer'Last);
   begin
      Scan_Buffer_No_Scope
        (Context, Buffer, Start, Last, Callback'Unrestricted_Access,
         Index, Line, Column, Was_Partial);
      return Result;
   end Match;

   --------------------------
   -- Scan_Buffer_No_Scope --
   --------------------------

   procedure Scan_Buffer_No_Scope
     (Context     : access Root_Search_Context;
      Buffer      : String;
      Start_Index : Natural;
      End_Index   : Natural;
      Callback    : Scan_Callback;
      Ref_Index   : in out Integer;
      Ref_Line    : in out Natural;
      Ref_Column  : in out Character_Offset_Type;
      Was_Partial : out Boolean)
   is
      Last_Line_Start    : Natural;
      Ref_Visible_Column : Visible_Column_Type;

      function Pretty_Print_Line
        (Line      : String;
         Start_Pos : Integer;
         End_Pos   : Integer) return String;
      --  Return a version of the string that highlights the pattern between
      --  Start_Pos and End_Pos, using the pango markup language.
      --  Start_Pos and End_Pos should be valid indexes in Line.

      procedure Re_Search;
      --  Handle the search for a regular expression

      procedure BM_Search;
      --  Handle the search for a constant string

      -----------------------
      -- Pretty_Print_Line --
      -----------------------

      function Pretty_Print_Line
        (Line      : String;
         Start_Pos : Integer;
         End_Pos   : Integer) return String is
      begin
         if Line = "" then
            return "";
         end if;

         if End_Pos < Start_Pos then
            return Escape_Text (Line);
         end if;

         return Escape_Text (Line (Line'First .. Start_Pos - 1))
           & "<span foreground=""red"">" &
         Escape_Text (Line (Start_Pos .. End_Pos - 1))
           & "</span>" &
         Escape_Text (Line (End_Pos .. Line'Last));
      end Pretty_Print_Line;

      ---------------
      -- Re_Search --
      ---------------

      procedure Re_Search is
         RE  : constant Pattern_Matcher := Context_As_Regexp (Context);
         Pos : Natural := Start_Index;
      begin
         --  Special case here: If we have an empty section, do nothing. In
         --  fact, End_Index might be 0 in the following case: we search in
         --  one of the GPS source files for "all but comments". The first
         --  section is empty, and End_Index is 0. However, it is
         --  legitimate, if inefficient, to have an empty section

         if End_Index = 0 then
            return;
         end if;

         loop
            Match (RE, Buffer, Context.Sub_Matches.all, Pos, End_Index);

            --  The second test below works around an apparent bug in
            --  GNAT.Regpat
            exit when Context.Sub_Matches (0) = No_Match
              or else Context.Sub_Matches (0).First > Buffer'Last;

            Pos := Context.Sub_Matches (0).First;
            Ref_Column := 1;
            Ref_Visible_Column := 1;
            To_Line_Column
              (Buffer (Last_Line_Start .. Buffer'Last),
               Pos, Ref_Line, Ref_Column,
               Ref_Visible_Column,
               Last_Line_Start);
            Ref_Index := Pos;

            declare
               End_Col : constant Natural :=
                 Natural (UTF8_Strlen
                   (Buffer (Context.Sub_Matches (0).First ..
                        Context.Sub_Matches (0).Last)));

               Line_End : constant Natural := End_Of_Line (Buffer, Pos);

               End_Pos  : constant Natural := Natural'Min
                 (Pos + End_Col, Line_End);

               Match_Length : constant Natural :=
                 Context.Sub_Matches (0).Last - Pos + 1;

               Line    : constant String := Pretty_Print_Line
                 (Buffer (Last_Line_Start .. Line_End), Pos, End_Pos);

               End_Line   : Natural := Ref_Line;
               End_Column : Character_Offset_Type := 1;
               End_Visible_Column : Visible_Column_Type := 1;
               Dummy      : Natural := 0;
            begin
               To_Line_Column
                 (Buffer (Last_Line_Start .. Buffer'Last),
                  Pos + End_Col,
                  End_Line, End_Column, End_Visible_Column, Dummy);

               if not Callback (Match_Result'
                   (Length         => Line'Length,
                    Pattern_Length => Match_Length,
                    Index          => Pos,
                    Begin_Line     => Ref_Line,
                    End_Line       => End_Line,
                    Begin_Column   => Ref_Column,
                    Visible_Begin_Column => Ref_Visible_Column,
                    Visible_End_Column   => End_Visible_Column,
                    End_Column     => End_Column,
                    Text           => Line))
               then
                  Was_Partial := True;
                  return;
               end if;
            end;

            Pos := Pos + 1;
         end loop;

         Was_Partial := False;
      end Re_Search;

      ---------------
      -- BM_Search --
      ---------------

      procedure BM_Search is
         BM  : Boyer_Moore.Pattern;
         Pos : Integer := Start_Index;
      begin
         Context_As_Boyer_Moore (Context, BM);

         --  The loop is optimized so that the search is as efficient as
         --  possible (we scan the whole buffer, instead of line-by-line
         --  search). We then pay a small price to actually compute the
         --  buffer coordinates, but this algorithm is much faster for files
         --  that don't match.

         loop
            Pos := Search (BM, Buffer (Pos .. End_Index));
            exit when Pos = -1;

            if not Context.Options.Whole_Word
              or else
              ((Pos = Start_Index
                or else Is_Word_Delimiter (Buffer (Pos - 1)))
               and then
                 (Pos + Context.Look_For'Length - 1 = End_Index
                  or else Is_Word_Delimiter
                    (Buffer (Pos + Context.Look_For'Length))))
            then
               Ref_Column := 1;
               Ref_Visible_Column := 1;
               To_Line_Column
                 (Buffer (Last_Line_Start .. Buffer'Last),
                  Pos, Ref_Line, Ref_Column, Ref_Visible_Column,
                  Last_Line_Start);
               Ref_Index := Pos;

               declare
                  Line_End_Pos : constant Natural := End_Of_Line
                    (Buffer, Pos + Context.Look_For'Length);

                  Line : constant String :=
                    Pretty_Print_Line
                      (Buffer (Last_Line_Start .. Line_End_Pos),
                       Pos, Pos + Context.Look_For'Length);

                  End_Line   : Natural := Ref_Line;
                  End_Column : Character_Offset_Type := 1;
                  End_Visible_Column : Visible_Column_Type := 1;
                  Dummy      : Natural := 0;
               begin
                  To_Line_Column
                    (Buffer (Last_Line_Start .. Buffer'Last),
                     Pos + Context.Look_For'Length,
                     End_Line, End_Column, End_Visible_Column, Dummy);

                  if not Callback (Match_Result'
                      (Length         => Line'Length,
                       Pattern_Length => Context.Look_For'Length,
                       Index          => Pos,
                       Begin_Line     => Ref_Line,
                       End_Line       => End_Line,
                       Begin_Column   => Ref_Column,
                       End_Column     => End_Column,
                       Visible_Begin_Column => Ref_Visible_Column,
                       Visible_End_Column   => End_Visible_Column,
                       Text           => Line))
                  then
                     Was_Partial := True;
                     return;
                  end if;
               end;
            end if;

            Pos := Pos + 1;
         end loop;
         Was_Partial := False;
      end BM_Search;

   begin
      Was_Partial := False;

      --  Initialize the value of Last_Line_Start.

      Last_Line_Start := Buffer'First;

      declare
         J : Integer := Start_Index - 1;
      begin
         loop
            exit when J < Buffer'First;

            if Buffer (J) = ASCII.LF then
               Last_Line_Start := J + 1;
               exit;
            end if;

            J := UTF8_Find_Prev_Char (Buffer, J);
         end loop;
      end;

      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      if Context.Options.Regexp then
         Re_Search;
      else
         BM_Search;
      end if;

   exception
      when Invalid_Context =>
         null;
   end Scan_Buffer_No_Scope;

   --------------------
   -- To_Line_Column --
   --------------------

   procedure To_Line_Column
     (Buffer         : Glib.UTF8_String;
      Pos            : Natural;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Visible_Column : in out Visible_Column_Type;
      Line_Start     : in out Natural)
   is
      J          : Natural := Buffer'First;
      Tab_Width  : constant Visible_Column_Type :=
        Visible_Column_Type (Get_Tab_Width);

      function At_Line_End return Boolean;
      pragma Inline (At_Line_End);
      --  Return True if J points to an end-of-line character

      function At_Line_End return Boolean is
      begin
         return Buffer (J) = ASCII.LF
           or else (Buffer (J) = ASCII.CR
                    and then Buffer (J + 1) /= ASCII.LF);
      end At_Line_End;

   begin
      loop
         exit when J > Pos - 1;

         if At_Line_End then
            Line           := Line + 1;
            Column         := 1;
            Visible_Column := 1;
            Line_Start := J + 1;
         else
            Column := Column + 1;

            if Buffer (J) = ASCII.HT then
               Visible_Column := Visible_Column
                 + Tab_Width - (Visible_Column mod Tab_Width) + 1;
            else
               Visible_Column := Visible_Column + 1;
            end if;
         end if;

         J := UTF8_Next_Char (Buffer, J);
      end loop;
   end To_Line_Column;

   ----------------------
   -- Context_Look_For --
   ----------------------

   function Context_Look_For
     (Context : access Root_Search_Context) return String is
   begin
      if Context.Look_For = null then
         return "";
      else
         return Context.Look_For.all;
      end if;
   end Context_Look_For;

   -----------------------
   -- Context_As_String --
   -----------------------

   function Context_As_String
     (Context : access Root_Search_Context) return String is
   begin
      if Context.Look_For = null or else Context.Options.Regexp then
         raise Invalid_Context;
      end if;

      return Context.Look_For.all;
   end Context_As_String;

   -----------------------
   -- Context_As_Regexp --
   -----------------------

   function Context_As_Regexp
     (Context : access Root_Search_Context)
      return GNAT.Regpat.Pattern_Matcher
   is
      Flags : Regexp_Flags := Multiple_Lines;
      WD    : constant String := "\b";  -- Word_Delimiter
   begin
      if Context.RE_Matcher = null then
         if not Context.Options.Regexp or else Context.Look_For = null then
            raise Invalid_Context;
         end if;

         if not Context.Options.Case_Sensitive then
            Flags := Flags or Case_Insensitive;
         end if;

         if Context.Options.Whole_Word then
            Context.RE_Matcher := new Pattern_Matcher'
              (Compile (WD & Context.Look_For.all & WD, Flags));
         else
            Context.RE_Matcher := new Pattern_Matcher'
              (Compile (Context.Look_For.all, Flags));
         end if;

         Context.Sub_Matches :=
           new Match_Array (0 .. Paren_Count (Context.RE_Matcher.all));
      end if;

      return Context.RE_Matcher.all;

   exception
      when Expression_Error =>
         Trace (Me, "Invalid regexp: " & Context.Look_For.all);
         raise Invalid_Context;
   end Context_As_Regexp;

   ----------------------------
   -- Context_As_Boyer_Moore --
   ----------------------------

   procedure Context_As_Boyer_Moore
     (Context : access Root_Search_Context;
      Matcher : out Boyer_Moore.Pattern) is
   begin
      if not Context.BM_Initialized then
         if Context.Options.Regexp or else Context.Look_For = null then
            raise Invalid_Context;
         end if;

         Context.BM_Initialized := True;
         Compile (Context.BM_Matcher, Context.Look_For.all,
                  Context.Options.Case_Sensitive);
         Context.Sub_Matches := new Match_Array'(0 => No_Match);
      end if;

      Matcher := Context.BM_Matcher;
   end Context_As_Boyer_Moore;

   ------------------------
   -- Set_End_Notif_Done --
   ------------------------

   procedure Set_End_Notif_Done
     (Context : in out Root_Search_Context; Value : Boolean) is
   begin
      Context.End_Notif_Done := Value;
   end Set_End_Notif_Done;

   ------------------------
   -- Get_End_Notif_Done --
   ------------------------

   function Get_End_Notif_Done
     (Context : Root_Search_Context) return Boolean is
   begin
      return Context.End_Notif_Done;
   end Get_End_Notif_Done;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context  : access Root_Search_Context'Class;
      Look_For : String;
      Options  : Search_Options) is
   begin
      Free (Root_Search_Context (Context.all));
      Context.Look_For       := new String'(Look_For);
      Context.Options        := Options;
      Context.BM_Initialized := False;
   end Set_Context;

   ----------
   -- Free --
   ----------

   procedure Free (Context : in out Root_Search_Context) is
   begin
      Free (Context.Look_For);
      Free_Pattern_Matcher (Context.RE_Matcher);
      Free (Context.BM_Matcher);
      Free_Match_Array (Context.Sub_Matches);
   end Free;

   procedure Free (Context : in out Search_Context_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Search_Context'Class, Search_Context_Access);
   begin
      if Context /= null then
         Free (Context.all);
         Unchecked_Free (Context);
      end if;
   end Free;

   -------------
   -- Replace --
   -------------

   function Replace
     (Context         : access Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean
   is
      pragma Unreferenced
        (Context, Kernel, Replace_String, Search_Backward, Give_Focus);
   begin
      return False;
   end Replace;

   ------------------
   -- Reset_Search --
   ------------------

   procedure Reset_Search
     (Object : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Object);
   begin
      Run_Hook (Kernel, Search_Reset_Hook);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Reset_Search;

   -----------------
   -- Get_Options --
   -----------------

   function Get_Options
     (Context : access Root_Search_Context) return Search_Options is
   begin
      return Context.Options;
   end Get_Options;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   function Get_Current_Progress
     (Context : access Search_Context) return Integer
   is
      pragma Unreferenced (Context);
   begin
      return 0;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   function Get_Total_Progress
     (Context : access Search_Context) return Integer
   is
      pragma Unreferenced (Context);
   begin
      return 1;
   end Get_Total_Progress;

   ------------------------
   -- Find_Closest_Match --
   ------------------------

   procedure Find_Closest_Match
     (Buffer         : String;
      Line           : in out Natural;
      Column         : in out Character_Offset_Type;
      Str            : String;
      Case_Sensitive : Boolean)
   is
      Best_Line     : Integer := 0;
      Best_Column   : Character_Offset_Type := 0;

      function Callback (Match : Match_Result) return Boolean;
      --  Called every time a reference to the entity is found

      --------------
      -- Callback --
      --------------

      function Callback (Match : Match_Result) return Boolean is
         Line_Diff : constant Integer :=
           Integer (abs (Match.Begin_Line - Line)
                    - abs (Best_Line - Line));
         Col_Diff : constant Integer :=
           Integer (abs (Match.Begin_Column - Column)
                    - abs (Best_Column - Column));
      begin
         if Line_Diff < 0
           or else (Line_Diff = 0 and then Col_Diff < 0)
         then
            Best_Line := Match.Begin_Line;
            Best_Column := Match.Begin_Column;
         end if;

         return True;
      end Callback;

      Context     : aliased Root_Search_Context;
      L           : Integer := 1;
      C           : Character_Offset_Type := 1;
      Index       : Integer;
      Was_Partial : Boolean;
   begin
      Index  := Buffer'First;

      Set_Context
        (Context'Access,
         Look_For => Str,
         Options  =>
           (Case_Sensitive => Case_Sensitive,
            Whole_Word     => True,
            Regexp         => False));

      Scan_Buffer_No_Scope
        (Context     => Context'Access,
         Buffer      => Buffer,
         Start_Index => Buffer'First,
         End_Index   => Buffer'Last,
         Callback    => Callback'Unrestricted_Access,
         Ref_Index   => Index,
         Ref_Line    => L,
         Ref_Column  => C,
         Was_Partial => Was_Partial);

      Line   := Best_Line;
      Column := Best_Column;
   end Find_Closest_Match;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Context : access Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Context, Kernel);
   begin
      null;
   end Reset;

end Find_Utils;
