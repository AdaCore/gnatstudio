-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Widget;                use Gtk.Widget;
with Files_Extra_Info_Pkg;      use Files_Extra_Info_Pkg;
with Osint;                     use Osint;
with Prj_API;                   use Prj_API;
with Basic_Types;               use Basic_Types;
with Boyer_Moore;               use Boyer_Moore;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with OS_Utils;                  use OS_Utils;

with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;

package body Find_Utils is

   Me : constant Debug_Handle := Create ("Find_Utils");

   procedure Free_Pattern_Matcher is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   procedure Free_Match_Array is new Ada.Unchecked_Deallocation
     (Match_Array, Match_Array_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Match_Result_Array, Match_Result_Array_Access);

   type Recognized_Lexical_States is
     (Statements, Strings, Mono_Comments, Multi_Comments);
   --  Current lexical state of the currently parsed file.
   --
   --  Statements      all but comments and strings
   --  Strings         string literals
   --  Mono_Comments   end of line terminated comments
   --  Multi_Comments  (possibly) multi-line comments

   type Scan_Callback is access
     function (Match : Match_Result) return Boolean;
   --  Callback for a match in a buffer.
   --  If it returns False, no more match will be checked.

   procedure Scan_Buffer_No_Scope
     (Buffer     : String;
      Context    : access Search_Context'Class;
      Callback   : Scan_Callback;
      Ref_Index  : in out Integer;
      Ref_Line   : in out Integer;
      Ref_Column : in out Integer);
   --  Find matches of Context in Buffer, starting at Buffer'FIrst, and until
   --  either the end of the buffer or Callback returns False.
   --  Buffer is assumes to be a single valid scope, and thus no scope handling
   --  is performed.
   --  Ref_Index is assumed to correspond to position Ref_Line and
   --  Ref_Column in the original file. They are automatically updated when new
   --  positions are computed, so that they can be used during the next call to
   --  Scan_Buffer_No_Scope.

   procedure Scan_Buffer
     (Buffer   : String;
      Context  : access Search_Context'Class;
      Callback : Scan_Callback;
      Lang     : Language_Access := null);
   --  Search Context in buffer, searching only in the appropriate scope.
   --  Buffer is assumed to contain complete contexts (e.g the contents of
   --  a whole file).

   procedure Scan_File
     (Context  : access Search_Context'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Name     : String;
      Callback : Scan_Callback);
   --  Search Context in the file Name, searching only in the appropriate
   --  scope.

   function Scan_And_Store
     (Context  : access Search_Context'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Str      : String;
      Is_File  : Boolean;
      Lang     : Language_Access := null) return Match_Result_Array_Access;
   --  Same as above, but behaves as if there was a default callback that
   --  prints the result in the Glide console.
   --  Str can be either a file name or a file contents, depending whether
   --  Is_File is resp. True or False.
   --  It returns the list of matches that were found in the buffer, or null if
   --  no match was found. It is the responsability of the caller to free the
   --  returned array.

   procedure Highlight_Result
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String;
      Match     : Match_Result);
   --  Print the result of the search in the glide console

   function End_Of_Line (Buffer : String; Pos : Natural) return Integer;
   pragma Inline (End_Of_Line);
   --  Return the index for the end of the line containing Pos

   function Is_Word_Delimiter (C : Character) return Boolean;
   pragma Inline (Is_Word_Delimiter);
   --  Return True if C is a character which can't be in a word.

   procedure Free (Result : in out Match_Result_Array_Access);
   --  Free Result and its components

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
         if Buffer (J) = ASCII.LF then
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
     (Context : access Search_Context'Class; Buffer : String) return Integer
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

      Index : Integer := Buffer'First;
      Line, Column : Integer := 0;
   begin
      Scan_Buffer_No_Scope
        (Buffer, Context, Callback'Unrestricted_Access,
         Index, Line, Column);
      return Result;
   end Match;

   --------------------------
   -- Scan_Buffer_No_Scope --
   --------------------------

   procedure Scan_Buffer_No_Scope
     (Buffer     : String;
      Context    : access Search_Context'Class;
      Callback   : Scan_Callback;
      Ref_Index  : in out Integer;
      Ref_Line   : in out Integer;
      Ref_Column : in out Integer)
   is
      Last_Line_Start : Natural := Buffer'First;

      procedure To_Line_Column (Pos : Natural);
      --  Set Line and Column to the appropriate for the Pos-th character in
      --  Buffer.

      procedure Re_Search;
      --  Handle the search for a regular expression

      procedure BM_Search;
      --  Handle the search for a constant string

      --------------------
      -- To_Line_Column --
      --------------------

      procedure To_Line_Column (Pos : Natural) is
      begin
         for J in Ref_Index .. Pos - 1 loop
            if Buffer (J) = ASCII.LF then
               Ref_Line        := Ref_Line + 1;
               Ref_Column      := 1;
               Last_Line_Start := J + 1;
            else
               Ref_Column := Ref_Column + 1;
            end if;
         end loop;

         Ref_Index := Pos;
      end To_Line_Column;

      ---------------
      -- Re_Search --
      ---------------

      procedure Re_Search is
         RE  : constant Pattern_Matcher := Context_As_Regexp (Context);
         Pos : Natural := Buffer'First;
      begin
         loop
            Match
              (RE, Buffer (Pos .. Buffer'Last), Context.Sub_Matches.all);

            exit when Context.Sub_Matches (0) = No_Match;

            Pos := Context.Sub_Matches (0).First;
            To_Line_Column (Pos);

            declare
               Line : constant String :=
                 Buffer (Last_Line_Start .. End_Of_Line (Buffer, Pos));
            begin
               if not Callback (Match_Result'
                 (Length     => Line'Length,
                  Index      => Pos,
                  Line       => Ref_Line,
                  Column     => Ref_Column,
                  End_Column =>
                    Ref_Column + Context.Sub_Matches (0).Last - Pos + 1,
                  Text       => Line))
               then
                  return;
               end if;
            end;

            Pos := Pos + 1;
         end loop;
      end Re_Search;

      ---------------
      -- BM_Search --
      ---------------

      procedure BM_Search is
         BM  : Boyer_Moore.Pattern;
         Pos : Integer := Buffer'First;
      begin
         Context_As_Boyer_Moore (Context, BM);

         --  The loop is optimized so that the search is as efficient as
         --  possible (we scan the whole buffer, instead of line-by-line
         --  search). We then pay a small price to actually compute the
         --  buffer coordinates, but this algorithm is much faster for files
         --  that don't match.

         loop
            Pos := Search (BM, Buffer (Pos .. Buffer'Last));
            exit when Pos = -1;

            if not Context.Options.Whole_Word
              or else
              ((Pos = Buffer'First
                or else Is_Word_Delimiter (Buffer (Pos - 1)))
               and then
               (Pos + Context.Look_For'Length - 1 = Buffer'Last
                or else Is_Word_Delimiter
                (Buffer (Pos + Context.Look_For'Length))))
            then
               To_Line_Column (Pos);

               declare
                  Line : constant String :=
                    Buffer (Last_Line_Start .. End_Of_Line (Buffer, Pos));
               begin
                  if not Callback (Match_Result'
                    (Length     => Line'Length,
                     Index      => Pos,
                     Line       => Ref_Line,
                     Column     => Ref_Column,
                     End_Column => Ref_Column + Context.Look_For'Length,
                     Text       => Line))
                  then
                     return;
                  end if;
               end;
            end if;

            Pos := Pos + 1;
         end loop;
      end BM_Search;

   begin
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

   -----------------
   -- Scan_Buffer --
   -----------------

   procedure Scan_Buffer
     (Buffer     : String;
      Context    : access Search_Context'Class;
      Callback   : Scan_Callback;
      Lang       : Language_Access := null)
   is
      Scope            : constant Search_Scope := Context.Options.Scope;
      Scanning_Allowed : constant array (Recognized_Lexical_States) of Boolean
        := (Statements     => Scope = Whole or else Scope = All_But_Comments,
            Strings        => Scope = Whole
              or else Scope in Comments_And_Strings .. All_But_Comments,
            Mono_Comments  => Scope in Whole .. Comments_And_Strings,
            Multi_Comments => Scope in Whole .. Comments_And_Strings);
      --  Indicates what lexical states are valid, depending on the current
      --  scope.

      procedure Next_Scope_Transition
        (Buffer      : String;
         Pos         : in out Positive;
         State       : in out Recognized_Lexical_States;
         Section_End : out Integer;
         Lang        : Language_Context);
      --  Move Pos to the first character in buffer that isn't in the same
      --  lexical state as State (ie if State is one we want to search in, then
      --  Pos will be left on the first character we do not want to search).
      --
      --  Pos is purely internal, and represents the first character into the
      --  next section (ie after passing the section start string, like -- for
      --  comments). Section_End on the last point in the current section.

      ---------------------------
      -- Next_Scope_Transition --
      ---------------------------

      procedure Next_Scope_Transition
        (Buffer      : String;
         Pos         : in out Positive;
         State       : in out Recognized_Lexical_States;
         Section_End : out Integer;
         Lang        : Language_Context)
      is
         Str_Delim     : Character renames Lang.String_Delimiter;
         Quote_Char    : Character renames Lang.Quote_Character;
         NL_Comm_Start : String    renames Lang.New_Line_Comment_Start;
         M_Comm_Start  : String    renames Lang.Comment_Start;
         M_Comm_End    : String    renames Lang.Comment_End;
         Char_Delim    : Character renames Lang.Constant_Character;

         Looking_For : constant Boolean := not Scanning_Allowed (State);
         --  Whether the final range should or should not be scanned.

      begin
         while Pos <= Buffer'Last
           and then Scanning_Allowed (State) /= Looking_For
         loop
            case State is
               --  Statements end on any other state

               when Statements =>
                  while Pos <= Buffer'Last loop
                     if M_Comm_Start'Length /= 0
                       and then Pos + M_Comm_Start'Length - 1 <= Buffer'Last
                       and then Buffer (Pos .. Pos + M_Comm_Start'Length - 1) =
                       M_Comm_Start
                     then
                        State := Multi_Comments;
                        Section_End := Pos - 1;
                        Pos := Pos + M_Comm_Start'Length;
                        exit;

                     elsif NL_Comm_Start'Length /= 0
                       and then Pos + NL_Comm_Start'Length - 1 <= Buffer'Last
                       and then Buffer (Pos .. Pos + NL_Comm_Start'Length - 1)
                       = NL_Comm_Start
                     then
                        State := Mono_Comments;
                        Section_End := Pos - 1;
                        Pos := Pos + NL_Comm_Start'Length;
                        exit;

                     elsif Buffer (Pos) = Str_Delim
                       and then (Pos = Buffer'First
                                 or else Pos = Buffer'Last
                                 or else Buffer (Pos - 1) /= Char_Delim
                                 or else Buffer (Pos + 1) /= Char_Delim)
                     then
                        State := Strings;
                        Section_End := Pos - 1;
                        Pos := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               --  Strings end on string delimiters

               when Strings =>
                  while Pos <= Buffer'Last loop
                     if Buffer (Pos) = Str_Delim
                       and then (Quote_Char = ASCII.NUL or else
                                 (Pos > Buffer'First and then
                                  Buffer (Pos - 1) /= Quote_Char))
                     then
                        State := Statements;
                        Section_End := Pos - 1;
                        Pos := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               --  Single line comments end on ASCII.LF characters

               when Mono_Comments =>
                  while Pos <= Buffer'Last
                    and then Buffer (Pos) /= ASCII.LF
                  loop
                     Pos := Pos + 1;
                  end loop;

                  Section_End := Pos - 1;
                  if Pos <= Buffer'Last then
                     Pos := Pos + 1;
                  end if;
                  State := Statements;

               --  Multi-line comments end with specific sequences

               when Multi_Comments =>
                  while Pos <= Buffer'Last loop
                     if M_Comm_End'Length /= 0
                       and then Pos + M_Comm_End'Length - 1 <= Buffer'Last
                       and then Buffer (Pos .. Pos + M_Comm_End'Length - 1) =
                       M_Comm_End
                     then
                        State := Statements;
                        Section_End := Pos - 1;
                        Pos := Pos + M_Comm_End'Length;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;
            end case;
         end loop;

         if Pos > Buffer'Last then
            Section_End := Buffer'Last;
         end if;
      end Next_Scope_Transition;

      Pos           : Positive := 1;
      Line_Start    : Positive;
      Line          : Natural := 1;
      Column        : Natural := 1;
      Last_Index    : Positive := 1;
      Section_End   : Integer;
      Lexical_State : Recognized_Lexical_States := Statements;
      Old_State     : Recognized_Lexical_States;

   begin  --  Scan_Buffer
      if Buffer'Length = 0 then
         return;
      end if;

      --  If the language is null, we simply use the more efficient algorithm

      if Context.Options.Scope = Whole or else Lang = null then
         Scan_Buffer_No_Scope
           (Buffer,
            Context,
            Callback,
            Pos, Line, Column);
         return;
      end if;

      declare
         Language : constant Language_Context := Get_Language_Context (Lang);
      begin
         --  Always find the longest possible range, so that we can benefit
         --  as much as possible from the efficient string searching
         --  algorithms.

         while Pos <= Buffer'Last loop
            Line_Start := Pos;
            Old_State  := Lexical_State;

            Next_Scope_Transition
              (Buffer, Pos, Lexical_State, Section_End, Language);

            if Scanning_Allowed (Old_State) then
               Scan_Buffer_No_Scope
                 (Buffer (Line_Start .. Section_End), Context,
                  Callback, Last_Index, Line, Column);
            end if;

            for J in Last_Index .. Pos - 1 loop
               if Buffer (J) = ASCII.LF then
                  Line := Line + 1;
                  Column := 0;
               else
                  Column := Column + 1;
               end if;
            end loop;

            Last_Index := Pos;
         end loop;
      end;
   end Scan_Buffer;

   ---------------
   -- Scan_File --
   ---------------

   procedure Scan_File
     (Context  : access Search_Context'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Name     : String;
      Callback : Scan_Callback)
   is
      Max_File_Len  : constant := 2 ** 21;
      FD            : constant File_Descriptor := Open_Read (Name, Text);
      Lang          : Language_Access;
      Len           : Natural;
      Buffer        : Basic_Types.String_Access;

   begin
      --  ??? Would be nice to handle backward search, which is extremely hard
      --  with regular expressions

      Lang := Get_Language_From_File (Get_Language_Handler (Kernel), Name);

      if FD = Invalid_FD then
         return;
      end if;

      Len := Natural (File_Length (FD));

      --  ??? Temporary, until we are sure that we only manipulate text files.

      if Len > Max_File_Len then
         Close (FD);
         return;
      end if;

      Buffer := new String (1 .. Len);
      Len := Read (FD, Buffer.all'Address, Len);
      Close (FD);

      Scan_Buffer (Buffer (1 .. Len), Context, Callback, Lang);
      Free (Buffer);

   exception
      when Invalid_Context =>
         Close (FD);
         Free (Buffer);
   end Scan_File;

   ----------------------
   -- Highlight_Result --
   ----------------------

   procedure Highlight_Result
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String;
      Match     : Match_Result) is
   begin
      Insert (Kernel,
              File_Name
              & ":" & Image (Match.Line)
              & ":" & Image (Match.Column)
              & ": " & Match.Text);
   end Highlight_Result;

   --------------------
   -- Scan_And_Store --
   --------------------

   function Scan_And_Store
     (Context  : access Search_Context'Class;
      Kernel   : access Kernel_Handle_Record'Class;
      Str      : String;
      Is_File  : Boolean;
      Lang     : Language_Access := null) return Match_Result_Array_Access
   is
      Result : Match_Result_Array_Access := null;
      Count  : Natural := 0;

      function Callback (Match : Match_Result) return Boolean;
      --  Save Match in the result array.

      function Callback (Match : Match_Result) return Boolean is
         Tmp  : Match_Result_Array_Access;
         Size : Natural := 0;
      begin
         Count := Count + 1;

         if Result = null then
            Size := 16;
         elsif Count > Result'Last then
            Size := Result'Last * 2;
         end if;

         if Size /= 0 then
            Tmp := Result;
            Result := new Match_Result_Array (1 .. Size);

            if Tmp /= null then
               Result (1 .. Count - 1) := Tmp (1 .. Count - 1);
               Unchecked_Free (Tmp);
            end if;
         end if;

         Result (Count) := new Match_Result' (Match);
         return True;
      end Callback;

   begin
      if Is_File then
         Scan_File (Context, Kernel, Str, Callback'Unrestricted_Access);
      else
         Scan_Buffer (Str, Context, Callback'Unrestricted_Access, Lang);
      end if;

      return Result;
   end Scan_And_Store;

   -----------------------
   -- Context_As_String --
   -----------------------

   function Context_As_String
     (Context : access Search_Context) return String is
   begin
      if Context.Look_For = null or else Context.Options.Regexp then
         raise Invalid_Context;
      end if;

      return Context.Look_For.all;
   end Context_As_String;

   -----------------------
   -- Context_As_Regexp --
   -----------------------

   function Context_As_Regexp (Context : access Search_Context)
      return GNAT.Regpat.Pattern_Matcher
   is
      Flags : Regexp_Flags := No_Flags;
      WD    : constant String := "\b";  -- Word_Delimiter
   begin
      if Context.RE_Matcher = null then
         if not Context.Options.Regexp or else Context.Look_For = null then
            raise Invalid_Context;
         end if;

         if not Context.Options.Case_Sensitive then
            Flags := Case_Insensitive;
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
     (Context : access Search_Context;
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

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Context  : access Search_Context;
      Look_For : String;
      Options  : Search_Options) is
   begin
      Free (Context.all);
      Context.Look_For       := new String' (Look_For);
      Context.Options        := Options;
      Context.BM_Initialized := False;
   end Set_Context;

   ----------
   -- Free --
   ----------

   procedure Free (Result : in out Match_Result_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Match_Result, Match_Result_Access);
   begin
      if Result /= null then
         for R in Result'Range loop
            Unchecked_Free (Result (R));
         end loop;

         Unchecked_Free (Result);
      end if;
   end Free;

   procedure Free (Context : in out Search_Context) is
   begin
      Free (Context.Look_For);
      Free_Pattern_Matcher (Context.RE_Matcher);
      Free (Context.BM_Matcher);
      Free_Match_Array (Context.Sub_Matches);
   end Free;

   procedure Free (Context : in out Current_File_Context) is
   begin
      Free (Context.Next_Matches_In_File);
      Free (Context.Current_File);
   end Free;

   procedure Free (Context : in out Files_Context) is
   begin
      Directory_List.Free (Context.Dirs);
      Free (Context.Directory);
      Free (Context.Current_File);
      Free (Context.Next_Matches_In_File);
      Free (Search_Context (Context));
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

   procedure Free (Context : in out Files_Project_Context) is
   begin
      Free (Context.Files);
      Free (Context.Next_Matches_In_File);
      Free (Search_Context (Context));
   end Free;

   ----------------------
   -- Set_Current_File --
   ----------------------

   procedure Set_Current_File
     (Context : access Current_File_Context;
      File    : String) is
   begin
      Context.Current_File := new String' (File);
   end Set_Current_File;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : Basic_Types.String_Array_Access) is
   begin
      Free (Context.Files);
      Free (Context.Next_Matches_In_File);
      Context.Files := Files;
      Context.Current_File := Context.Files'First;
   end Set_File_List;

   -------------------
   -- Set_File_List --
   -------------------

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False) is
   begin
      Free (Context.Directory);
      Free (Context.Next_Matches_In_File);
      Context.Files_Pattern := Files_Pattern;
      Context.Recurse := Recurse;

      if Directory = "" then
         Context.Directory := new String' (Get_Current_Dir);
      else
         Context.Directory := new String' (Name_As_Directory (Directory));
      end if;
   end Set_File_List;

   --------------------------
   -- Current_File_Factory --
   --------------------------

   function Current_File_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Extra_Information);
      Context : Current_File_Context_Access;
      Editor : constant Source_Editor_Box :=
        Find_Current_Editor (Kernel);
   begin
      if Editor = null then
         return null;
      else
         Context := new Current_File_Context;
         Set_Current_File (Context, Get_Filename (Editor));
         Context.All_Occurences := All_Occurences;
         return Search_Context_Access (Context);
      end if;
   end Current_File_Factory;

   --------------------------------
   -- Files_From_Project_Factory --
   --------------------------------

   function Files_From_Project_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Extra_Information, All_Occurences);

      Context : Files_Project_Context_Access;
   begin
      Context := new Files_Project_Context;
      Set_File_List
        (Context,
         Get_Source_Files (Get_Project_View (Kernel), True));
      return Search_Context_Access (Context);
   end Files_From_Project_Factory;

   -------------------
   -- Files_Factory --
   -------------------

   function Files_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Kernel, All_Occurences);

      Context : Files_Context_Access;
      Extra   : constant Files_Extra_Info_Access := Files_Extra_Info_Access
        (Extra_Information);
      Re      : GNAT.Regexp.Regexp;

   begin
      if Get_Text (Extra.Files_Entry) /= "" then
         Context := new Files_Context;
         Re := Compile
           (Get_Text (Extra.Files_Entry),
            Glob => True,
            Case_Sensitive => Integer (Get_File_Names_Case_Sensitive) /= 0);
         Set_File_List
           (Context,
            Files_Pattern => Re,
            Directory     => Get_Text (Extra.Directory_Entry),
            Recurse       => Get_Active (Extra.Subdirs_Check));
         return Search_Context_Access (Context);
      end if;

      return null;
   exception
      when Error_In_Regexp =>
         return null;
   end Files_Factory;

   ------------
   -- Search --
   ------------

   function Search
     (Context         : access Current_File_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      Editor : Source_Editor_Box;
      Lang   : Language_Access;
      Match  : Match_Result_Access;
      Inc    : Integer;
      Line, Column : Natural;

   begin
      Open_File_Editor (Kernel, Filename => Context.Current_File.all);
      Editor := Get_Source_Box_From_MDI
        (Get_File_Editor (Kernel, Context.Current_File.all));

      if Editor = null then
         return False;
      end if;

      if Search_Backward then
         Inc := -1;
      else
         Inc := 1;
      end if;

      --  If there are still some matches in the current file that we haven't
      --  returned, do it now.

      if Context.Next_Matches_In_File /= null then
         Context.Last_Match_Returned := Context.Last_Match_Returned + Inc;

         --  Start from the beginning again if possible
         if Context.Last_Match_Returned <= 0
           or else Context.Last_Match_Returned
             > Context.Next_Matches_In_File'Last
           or else Context.Next_Matches_In_File (Context.Last_Match_Returned)
           = null
         then
            Context.Last_Match_Returned := Context.Next_Matches_In_File'First;
         end if;

         Match :=
           Context.Next_Matches_In_File (Context.Last_Match_Returned);
         Unhighlight_All (Editor);

         if Is_Valid_Location (Editor, Match.Line, Match.End_Column) then
            Highlight_Region
              (Editor,
               Match.Line, Match.Column,
               Match.Line, Match.End_Column);
            Set_Cursor_Location (Editor, Match.Line, Match.Column);

            return True;

         else
            --  ??? Contents of buffer has changed. See comment below about
            --  the use of tags.

            Unhighlight_All (Editor);
            Free (Context.Next_Matches_In_File);
            return False;
         end if;
      end if;

      if Editor = null then
         return False;
      end if;

      --  First search

      Lang := Get_Language_From_File
        (Get_Language_Handler (Kernel), Get_Filename (Editor));

      declare
         Buffer : constant String := Get_Slice (Editor, 1, 1);
      begin
         Context.Next_Matches_In_File := Scan_And_Store
           (Context, Kernel, Buffer, Is_File => False, Lang => Lang);
      end;

      if Context.Next_Matches_In_File = null then
         return False;
      end if;

      --  ??? Should use tags to take into account changes in the buffer
      --  between two searches.

      if Search_Backward then
         for J in reverse Context.Next_Matches_In_File'Range loop
            if Context.Next_Matches_In_File (J) /= null then
               Context.Last_Match_Returned := J;
               exit;
            end if;
         end loop;
      else
         Context.Last_Match_Returned := Context.Next_Matches_In_File'First;

         if not Context.All_Occurences then
            Get_Cursor_Location (Editor, Line, Column);

            loop
               Match :=
                 Context.Next_Matches_In_File (Context.Last_Match_Returned);
               exit when Match.Line > Line
                 or else (Match.Line = Line and then Match.Column >= Column);
               Context.Last_Match_Returned := Context.Last_Match_Returned + 1;
            end loop;
         end if;
      end if;

      Match :=
        Context.Next_Matches_In_File (Context.Last_Match_Returned);
      Unhighlight_All (Editor);
      Highlight_Region
        (Editor, Match.Line, Match.Column, Match.Line, Match.End_Column);
      Set_Cursor_Location (Editor, Match.Line, Match.Column);
      return True;

   exception
      when E : others =>
         Trace (Me, "unexpected exception: " & Exception_Information (E));
         return False;
   end Search;

   function Search
     (Context         : access Files_Project_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      pragma Unreferenced (Search_Backward);
   begin
      --  If there are still some matches in the current file that we haven't
      --  returned, do it now.

      if Context.Next_Matches_In_File /= null then
         Context.Last_Match_Returned := Context.Last_Match_Returned + 1;
         if Context.Last_Match_Returned <= Context.Next_Matches_In_File'Last
           and then Context.Next_Matches_In_File (Context.Last_Match_Returned)
             /= null
         then
            Highlight_Result
              (Kernel, Context.Files (Context.Current_File - 1).all,
               Context.Next_Matches_In_File (Context.Last_Match_Returned).all);
            return True;
         else
            Free (Context.Next_Matches_In_File);
         end if;
      end if;

      if Context.Files = null then
         return False;
      end if;

      --  Loop until at least one match
      loop
         if Context.Current_File > Context.Files'Last then
            return False;
         end if;

         Context.Next_Matches_In_File := Scan_And_Store
           (Context, Kernel, Context.Files (Context.Current_File).all,
            Is_File => True);
         Context.Current_File := Context.Current_File + 1;

         exit when Context.Next_Matches_In_File /= null;
      end loop;

      Context.Last_Match_Returned := Context.Next_Matches_In_File'First;
      Highlight_Result
        (Kernel, Context.Files (Context.Current_File - 1).all,
         Context.Next_Matches_In_File (Context.Last_Match_Returned).all);
      return True;
   end Search;

   function Search
     (Context         : access Files_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      pragma Unreferenced (Search_Backward);

      use Directory_List;
      File_Name : String (1 .. Max_Path_Len);
      Last      : Natural;

   begin
      --  If there are still some matches in the current file that we haven't
      --  returned , do it now.

      if Context.Next_Matches_In_File /= null then
         Context.Last_Match_Returned := Context.Last_Match_Returned + 1;
         if Context.Last_Match_Returned <= Context.Next_Matches_In_File'Last
           and then Context.Next_Matches_In_File (Context.Last_Match_Returned)
           /= null
         then
            Highlight_Result
              (Kernel, Context.Current_File.all,
               Context.Next_Matches_In_File (Context.Last_Match_Returned).all);
            return True;
         else
            Free (Context.Next_Matches_In_File);
         end if;
      end if;

      if Context.Directory = null then
         return False;
      end if;

      if Context.Dirs = Null_List then
         Prepend (Context.Dirs, new Dir_Data);
         Head (Context.Dirs).Name := new String' (Context.Directory.all);
         Open (Head (Context.Dirs).Dir, Context.Directory.all);
      end if;

      while Context.Next_Matches_In_File = null loop
         Read (Head (Context.Dirs).Dir, File_Name, Last);

         if Last = 0 then
            Next (Context.Dirs);

            if Context.Dirs = Null_List then
               return False;
            end if;

         else
            declare
               Full_Name : constant String :=
                 Head (Context.Dirs).Name.all & File_Name (1 .. Last);
            begin
               if Is_Directory (Full_Name) then
                  if Context.Recurse
                    and then File_Name (1 .. Last) /= "."
                    and then File_Name (1 .. Last) /= ".."
                    and then not Is_Symbolic_Link (File_Name (1 .. Last))
                  --  ??? Do not try to follow symbolic links for now,
                  --  so that we avoid infinite recursions.
                  then
                     Prepend (Context.Dirs, new Dir_Data);
                     Head (Context.Dirs).Name := new String'
                       (Name_As_Directory (Full_Name));
                     Open (Head (Context.Dirs).Dir, Full_Name);
                  end if;

               --  ??? Should check that we have a text file
               elsif Match (File_Name (1 .. Last), Context.Files_Pattern) then
                  Context.Next_Matches_In_File :=
                    Scan_And_Store
                      (Context, Kernel, Full_Name, Is_File => True);

                  if Context.Next_Matches_In_File /= null then
                     Free (Context.Current_File);
                     Context.Current_File := new String' (Full_Name);
                  end if;
               end if;
            end;
         end if;
      end loop;

      Context.Last_Match_Returned := Context.Next_Matches_In_File'First;
      Highlight_Result
        (Kernel, Context.Current_File.all,
         Context.Next_Matches_In_File (Context.Last_Match_Returned).all);
      return True;

   exception
      when Directory_Error =>
         return False;
   end Search;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Dir_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dir_Data, Dir_Data_Access);
   begin
      Close (D.Dir);
      Free (D.Name);
      Unchecked_Free (D);
   end Free;

   -------------
   -- Replace --
   -------------

   function Replace
     (Context         : access Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Search_Backward : Boolean) return Boolean
   is
      pragma Unreferenced (Context, Kernel, Replace_String, Search_Backward);
   begin
      return False;
   end Replace;

end Find_Utils;
