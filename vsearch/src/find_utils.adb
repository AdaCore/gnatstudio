-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Language; use Language;
with OS_Utils; use OS_Utils;

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

package body Find_Utils is

   procedure Common_Init
     (Search     : out Code_Search;
      Look_For   : String;
      Match_Case : Boolean;
      Whole_Word : Boolean;
      Regexp     : Boolean;
      Scope      : Search_Scope);
   --  Initialize file-independent fields.
   --
   --  Raise Search_Error if:
   --  * Look_For is empty, or can't compile

   -----------------
   -- Common_Init --
   -----------------

   procedure Common_Init
     (Search     : out Code_Search;
      Look_For   : String;
      Match_Case : Boolean;
      Whole_Word : Boolean;
      Regexp     : Boolean;
      Scope      : Search_Scope)
   is
      Flags : Regexp_Flags := No_Flags;
      WD    : constant String := "\b";  -- Word_Delimiter

   begin
      if Look_For = "" then
         raise Search_Error;
      end if;

      if not Match_Case then
         Flags := Case_Insensitive;
      end if;

      if Regexp then
         if Whole_Word then
            Search.Pattern :=
              new Pattern_Matcher' (Compile (WD & Look_For & WD, Flags));

         else
            Search.Pattern :=
              new Pattern_Matcher' (Compile (Look_For, Flags));
         end if;
      else
         if Whole_Word then
            Search.Pattern := new Pattern_Matcher'
              (Compile (WD & Quote (Look_For) & WD, Flags));

         elsif Match_Case then
            null;

            --  Search will be optimized by Ada.Strings.Fixed.Index

         else
            Search.Pattern := new Pattern_Matcher'
              (Compile (Quote (Look_For), Flags));
         end if;
      end if;

      Search.Look_For   := new String' (Look_For);
      Search.Match_Case := Match_Case;
      Search.Whole_Word := Whole_Word;
      Search.Regexp     := Regexp;
      Search.Scope      := Scope;

   exception
      when Expression_Error =>
         raise Search_Error;
   end Common_Init;

   ---------------
   -- Do_Search --
   ---------------

   procedure Do_Search
     (Search   : in out Code_Search;
      Callback : Poll_Search_Handler)
   is
      function Explore_Directory (Directory : String) return Boolean;
      --  Explore the directory to scan files.
      --  Sub-directories are recursively explored iff Search.Recurse.
      --  Scanned files are selected with Search.Files_Pattern.
      --  Return False when the search was aborted.
      --
      --  Directory: Location of beginning of the search.

      function Scan_File (Name : String) return Boolean;
      --  Determine the language context of the file, and then scan it.
      --  Return False when the search was aborted.
      --
      --  Name: The file to scan

      function Scan_File_Without_Context (Name : String) return Boolean;
      --  Scan the file in order to find matches, with no context.
      --  Callback is called whenever a match is found, and at the end of the
      --  file.
      --  Return False when the search was aborted.
      --
      --  Name: The file to scan

      function Scan_File_With_Context
        (Name    : String;
         Context : Language_Context) return Boolean;
      --  Scan the file in order to find matches, using the given context.
      --  Callback is called whenever a match is found, and at the end of the
      --  file.
      --  Return False when the search was aborted.
      --
      --  Name     The file to scan
      --  Context  The language syntactic context used within the file

      function Scan_Line_With_Context
        (Name    : String;
         Line    : String;
         Line_Nr : Positive;
         Context : Language_Context) return Boolean;
      --  Scan a line given the current lexical state and the language context.
      --  Callback isn't called iff no match occurs in the line within allowed
      --  lexical states (ie called once when multiple matches).
      --  Return False when the search was aborted.
      --
      --  Name     File currently scanned
      --  Line     Line to check for matches
      --  Line_Nr  Line number
      --  Context  The language syntactic context used within the file

      function Contain_Match (Text : String) return Boolean;
      --  Return False iff no match occurs within the given text (ie True if a
      --  match or more).
      --
      --  Text: Text to check for matches

      -------------------
      -- Contain_Match --
      -------------------

      function Contain_Match (Text : String) return Boolean is
         use Ada.Strings.Fixed;
      begin
         --  ??? Should compute the following test once.

         if Search.Match_Case
           and then not Search.Whole_Word
           and then not Search.Regexp
         then
            return Index (Text, Search.Look_For.all) /= 0;
         else
            return Match (Search.Pattern.all, Text) /= Text'First - 1;
         end if;
      end Contain_Match;

      -----------------------
      -- Explore_Directory --
      -----------------------

      function Explore_Directory (Directory : String) return Boolean is
         Continue  : Boolean := True;
         Dir_Name  : constant Dir_Name_Str (1 .. Directory'Length + 1) :=
                       Directory & Directory_Separator;
         Dir       : Dir_Type;
         File_Name : String (1 .. Max_Path_Len);
         Last      : Natural;

      begin
         Open (Dir, Dir_Name);

         loop
            Read (Dir, File_Name, Last);

            exit when Last = 0;

            declare
               Full_Name : constant String (1 .. Dir_Name'Length + Last) :=
                             Dir_Name & File_Name (1 .. Last);
            begin
               --  Unlike Open_Read, Is_Directory adds ASCII.NUL automatically

               if Is_Directory (Full_Name) then
                  if Search.Recurse
                    and then File_Name (1 .. Last) /= "."
                    and then File_Name (1 .. Last) /= ".."
                  then
                     Continue := Explore_Directory (Full_Name);
                  end if;
               elsif Match (File_Name (1 .. Last), Search.Files_Pattern) then
                  Continue := Scan_File (Full_Name);
               end if;
            end;

            exit when not Continue;
         end loop;

         Close (Dir);
         return Continue;

      exception
         when Directory_Error =>
            --  Ignore opening error

            return True;
      end Explore_Directory;

      ---------------
      -- Scan_File --
      ---------------

      function Scan_File (Name : String) return Boolean is
         Language : Language_Access := Get_Language_From_File (Name);
      begin
         if Search.Scope = Whole or else Language = null then
            return Scan_File_Without_Context (Name);
         else
            return Scan_File_With_Context
              (Name, Get_Language_Context (Language));
         end if;
      end Scan_File;

      ----------------------------
      -- Scan_File_With_Context --
      ----------------------------

      function Scan_File_With_Context
        (Name    : String;
         Context : Language_Context) return Boolean
      is
         FD       : constant File_Descriptor :=
           Open_Read (Name & ASCII.NUL, Text);
         Continue : Boolean := True;

      begin
         if FD = Invalid_FD then
            return Callback (False, Name);
         end if;

         declare
            Len        : Natural := Natural (File_Length (FD));
            Buffer     : aliased String (1 .. Len);
            Pos        : Positive := 1;
            Line_Start : Positive;
            Line_Nr    : Positive := 1;

         begin
            Len := Read (FD, Buffer'Address, Len);

            Search.Lexical_State := Statements;

            --  NOTE: Empty files are skipped

            while Pos <= Len loop
               Line_Start := Pos;

               while Pos <= Len and then Buffer (Pos) /= ASCII.LF loop
                  Pos := Pos + 1;
               end loop;

               Continue :=
                 Scan_Line_With_Context
                   (Name, Buffer (Line_Start .. Pos - 1), Line_Nr, Context);

               exit when not Continue;

               --  Skip ASCII.LF

               Pos := Pos + 1;
               Line_Nr := Line_Nr + 1;
            end loop;
         end;

         Close (FD);
         return Continue and then Callback (False, Name);
      end Scan_File_With_Context;

      -------------------------------
      -- Scan_File_Without_Context --
      -------------------------------

      function Scan_File_Without_Context (Name : String) return Boolean is
         FD       : constant File_Descriptor :=
           Open_Read (Name & ASCII.NUL, Text);
         Continue : Boolean := True;

      begin
         if FD = Invalid_FD then
            return Callback (False, Name);
         end if;

         declare
            Len        : Natural := Natural (File_Length (FD));
            Buffer     : aliased String (1 .. Len);
            Pos        : Positive := 1;
            Line_Start : Positive;
            Line_Nr    : Positive := 1;

         begin
            Len := Read (FD, Buffer'Address, Len);

            --  NOTE: Empty files are skipped

            while Pos <= Len loop
               Line_Start := Pos;

               while Pos <= Len and then Buffer (Pos) /= ASCII.LF loop
                  Pos := Pos + 1;
               end loop;

               if Contain_Match (Buffer (Line_Start .. Pos - 1)) then
                  Continue :=
                    Callback
                      (True, Name, Line_Nr, Buffer (Line_Start .. Pos - 1));

                  exit when not Continue;
               end if;

               --  Skip ASCII.LF

               Pos := Pos + 1;
               Line_Nr := Line_Nr + 1;
            end loop;
         end;

         Close (FD);
         return Continue and then Callback (False, Name);
      end Scan_File_Without_Context;

      ----------------------------
      -- Scan_Line_With_Context --
      ----------------------------

      function Scan_Line_With_Context
        (Name    : String;
         Line    : String;
         Line_Nr : Positive;
         Context : Language_Context) return Boolean
      is
         Scanning_Allowed :
           constant array (Recognized_Lexical_States) of Boolean :=
             (Statements     => Search.Scope = All_But_Comm,
              Strings        => Search.Scope in Comm_Str .. All_But_Comm,
              Mono_Comments  => Search.Scope in Comm_Only .. Comm_Str,
              Multi_Comments => Search.Scope in Comm_Only .. Comm_Str);

         EOL : constant Natural := Line'Last;  -- End Of Line

         Reached : Positive;
         Pos     : Positive;
         --  Search.Lexical_State applies on Line (Reached .. Pos - 1)

         Next               : Positive;
         Next_Lexical_State : Recognized_Lexical_States;
         --  Next_Lexical_State applies from Line (Next)

         Str_Delim     : Character renames Context.String_Delimiter;
         Quote_Char    : Character renames Context.Quote_Character;
         NL_Comm_Start : String    renames Context.New_Line_Comment_Start;
         M_Comm_Start  : String    renames Context.Comment_Start;
         M_Comm_End    : String    renames Context.Comment_End;

         NL_Comm_Start_Length : Natural renames
                                  Context.New_Line_Comment_Start_Length;
         M_Comm_Start_Length  : Natural renames Context.Comment_Start_Length;
         M_Comm_End_Length    : Natural renames Context.Comment_End_Length;

         Called   : Boolean := False; --  Was callback called on this line ?
         Continue : Boolean := True;  --  really necessary ???

      begin
         Reached := Line'First;

         Whole_Line_Loop : loop
            Pos := Reached;

            --  Default values when the line contains no other lexical state

            Next := EOL + 1;
            Next_Lexical_State := Search.Lexical_State;

            case Search.Lexical_State is
               when Statements =>
                  while Pos <= EOL loop
                     if M_Comm_Start_Length /= 0
                       and then Pos + M_Comm_Start_Length - 1 <= EOL
                       and then Line (Pos .. Pos + M_Comm_Start_Length - 1)
                                = M_Comm_Start
                     then
                        Next_Lexical_State := Multi_Comments;
                        Next := Pos + M_Comm_Start_Length;
                        exit;

                     elsif NL_Comm_Start_Length /= 0
                       and then Pos + NL_Comm_Start_Length - 1 <= EOL
                       and then Line (Pos .. Pos + NL_Comm_Start_Length - 1)
                                = NL_Comm_Start
                     then
                        Next_Lexical_State := Mono_Comments;
                        Next := Pos + NL_Comm_Start_Length;
                        exit;

                     elsif Line (Pos) = Str_Delim then
                        Next_Lexical_State := Strings;
                        Next := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               when Strings =>
                  while Pos <= EOL loop
                     if Line (Pos) = Str_Delim
                       and then (Quote_Char = ASCII.NUL or else
                                 (Pos > Line'First and then
                                  Line (Pos - 1) /= Quote_Char))
                     then
                        Next_Lexical_State := Statements;
                        Next := Pos + 1;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;

               when Mono_Comments =>
                  Pos := EOL + 1;
                  Next_Lexical_State := Statements;

               when Multi_Comments =>
                  while Pos <= EOL loop
                     if M_Comm_End_Length /= 0
                       and then Pos + M_Comm_End_Length - 1 <= EOL
                       and then Line (Pos .. Pos + M_Comm_End_Length - 1)
                                = M_Comm_End
                     then
                        Next_Lexical_State := Statements;
                        Next := Pos + M_Comm_End_Length;
                        exit;
                     end if;

                     Pos := Pos + 1;
                  end loop;
            end case;

            if not Called
              and then Scanning_Allowed (Search.Lexical_State)
              and then Contain_Match (Line (Reached .. Pos - 1))
            then
               Continue := Callback (True, Name, Line_Nr, Line);
               Called := True;

               exit Whole_Line_Loop when not Continue;

               --  ??? improve by 'exit when not Callback'
               --  ??? OK since Search.Lexical_State doesn't need to be updated
               --  ??? and is no longer called.
            end if;

            --  Skip the processed text

            Reached := Next;
            Search.Lexical_State := Next_Lexical_State;

            exit Whole_Line_Loop when Reached > EOL;
         end loop Whole_Line_Loop;

         return Continue;
      end Scan_Line_With_Context;

   --  Start of processing for Do_Search

      Continue : Boolean;

   begin
      pragma Assert (Callback /= null);

      if Search.Files = null then
         Continue := Explore_Directory (Search.Directory.all);
      else
         for F in Search.Files'Range loop
            if Search.Files (F) /= null
              and then Search.Files (F).all /= ""
            then
               Continue := Scan_File (Search.Files (F).all);

               exit when not Continue;
            end if;
         end loop;
      end if;
   end Do_Search;

   ----------
   -- Free --
   ----------

   procedure Free (S : in out Code_Search) is
      procedure Free_String is new
        Standard.Ada.Unchecked_Deallocation (String, String_Access);

      procedure Free_Pattern_Matcher is new
        Standard.Ada.Unchecked_Deallocation
          (Pattern_Matcher, Pattern_Matcher_Access);

   begin
      Free_String (S.Look_For);
      Free_String (S.Directory);
      Free_Pattern_Matcher (S.Pattern);
   end Free;

   -----------------
   -- Init_Search --
   -----------------

   procedure Init_Search
     (Search     : out Code_Search;
      Look_For   : String;
      Files      : Project_Files_Access;
      Match_Case : Boolean := False;
      Whole_Word : Boolean := False;
      Regexp     : Boolean := False;
      Scope      : Search_Scope := Whole) is
   begin
      if Files = null then
         raise Search_Error;
      end if;

      Common_Init
        (Search, Look_For, Match_Case, Whole_Word, Regexp, Scope);
      Search.Files := Files;
   end Init_Search;

   -----------------
   -- Init_Search --
   -----------------

   procedure Init_Search
     (Search        : out Code_Search;
      Look_For      : String;
      Files_Pattern : Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False;
      Match_Case    : Boolean := False;
      Whole_Word    : Boolean := False;
      Regexp        : Boolean := False;
      Scope         : Search_Scope := Whole)
   is
      Result : Boolean;
   begin
      --  Ensure Files_Pattern is initialized

      begin
         Result := Match ("", Files_Pattern);
      exception
         when Constraint_Error =>
            raise Search_Error;
      end;

      Common_Init
        (Search, Look_For, Match_Case, Whole_Word, Regexp, Scope);
      Search.Files_Pattern := Files_Pattern;
      Search.Recurse := Recurse;

      if Directory = "" then
         Search.Directory := new String' (Get_Current_Dir);
      else
         Search.Directory := new String' (Directory);
      end if;
   end Init_Search;

end Find_Utils;
