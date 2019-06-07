------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Strings.Maps;          use Ada.Strings.Maps;
with GNAT.Expect;
with GNAT.Strings;              use GNAT.Strings;

with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Language_Handlers;         use Language_Handlers;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;

with GNATCOLL.SQL;              use GNATCOLL.SQL;
with GNATCOLL.SQL.Inspect;      use GNATCOLL.SQL.Inspect;
with GNAT.Regpat;               use GNAT.Regpat;

package body Xref.Docgen is

   --------
   -- No --
   --------

   function No (Comment : Comment_Result) return Boolean is
   begin
      return Comment = No_Comment_Result;
   end No;

   -------------
   -- Present --
   -------------

   function Present (Comment : Comment_Result) return Boolean is
   begin
      return Comment /= No_Comment_Result;
   end Present;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Entity : Root_Entity'Class) return General_Location
   is
      Decl : constant General_Entity_Declaration :=
               Get_Declaration (Entity);
   begin
      return Decl.Loc;
   end Get_Location;

   --------------------------------
   -- GNATCOLL Package Extension --
   --------------------------------

   package GNATCOLL_Extensions is

      function Comment
        (Self     : Xref_Database;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Language : Language_Syntax;
         Format   : Formatting := Text;
         End_Loc  : General_Location := No_Location) return Comment_Result;
      --  Returns the comment (extracted from the source file) for the entity.
      --  This is looked for just before or just after the declaration of the
      --  entity.

      function Extract_Comment
        (Buffer           : String;
         Decl_Start_Line  : Integer;
         Decl_Start_Index : Integer;
         Decl_End_Index   : Integer;
         Language         : Language_Syntax;
         Format           : Formatting := Text) return Comment_Result;
      --  Extra comment from the source code, given the range of an entity
      --  declaration. This program is made public so that you can reuse it
      --  if you need to override Comment below, or have other means to get the
      --  information about an entity's location (for instance, in an IDE where
      --  the editor might change and the LI files are not regenerated
      --  immediately).
      --  In this version, the start and end of the declaration are given as
      --  indexes in Buffer.

      function Extract_Comment
        (Buffer            : String;
         Decl_Start_Line   : Integer;
         Decl_Start_Column : Integer;
         Decl_End_Line     : Integer := -1;
         Decl_End_Column   : Integer := -1;
         Language          : Language_Syntax;
         Format            : Formatting := Text) return Comment_Result;
      --  Same as above, but the scope of the declaration is given as line and
      --  column. By default, the end is on the same position as the start.

   end GNATCOLL_Extensions;

   --------------------------------
   -- GNATCOLL Package Extension --
   --------------------------------

   package body GNATCOLL_Extensions is

      procedure Skip_To_Next_Comment_Start
        (Context : Language_Syntax;
         Buffer  : String;
         Index   : in out Natural);
      --  Skip lines of code until we find the beginning of a comment.
      --  If we see an empty line first Index is set to 0.
      --  Likewise if no comment is found before the end of the buffer.

      package Duplicate is

         --  Routines in this package are duplicates of routines locally
         --  defined in GNATCOLL.Xref which are temporarily needed here
         --  to develop the new docgen routines.

         procedure Get_Documentation_Before
           (Context       : Language_Syntax;
            Buffer        : String;
            Decl_Index    : Natural;
            Comment_Start : out Natural;
            Comment_End   : out Natural;
            Allow_Blanks  : Boolean := False);
         procedure Get_Documentation_After
           (Context       : Language_Syntax;
            Buffer        : String;
            Decl_Index    : Natural;
            Comment_Start : out Natural;
            Comment_End   : out Natural);
         --  Get the comment just before or just after Decl_Index, skipping
         --  code lines as needed. If Allow_Blanks is True, then skip blank
         --  lines before looking for comments.

         type Comment_Type is
           (No_Comment, Comment_Single_Line, Comment_Multi_Line);

         function Looking_At_Start_Of_Comment
           (Context : Language_Syntax;
            Buffer  : String;
            Index   : Natural) return Comment_Type;
         --  Whether we have the start of a comment at Index in Buffer

         procedure Skip_To_Current_Comment_Block_End
           (Context            : Language_Syntax;
            Buffer             : String;
            Index              : in out Natural;
            Ignore_Blank_Lines : Boolean := False);
         --  Same as Skip_To_Current_Comment_Block_Start, except we move
         --  forward to the beginning of the last line of comments in the
         --  block. If Ignore_Blank_Lines is set to True, blocks separated
         --  from one another with blank lines are considered as a single one.

         procedure Skip_To_Current_Comment_Block_Start
           (Context : Language_Syntax;
            Buffer  : String;
            Index   : in out Natural);
         --  Assuming that Index is at the beginning or inside a comment line,
         --  moves upward in the file till the end of the current block of
         --  comments. This block is defined as a group of commented out lines,
         --  until a non-comment line is seen. If Index is not at the beginning
         --  or inside a comment line, Index is set to 0.

         procedure Skip_To_Previous_Comment_Start
           (Context      : Language_Syntax;
            Buffer       : String;
            Index        : in out Natural;
            Allow_Blanks : Boolean := False);
         --  Skip lines of code (backward) until we find the start of a
         --  comment. If we see an empty line first Index is set to 0, unless
         --  Allow_Blanks. Likewise if no comment is found before the beginning
         --  of the buffer.

      end Duplicate;

      package body Duplicate is

         -----------------------------
         -- Get_Documentation_After --
         -----------------------------

         procedure Get_Documentation_After
           (Context       : Language_Syntax;
            Buffer        : String;
            Decl_Index    : Natural;
            Comment_Start : out Natural;
            Comment_End   : out Natural)
         is
         begin
            --  Else look after the comment after the declaration (which is the
            --  first block of comments after the declaration line, and not
            --  separated by a blank line)
            Comment_Start := Decl_Index;
            Skip_To_Next_Comment_Start (Context, Buffer, Comment_Start);
            Comment_End := Comment_Start;

            if Comment_Start /= 0 then
               Skip_To_Current_Comment_Block_End
                 (Context, Buffer, Comment_End);
               Comment_End := Line_End (Buffer, Comment_End);
            end if;
         end Get_Documentation_After;

         ------------------------------
         -- Get_Documentation_Before --
         ------------------------------

         procedure Get_Documentation_Before
           (Context       : Language_Syntax;
            Buffer        : String;
            Decl_Index    : Natural;
            Comment_Start : out Natural;
            Comment_End   : out Natural;
            Allow_Blanks  : Boolean := False) is
         begin
            Comment_Start := Decl_Index;
            Skip_To_Previous_Comment_Start
              (Context, Buffer, Comment_Start, Allow_Blanks);
            Comment_End := Comment_Start;

            if Comment_Start /= 0 then
               Skip_To_Current_Comment_Block_End
                 (Context, Buffer, Comment_End);
               Comment_End := Line_End (Buffer, Comment_End);
            end if;
         end Get_Documentation_Before;

         ---------------------------------
         -- Looking_At_Start_Of_Comment --
         ---------------------------------

         function Looking_At_Start_Of_Comment
           (Context : Language_Syntax;
            Buffer  : String;
            Index   : Natural) return Comment_Type
         is
            use type GNAT.Expect.Pattern_Matcher_Access;

         begin
            if Context.New_Line_Comment_Start /= null
              and then Index + Context.New_Line_Comment_Start'Length
                <= Buffer'Last
              and then Buffer
                (Index .. Index + Context.New_Line_Comment_Start'Length - 1) =
                Context.New_Line_Comment_Start.all
            then
               return Comment_Single_Line;
            end if;

            if Context.New_Line_Comment_Start_Regexp /= null
              and then Match (Context.New_Line_Comment_Start_Regexp.all,
                              Buffer, Data_First => Index)
            then
               return Comment_Single_Line;
            end if;

            if Context.Comment_Start /= null
              and then Index + Context.Comment_Start'Length <= Buffer'Last
              and then
                Buffer (Index .. Index + Context.Comment_Start'Length - 1)
                = Context.Comment_Start.all
            then
               return Comment_Multi_Line;
            end if;

            return No_Comment;
         end Looking_At_Start_Of_Comment;

         ---------------------------------------
         -- Skip_To_Current_Comment_Block_End --
         ---------------------------------------

         procedure Skip_To_Current_Comment_Block_End
           (Context            : Language_Syntax;
            Buffer             : String;
            Index              : in out Natural;
            Ignore_Blank_Lines : Boolean := False)
         is
            Last_Comment_Index : Integer := Index;
            Typ                : Comment_Type;
            Lines_Skipped      : Natural;
         begin
            Block_Iteration : loop
               Typ := Looking_At_Start_Of_Comment (Context, Buffer, Index);

               case Typ is
               when No_Comment =>
                  Index := Last_Comment_Index;
                  exit Block_Iteration;

                  when Comment_Single_Line =>
                  Index := Line_End (Buffer, Index);

                  declare
                     Tmp : Integer := Index;
                  begin
                     loop
                        Skip_Lines (Buffer, 1, Tmp, Lines_Skipped);

                        exit when Lines_Skipped /= 1;

                        while Tmp <= Buffer'Last
                          and then
                            (Buffer (Tmp) = ' ' or Buffer (Tmp) = ASCII.HT)
                        loop
                           Tmp := Tmp + 1;
                        end loop;

                        exit when
                          Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
                          No_Comment;

                        Index := Tmp;
                     end loop;
                  end;

               when Comment_Multi_Line =>
                  Skip_To_String (Buffer, Index, Context.Comment_End.all);
                  Index := Index - 1;
               end case;

               if Ignore_Blank_Lines then
                  Last_Comment_Index := Index;
                  Skip_Lines (Buffer, 1, Index, Lines_Skipped);

                  exit Block_Iteration when Lines_Skipped /= 1;

                  Skip_Blanks (Buffer, Index);
               else
                  exit Block_Iteration;
               end if;

            end loop Block_Iteration;
         end Skip_To_Current_Comment_Block_End;

         -----------------------------------------
         -- Skip_To_Current_Comment_Block_Start --
         -----------------------------------------

         procedure Skip_To_Current_Comment_Block_Start
           (Context : Language_Syntax;
            Buffer  : String;
            Index   : in out Natural)
         is
            Initial_Index : constant Natural := Index;
            Lines_Skipped : Natural;
            Tmp           : Integer;

            function Only_Blanks_Before
              (Buffer : String;
               Index  : Natural)
            return Boolean;
            --  Return True if there are only blanks characters before the one
            --  pointed by Index in Buffer.
            --  Return False otherwise.

            ------------------------
            -- Only_Blanks_Before --
            ------------------------

            function Only_Blanks_Before
              (Buffer : String;
               Index  : Natural)
            return Boolean
            is
               Tmp : Natural := Index - 1;
            begin
               Skip_Blanks_Backward (Buffer, Tmp);
               return Buffer'First = Tmp + 1;
            end Only_Blanks_Before;

         begin
            --  Are we in a multi-line comment ?

            if Context.Comment_End /= null then
               Tmp := Line_End (Buffer, Index);

               if Tmp - Context.Comment_End'Length + 1 >= Index
                 and then Buffer
                   (Tmp - Context.Comment_End'Length + 1 .. Tmp) =
                   Context.Comment_End.all
               then -- The end of a multi-line comment has been found
                  while Index >= Buffer'First
                    and then Buffer
                      (Index .. Index + Context.Comment_Start'Length - 1) /=
                      Context.Comment_Start.all
                  loop
                     Index := Index - 1;
                  end loop;

                  if Looking_At_Start_Of_Comment (Context, Buffer, Index) =
                    Comment_Multi_Line
                  then -- The beginning of a multi-line comment has been found
                     return;
                  end if;
               end if;
            end if;

            --  Check for single line comments

            Tmp := Initial_Index;

            loop
               while Tmp <= Buffer'Last
                 and then (Buffer (Tmp) = ' ' or else Buffer (Tmp) = ASCII.HT)
               loop
                  Tmp := Tmp + 1;
               end loop;

               exit when Looking_At_Start_Of_Comment (Context, Buffer, Tmp) =
                 No_Comment;

               Index := Tmp;

               exit when Only_Blanks_Before (Buffer, Tmp);

               Skip_Lines (Buffer, -1, Tmp, Lines_Skipped);

               exit when Lines_Skipped /= 1;
            end loop;

            if Looking_At_Start_Of_Comment (Context, Buffer, Index)
              = No_Comment
            then
               Index := 0;
            end if;
         end Skip_To_Current_Comment_Block_Start;

         ------------------------------------
         -- Skip_To_Previous_Comment_Start --
         ------------------------------------

         procedure Skip_To_Previous_Comment_Start
           (Context      : Language_Syntax;
            Buffer       : String;
            Index        : in out Natural;
            Allow_Blanks : Boolean := False)
         is
            Lines_Skipped   : Natural;
            No_Blanks       : Boolean := not Allow_Blanks;
            Non_Blank_Found : Boolean := False;
         begin
            if Index = Buffer'First then
               Skip_Blanks (Buffer, Index);

               if Looking_At_Start_Of_Comment
                 (Context, Buffer, Index) /= No_Comment
               then
                  Skip_To_Current_Comment_Block_Start (Context, Buffer, Index);
               else
                  Index := 0;
               end if;

               return;
            end if;

            loop
               Skip_Lines (Buffer, -1, Index, Lines_Skipped);

               exit when Lines_Skipped /= 1;

               if Is_Blank_Line (Buffer, Index) then
                  exit when No_Blanks;
               else
                  if Non_Blank_Found then
                     --  No longer allow blank lines after a first non blank
                     --  one
                     No_Blanks := True;
                  else
                     Non_Blank_Found := True;
                  end if;

                  Skip_Blanks (Buffer, Index);

                  if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
                    No_Comment
                  then
                     Skip_To_Current_Comment_Block_Start
                       (Context, Buffer, Index);
                     return;
                  end if;
               end if;
            end loop;

            Index := 0;
         end Skip_To_Previous_Comment_Start;

      end Duplicate;
      use Duplicate;

      --------------------------------
      -- Skip_To_Next_Comment_Start --
      --------------------------------

      procedure Skip_To_Next_Comment_Start
        (Context : Language_Syntax;
         Buffer  : String;
         Index   : in out Natural)
      is
         Orig_Index : constant Natural := Index;
         Lines_Skipped : Natural;
      begin
         Skip_Lines (Buffer, 1, Index, Lines_Skipped);

         --  Search for a comment in the current line
         for J in Orig_Index .. Index loop
            if Looking_At_Start_Of_Comment (Context, Buffer, J) /=
              No_Comment
            then
               Index := J;
               return;
            end if;
         end loop;

         Index := Orig_Index;
         while Index < Buffer'Last loop
            Skip_Lines (Buffer, 1, Index, Lines_Skipped);

            exit when Lines_Skipped /= 1 or else Is_Blank_Line (Buffer, Index);

            Skip_Blanks (Buffer, Index);

            if Looking_At_Start_Of_Comment (Context, Buffer, Index) /=
              No_Comment
            then
               return;
            end if;
         end loop;

         Index := 0;
      end Skip_To_Next_Comment_Start;

      ---------------------
      -- Extract_Comment --
      ---------------------

      function Extract_Comment
        (Buffer           : String;
         Decl_Start_Line  : Integer;
         Decl_Start_Index : Integer;
         Decl_End_Index   : Integer;
         Language         : Language_Syntax;
         Format           : Formatting := Text) return Comment_Result
      is
         pragma Unreferenced (Format);
         Beginning : Natural;
         Current   : Natural;
         Result    : GNATdoc.Unbounded_String_Vectors.Vector;
         Pos, Last : Integer;

         Leading_Spaces : Integer := -1;
         --  Maximum number of spaces to remove in the comments. This
         --  is computed from the first line of the comment block (by
         --  left-aligning the text on that line), and is meant to preserve
         --  indentation for later lines (in particular if they contain code
         --  sample for instance)

         C_Result : Comment_Result := No_Comment_Result;

         Aux_Index    : Integer;
         Comment_Line : Integer;
         Skipped      : Integer;
         Line_Step    : Integer := 1;
      begin
         Get_Documentation_Before
           (Context       => Language,
            Buffer        => Buffer,
            Decl_Index    => Decl_Start_Index,
            Comment_Start => Beginning,
            Comment_End   => Current,
            Allow_Blanks  => False);

         if Beginning /= 0 then
            Line_Step := -1;

         else
            Get_Documentation_After
              (Context       => Language,
               Buffer        => Buffer,
               Decl_Index    => Decl_End_Index,
               Comment_Start => Beginning,
               Comment_End   => Current);
         end if;

         --  Cleanup comment marks
         if Beginning /= 0 then
            Comment_Line := Decl_Start_Line;
            Aux_Index := Decl_Start_Index;

            if Line_Step = -1 then
               while Aux_Index > Beginning loop
                  Comment_Line := Comment_Line - 1;
                  Skip_Lines
                    (Buffer,
                     Lines         => -1,
                     Index         => Aux_Index,
                     Lines_Skipped => Skipped);

                  if Skipped /= 1 then
                     return No_Comment_Result;
                  end if;
               end loop;

               C_Result.Start_Line := Comment_Line;

            else
               while Aux_Index < Beginning loop
                  Comment_Line := Comment_Line + 1;
                  Skip_Lines
                    (Buffer,
                     Lines         => 1,
                     Index         => Aux_Index,
                     Lines_Skipped => Skipped);

                  if Skipped /= 1 then  --  End of file???
                     return No_Comment_Result;
                  end if;
               end loop;

               C_Result.Start_Line := Comment_Line - 1;
            end if;

            Pos := Beginning;

            if Language.Comment_Start /= null
              and then Starts_With
                (Buffer (Pos .. Current), Language.Comment_Start.all)
            then
               Pos := Pos + Language.Comment_Start'Length;
            end if;

            if Language.Comment_End /= null
              and then Ends_With
                (Buffer (Pos .. Current), Language.Comment_End.all)
            then
               Current := Current - Language.Comment_End'Length;
            end if;
            Skip_Blanks_Backward (Buffer (Pos .. Current), Current);

            while Pos <= Current loop
               Last := EOL (Buffer (Pos .. Current - 1));

               if Language.New_Line_Comment_Start /= null then
                  Skip_Blanks (Buffer, Pos);

                  if Starts_With (Buffer (Pos .. Buffer'Last),
                                  Language.New_Line_Comment_Start.all)
                  then
                     Pos := Pos + Language.New_Line_Comment_Start'Length;
                  end if;

                  if Leading_Spaces = -1 then
                     --  First line, compute the number of spaces

                     Leading_Spaces := Pos;
                     Skip_Blanks (Buffer (Pos .. Buffer'Last), Pos);
                     Leading_Spaces := Pos - Leading_Spaces;
                  end if;

                  --  Remove leading blanks, to preserve indented code in the
                  --  comments.
                  for N in 1 .. Leading_Spaces loop
                     if Pos <= Buffer'Last
                       and then Buffer (Pos) = ' '
                     then
                        Pos := Pos + 1;
                     else
                        exit;
                     end if;
                  end loop;
               end if;

               if Pos = Last then
                  Result.Append (Null_Unbounded_String);

               else
                  Result.Append (To_Unbounded_String (Buffer (Pos .. Last)));
               end if;
               Pos := Last + 1;
            end loop;

            C_Result.Text := Result;
         end if;

         return C_Result;
      end Extract_Comment;

      ---------------------
      -- Extract_Comment --
      ---------------------

      function Extract_Comment
        (Buffer            : String;
         Decl_Start_Line   : Integer;
         Decl_Start_Column : Integer;
         Decl_End_Line     : Integer := -1;
         Decl_End_Column   : Integer := -1;
         Language          : Language_Syntax;
         Format            : Formatting := Text) return Comment_Result
      is
         Start, Last, Skipped : Integer;
      begin
         Start := Buffer'First;
         Skip_Lines
           (Buffer,
            Lines         => Decl_Start_Line - 1,
            Index         => Start,
            Lines_Skipped => Skipped);
         if Skipped /= Decl_Start_Line - 1 then
            return No_Comment_Result;
         end if;

         Skip_To_Column
           (Buffer,
            Columns => Decl_Start_Column - 1,
            Index   => Start);

         Last := Start;

         if Decl_End_Line /= -1 then
            Skip_Lines
              (Buffer,
               Lines         => Decl_End_Line - Decl_Start_Line,
               Index         => Last,
               Lines_Skipped => Skipped);

            if Decl_End_Column /= -1 then
               Skip_To_Column (Buffer, Decl_End_Column, Last);
            end if;
         end if;

         return Extract_Comment
           (Buffer           => Buffer,
            Decl_Start_Line  => Decl_Start_Line,
            Decl_Start_Index => Start,
            Decl_End_Index   => Last,
            Language         => Language,
            Format           => Format);
      end Extract_Comment;

      -------------
      -- Comment --
      -------------

      function Comment
        (Self     : Xref_Database;
         Buffer   : GNAT.Strings.String_Access;
         Location : General_Location;
         Language : Language_Syntax;
         Format   : Formatting := Text;
         End_Loc  : General_Location := No_Location) return Comment_Result
      is
         pragma Unreferenced (Self);

         Load_Buffer : constant Boolean := Buffer = null;
         Aux_Buffer  : GNAT.Strings.String_Access;

         procedure Free_Buffer;
         procedure Free_Buffer is
         begin
            if Load_Buffer then
               Free (Aux_Buffer);
            end if;
         end Free_Buffer;

      begin
         if Location = No_Location then
            return No_Comment_Result;
         end if;

         if Load_Buffer then
            Aux_Buffer := Location.File.Read_File;
         else
            Aux_Buffer := Buffer;
         end if;

         if Aux_Buffer /= null then
            declare
               Result : constant Comment_Result := Extract_Comment
                 (Buffer            => Aux_Buffer.all,
                  Decl_Start_Line   => Location.Line,
                  Decl_Start_Column => Integer (Location.Column),
                  Decl_End_Line     => (if End_Loc = No_Location then -1
                                           else End_Loc.Line),
                  Decl_End_Column   => (if End_Loc = No_Location then -1
                                           else Integer (End_Loc.Column)),
                  Language          => Language,
                  Format            => Format);

            begin
               if not Result.Text.Is_Empty then
                  Free_Buffer;

                  return Result;
               end if;
            end;

            Free_Buffer;
         end if;

         return No_Comment_Result;
      end Comment;

   end GNATCOLL_Extensions;

   ----------------------
   -- Local Subprogram --
   ----------------------

   function Doc_From_LI
     (Self           : access General_Xref_Database_Record;
      Handler        : Language_Handlers.Language_Handler;
      Buffer         : GNAT.Strings.String_Access;
      Location       : General_Location;
      Form           : Formatting;
      With_Text_Decl : Boolean := True;
      End_Loc        : General_Location := No_Location) return Comment_Result;

   -----------------
   -- Doc_From_LI --
   -----------------

   function Doc_From_LI
     (Self           : access General_Xref_Database_Record;
      Handler        : Language_Handlers.Language_Handler;
      Buffer         : GNAT.Strings.String_Access;
      Location       : General_Location;
      Form           : Formatting;
      With_Text_Decl : Boolean := True;
      End_Loc        : General_Location := No_Location) return Comment_Result
   is
      pragma Unreferenced (With_Text_Decl);

      Context : constant Language.Language_Context_Access :=
        Language.Get_Language_Context
          (Get_Language_From_File (Handler, Source_Filename => Location.File));

   begin
      if Location /= No_Location then
         declare
            C_Result : Comment_Result :=
              GNATCOLL_Extensions.Comment
                (Self     => Xref_Database (Self.Xref.all),
                 Buffer   => Buffer,
                 Location => Location,
                 End_Loc  => End_Loc,
                 Language => Context.Syntax,
                 Format   => Form);

            --  This call will be replaced by:
            --   Self.Xref.Comment
            --     (Buffer, Entity.Entity, Context.Syntax, Form);

         begin
            for J in C_Result.Text.First_Index .. C_Result.Text.Last_Index loop
               C_Result.Text (J) :=
                 Ada.Strings.Unbounded.Trim
                   (C_Result.Text (J),
                    Left => Ada.Strings.Maps.Null_Set,
                    Right => Ada.Strings.Maps.To_Set
                      (' ' & ASCII.HT & ASCII.LF & ASCII.CR));
            end loop;

            return C_Result;
         end;
      end if;

      return No_Comment_Result;
   end Doc_From_LI;

   ------------------------------
   -- Get_Docgen_Documentation --
   ------------------------------

   function Get_Docgen_Documentation
     (Self     : access General_Xref_Database_Record;
      Handler  : Language_Handlers.Language_Handler;
      Buffer   : GNAT.Strings.String_Access;
      Location : General_Location;
      End_Loc  : General_Location := No_Location) return Comment_Result
   is
   begin
      return
        Doc_From_LI
          (Self, Handler, Buffer, Location,
           End_Loc => End_Loc, Form => Text, With_Text_Decl => False);
   end Get_Docgen_Documentation;

end Xref.Docgen;
