------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Basic_Types;              use Basic_Types;
with Docgen3.Comment;          use Docgen3.Comment;
with Docgen3.Utils;            use Docgen3.Utils;
with Docgen3.Errout;           use Docgen3.Errout;
with Docgen3.Frontend.Builder; use Docgen3.Frontend.Builder;
with Docgen3.Time;             use Docgen3.Time;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Utils;
with Language;                 use Language;
with Language.Ada;
with Language.Tree;            use Language.Tree;
with Language.Tree.Database;   use Language.Tree.Database;
with Traces;                   use Traces;
with Xref.Docgen;              use Xref.Docgen;
with Xref;

package body Docgen3.Frontend is
   Me : constant Debug_Handle := Create ("Docgen3.1-Frontend");

   ----------------------
   -- Local_Subrograms --
   ----------------------

   procedure Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type);
   --  Add to the nodes their blocks of documentation & sources

   procedure Build_Structured_Comments
     (Context   : access constant Docgen_Context;
      Root      : Entity_Id;
      In_C_Lang : Boolean);
   --  Traverse the Tree of entities and replace blocks of comments by
   --  structured comments.

   ---------------------------------
   -- Append_Comments_And_Sources --
   ---------------------------------

   procedure Add_Documentation_From_Sources
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type)
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang     : constant Boolean := not In_Ada_Lang;

      Buffer           : GNAT.Strings.String_Access;
      C_Headers_Buffer : GNAT.Strings.String_Access;
      Buffers_Swapped  : Boolean;

      procedure Ada_Get_Doc (E : Entity_Id);
      --  Retrieve the documentation associated with E

      procedure Ada_Get_Source (E : Entity_Id);
      --  Retrieve the Ada source associated with E

      procedure CPP_Get_Doc (E : Entity_Id);
      --  Retrieve the C/C++ documentation associated with E

      procedure CPP_Get_Source (E : Entity_Id);
      --  Retrieve the C/C++ source associated with E

      procedure Previous_Word
        (Index           : Natural;
         Prev_Word_Begin : out Natural;
         Prev_Word_End   : out Natural);
      --  Return the indexes to the first word in Buffer located before Index

      function Search_Backward
        (Word : String;
         From : Natural) return Natural;
      --  Search backward in Buffer (starting at index From) for the index
      --  of Word (in lowercase).

      function Skip_Blanks_Backward (Index : Natural) return Natural;
      --  Displace Idx backwards skipping character ' '

      procedure Swap_Buffers;
      --  Swap the contents of Buffer and C_Headers_Buffer. Used to retrieve
      --  the sources and the documentation located in the header file.

      -----------------
      -- Ada_Get_Doc --
      -----------------

      procedure Ada_Get_Doc (E : Entity_Id) is
      begin
         --  Skip processing entities defined in other files (for example,
         --  primitives inherited from the parent type).

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         Set_Doc (E,
           Xref.Docgen.Get_Docgen_Documentation
             (Self =>
                General_Xref_Database_Record (Context.Database.all)'Access,
              Handler  => Context.Lang_Handler,
              Buffer   => Buffer,
              Location => LL.Get_Location (E)));

         --           Set_Doc (E,
         --             Db.Get_Docgen_Documentation
         --               (Handler => Lang_Handler,
         --                Buffer  => Buffer,
         --                Entity  => LL.Get_Entity (E)));

         if Is_Partial_View (E) then
            Set_Full_View_Doc (E,
              Xref.Docgen.Get_Docgen_Documentation
                (Self =>
                   General_Xref_Database_Record (Context.Database.all)'Access,
                 Handler => Context.Lang_Handler,
                 Buffer  => Buffer,
                 Location => LL.Get_Body_Loc (E)));
         end if;
      end Ada_Get_Doc;

      --------------------
      -- Ada_Get_Source --
      --------------------

      procedure Ada_Get_Source (E : Entity_Id) is

         function Get_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the declaration E

         function Get_Record_Type_Source
           (Loc          : General_Location;
            Is_Full_View : Boolean := False) return Unbounded_String;
         --  Retrieve the source of record type E

         function Get_Subprogram_Source return Unbounded_String;
         --  Retrieve the source of subprogram E

         function Get_Type_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the declaration E

         ----------------------------
         -- Get_Declaration_Source --
         ----------------------------

         function Get_Declaration_Source return Unbounded_String is
            Printout      : Unbounded_String;
            From          : Natural;
            Idx           : Natural;
            Index         : Natural;
            Lines_Skipped : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);
            From := Index;

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            Idx := Index;
            while Idx < Buffer'Last
              and then Buffer (Idx) /= ';'
            loop
               Idx := Idx + 1;
            end loop;

            Printout := To_Unbounded_String (Buffer (From .. Idx));

            return Printout;
         end Get_Declaration_Source;

         ----------------------------
         -- Get_Record_Type_Source --
         ----------------------------

         function Get_Record_Type_Source
           (Loc          : General_Location;
            Is_Full_View : Boolean := False) return Unbounded_String
         is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            End_Record_Found : Boolean := False;
            In_Parent_Part   : Boolean := False;
            --  In_Parent_Part is set when we identify the sequence "is new"
            --  or the sequence "interface and" which indicate that the next
            --  token corresponds with the parent type of a tagged type or an
            --  interface type.

            Is_Interface : Boolean := False;
            Is_Null      : Boolean := False;
            Tagged_Null  : Boolean := False;
            With_Null    : Boolean := False;

            type Tokens is
              (Tok_Unknown,
               Tok_Abstract,
               Tok_And,
               Tok_Aliased,
               Tok_Case,
               Tok_End,
               Tok_Interface,
               Tok_Is,
               Tok_Limited,
               Tok_New,
               Tok_Null,
               Tok_Others,
               Tok_Private,
               Tok_Record,
               Tok_Tagged,
               Tok_Type,
               Tok_When,
               Tok_With);

            Prev_Token : Tokens := Tok_Unknown;
            Token      : Tokens := Tok_Unknown;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

               function Get_Token return Tokens;
               --  Return the token associated with S

               function Get_Token return Tokens is
                  Keyword : constant String := To_Lower (S);

               begin
                  if Keyword = "abstract" then
                     return Tok_Abstract;
                  elsif Keyword = "aliased" then
                     return Tok_Aliased;
                  elsif Keyword = "and" then
                     return Tok_And;
                  elsif Keyword = "case" then
                     return Tok_Case;
                  elsif Keyword = "end" then
                     return Tok_End;
                  elsif Keyword = "is" then
                     return Tok_Is;
                  elsif Keyword = "interface" then
                     return Tok_Interface;
                  elsif Keyword = "limited" then
                     return Tok_Limited;
                  elsif Keyword = "new" then
                     return Tok_New;
                  elsif Keyword = "null" then
                     return Tok_Null;
                  elsif Keyword = "others" then
                     return Tok_Others;
                  elsif Keyword = "private" then
                     return Tok_Private;
                  elsif Keyword = "record" then
                     return Tok_Record;
                  elsif Keyword = "tagged" then
                     return Tok_Tagged;
                  elsif Keyword = "type" then
                     return Tok_Type;
                  elsif Keyword = "when" then
                     return Tok_When;
                  elsif Keyword = "with" then
                     return Tok_With;
                  else
                     return Tok_Unknown;
                  end if;
               end Get_Token;

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Identifier_Text then

                  --  No action needed if we already know the parent type

                  if In_Parent_Part
                    and then No (Get_Parent (E))
                  then
                     --  In the partial view Xref returns the parent in the
                     --  list of parents (and hence, at current stage it is
                     --  stored in the list of progenitors). We localize it
                     --  and remove it from the list of progenitors.

                     if not Is_Full_View
                       and then Is_Tagged_Type (E)
                     then
                        declare
                           Parent : Entity_Id;
                        begin
                           Parent :=
                             Find_Entity (Get_Progenitors (E).all, Name => S);
                           pragma Assert (Present (Parent));

                           Set_Parent (E, Parent);
                           Delete_Entity (Get_Progenitors (E).all, Parent);
                        end;

                     --  Derivation of untagged type. For instance:
                     --     type Rec_A is record ...
                     --     subtype Rec_B is Rec_A;

                     elsif not Is_Full_View
                       and then not Is_Tagged_Type (E)
                     then
                        --  Unhandled yet???
                        null;

                     --  If the parent of a private type is only specified
                     --  in its full view, then Xref cannot locate it because
                     --  currently the compiler does not generate such
                     --  information in the ALI file (see comment in
                     --  docgen3-atree.adb).

                     else
                        if not Is_Tagged_Type (E) then
                           Set_Is_Tagged_Type (E);
                        end if;

                        --  We don't know the exact location associated with
                        --  the entity in the database. Hence for now we take a
                        --  conservative approach and we first retry the entity
                        --  from the database and use its location to retry its
                        --  associated unique high-level entity.

                        declare
                           Tok_Loc   : General_Location;
                           LL_Parent : General_Entity;
                           Parent    : Entity_Id;

                        begin
                           Tok_Loc :=
                             General_Location'
                               (File   => File,
                                Line   => Loc.Line + Sloc_Start.Line - 1,
                                Column =>
                                  Visible_Column_Type (Sloc_Start.Column));

                           LL_Parent :=
                             Xref.Get_Entity
                               (Db   => Context.Database,
                                Name => Get_Short_Name (S),
                                Loc  => Tok_Loc);
                           pragma Assert (Present (LL_Parent));

                           Parent :=
                             Builder.Find_Unique_Entity
                               (Get_Location (Context.Database, LL_Parent));
                           pragma Assert (Present (Parent));

                           Set_Parent (E, Parent);
                        end;
                     end if;

                     In_Parent_Part := False;
                  end if;

               elsif Entity = Keyword_Text then
                  Prev_Token := Token;
                  Token      := Get_Token;

                  if Prev_Token = Tok_Is then
                     if Token = Tok_New then
                        In_Parent_Part := True;
                     elsif Token = Tok_Interface then
                        Is_Interface := True;
                     elsif Token = Tok_Null then
                        Is_Null := True;
                     end if;

                  elsif (Prev_Token = Tok_Is or else Prev_Token = Tok_Abstract)
                    and then Token = Tok_Tagged
                  then
                     --  Complete the decoration of this type since Xref does
                     --  not facilitate decorating well tagged types that
                     --  have no primitives (for example, a root of derivation
                     --  defined as abstract tagged null record without
                     --  primitives)

                     if not Is_Tagged_Type (E) then
                        Set_Is_Tagged_Type (E);
                     end if;

                  elsif Prev_Token = Tok_Interface
                    and then Token = Tok_And
                  then
                     In_Parent_Part := True;

                  elsif Prev_Token = Tok_Tagged
                    and then Token = Tok_Null
                  then
                     Tagged_Null := True;

                  elsif Prev_Token = Tok_With
                    and then Token = Tok_Null
                  then
                     With_Null := True;

                  elsif (Is_Null
                           or else Tagged_Null
                           or else With_Null
                           or else Prev_Token = Tok_End)
                    and then Token = Tok_Record
                  then
                     End_Record_Found := True;
                  end if;

               elsif Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;
                  elsif S = ";" then
                     if Par_Count = 0 then
                        return Prev_Token = Tok_Private
                          or else End_Record_Found
                          or else Is_Interface;
                     end if;
                  end if;
               end if;

               return False; --  Continue
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            From            : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;

         --  Start of processing for Get_Record_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => Loc.Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (Loc.Column),
               Index         => Entity_Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            Prev_Word_Begin :=
              Search_Backward ("type", From => Entity_Index - 1);

            --  Append tabulation

            if Buffer (Prev_Word_Begin - 1) = ' ' then
               From := Skip_Blanks_Backward (Prev_Word_Begin - 1);

               if Buffer.all (From) = ASCII.LF then
                  From := From + 1;
               end if;

               Append (Buffer.all (From .. Prev_Word_Begin - 1));
            else
               From := Prev_Word_Begin;
            end if;

            --  For simple cases (interfaces, incomplete and private types)
            --  there is no need to use the Ada parser. We just need to locate
            --  the ';'

            if Is_Incomplete_Or_Private_Type (E)
              and then not Is_Full_View
            then
               declare
                  Idx : Natural := Entity_Index - 1;
               begin
                  while Idx > Buffer'First
                    and then Buffer (Idx) /= ';'
                  loop
                     Idx := Idx + 1;
                  end loop;

                  Append (Buffer.all (Prev_Word_Begin .. Idx + 1));
               end;

            else
               Parse_Entities
                 (Lang, Buffer.all (From .. Buffer'Last),
                  CB'Unrestricted_Access);
            end if;

            return Printout;
         end Get_Record_Type_Source;

         ---------------------------
         -- Get_Subprogram_Source --
         ---------------------------

         function Get_Subprogram_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               --  if Entity = Block_Text then  --  identifier???
               --     return False; --  continue

               if Entity = Comment_Text then
                  return False; --  continue

               elsif Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;
                  elsif S = ";" then
                     if Par_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            Index         : Natural;
            Lines_Skipped : Natural;

         --  Start of processing for Get_Subprogram_Source

         begin
            --  Displace the pointer to the beginning of the subprogram
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            Parse_Entities
              (Lang, Buffer.all (Index .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Subprogram_Source;

         ---------------------------------
         -- Get_Type_Declaration_Source --
         ---------------------------------

         function Get_Type_Declaration_Source return Unbounded_String is
            Printout      : Unbounded_String;
            From          : Natural;
            Idx           : Natural;
            Index         : Natural;
            Lines_Skipped : Natural;
            Par_Count     : Natural;

         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            --  Locate the beginning of the type declaration. This is a naive
            --  approach used in the prototype since it does not handle the
            --  word "type" located in a comment. A backward parser is required
            --  here. Must be improved???

            Index :=
              Search_Backward ("type", From => Index - 1);

            --  Append tabulation

            if Buffer (Index - 1) = ' ' then
               From := Skip_Blanks_Backward (Index - 1);
               Printout :=
                 To_Unbounded_String (Buffer.all (From .. Index - 1));
            end if;

            --  Parenthesis count used to handle access to subprogram types

            Par_Count := 0;

            Idx := Index;
            while Idx < Buffer'Last
              and then (Par_Count > 0 or else Buffer (Idx) /= ';')
            loop
               if Buffer (Idx) = '(' then
                  Par_Count := Par_Count + 1;
               elsif Buffer (Idx) = ')' then
                  Par_Count := Par_Count - 1;
               end if;

               Idx := Idx + 1;
            end loop;

            Printout :=
              Printout & To_Unbounded_String (Buffer (Index .. Idx));

            return Printout;
         end Get_Type_Declaration_Source;

      --  Start of processing for Ada_Get_Source

      begin
         --  Skip processing entities defined in other files (for example,
         --  primitives inherited of the parent type).

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         if LL.Is_Subprogram (E) then
            Set_Src (E, Get_Subprogram_Source);

         elsif Is_Class_Or_Record_Type (E) then
            Set_Src (E, Get_Record_Type_Source (LL.Get_Location (E)));

            if Is_Partial_View (E) then
               Set_Full_View_Src (E,
                 Get_Record_Type_Source
                   (LL.Get_Body_Loc (E), Is_Full_View => True));
            end if;

         elsif LL.Is_Type (E) then
            Set_Src (E, Get_Type_Declaration_Source);

         elsif Get_Kind (E) = E_Variable then
            Set_Src (E, Get_Declaration_Source);

         else
            Set_Src (E,
              To_Unbounded_String
                ("<<Get_Source under development for this kind of entity>>"));
         end if;
      end Ada_Get_Source;

      -----------------
      -- CPP_Get_Doc --
      -----------------

      procedure CPP_Get_Doc (E : Entity_Id) is
      begin
         --  No action needed if we already retrieved the documentation of
         --  this entity

         if Get_Doc (E) /= No_Comment_Result then
            return;
         end if;

         --  Documentation of C/C++ macros unsupported yet???

         if Get_Kind (E) = E_Macro then
            return;
         end if;

         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  C_Headers_Buffer := LL.Get_Location (E).File.Read_File;
               end if;

               Swap_Buffers;
            end if;
         end if;

         --  Skip processing entities defined in other files (for example,
         --  primitives inherited from the parent type).

         if LL.Get_Location (E).File /= File then
            return;
         end if;

         Set_Doc (E,
           Xref.Docgen.Get_Docgen_Documentation
             (Self =>
                General_Xref_Database_Record (Context.Database.all)'Access,
              Handler  => Context.Lang_Handler,
              Buffer   => Buffer,
              Location => LL.Get_Location (E)));

         --  Restore original contents of buffers

         if Buffers_Swapped then
            Swap_Buffers;
         end if;
      end CPP_Get_Doc;

      --------------------
      -- CPP_Get_Source --
      --------------------

      procedure CPP_Get_Source (E : Entity_Id) is

         function Get_Class_Type_Source return Unbounded_String;
         --  Retrieve the source of class type E

         function Get_Struct_Type_Source return Unbounded_String;
         --  Retrieve the source of record type E (To be removed???)

         function Get_Subprogram_Source return Unbounded_String;
         --  Retrieve the source of subprogram E

         function Get_Variable_Declaration_Source return Unbounded_String;
         --  Retrieve the source of the variable/object declaration E

         ---------------------------
         -- Get_Class_Type_Source --
         ---------------------------

         function Get_Class_Type_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Brackets_Count : Natural := 0;
            Last_Idx       : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "{" then
                     Brackets_Count := Brackets_Count + 1;
                  elsif S = "}" then
                     Brackets_Count := Brackets_Count - 1;
                  elsif S = ";"
                     and then Brackets_Count = 0
                  then
                     return True;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;

         --  Start of processing for Get_Class_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Entity_Index);

            --  Locate the beginning of the previous word
            declare
               Idx : Natural := Entity_Index - 1;
            begin
               while Idx > Buffer'First
                 and then (Buffer (Idx) = ' '
                           or else Buffer (Idx) = ASCII.LF)
               loop
                  Idx := Idx - 1;
               end loop;
               Prev_Word_End := Idx;

               while Idx > Buffer'First
                 and then (Buffer (Idx) /= ' '
                             and then Buffer (Idx) /= ASCII.LF)
               loop
                  Idx := Idx - 1;
               end loop;
               Prev_Word_Begin := Idx + 1;
            end;

            pragma Assert
              (To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End))
                = "class");

            Last_Idx := Prev_Word_Begin - 1;
            Parse_Entities
              (Lang, Buffer.all (Prev_Word_Begin .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Class_Type_Source;

         ----------------------------
         -- Get_Record_Type_Source --
         ----------------------------

         function Get_Struct_Type_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Brackets_Count : Natural := 0;
            Last_Idx       : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "{" then
                     Brackets_Count := Brackets_Count + 1;
                  elsif S = "}" then
                     Brackets_Count := Brackets_Count - 1;
                  elsif S = ";" then
                     if Brackets_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            --  Local variables

            Entity_Index    : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;

         --  Start of processing for Get_Struct_Type_Source

         begin
            --  Displace the pointer to the beginning of the record type
            --  declaration

            Entity_Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Entity_Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Entity_Index);

            --  Supported cases of C structs:

            --     struct Foo                 --  [1]
            --     | typedef struct ...       --  [2]
            --     | static const struct ...  --  [3]

            --  For [1] Entity_Index references Foo; for [2] and [3] it points
            --  to the first letter of the word "struct"

            declare
               Index : Natural := Entity_Index;

            begin
               loop
                  Previous_Word (Index, Prev_Word_Begin, Prev_Word_End);

                  declare
                     Prev_Word : constant String :=
                                   To_Lower (Buffer.all
                                     (Prev_Word_Begin .. Prev_Word_End));
                  begin
                     exit when  Prev_Word /= "struct"
                       and then Prev_Word /= "const"
                       and then Prev_Word /= "static";

                     Index := Prev_Word_Begin;
                  end;
               end loop;

               Parse_Entities
                 (Lang, Buffer.all (Index .. Buffer'Last),
                  CB'Unrestricted_Access);
            end;

            return Printout;
         end Get_Struct_Type_Source;

         ---------------------------
         -- Get_Subprogram_Source --
         ---------------------------

         function Get_Subprogram_Source return Unbounded_String is
            Printout : Unbounded_String;

            procedure Append (Text : String);
            --  Append Text to Printout

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            -----------------
            -- Append_Line --
            -----------------

            procedure Append (Text : String) is
            begin
               Printout := Printout & Text;
            end Append;

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;
            Last_Idx  : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               S : String renames
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

            begin
               --  Print all text between previous call and current one

               if Last_Idx /= 0 then
                  Append (Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;
               Append (S);

               if Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;

                     if Par_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            Index         : Natural;
            Lines_Skipped : Natural;

         --  Start of processing for Get_Subprogram_Source

         begin
            --  Displace the pointer to the beginning of the subprogram
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            Parse_Entities
              (Lang, Buffer.all (Index .. Buffer'Last),
               CB'Unrestricted_Access);

            return Printout;
         end Get_Subprogram_Source;

         -------------------------------------
         -- Get_Variable_Declaration_Source --
         -------------------------------------

         function Get_Variable_Declaration_Source return Unbounded_String is
            Printout        : Unbounded_String;
            Idx             : Natural;
            Index           : Natural;
            Lines_Skipped   : Natural;
            Prev_Word_Begin : Natural;
            Prev_Word_End   : Natural;
         begin
            --  Displace the pointer to the beginning of the declaration
            Index := Buffer'First;
            GNATCOLL.Utils.Skip_Lines
              (Str           => Buffer.all,
               Lines         => LL.Get_Location (E).Line - 1,
               Index         => Index,
               Lines_Skipped => Lines_Skipped);

            GNATCOLL.Utils.Skip_To_Column
              (Str           => Buffer.all,
               Columns       => Natural (LL.Get_Location (E).Column),
               Index         => Index);

            if Index > Buffer'First then
               Previous_Word (Index - 1, Prev_Word_Begin, Prev_Word_End);
            else
               Prev_Word_Begin := Index;
            end if;

            Idx := Index;
            while Idx < Buffer'Last
              and then Buffer (Idx) /= ';'
            loop
               Idx := Idx + 1;
            end loop;

            Printout := To_Unbounded_String (Buffer (Prev_Word_Begin .. Idx));

            return Printout;
         end Get_Variable_Declaration_Source;

         Under_Development_Text : constant String :=
           "<<Get_Source under development for this kind of entity>>";

      --  Start of processing for CPP_Get_Source

      begin
         --  Documentation of C/C++ macros unsupported yet???

         if Get_Kind (E) = E_Macro then
            return;
         end if;

         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  C_Headers_Buffer := LL.Get_Location (E).File.Read_File;
               end if;

               Swap_Buffers;
            end if;
         end if;

         if LL.Is_Subprogram (E) then
            Set_Src (E, Get_Subprogram_Source);

         elsif Get_Kind (E) = E_Class then
            Set_Src (E, Get_Class_Type_Source);

         elsif Is_Class_Or_Record_Type (E) then
            --  There is no need to check that it is NOT a class since classes
            --  have been handled immediately before!
            Set_Src (E, Get_Struct_Type_Source);

         elsif Get_Kind (E) = E_Variable then
            Set_Src (E, Get_Variable_Declaration_Source);

         --  C and C++ typedef

         elsif LL.Is_Type (E) then
            Set_Src (E, Get_Variable_Declaration_Source);

         --  Do not add any message to a C/C++ entity which already has the
         --  "under development" text

         elsif not In_Ada_Language (E)
           and then Get_Src (E) /= Null_Unbounded_String
           and then Get_Src (E) = Under_Development_Text
         then
            null;

         else
            Set_Src (E,
              To_Unbounded_String (Under_Development_Text));
         end if;

         --  Restore original contents of buffers

         if Buffers_Swapped then
            Swap_Buffers;
         end if;
      end CPP_Get_Source;

      -------------------
      -- Previous_Word --
      -------------------

      procedure Previous_Word
        (Index           : Natural;
         Prev_Word_Begin : out Natural;
         Prev_Word_End   : out Natural)
      is
         Idx : Natural := Index;
      begin
         --  Skip current word
         while Idx > Buffer'First
           and then (Buffer (Idx) /= ' '
                     and then Buffer (Idx) /= ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;

         --  Skip spaces and line terminators

         while Idx > Buffer'First
           and then (Buffer (Idx) = ' '
                     or else Buffer (Idx) = ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;
         Prev_Word_End := Idx;

         while Idx > Buffer'First
           and then (Buffer (Idx) /= ' '
                     and then Buffer (Idx) /= ASCII.LF)
         loop
            Idx := Idx - 1;
         end loop;
         Prev_Word_Begin := Idx + 1;
      end Previous_Word;

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result
      is
         pragma Unreferenced (Entity, Scope_Level);
      begin
         return OK;
      end Process_Node;

      ---------------------
      -- Search_Backward --
      ---------------------

      function Search_Backward
        (Word : String;
         From : Natural) return Natural
      is
         Lowercase_Word  : constant String := To_Lower (Word);
         Idx             : Natural := From;
         Prev_Word_Begin : Natural := Buffer.all'First;
         Prev_Word_End   : Natural;

      begin
         --  Locate the beginning of the type declaration. This is a
         --  naive approach used in the prototype since it does not
         --  handle the word "type" located in a comment. A backward
         --  parser is required here. Must be improved???

         loop
            while Idx > Buffer.all'First
              and then (Buffer (Idx) = ' '
                        or else Buffer (Idx) = ASCII.LF)
            loop
               Idx := Idx - 1;
            end loop;
            Prev_Word_End := Idx;

            while Idx > Buffer.all'First
              and then (Buffer (Idx) /= ' '
                        and then Buffer (Idx) /= ASCII.LF)
            loop
               Idx := Idx - 1;
            end loop;
            Prev_Word_Begin := Idx + 1;

            exit when Idx = Buffer.all'First
              or else
                To_Lower (Buffer.all (Prev_Word_Begin .. Prev_Word_End))
                  = Lowercase_Word;
         end loop;

         return Prev_Word_Begin;
      end Search_Backward;

      --------------------------
      -- Skip_Blanks_Backward --
      --------------------------

      function Skip_Blanks_Backward (Index : Natural) return Natural is
         Idx : Natural := Index;
      begin
         while Idx > Buffer'First
           and then Buffer (Idx) = ' '
         loop
            Idx := Idx - 1;
         end loop;

         return Idx;
      end Skip_Blanks_Backward;

      ------------------
      -- Swap_Buffers --
      ------------------

      procedure Swap_Buffers is
         Tmp : constant GNAT.Strings.String_Access := Buffer;
      begin
         Buffer           := C_Headers_Buffer;
         C_Headers_Buffer := Tmp;

         Buffers_Swapped  := True;
      end Swap_Buffers;

   --  Start of processing for Add_Documentation_From_Sources

   begin
      EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);

      Buffer := File.Read_File;

      if False then
         if Present (File_Entities.Tree_Root) then
            Traverse_Tree (File_Entities.Tree_Root, Process_Node'Access);
         end if;

      else
         --  Retrieve the documentation (if any) and sources

         if In_Ada_Lang then
            For_All (File_Entities.All_Entities, Ada_Get_Doc'Access);
            For_All (File_Entities.All_Entities, Ada_Get_Source'Access);
         else pragma Assert (In_C_Lang);
            For_All (File_Entities.All_Entities, CPP_Get_Doc'Access);
            For_All (File_Entities.All_Entities, CPP_Get_Source'Access);
         end if;
      end if;

      Free (Buffer);
   end Add_Documentation_From_Sources;

   -------------------------------
   -- Build_Structured_Comments --
   -------------------------------

   procedure Build_Structured_Comments
     (Context   : access constant Docgen_Context;
      Root      : Entity_Id;
      In_C_Lang : Boolean)
   is
      function Is_Custom_Tag (Tag : String) return Boolean;
      --  Return True if Tag is a supported tag.
      --  ??? This info should be configurable in a separate file to allow
      --  customers to define their own tags

      procedure Parse_Subprogram_Comments (Subp : Entity_Id);
      --  Initialize the structured comment associated with Subp, parse the
      --  block of comments retrieved from sources (and clean it), and report
      --  errors/warnings on missing documentation.

      procedure Parse_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False);
      --  Parse the contents of S and store its contents in the structured
      --  comment of E (ie. E.Comment)

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Dispatch a call to build an structured comment between routines
      --  Parse_Doc and Parse_Subprogram_Comments.

      -------------------
      -- Is_Custom_Tag --
      -------------------
      --  http://docs.oracle.com/javase/1.4.2/docs/tooldocs/windows/
      --    javadoc.html#javadoctags
      --  file:///home/miranda/gps520/share/doc/gps/html/users_guide/
      --    tools.html#documentation-generation

      function Is_Custom_Tag (Tag : String) return Boolean is
      begin
         return Tag = "description"
            --  Full description of a package or method
           or else Tag = "summary"
            --  Short description of a package or method
           or else Tag = "param"
            --  Description of a parameter
           or else Tag = "exception"
            --  Description of possible exceptions raised by the
            --  method/subprogram
           or else Tag = "seealso"
            --  Reference to another package, method, type, etc.
           or else Tag = "c_version"
            --  For bindings, the version of the C file
            --  or else Tag = "group" --  ???
            --  or else Tag = "code"  --  ???

            --  JavaDoc possible additional tags???
           or else Tag = "author"
           or else Tag = "deprecated"
           or else Tag = "return"
           or else Tag = "serial"
           or else Tag = "since"
           or else Tag = "version"

            --  Proposed enhancements
           or else Tag = "field";
      end Is_Custom_Tag;

      ---------------
      -- Parse_Doc --
      ---------------

      procedure Parse_Doc
        (Context      : access constant Docgen_Context;
         E            : Entity_Id;
         S            : String;
         Is_Full_View : Boolean := False)
      is
         Comment : constant Structured_Comment :=
                    (if Is_Full_View then Get_Full_View_Comment (E)
                                     else Get_Comment (E));
         Current : Tag_Cursor := New_Cursor (Comment);

         procedure Parse (S : String);
         --  Parse the contents of S searching for the next tag

         procedure Parse (S : String) is
            Tag_Indicator : constant Character := '@';

            type Location is record
               First : Natural;
               Last  : Natural;
            end record;
            No_Location : constant Location := (0, 0);

            function Scan_Tag return Location;
            --  Scan text in S searching for the next tag

            procedure Scan_Word
              (J   : in out Natural;
               Loc : out Location);
            --  Scan next word in S

            procedure Scan_Line
              (J   : in out Natural;
               Loc : out Location);
            --  Scan S searching for the next end of line

            --------------
            -- Scan_Tag --
            --------------

            function Scan_Tag return Location is
               J     : Natural := S'First;
               First : Natural;
               Last  : Natural;
            begin
               while J <= S'Last
                 and then S (J) /= Tag_Indicator
               loop
                  J := J + 1;
               end loop;

               if J <= S'Last then
                  First := J;

                  J := J + 1; --  past '@'
                  while J <= S'Last
                    and then S (J) /= ' '
                    and then S (J) /= ASCII.LF
                  loop
                     J := J + 1;
                  end loop;
                  Last := J - 1;

                  if Last > First then
                     return Location'(First, Last);
                  end if;
               end if;

               return No_Location;
            end Scan_Tag;

            ---------------
            -- Scan_Word --
            ---------------

            procedure Scan_Word
              (J   : in out Natural;
               Loc : out Location)
            is
               First : Natural;
               Last  : Natural;
            begin
               Loc := No_Location;

               while J <= S'Last
                 and then S (J) = ' '
                 and then S (J) /= ASCII.LF
               loop
                  J := J + 1;
               end loop;

               First := J;
               while J <= S'Last
                 and then S (J) /= ' '
                 and then S (J) /= ASCII.LF
               loop
                  J := J + 1;
               end loop;
               Last := J - 1;

               if Last >= First then
                  Loc := Location'(First, Last);
               end if;
            end Scan_Word;

            ---------------
            -- Scan_Line --
            ---------------

            procedure Scan_Line
              (J   : in out Natural;
               Loc : out Location)
            is
               First : constant Natural := J;
               Last  : Natural;
            begin
               while J <= S'Last
                 and then S (J) /= ASCII.LF
               loop
                  J := J + 1;
               end loop;

               Last := J - 1;
               if Last > First then
                  Loc := Location'(First, Last);
               else
                  Loc := No_Location;
               end if;
            end Scan_Line;

            Tag_Loc : Location;
         begin
            Tag_Loc := Scan_Tag;

            --  Regular string. Let's append it to the current node value.

            if Tag_Loc = No_Location then
               Append_Text (Current, S);
               return;
            end if;

            --  Append characters to the last opened tag.

            if Tag_Loc.First > S'First then
               Append_Text
                 (Current, Filter (S (S'First .. Tag_Loc.First - 2)));
            end if;

            declare
               Tag_Text  : constant String :=
                             To_Lower (S (Tag_Loc.First + 1 .. Tag_Loc.Last));
               Attr_Loc  : Location;
               Text_Loc  : Location;
               Line_Last : Natural;
               J : Natural;
            begin
               --  If we found an unexpected tag, then treat it like raw text

               if not Is_Custom_Tag (Tag_Text) then
                  Trace (Me, "--> Unknown tag: >" & Tag_Text & "<");

                  Append_Text (Current, S (Tag_Loc.First .. Tag_Loc.Last));
                  Line_Last := Tag_Loc.Last + 1;

               else
                  J := Tag_Loc.Last + 1;

                  Scan_Word
                    (J   => J,
                     Loc => Attr_Loc);

                  Scan_Line
                    (J   => J,
                     Loc => Text_Loc);

                  Line_Last := J;
                  pragma Assert (J > S'Last or else S (J) = ASCII.LF);

                  if Tag_Text = "return"
                    and then Get_Kind (E) = E_Procedure
                  then
                     Error
                       (Context,
                        LL.Get_Entity (E),
                        "@return not applicable to procedures");
                  end if;

                  if Tag_Text = "param" then
                     if Attr_Loc = No_Location then
                        Error
                          (Context,
                           LL.Get_Entity (E),
                           "missing parameter name");

                     else
                        declare
                           Param_Name : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                           Cursor : Tag_Cursor;
                        begin
                           Cursor :=
                             Search_Param (Comment, Param_Name);

                           if Cursor = No_Cursor then
                              Error
                                (Context,
                                 LL.Get_Entity (E),
                                 Msg =>
                                   "wrong parameter name '"
                                 & Param_Name & "'");

                           elsif Get (Cursor).Text
                             /= Null_Unbounded_String
                           then
                              Error
                                (Context,
                                 Get (Cursor).Entity,
                                 "parameter '"
                                 & Param_Name & "' documented twice");

                           elsif Text_Loc /= No_Location then
                              Current := Cursor; -- needed???

                              declare
                                 Text : String renames
                                   S (Text_Loc.First .. Text_Loc.Last);
                              begin
                                 Append_Text (Current, Text);
                              end;
                           end if;
                        end;
                     end if;

                     --  Opening tag

                  else

                     --  Now initialize the attributes field
                     if Attr_Loc /= No_Location then
                        declare
                           Text : String renames
                             S (Attr_Loc.First .. Attr_Loc.Last);
                        begin
                           if Tag_Text = "seealso" then
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_General_Entity,
                                   Attribute => To_Unbounded_String (Text));
                           else
                              Current :=
                                Append_Tag
                                  (Comment,
                                   Tag       => To_Unbounded_String (Tag_Text),
                                   Entity    => No_General_Entity,
                                   Attribute => Null_Unbounded_String);
                              Append_Text (Current, Text);
                           end if;
                        end;
                     else
                        Current :=
                          Append_Tag
                            (Comment,
                             Tag       => To_Unbounded_String (Tag_Text),
                             Entity    => No_General_Entity,
                             Attribute => Null_Unbounded_String);
                     end if;

                     if Text_Loc /= No_Location then
                        declare
                           Text : constant String :=
                             S (Text_Loc.First .. Text_Loc.Last);
                        begin
                           Append_Text (Current, Text);
                        end;
                     end if;
                  end if;
               end if;

               if Line_Last < S'Last then
                  Parse (S (Line_Last + 1 .. S'Last));
               end if;
            end;
         end Parse;

      --  Start of processing for Parse_Doc

      begin
         if S = "" then
            return;
         end if;

         Parse (S);

         if Is_Full_View then
            Set_Full_View_Comment (E, Comment);
         else
            Set_Comment (E, Comment);
         end if;
      end Parse_Doc;

      ------------------------------
      -- Parse_Subprogram_Comment --
      ------------------------------

      procedure Parse_Subprogram_Comments (Subp : Entity_Id) is
         Cursor         : EInfo_List.Cursor;
         Has_Params     : Boolean;
         Param          : Entity_Id;
         Param_End_Line : Integer;
      begin
         --  Initialize the structured comment associated with this entity

         Set_Comment (Subp, New_Structured_Comment);

         --  Search for documentation located in the subprogram profile
         --  (that is, comments located close to the parameter declarations)

         Cursor := Get_Entities (Subp).First;
         Has_Params := EInfo_List.Has_Element (Cursor);

         while EInfo_List.Has_Element (Cursor) loop
            Param := EInfo_List.Element (Cursor);
            pragma Assert (Get_Kind (Param) = E_Formal);
            EInfo_List.Next (Cursor);

            if Get_Doc (Param) = No_Comment_Result
              or else In_C_Lang --  Unsupported feature yet???
            then
               Append_Param_Tag
                 (Comment    => Get_Comment (Subp),
                  Entity     => LL.Get_Entity (Param),
                  Param_Name =>
                    To_Unbounded_String
                      (Get_Name (Context.Database, LL.Get_Entity (Param))),
                  Text       => Null_Unbounded_String);
            else
               --  Calculate the last line where the comment of this parameter
               --  can be correctly located

               --  Case 1: For the last parameter the parameter comment (if
               --  any) must be located before the location of the full comment
               --  of the subprogram.

               if not EInfo_List.Has_Element (Cursor) then
                  Param_End_Line := Get_Doc (Subp).Start_Line;

               --  Case 2: For other parameters their comment must be
               --  located before the location of the next parameter.

               else
                  Param_End_Line :=
                    LL.Get_Location (EInfo_List.Element (Cursor)).Line;
               end if;

               if Get_Doc (Param).Start_Line < Param_End_Line then
                  Append_Param_Tag
                    (Comment    => Get_Comment (Subp),
                     Entity     => LL.Get_Entity (Param),
                     Param_Name =>
                       To_Unbounded_String (Get_Short_Name (Param)),
                     Text       => Get_Doc (Param).Text);
               else
                  Append_Param_Tag
                    (Comment    => Get_Comment (Subp),
                     Entity     => LL.Get_Entity (Param),
                     Param_Name =>
                       To_Unbounded_String
                         (Get_Name
                           (Context.Database, LL.Get_Entity (Param))),
                     Text       => Null_Unbounded_String);
               end if;

               Set_Doc (Param, No_Comment_Result);
            end if;
         end loop;

         --  Parse the documentation of the subprogram

         if Get_Doc (Subp) /= No_Comment_Result then
            Parse_Doc (Context, Subp, To_String (Get_Doc (Subp).Text));
            Set_Doc (Subp, No_Comment_Result);
         end if;

         --  Report warning on undocumented parameters

         if Has_Params then
            declare
               C        : Tag_Cursor := First_Param (Get_Comment (Subp));
               Tag_Info : Tag_Info_Ptr;
            begin
               loop
                  Tag_Info := Get (C);

                  if Tag_Info.Text = Null_Unbounded_String then
                     Warning
                       (Context,
                        Tag_Info.Entity, --  LL.Get_Entity (Subp),
                        "undocumented parameter ("
                        & To_String (Tag_Info.Attr)
                        & ")");
                  end if;

                  exit when C = Last_Param (Get_Comment (Subp));
                  Next (C);
               end loop;
            end;
         end if;
      end Parse_Subprogram_Comments;

      ---------------
      -- Parse_Doc --
      ---------------

      function Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result is
         pragma Unreferenced (Scope_Level);
      begin
         --  Do not document again C/C++ entities which are already documented

         if not In_Ada_Language (Entity)
           and then Get_Comment (Entity) /= No_Structured_Comment
         then
            return Skip;
         end if;

         pragma Assert (Get_Comment (Entity) = No_Structured_Comment);

         if LL.Is_Subprogram (Entity) then
            Parse_Subprogram_Comments (Entity);
            return Skip;

         elsif Get_Doc (Entity).Text /= Null_Unbounded_String then
            Set_Comment (Entity, New_Structured_Comment);
            Parse_Doc (Context, Entity, To_String (Get_Doc (Entity).Text));
            Set_Doc (Entity, No_Comment_Result);

            if Is_Partial_View (Entity) then
               Set_Full_View_Comment (Entity, New_Structured_Comment);
               Parse_Doc
                 (Context      => Context,
                  E            => Entity,
                  S            => To_String (Get_Full_View_Doc (Entity).Text),
                  Is_Full_View => True);
               Set_Full_View_Doc (Entity, No_Comment_Result);
            end if;
         end if;

         return OK;
      end Process_Node;

   begin
      Traverse_Tree (Root, Process_Node'Access);
   end Build_Structured_Comments;

   ----------------
   -- Build_Tree --
   ----------------

   function Build_Tree
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Tree_Type
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang     : constant Boolean := not In_Ada_Lang;

      Built_Tree    : Entity_Id;
      Tree          : aliased Tree_Type;

      My_Time       : Delay_Time;
      Tree_Time     : Delay_Time;
      Doc_Time      : Delay_Time;
      Comments_Time : Delay_Time;

   begin
      Trace (Me, "Build_Tree " & (+File.Base_Name));
      Start (My_Time);

      --  Step 1: Build the tree

      Start (Tree_Time);

      Built_Tree := Build_File_Tree (Context, File, Tree'Access);

      --  ??? Until we improve the performance Build_File_Tree does not
      --  generate the tree associated with large files.

      if No (Built_Tree) then
         return No_Tree;
      end if;

      Tree.Tree_Root := Built_Tree;
      Tree.File      := File;

      Stop (Tree_Time, Build_Tree_Time);
      --  Step 2: Add documentation from sources

      Start (Doc_Time);
      Add_Documentation_From_Sources (Context, File, Tree'Access);
      Stop (Doc_Time, GetDoc_Time);

      --  Step 3: Convert blocks of comments into structured comments

      Start (Comments_Time);
      Build_Structured_Comments (Context, Tree.Tree_Root, In_C_Lang);
      Stop (Comments_Time, Build_Comments_Time);

      Stop (My_Time, Frontend_Time);
      return Tree;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return No_Tree;
   end Build_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Builder.Initialize;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Unique_Entity (Location : General_Location) return Entity_Id
   is
   begin
      return Builder.Find_Unique_Entity (Location);
   end Find_Unique_Entity;

end Docgen3.Frontend;
