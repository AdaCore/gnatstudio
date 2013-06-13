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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.HTable;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;
with GNATCOLL.Xref;
with Basic_Types;
with Docgen3.Comment;         use Docgen3.Comment;
with Docgen3.Files;           use Docgen3.Files;
with Docgen3.Utils;           use Docgen3.Utils;
with Docgen3.Errout;          use Docgen3.Errout;
with Docgen3.Time;            use Docgen3.Time;
with Language;                use Language;
with Language.Ada;
with Language.Cpp;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;
with UTF8_Utils;              use UTF8_Utils;
with Xref.Docgen;             use Xref.Docgen;
with Xref;
with GNAT.IO;

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

   type Built_Tree_Type is record
      Root          : Entity_Id;
      --  Root of the built tree

      C_Header_File : Virtual_File;
      --  (C/C++): Corrresponding header file (.h)
   end record;

   No_Tree_Type : constant Built_Tree_Type := (null, No_File);

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Built_Tree_Type;
   --  Build the tree associated with File. Subsidiary function of Build_Tree
   --  which takes care of building the tree and leaves more clear the high
   --  level actions performed by Build_Tree.

   procedure Build_Structured_Comments
     (Context   : access constant Docgen_Context;
      Root      : Entity_Id;
      In_C_Lang : Boolean);
   --  Traverse the Tree of entities and replace blocks of comments by
   --  structured comments.

   ---------------
   -- EInfo_Map --
   ---------------

   function Hash (Key : General_Location) return Ada.Containers.Hash_Type;
   function Equivalent_Keys (Left, Right : General_Location) return Boolean;
   package EInfo_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => General_Location,
      Element_Type    => Entity_Id,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   -----------------
   -- Scope_Stack --
   -----------------

   --  Subsidiary package used to build the tree

   package Scopes_Stack is

      procedure Clear;
      --  Clear the contents of the Scope Stack and unregister the entity
      --  which represents the standard entity.

      function Current_Scope return Entity_Id;
      --  Return the entity in the top of the stack

      function Current_Scope_Depth return Natural;
      --  Return the depth of the stack

      procedure Enter_Scope (Scope : Entity_Id);
      --  Push Scope

      procedure Exit_Scope;
      --  Pop an entity from the stack

      function Find_Entity
        (Scope : Entity_Id;
         Loc   : General_Location) return Entity_Id;
      --  Search for the entity at Loc in all the enclosing scopes of
      --  Scope.

      function In_Open_Scopes (E : General_Entity) return Boolean;
      --  E is the entity of a scope. This function determines if this scope
      --  is currently open (i.e. it appears somewhere in the scope stack).

      function In_Generic_Scope return Boolean;
      --  Return true if some enclosing scopes is generic

      procedure Register_Std_Entity (E : Entity_Id);
      --  Register in the package the entity used to represent the standard
      --  entity. Needed internally to identify the outermost scope.

   private
      pragma Unreferenced (In_Generic_Scope);
   end Scopes_Stack;

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

      procedure Load
        (File   : Virtual_File;
         Buffer : in out GNAT.Strings.String_Access);
      --  Read File and store its contents in Buffer

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

      -------------
      -- Get_Doc --
      -------------

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

            Private_Found    : Boolean := False;
            End_Found        : Boolean := False;
            End_Record_Found : Boolean := False;

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

               elsif Entity = Keyword_Text then
                  declare
                     Keyword : constant String := To_Lower (S);
                  begin
                     if Keyword  = "private" then
                        Private_Found := True;
                     elsif Keyword = "end" then
                        End_Found := True;
                     elsif End_Found and then Keyword = "record" then
                        End_Record_Found := True;
                     else
                        End_Found := False;
                     end if;
                  end;
               elsif Entity = Operator_Text then
                  if S = "(" then
                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;
                  elsif S = ";" then
                     if Par_Count = 0 then
                        return Private_Found
                          or else End_Record_Found;
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
               Append (Buffer.all (From .. Prev_Word_Begin - 1));
            end if;

            --  For simple cases (interfaces, incomplete and private types)
            --  there is no need to use the Ada parser. We just need to locate
            --  the ';'

            if Get_Kind (E) = E_Interface
              or else (Is_Incomplete_Or_Private_Type (E)
                         and then not Is_Full_View)
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
                 (Lang, Buffer.all (Prev_Word_Begin .. Buffer'Last),
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

            Idx := Index;
            while Idx < Buffer'Last
              and then Buffer (Idx) /= ';'
            loop
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
         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  Load (LL.Get_Location (E).File, C_Headers_Buffer);
               end if;

               Swap_Buffers;
            end if;
         end if;

         Ada_Get_Doc (E);

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

      --  Start of processing for CPP_Get_Source

      begin
         Buffers_Swapped := False;

         if LL.Get_Location (E).File /= File then
            if LL.Get_Location (E).File /= File_Entities.Header_File then
               return;
            else
               if C_Headers_Buffer = null then
                  Load (LL.Get_Location (E).File, C_Headers_Buffer);
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

         elsif LL.Is_Type (E) then
            pragma Assert (False);
            Set_Src (E, Get_Variable_Declaration_Source);

         else
            Set_Src (E,
              To_Unbounded_String
                ("<<Get_Source under development for this kind of entity>>"));
         end if;

         --  Restore original contents of buffers

         if Buffers_Swapped then
            Swap_Buffers;
         end if;
      end CPP_Get_Source;

      -----------
      --  Load --
      -----------

      procedure Load
        (File   : Virtual_File;
         Buffer : in out GNAT.Strings.String_Access)
      is
         Last    : Natural;
         Striped : Boolean;
         pragma Unreferenced (Striped);
      begin
         Buffer := File.Read_File;
         Strip_CR (Buffer.all, Last, Striped);

         declare
            Old_Buff : GNAT.Strings.String_Access := Buffer;
            Success  : aliased Boolean;
            N_String : constant String :=
                         Unknown_To_UTF8
                           (Old_Buff (Old_Buff'First .. Last),
                            Success'Access);
         begin
            if Success then
               Buffer := new String'(N_String);
               Free (Old_Buff);
            end if;
         end;
      end Load;

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

      Load (File, Buffer);

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

   ---------------------
   -- Build_File_Tree --
   ---------------------

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Built_Tree_Type
   is
      Lang           : constant Language_Access :=
                         Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang    : constant Boolean :=
                         Lang.all in Language.Ada.Ada_Language'Class;
      In_CPP_Lang    : constant Boolean :=
                         Lang.all in Language.Cpp.Cpp_Language'Class;
      In_C_Lang      : constant Boolean := not In_Ada_Lang;
      Entities_Map   : EInfo_Map.Map;
      C_Header_File  : Virtual_File;

      Std_Entity     : constant Entity_Id :=
                         New_Internal_Entity
                           (Context  => Context,
                            Language => Lang,
                            Name     => "Standard");

      use Scopes_Stack;

      procedure Append_To_File_Entities (E : Entity_Id);
      --  Append E to File_Entities.All_Entities

      procedure Append_To_Map (E : Entity_Id);
      --  Append E to Entities_Map

      procedure Append_To_Scope (Scope : Entity_Id; E : Entity_Id);
      --  Append E to the list of entities of Scope

      procedure Complete_Decoration (E : Entity_Id);
      --  Complete the decoration of entity E

      function New_Entity (E : General_Entity) return Entity_Id;
      --  Build a new entity for the Xref entity E

      -----------------------------
      -- Append_To_File_Entities --
      -----------------------------

      procedure Append_To_File_Entities (E : Entity_Id) is
      begin
         File_Entities.All_Entities.Append (E);
      end Append_To_File_Entities;

      -------------------
      -- Append_To_Map --
      -------------------

      procedure Append_To_Map (E : Entity_Id) is
      begin
         Entities_Map.Include (LL.Get_Location (E), E);
         Append_To_File_Entities (E);
      end Append_To_Map;

      ---------------------
      -- Append_To_Scope --
      ---------------------

      procedure Append_To_Scope (Scope : Entity_Id; E : Entity_Id) is
      begin
         Append_Entity (Scope, E);
         Set_Scope (E, Scope);
         Append_To_Map (E);
      end Append_To_Scope;

      -------------------------
      -- Complete_Decoration --
      -------------------------

      procedure Complete_Decoration (E : Entity_Id) is

         procedure Decorate_Record_Type (E : Entity_Id);
         --  Complete the decoration of a record type entity

         procedure Decorate_Subprogram (E : Entity_Id);
         --  Complete the decoration of a subprogram entity

         --------------------------
         -- Decorate_Record_Type --
         --------------------------

         procedure Decorate_Record_Type (E : Entity_Id) is
         begin
            if In_Ada_Lang then
               declare
                  Discrim : constant Xref.Entity_Array :=
                              Discriminants
                                (Context.Database, LL.Get_Entity (E));
                  D : Entity_Id;
               begin
                  for J in Discrim'Range loop
                     D := New_Entity (Discrim (J));
                     Set_Kind (D, E_Discriminant);
                     Entities_Map.Include (LL.Get_Location (D), D);
                     Append_Discriminant (E, D);
                  end loop;
               end;
            end if;

            --  Check_Record_Components;

            declare
               Components : constant Xref.Entity_Array :=
                              Fields (Context.Database, LL.Get_Entity (E));
               Comp_E : Entity_Id;

            begin
               for J in Components'Range loop
                  Comp_E := New_Entity (Components (J));
                  --  In C++ we have here formals of primitives???
                  Set_Kind (Comp_E, E_Component);
                  Append_To_Scope (E, Comp_E);
               end loop;
            end;

            if Is_Tagged (E)
              or else (In_CPP_Lang and then Get_Kind (E) = E_Class)
            then
               declare
                  All_Methods : constant Xref.Entity_Array :=
                                  Methods
                                    (Context.Database, LL.Get_Entity (E),
                                     Include_Inherited => True);
                  Meth_E : Entity_Id;
               begin
                  for J in All_Methods'Range loop
                     Meth_E := New_Entity (All_Methods (J));

                     --  Fails with inherited primtivives of package
                     --  Ada.Finalization???
                     --  pragma Assert (LL.Is_Primitive (Meth_E));

                     if No (Meth_E) then
                        --  Inherited routine of other package??? Fails with
                        --  inherited primitives of Ada.Finalization???
                        null;
                        pragma Assert (False);
                     else
                        if In_Ada_Language (Meth_E) then
                           Append_To_Scope (Get_Scope (E), Meth_E);
                        else
                           Append_To_Scope (E, Meth_E);
                        end if;

                        Decorate_Subprogram (Meth_E);
                        Append_Method (E, Meth_E);
                     end if;
                  end loop;
               end;
            end if;
         end Decorate_Record_Type;

         -------------------------
         -- Decorate_Subprogram --
         -------------------------

         procedure Decorate_Subprogram (E : Entity_Id) is
            Formals  : constant Xref.Parameter_Array :=
                         Parameters (Context.Database, LL.Get_Entity (E));
            Formal_E : Entity_Id;

         begin
            Enter_Scope (E);

            for J in Formals'Range loop

               Formal_E := New_Entity (Formals (J).Parameter);
               --  Formals (J).Kind ???
               --  Is_Full_View is erroneusly set in formals ???
               if Present (Formal_E) then
                  Set_Kind (Formal_E, E_Formal);
                  Set_Scope (Formal_E, E);

                  Append_To_Scope (Current_Scope, Formal_E);

                  --  Local variables defined in the body of this
                  --  subprogram.

               else
                  null;
               end if;
            end loop;

            Exit_Scope;
         end Decorate_Subprogram;

      --  Start of processing for Complete_Decoration

      begin
         if LL.Is_Container (E) then
            if Is_Class_Or_Record_Type (E) then
               Decorate_Record_Type (E);

               --  Although formals are available in the list of
               --  entities of the file we are traversing, it is not
               --  easy to identify and set the scope of formals just
               --  traversing these entities since some entities do
               --  not have its Xref.Scope entity available.

            elsif LL.Is_Subprogram (E) then
               Decorate_Subprogram (E);
            end if;
         end if;
      end Complete_Decoration;

      ----------------
      -- New_Entity --
      ----------------

      function New_Entity
        (E : General_Entity) return Entity_Id
      is
         Db      : General_Xref_Database renames Context.Database;
         E_Loc   : constant General_Location := Get_Location (Db, E);
         Is_Prim : constant Boolean := Present (Is_Primitive_Of (Db, E));
         --  Avoid calling twice this service???

      begin
         --  Case 1: Entities defined in other packages/files

         if In_Ada_Lang then

            if E_Loc.File /= File
              and then not Is_Prim
            then
               return null;
            else
               return Atree.New_Entity (Context, Lang, E, E_Loc);
            end if;

         --  C++

         elsif In_CPP_Lang then
            declare
               Kind   : constant Entity_Kind :=
                          LL.Get_Ekind (Db, E, In_Ada_Lang => False);
               New_E  : Entity_Id;

            begin
               if Kind = E_Include_File then
                  return null;

               elsif E_Loc.File = File then
                  return Atree.New_Entity (Context, Lang, E, E_Loc);

               elsif C_Header_File /= No_File then
                  if E_Loc.File = C_Header_File then
                     return Atree.New_Entity (Context, Lang, E, E_Loc);
                  else
                     return null;
                  end if;

               elsif E_Loc.File /= File then
                  New_E := Atree.New_Entity (Context, Lang, E, E_Loc);

                  if LL.Is_Subprogram (New_E)
                    and then LL.Is_Primitive (New_E)
                    and then Present (LL.Get_Scope (New_E))
                  then
                     return New_E;
                  end if;

                  Free (New_E);
                  return null;

               else
                  return Atree.New_Entity (Context, Lang, E, E_Loc);
               end if;
            end;

         --  C

         else
            declare
               E_Body_Loc : constant General_Location := Get_Body (Db, E);
               Kind       : constant Entity_Kind :=
                              LL.Get_Ekind (Db, E, In_Ada_Lang => False);
               New_E      : Entity_Id;

            begin
               if Kind = E_Include_File
                 or else Kind = E_Unknown
               then
                  return null;

               elsif E_Loc.File /= File then

                  --  Handle entities defined in the header file (.h)

                  --  If we already know the associated header file we can
                  --  safely take a decision

                  if C_Header_File /= No_File then
                     if E_Loc.File /= No_File
                       and then E_Loc.File = C_Header_File
                     then
                        New_E := Atree.New_Entity (Context, Lang, E, E_Loc);
                        Set_Ref_File (New_E, File);
                        return New_E;
                     else
                        return null;
                     end if;

                  --  Otherwise this works only if the first entity referenced
                  --  from the header file is a subprogram. More work needed
                  --  here to handle other entities???

                  elsif Present (E_Body_Loc)
                    and then E_Body_Loc.File = File
                  then
                     New_E := Atree.New_Entity (Context, Lang, E, E_Loc);
                     Set_Ref_File (New_E, File);
                     return New_E;

                  --  We assume that there is only a header file associated
                  --  with each .c or .cpp file. We also assume that the
                  --  name of the header file and the name of the .c/.cpp
                  --  files (without their extension) matches. Otherwise
                  --  more work is needed here to handle these entities???

                  elsif Filename (E_Loc.File) = Filename (File) then
                     New_E := Atree.New_Entity (Context, Lang, E, E_Loc);
                     Set_Ref_File (New_E, File);
                     return New_E;

                  else
                     return null;
                  end if;
               end if;

               return Atree.New_Entity (Context, Lang, E, E_Loc);
            end;
         end if;

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return null;
      end New_Entity;

      -------------------------
      -- Update_Scopes_Stack --
      -------------------------
      procedure Update_Scopes_Stack (New_E : Entity_Id); --  ???

      procedure Update_Scopes_Stack (New_E : Entity_Id) is
         Scope_Id : Entity_Id;

      begin
         if LL.Get_Scope (New_E) = No_General_Entity then
            Set_Scope (New_E, Current_Scope);

            --  More work needed with generic types since in some cases
            --  the scope is set???

            --  Fails also in subprogram GNATCOLL.Projects.Initialize
            --  but I cannot see why???
            --                pragma Assert (Current_Scope = Std_Entity
            --                                 or else In_Generic_Scope);
            --   or else Current_Scope.Xref.Is_Generic);
            --   or else New_E.Kind = E_Access_Type);

         else
            if In_Open_Scopes (LL.Get_Scope (New_E)) then
               while LL.Get_Scope (New_E)
                 /= LL.Get_Entity (Current_Scope)
               loop
                  Exit_Scope;
               end loop;

            elsif LL.Is_Type (New_E) then
               Scope_Id :=
                 Find_Entity
                   (Current_Scope,
                    Get_Location (Context.Database, LL.Get_Scope (New_E)));

               Set_Scope (New_E, Scope_Id);

               --  pragma Assert (Get_Scope (New_E) /= null);

               if No (Get_Scope (New_E)) then
                  pragma Assert (Get_Kind (New_E) = E_Access_Type);
                  Set_Scope (New_E, Current_Scope);
               end if;
            end if;
         end if;
      end Update_Scopes_Stack;

      --  Local variables

      --  This entity represents the outermost scope (ie. the standard scope).
      --  It is needed to associate some scope to generic formals of library
      --  level units.

      New_E                : Entity_Id;
      Skip_This_Entity     : Boolean := False;
      File_Entities_Cursor : Entities_In_File_Cursor;

      Total_Entities_Count : Natural := 0;
      Entities_Count       : Natural := 0;

   --  Start of processing for Build_File_Tree

   begin
      C_Header_File := No_File;

      Set_Kind (Std_Entity, E_Package);
      Register_Std_Entity (Std_Entity);
      Enter_Scope (Std_Entity);

      File_Entities_Cursor := Context.Database.Entities_In_File (File);
      while not At_End (File_Entities_Cursor) loop
         Total_Entities_Count := Total_Entities_Count + 1;
         File_Entities_Cursor.Next;
      end loop;

      --  Temporarily disable construction of large trees (until we improve
      --  the performance!). For example, sqlite3.c has 14860 entities and
      --  requires 46 minutes to build the tree in my virtual machine???

      if Total_Entities_Count > 3000 then
         Trace (Me,
           ">> Build_File_Tree (skipped): "
           & Total_Entities_Count'Img
           & " entities");

         EInfo_Map.Clear (Entities_Map);
         Scopes_Stack.Clear;
         return No_Tree_Type;
      end if;

      File_Entities_Cursor := Context.Database.Entities_In_File (File);

      --  Locate the root of the tree of entities

      if In_Ada_Lang then
         while not At_End (File_Entities_Cursor) loop
            New_E := New_Entity (File_Entities_Cursor.Get);
            File_Entities_Cursor.Next;

            if Present (New_E) then
               Complete_Decoration (New_E);
               Append_To_Scope (Current_Scope, New_E);

               --  Avoid spurious entity GNATCOLL.Any_Types (procedure???)

               if LL.Is_Container (New_E)
                 and then LL.Is_Global (New_E)
               then
                  Enter_Scope (New_E);
                  exit;
               end if;
            end if;
         end loop;
      end if;

      --  Process all its entities

      while not At_End (File_Entities_Cursor) loop
         Entities_Count := Entities_Count + 1;

         if Entities_Count mod 50 = 0 then
            GNAT.IO.Put_Line
              (+File.Base_Name
               & ":"
               & To_String (Entities_Count)
               & "/"
               & To_String (Total_Entities_Count));
         end if;

         Skip_This_Entity := False;
         New_E := New_Entity (File_Entities_Cursor.Get);

         if Present (New_E) then

            --  Update the scopes stack using the reliable value provided by
            --  the low level (ie. Xref).

            if In_Ada_Lang
              and then Present (LL.Get_Scope (New_E))
            then
               --  Skip the full view of incomplete or private types because
               --  their Xref.Scope references the partial view (instead of
               --  referencing its syntax scope)

               if Is_Incomplete_Or_Private_Type (New_E)
                 and then Is_Full_View (New_E)
               then
                  null;
               else
                  Update_Scopes_Stack (New_E);
               end if;
            end if;

            --  (C/C++): Replace virtual primitives by their class entity. Done
            --  because we may not have other references to the corresponding
            --  class in the LI file.

            if In_CPP_Lang
              and then LL.Get_Location (New_E).File /= File
              and then LL.Is_Subprogram (New_E)
              and then LL.Is_Primitive (New_E)
              and then Present (LL.Get_Scope (New_E))
            then
               declare
                  Prev_E : Entity_Id := New_E;

               begin
                  New_E :=
                    Atree.New_Entity
                      (Context  => Context,
                       Language => Lang,
                       E        => LL.Get_Scope (New_E),
                       Loc      => LL.Get_Scope_Loc (New_E));
                  Free (Prev_E);
               end;
            end if;

            --  (C/C++) Locate the associated header file (if any). Currently
            --  we assume that each .c/.cpp file has a single associated
            --  header file.

            if In_CPP_Lang
              and then Get_Kind (New_E) = E_Class
              and then LL.Get_Location (New_E).File /= File
            then
               pragma Assert (C_Header_File = No_File
                  or else C_Header_File = LL.Get_Location (New_E).File);
               C_Header_File := LL.Get_Location (New_E).File;

            elsif In_C_Lang
              and then LL.Get_Location (New_E).File /= File
            then
               pragma Assert (C_Header_File = No_File
                 or else C_Header_File = LL.Get_Location (New_E).File);
               C_Header_File := LL.Get_Location (New_E).File;
            end if;

            --  Decorate the new entity

            if In_CPP_Lang
              and then Get_Kind (New_E) = E_Class
              and then Entities_Map.Contains (LL.Get_Location (New_E))
            then
               --  No need to handle twice the same class
               Skip_This_Entity := True;

            elsif In_C_Lang
              and then Get_Kind (New_E) = E_Variable
              and then not LL.Is_Global (New_E)
            then
               Skip_This_Entity := True;

            elsif In_Ada_Lang
              and then not Kind_In (Get_Kind (New_E), E_Variable,
                                                      E_Discriminant,
                                                      E_Component)
              and then not LL.Is_Primitive (New_E)
            then
               pragma Assert
                 (not Entities_Map.Contains (LL.Get_Location (New_E)));

               --  Skip processing the full-view of a private or incomplete
               --  type since its components are retrieved from Xref when
               --  we process its partial view.

               if Is_Incomplete_Or_Private_Type (New_E)
                 and then Is_Full_View (New_E)
               then
                  Skip_This_Entity := True;
               end if;

            --  Skip methods since they are entered in the tree as part of
            --  processing its class/tagged type

            elsif LL.Is_Primitive (New_E) then
               Skip_This_Entity := True;

            elsif In_Ada_Lang then

               --  An E_Variable may be in fact a component of an incomplete
               --  or private type

               if LL.Get_Kind (New_E) = E_Variable then
                  declare
                     Map_Cursor : constant EInfo_Map.Cursor :=
                       Entities_Map.Find (LL.Get_Location (New_E));
                     Prev_E     : Entity_Id;
                     use type EInfo_Map.Cursor;
                  begin
                     if Map_Cursor /= EInfo_Map.No_Element then
                        Prev_E := EInfo_Map.Element (Map_Cursor);

                        case Get_Kind (Prev_E) is
                        when E_Discriminant |
                             E_Component    |
                             E_Formal       =>
                           null;

                        when others =>
                           pragma Assert (False);
                        end case;

                        Skip_This_Entity := True;
                     end if;
                  end;
               end if;

            elsif In_C_Lang
              and then Present (LL.Get_Scope (New_E))
              and then LL.Is_Global (New_E)
            then
               if Is_Class_Or_Record_Type (New_E)
                 and then Get_Kind (New_E) /= E_Class
               then
                  --  Handle named typedef structs since the compiler
                  --  generates two entites in the LI file with the
                  --  same name.

                  declare
                     Scope_Id : constant General_Entity :=
                                  LL.Get_Scope (New_E);
                  begin
                     if Context.Database.Get_Name (LL.Get_Entity (New_E))
                       = Context.Database.Get_Name (Scope_Id)
                     then
                        Free (New_E);
                        New_E :=
                          New_Entity
                            (Context => Context,
                             Language    => Lang,
                             E       => Scope_Id,
                             Loc     => Get_Location
                                          (Context.Database, Scope_Id));
                     end if;
                  end;

               elsif Get_Kind (New_E) = E_Variable then
                  declare
                     Scope_Id   : constant General_Entity :=
                                    LL.Get_Scope (New_E);
                     use type EInfo_Map.Cursor;
                  begin
                     --  Handle fields of structs. Must use the Xref support
                     --  directly since we may have not seen yet the full
                     --  declaration of the struct.

                     if Context.Database.Is_Type (Scope_Id) then
                        declare
                           Kind : constant Entity_Kind :=
                                    LL.Get_Ekind (Context.Database,
                                                  Scope_Id,
                                                  In_Ada_Lang => False);
                        begin
                           pragma Assert (Kind_In (Kind, E_Record_Type,
                                                         E_Class));
                           Skip_This_Entity := True;
                        end;
                     end if;
                  end;
               end if;
            end if;

            if Skip_This_Entity then
               Free (New_E);

            else
               Append_To_Scope (Current_Scope, New_E);
               Complete_Decoration (New_E);

               if Get_Kind (New_E) = E_Enumeration_Type then
                  Enter_Scope (New_E);

               elsif Is_Package (New_E) then
                  Enter_Scope (New_E);
               end if;
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      EInfo_Map.Clear (Entities_Map);
      Scopes_Stack.Clear;

      return Built_Tree_Type'(Std_Entity, C_Header_File);
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return Built_Tree_Type'(null, No_File);
   end Build_File_Tree;

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

      Built_Tree    : Built_Tree_Type;
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

      if Built_Tree = No_Tree_Type then
         return No_Tree;
      end if;

      Tree.Tree_Root   := Built_Tree.Root;
      Tree.File        := File;
      Tree.Header_File := Built_Tree.C_Header_File;

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

   ----------
   -- Hash --
   ----------

   function Hash (Key : General_Location) return Ada.Containers.Hash_Type is
      type Internal_Hash_Type is range 0 .. 2 ** 31 - 1;
      function Internal is new GNAT.HTable.Hash
        (Header_Num => Internal_Hash_Type);
   begin
      return Ada.Containers.Hash_Type
        (Internal
           (+Key.File.Full_Name
            & Natural'Image (Key.Line)
            & Basic_Types.Visible_Column_Type'Image (Key.Column)));
   end Hash;

   ---------------------
   -- Equivalent_Keys --
   ---------------------

   function Equivalent_Keys
     (Left, Right : General_Location) return Boolean
   is
      use type GNATCOLL.Xref.Visible_Column;
   begin
      return Left.File = Right.File
        and then Left.Line = Right.Line
        and then Left.Column = Right.Column;
   end Equivalent_Keys;

   -----------------
   -- Scope_Stack --
   -----------------

   package body Scopes_Stack is
      Std_Entity : Entity_Id;
      Stack      : EInfo_List.Vector;

      procedure Clear is
      begin
         Stack.Clear;
         Std_Entity := null;
      end Clear;

      function Current_Scope return Entity_Id is
      begin
         return Stack.Element (0);
      end Current_Scope;

      function Current_Scope_Depth return Natural is
      begin
         return Natural (Stack.Length);
      end Current_Scope_Depth;

      procedure Enter_Scope (Scope : Entity_Id) is
      begin
         Stack.Prepend (Scope);
      end Enter_Scope;

      procedure Exit_Scope is
      begin
         Stack.Delete_First;
      end Exit_Scope;

      function Find_Entity
        (Scope : Entity_Id;
         Loc   : General_Location) return Entity_Id
      is
         Cursor : EInfo_List.Cursor;
         E      : Entity_Id;
         S      : Entity_Id := Scope;

      begin
         loop
            Cursor := Get_Entities (S).First;
            while EInfo_List.Has_Element (Cursor) loop
               E := EInfo_List.Element (Cursor);

               if LL.Get_Location (E) = Loc then
                  return E;
               end if;

               EInfo_List.Next (Cursor);
            end loop;

            exit when Get_Scope (S) = Std_Entity;
            S := Get_Scope (S);
         end loop;

         return null;
      end Find_Entity;

      function In_Generic_Scope return Boolean is
         Last : constant Integer :=
           Current_Scope_Depth - 2; -- Skip standard
         use type Ada.Containers.Count_Type;
         S : Entity_Id;
      begin
         for J in 0 .. Last loop
            S := Stack.Element (Natural (J));

            if LL.Is_Generic (S) then
               return True;
            end if;
         end loop;

         return False;
      end In_Generic_Scope;

      function In_Open_Scopes (E : General_Entity) return Boolean is
         Last : constant Integer :=
           Current_Scope_Depth - 2; -- Skip standard

         use type Ada.Containers.Count_Type;
         S : Entity_Id;
      begin
         for J in 0 .. Last loop
            S := Stack.Element (Natural (J));

            if LL.Get_Entity (S) = E then
               return True;
            end if;
         end loop;

         return False;
      end In_Open_Scopes;

      procedure Register_Std_Entity (E : Entity_Id) is
      begin
         pragma Assert (No (Std_Entity));
         Std_Entity := E;
      end Register_Std_Entity;

   end Scopes_Stack;

end Docgen3.Frontend;
