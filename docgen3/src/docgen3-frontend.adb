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
with Ada.Unchecked_Deallocation;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.HTable;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Utils;
with GNATCOLL.Xref;
with Basic_Types;             use Basic_Types;
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
with Traces;                  use Traces;
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

   -----------------------------
   -- Unique_Entity_Allocator --
   -----------------------------

   --  This package gives support to unique allocation of tree entities (that
   --  is, it ensures no entity is duplicated in the trees composing a full
   --  project). This is required to ensure consistency and also to facilitate
   --  to the backend the generation of entity dependency graphs.

   package Unique_Entity_Allocator is

      type Unique_Entity_Info is private;
      type Unique_Entity_Id is access all Unique_Entity_Info;
      No_Entity : constant Unique_Entity_Id := null;

      function Get_Entity (Entity : Unique_Entity_Id) return Entity_Id;
      --  Return the entity

      procedure Get_Unique_Entity
        (Entity  : out Unique_Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : General_Entity;
         Forced  : Boolean := False;
         C_Header_File : Virtual_File := No_File);
      --  Searchs for E in a hash table containing all the project entities.
      --  If found then return such entity; if not found then allocates a new
      --  entity for E. If Forced is True then the entity is searched and built
      --  even if it is defined in another file.

      function Is_New (Entity : Unique_Entity_Id) return Boolean;
      --  True if the tree entity was allocated when this unique Entity was
      --  built

      function New_Internal_Entity
        (Context  : access constant Docgen_Context;
         Language : Language_Access;
         Name     : String) return Unique_Entity_Id;
      --  Allocate an internal entity. Used to build the standard entity.

      function Present (Entity : Unique_Entity_Id) return Boolean;
      --  Return True if Entity /= No_Entity

      procedure Update_Entity
        (Entity : in out Unique_Entity_Id;
         E      : General_Entity;
         Forced : Boolean := False);
      --  Free the previous entity associated with Entity and associate it E

      procedure Free (Entity : in out Unique_Entity_Id);
      --  If this is a new entity the remove it; otherwise no action is
      --  performed since there are references to it in the tree.

      ----------------
      -- Hash_Table --
      ----------------

      --  Hash table containing all the entities of the project. Used to avoid
      --  generating duplicate entities.

      package Hash_Table is

         function Hash
           (Key : General_Location) return Ada.Containers.Hash_Type;

         function Equivalent_Keys
           (Left, Right : General_Location) return Boolean;

         package EInfo_Map is new Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => General_Location,
            Element_Type    => Entity_Id,
            Hash            => Hash,
            Equivalent_Keys => Equivalent_Keys);

         Entities_Map : EInfo_Map.Map;

         procedure Append_To_Map (E : Unique_Entity_Id);
         --  Append the entity of E to Entities_Map

      end Hash_Table;
      use Hash_Table;

      ------------------------------------------
      --  Debugging routines (for use in gdb) --
      ------------------------------------------

      procedure pn (E : Unique_Entity_Id);
      --  (gdb) Prints a single tree node (full output), without printing
      --  descendants.

      procedure pnid (Unique_Id : Natural);
      --  (gdb) Search for and entity in the hash-table with Unique_Id and
      --  prints its contents. No output generated if the entity is not found.

   private
      type Unique_Entity_Info is record
         Context       : access constant Docgen_Context;
         Entity        : Entity_Id    := Atree.No_Entity;
         File          : Virtual_File := No_File;
         C_Header_File : Virtual_File := No_File;
         Is_New        : Boolean      := False;
      end record;

      pragma Inline (Get_Entity);

      pragma Export (Ada, pn);
      pragma Export (Ada, pnid);
   end Unique_Entity_Allocator;

   use Unique_Entity_Allocator;
   use Unique_Entity_Allocator.Hash_Table;

   ----------------------
   -- Entity_Allocator --
   ----------------------

   package body Unique_Entity_Allocator is

      ----------------
      -- Hash_Table --
      ----------------

      package body Hash_Table is

         -------------------
         -- Append_To_Map --
         -------------------

         procedure Append_To_Map (E : Unique_Entity_Id) is
         begin
            Entities_Map.Include
              (LL.Get_Location (Get_Entity (E)), Get_Entity (E));
         end Append_To_Map;

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

         ----------
         -- Hash --
         ----------

         function Hash (Key : General_Location) return Ada.Containers.Hash_Type
         is
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

      end Hash_Table;

      ----------
      -- Free --
      ----------

      procedure Free (Entity : in out Unique_Entity_Id) is
         procedure Internal_Free is
           new Ada.Unchecked_Deallocation
                (Unique_Entity_Info, Unique_Entity_Id);
      begin
         if Entity /= null then
            if Entity.Is_New then
               Free (Entity.Entity);
            end if;

            Internal_Free (Entity);
         end if;
      end Free;

      ----------------
      -- Get_Entity --
      ----------------

      function Get_Entity (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         pragma Assert (Present (Entity));
         return Entity.Entity;
      end Get_Entity;

      ------------
      -- Is_New --
      ------------

      function Is_New (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Entity.Is_New;
      end Is_New;

      ----------------
      -- New_Entity --
      ----------------

      procedure Get_Unique_Entity
        (Entity  : out Unique_Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : General_Entity;
         Forced  : Boolean := False;
         C_Header_File : Virtual_File := No_File)

      is
         Db    : General_Xref_Database renames Context.Database;
         E_Loc : constant General_Location := Get_Location (Db, E);

         Lang        : constant Language_Access :=
                         Get_Language_From_File (Context.Lang_Handler, File);
         In_Ada_Lang : constant Boolean :=
                         Lang.all in Language.Ada.Ada_Language'Class;
         In_CPP_Lang : constant Boolean :=
                         Lang.all in Language.Cpp.Cpp_Language'Class;

         procedure Build_New_Entity;
         procedure Build_New_Entity (Ref_File : Virtual_File);
         --  Local routines which factorize code used to allocate a new
         --  entity

         procedure Build_New_Entity is
         begin
            Entity :=
              new Unique_Entity_Info'
                    (Context       => Context,
                     Entity        => New_Entity (Context, Lang, E, E_Loc),
                     File          => File,
                     C_Header_File => C_Header_File,
                     Is_New        => True);
         end Build_New_Entity;

         procedure Build_New_Entity (Ref_File : Virtual_File) is
         begin
            Entity :=
              new Unique_Entity_Info'
                    (Context       => Context,
                     Entity        => New_Entity (Context, Lang, E, E_Loc),
                     File          => File,
                     C_Header_File => C_Header_File,
                     Is_New        => True);
            Set_Ref_File (Entity.Entity, Ref_File);
         end Build_New_Entity;

         --  Local variables

         Is_Prim : constant Boolean := Present (Is_Primitive_Of (Db, E));
         --  Avoid calling twice this service???

      begin
         Entity := null;

         --  Case 1: Entities defined in other packages/files

         if In_Ada_Lang then

            if E_Loc.File /= File
              and then not Is_Prim
              and then not Forced
            then
               return;
            end if;

            --  Before creating the entity we search for it in the hash
            --  table (to avoid duplicating it!)

            declare
               Map_Cursor : constant EInfo_Map.Cursor :=
                 Entities_Map.Find (E_Loc);
               use type EInfo_Map.Cursor;
            begin
               if Map_Cursor /= EInfo_Map.No_Element then
                  Entity :=
                    new Unique_Entity_Info'
                      (Context       => Context,
                       Entity        => EInfo_Map.Element (Map_Cursor),
                       File          => File,
                       C_Header_File => C_Header_File,
                       Is_New        => False);
               else
                  Build_New_Entity;
               end if;

               return;
            end;

         --  C++

         elsif In_CPP_Lang then
            declare
               Kind : constant Entity_Kind :=
                        LL.Get_Ekind (Db, E, In_Ada_Lang => False);
               Map_Cursor : EInfo_Map.Cursor;

               use type EInfo_Map.Cursor;

            begin
               if Kind = E_Include_File then
                  return;
               end if;

               --  Before creating the entity we search for it in the hash
               --  table (to avoid duplicating it!)

               Map_Cursor := Entities_Map.Find (E_Loc);

               if Map_Cursor /= EInfo_Map.No_Element then
                  Entity :=
                    new Unique_Entity_Info'
                      (Context       => Context,
                       Entity        => EInfo_Map.Element (Map_Cursor),
                       File          => File,
                       C_Header_File => C_Header_File,
                       Is_New        => False);
               else
                  Build_New_Entity;

                  if LL.Get_Location (Get_Entity (Entity)).File /= File then
                     Set_Ref_File (Get_Entity (Entity), File);
                  end if;
               end if;

               return;
            end;

         --  C

         else
            declare
               E_Body_Loc : constant General_Location := Get_Body (Db, E);
               Kind       : constant Entity_Kind :=
                              LL.Get_Ekind (Db, E, In_Ada_Lang => False);

            begin
               if Kind = E_Include_File
                 or else Kind = E_Unknown
               then
                  return;

               elsif E_Loc.File /= File then

                  --  Handle entities defined in the header file (.h)

                  --  If we already know the associated header file we can
                  --  safely take a decision

                  if C_Header_File /= No_File then
                     if E_Loc.File /= No_File
                       and then E_Loc.File = C_Header_File
                     then
                        Build_New_Entity (Ref_File => File);
                        return;
                     else
                        return;
                     end if;

                     --  Otherwise this works only if the first entity
                     --  referenced from the header file is a subprogram.
                     --  More work needed here to handle other entities???

                  elsif Present (E_Body_Loc)
                    and then E_Body_Loc.File = File
                  then
                     Build_New_Entity (Ref_File => File);
                     return;

                     --  We assume that there is only a header file
                     --  associated with each .c or .cpp file. We also
                     --  assume that the name of the header file and the
                     --  name of the .c/.cpp files (without their extension)
                     --  matches. Otherwise more work is needed here to
                     --  handle these entities???

                  elsif Filename (E_Loc.File) = Filename (File) then
                     Build_New_Entity (Ref_File => File);
                     return;

                  else
                     return;
                  end if;
               end if;

               Build_New_Entity;
               return;
            end;
         end if;

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return;
      end Get_Unique_Entity;

      -------------------------
      -- New_Internal_Entity --
      -------------------------

      function New_Internal_Entity
        (Context  : access constant Docgen_Context;
         Language : Language_Access;
         Name     : String) return Unique_Entity_Id
      is
      begin
         return
           new Unique_Entity_Info'
                 (Context       => Context,
                  Entity        => New_Internal_Entity
                                     (Context  => Context,
                                      Language => Language,
                                      Name     => Name),
                  File          => No_File,
                  C_Header_File => No_File,
                  Is_New        => True);
      end New_Internal_Entity;

      -------------
      -- Present --
      -------------

      function Present (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Entity /= No_Entity;
      end Present;

      -------------------
      -- Update_Entity --
      -------------------

      procedure Update_Entity
        (Entity : in out Unique_Entity_Id;
         E      : General_Entity;
         Forced : Boolean := False)
      is
         Context  : constant access constant Docgen_Context := Entity.Context;
         File     : constant Virtual_File := Entity.File;
         C_Header : constant Virtual_File := Entity.C_Header_File;

      begin
         Free (Entity);
         Get_Unique_Entity
           (Entity, Context, File, E, Forced, C_Header);
      end Update_Entity;

      --------
      -- pn --
      --------

      procedure pn (E : Unique_Entity_Id) is
      begin
         if Present (Get_Entity (E)) then
            Atree.pn (Get_Entity (E));
         end if;
      end pn;

      ----------
      -- pnid --
      ----------

      procedure pnid (Unique_Id : Natural) is
         Cursor : EInfo_Map.Cursor;
         E      : Entity_Id;
      begin
         Cursor := Entities_Map.First;
         while EInfo_Map.Has_Element (Cursor) loop
            E := EInfo_Map.Element (Cursor);

            if Get_Unique_Id (E) = Unique_Id then
               pn (E);
               exit;
            end if;

            EInfo_Map.Next (Cursor);
         end loop;
      end pnid;

   end Unique_Entity_Allocator;

   -----------------
   -- Scope_Stack --
   -----------------

   --  Subsidiary package used to build the tree

   package Scopes_Stack is

      procedure Clear;
      --  Clear the contents of the Scope Stack and unregister the entity
      --  which represents the standard entity.

      function Current_Scope return Unique_Entity_Id;
      --  Return the entity in the top of the stack

      function Current_Scope_Depth return Natural;
      --  Return the depth of the stack

      procedure Enter_Scope (Scope : Unique_Entity_Id);
      --  Push Scope

      procedure Exit_Scope;
      --  Pop an entity from the stack

      function Find_Entity
        (Scope : Unique_Entity_Id;
         Loc   : General_Location) return Entity_Id;
      --  Search for the entity at Loc in all the enclosing scopes of
      --  Scope.

      function In_Open_Scopes (E : General_Entity) return Boolean;
      --  E is the entity of a scope. This function determines if this scope
      --  is currently open (i.e. it appears somewhere in the scope stack).

      function In_Generic_Scope return Boolean;
      --  Return true if some enclosing scopes is generic

      procedure Register_Std_Entity (E : Unique_Entity_Id);
      --  Register in the package the entity used to represent the standard
      --  entity. Needed internally to identify the outermost scope.

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

                        declare
                           Tok_Loc   : General_Location;
                           LL_Parent : General_Entity;
                           Entity    : Unique_Entity_Id;

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

                           Get_Unique_Entity
                             (Entity  => Entity,
                              Context => Context,
                              File    => File,
                              E       => LL_Parent,
                              Forced  => True);
                           Set_Parent (E, Get_Entity (Entity));
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
                  C_Headers_Buffer := LL.Get_Location (E).File.Read_File;
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

         Under_Development_Text : constant String :=
           "<<Get_Source under development for this kind of entity>>";

      --  Start of processing for CPP_Get_Source

      begin
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

         elsif LL.Is_Type (E) then
            pragma Assert (False);
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
      C_Header_File  : Virtual_File;

      Std_Entity     : constant Unique_Entity_Id :=
                         New_Internal_Entity
                           (Context  => Context,
                            Language => Lang,
                            Name     => "Standard");

      use Scopes_Stack;

      procedure Append_To_File_Entities (E : Unique_Entity_Id);
      --  Append E to File_Entities.All_Entities

      procedure Append_To_Scope (Scope : Entity_Id; E : Unique_Entity_Id);
      --  Append E to the list of entities of Scope

      procedure Complete_Decoration (E : Unique_Entity_Id);
      --  Complete the decoration of entity E

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id);
      --  Update the contents of the scope stack trusting on the Scope provided
      --  by Xref (if available)

      -----------------------------
      -- Append_To_File_Entities --
      -----------------------------

      procedure Append_To_File_Entities (E : Unique_Entity_Id) is
      begin
         File_Entities.All_Entities.Append (Get_Entity (E));
      end Append_To_File_Entities;

      ---------------------
      -- Append_To_Scope --
      ---------------------

      procedure Append_To_Scope (Scope : Entity_Id; E : Unique_Entity_Id) is
      begin
         Append_Entity (Scope, Get_Entity (E));
         Set_Scope (Get_Entity (E), Scope);
         Append_To_File_Entities (E);
      end Append_To_Scope;

      -------------------------
      -- Complete_Decoration --
      -------------------------

      procedure Complete_Decoration (E : Unique_Entity_Id) is

         procedure Decorate_Record_Type (E : Unique_Entity_Id);
         --  Complete the decoration of a record type entity

         procedure Decorate_Subprogram (E : Unique_Entity_Id);
         --  Complete the decoration of a subprogram entity

         --------------------------
         -- Decorate_Record_Type --
         --------------------------

         procedure Decorate_Record_Type (E : Unique_Entity_Id) is

            procedure Append_Parent_And_Progenitors
              (Parents  : Xref.Entity_Array);

            procedure Append_Parent_And_Progenitors
              (Parents : Xref.Entity_Array)
            is
               Parent : Unique_Entity_Id;

            begin
               --  Identify the parent and the progenitors of the type. When
               --  the parent of a type T is an interface type and it T covers
               --  also several interfaces then Xref does not provide us with
               --  enough information to know which interface is the parent
               --  type. The best we can do for now is to catch the parent
               --  as part of processing the corresponding source file (see
               --  Get_Record_Type_Source).

               for J in Parents'Range loop
                  Get_Unique_Entity
                    (Parent, Context, File, Parents (J), Forced => True);
                  LL.Append_Parent_Type (Get_Entity (E), Get_Entity (Parent));

                  --  The list of parents returned by Xref does not help to
                  --  differentiate the parent type from the progenitors.

                  if In_Ada_Lang then
                     if Get_Kind (Get_Entity (Parent)) /= E_Interface then
                        Set_Parent (Get_Entity (E), Get_Entity (Parent));
                     else
                        Append_Progenitor
                          (Get_Entity (E), Get_Entity (Parent));
                     end if;
                  end if;

                  if Is_New (Parent) then
                     Append_To_Map (Parent);
                  end if;
               end loop;
            end Append_Parent_And_Progenitors;

         begin
            if In_Ada_Lang then
               declare
                  Discrim : constant Xref.Entity_Array :=
                              Discriminants
                                (Context.Database,
                                 LL.Get_Entity (Get_Entity (E)));
                  Entity  : Unique_Entity_Id;
               begin
                  for J in Discrim'Range loop
                     Get_Unique_Entity (Entity, Context, File, Discrim (J));
                     Set_Kind (Get_Entity (Entity), E_Discriminant);
                     Append_To_Map (Entity);
                     Append_Discriminant (Get_Entity (E), Get_Entity (Entity));
                     pragma Assert (Is_New (Entity));
                     Append_To_Map (Entity);
                  end loop;
               end;
            end if;

            --  Check_Record_Components

            declare
               Components : constant Xref.Entity_Array :=
                              Fields (Context.Database,
                                      LL.Get_Entity (Get_Entity (E)));
               Entity     : Unique_Entity_Id;

            begin
               for J in Components'Range loop
                  Get_Unique_Entity (Entity, Context, File, Components (J));
                  --  In C++ we have here formals of primitives???
                  Set_Kind (Get_Entity (Entity), E_Component);
                  Append_To_Scope (Get_Entity (E), Entity);
                  pragma Assert (Is_New (Entity));
                  Append_To_Map (Entity);
               end loop;
            end;

            if Is_Tagged_Type (Get_Entity (E))
              or else (In_CPP_Lang
                         and then Get_Kind (Get_Entity (E)) = E_Class)
            then
               --  ??? Xref bug (Xref.Methods): Include_Inherited returns
               --  the same array when set to True or False (that is, False
               --  has no effect).

               declare
                  All_Methods : constant Xref.Entity_Array :=
                                  Methods
                                    (Context.Database,
                                     LL.Get_Entity (Get_Entity (E)),
                                     Include_Inherited => True);
                  Method : Unique_Entity_Id;

               begin
                  for J in All_Methods'Range loop
                     Get_Unique_Entity
                       (Method, Context, File, All_Methods (J),
                        Forced => True);

                     if not Is_New (Method) then
                        Append_Inherited_Method
                          (Get_Entity (E), Get_Entity (Method));

                     elsif In_Ada_Language (Get_Entity (Method)) then

                        --  If Xref does not have available the scope of this
                        --  method it means that it is a primitive defined in
                        --  a file which is not directly part of this project
                        --  (that is, an entity defined in the runtime of the
                        --  compiler or in a library). In such case we consider
                        --  that it is an inherited primitive.

                        if No (LL.Get_Scope (Get_Entity (Method)))
                          or else LL.Get_Scope (Get_Entity (Method))
                                    /= LL.Get_Scope (Get_Entity (E))
                        then
                           --  For inherited primitives defined in other
                           --  files/scopes we cannot set their scope.

                           Decorate_Subprogram (Method);
                           Append_Inherited_Method
                             (Get_Entity (E), Get_Entity (Method));

                        else
                           Append_To_Scope
                             (Get_Scope (Get_Entity (E)), Method);
                           Decorate_Subprogram (Method);
                           Append_Method (Get_Entity (E), Get_Entity (Method));
                        end if;

                        Append_To_Map (Method);
                     else
                        if LL.Get_Scope (Get_Entity (Method))
                          = LL.Get_Entity (Get_Entity (E))
                        then
                           Append_To_Scope (Get_Entity (E), Method);
                           Decorate_Subprogram (Method);
                           Append_Method
                             (Get_Entity (E), Get_Entity (Method));

                        --  For inherited primitives defined in other
                        --  scopes we cannot set their scope.

                        else
                           Decorate_Subprogram (Method);
                           Append_Inherited_Method
                             (Get_Entity (E), Get_Entity (Method));
                        end if;

                        Append_To_Map (Method);
                     end if;
                  end loop;
               end;
            end if;

            Append_Parent_And_Progenitors
              (Xref.Parent_Types
                 (Self      => Context.Database,
                  Entity    => LL.Get_Entity (Get_Entity (E)),
                  Recursive => False));

            if In_Ada_Language (Get_Entity (E)) then

               --  Add information available in the full view (if the entity
               --  of its full view is available; see the comment describing
               --  this problem in docgen3-atree.adb???)

               if Is_Incomplete_Or_Private_Type (Get_Entity (E))
                 and then Present (LL.Get_Full_View (Get_Entity (E)))
               then
                  Append_Parent_And_Progenitors
                    (Parent_Types
                       (Context.Database, LL.Get_Full_View (Get_Entity (E)),
                        Recursive => False));
               end if;

               if No (Get_Parent (Get_Entity (E)))
                 and then Natural (Get_Progenitors (Get_Entity (E)).all.Length)
                            = 1
               then
                  Set_Parent
                    (Get_Entity (E),
                     Get_Progenitors (Get_Entity (E)).First_Element);
                  Get_Progenitors (Get_Entity (E)).Delete_First;
               end if;
            end if;

            declare
               Childs : constant Xref.Entity_Array :=
                          Child_Types
                           (Context.Database, LL.Get_Entity (Get_Entity (E)),
                            Recursive => False);
               Child  : Unique_Entity_Id;

            begin
               for J in Childs'Range loop

                  --  Do not add as a child type the second entity generated
                  --  by the compiler for named typedef structs (the compiler
                  --  generates two entites in the LI file with the same name)

                  if In_Ada_Language (Get_Entity (E))
                    or else not
                      LL.Is_Self_Referenced_Type
                        (Db   => Context.Database,
                         E    => Childs (J),
                         Lang => Get_Language (Get_Entity (E)))
                  then
                     Get_Unique_Entity
                       (Child, Context, File, Childs (J), Forced => True);

                     --  Avoid problems with wrong Xref decoration that I can
                     --  reproduces with gnatcoll-refcount-weakref.ads. To
                     --  be investigated???

                     if not Is_Class_Or_Record_Type (Get_Entity (Child)) then
                        Free (Child);

                     else
                        LL.Append_Child_Type
                          (Get_Entity (E), Get_Entity (Child));

                        if Is_New (Child) then
                           Append_To_Map (Child);
                        end if;
                     end if;
                  end if;
               end loop;
            end;
         end Decorate_Record_Type;

         -------------------------
         -- Decorate_Subprogram --
         -------------------------

         procedure Decorate_Subprogram (E : Unique_Entity_Id) is
            Formals : constant Xref.Parameter_Array :=
                        Parameters
                          (Context.Database, LL.Get_Entity (Get_Entity (E)));
            Formal  : Unique_Entity_Id;

         begin
            Enter_Scope (E);

            for J in Formals'Range loop
               Get_Unique_Entity
                 (Formal, Context, File, Formals (J).Parameter);

               --  Formals (J).Kind ???
               --  Is_Full_View is erroneusly set in formals ???
               if Present (Formal) then
                  Set_Kind (Get_Entity (Formal), E_Formal);
                  Set_Scope (Get_Entity (Formal), Get_Entity (E));

                  Append_To_Scope (Get_Entity (Current_Scope), Formal);
                  pragma Assert (Is_New (Formal)
                                 or else In_Generic_Scope);
                  --  For generic formals we probably should force the
                  --  generation of a new entity???

                  Append_To_Map (Formal);
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
         if LL.Is_Container (Get_Entity (E)) then
            if Is_Class_Or_Record_Type (Get_Entity (E)) then
               Decorate_Record_Type (E);

               --  Although formals are available in the list of
               --  entities of the file we are traversing, it is not
               --  easy to identify and set the scope of formals just
               --  traversing these entities since some entities do
               --  not have its Xref.Scope entity available.

            elsif LL.Is_Subprogram (Get_Entity (E)) then
               Decorate_Subprogram (E);
            end if;

         elsif Get_Kind (Get_Entity (E)) = E_Interface then
            Decorate_Record_Type (E);
         end if;
      end Complete_Decoration;

      -------------------------
      -- Update_Scopes_Stack --
      -------------------------

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id) is
         Scope_Id : Entity_Id;

      begin
         if LL.Get_Scope (Get_Entity (New_E)) = No_General_Entity then
            Set_Scope (Get_Entity (New_E), Get_Entity (Current_Scope));

            --  More work needed with generic types since in some cases
            --  the scope is set???

            --  Fails also in subprogram GNATCOLL.Projects.Initialize
            --  but I cannot see why???
            --                pragma Assert (Current_Scope = Std_Entity
            --                                 or else In_Generic_Scope);
            --   or else Current_Scope.Xref.Is_Generic);
            --   or else New_E.Kind = E_Access_Type);

         else
            if In_Open_Scopes (LL.Get_Scope (Get_Entity (New_E))) then
               while LL.Get_Scope (Get_Entity (New_E))
                 /= LL.Get_Entity (Get_Entity (Current_Scope))
               loop
                  Exit_Scope;
               end loop;

            elsif LL.Is_Type (Get_Entity (New_E)) then
               Scope_Id :=
                 Find_Entity
                   (Current_Scope,
                    Get_Location
                      (Context.Database, LL.Get_Scope (Get_Entity (New_E))));

               Set_Scope (Get_Entity (New_E), Scope_Id);

               --  pragma Assert (Get_Scope (New_E) /= null);

               if No (Get_Scope (Get_Entity (New_E))) then
                  pragma Assert
                    (Get_Kind (Get_Entity (New_E)) = E_Access_Type);
                  Set_Scope (Get_Entity (New_E), Get_Entity (Current_Scope));
               end if;
            end if;
         end if;
      end Update_Scopes_Stack;

      --  Local variables

      --  This entity represents the outermost scope (ie. the standard scope).
      --  It is needed to associate some scope to generic formals of library
      --  level units.

      New_E                : Unique_Entity_Id;
      Skip_This_Entity     : Boolean := False;
      File_Entities_Cursor : Entities_In_File_Cursor;

      Total_Entities_Count : Natural := 0;
      Entities_Count       : Natural := 0;

   --  Start of processing for Build_File_Tree

   begin
      C_Header_File := No_File;

      Set_Kind (Get_Entity (Std_Entity), E_Package);
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

         Scopes_Stack.Clear;
         return No_Tree_Type;
      end if;

      File_Entities_Cursor := Context.Database.Entities_In_File (File);

      --  Locate the root of the tree of entities

      if In_Ada_Lang then
         while not At_End (File_Entities_Cursor) loop
            Get_Unique_Entity
              (New_E, Context, File, File_Entities_Cursor.Get);
            File_Entities_Cursor.Next;

            if Present (New_E) then
               if Is_New (New_E) then
                  Append_To_Map (New_E);
               end if;

               Complete_Decoration (New_E);

               --  Do not set again the scope of formals which are already
               --  decorated. For instance:

               --     generic
               --        with procedure Error (Msg : String);
               --     ...

               if Get_Kind (Get_Entity (New_E)) /= E_Formal then
                  Append_To_Scope (Get_Entity (Current_Scope), New_E);
                  Append_To_Map (New_E);
               end if;

               --  Avoid spurious entity GNATCOLL.Any_Types (procedure???)

               if LL.Is_Container (Get_Entity (New_E))
                 and then LL.Is_Global (Get_Entity (New_E))
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

         if Entities_Count mod 75 = 0 then
            GNAT.IO.Put_Line
              (+File.Base_Name
               & ":"
               & To_String (Entities_Count)
               & "/"
               & To_String (Total_Entities_Count));
         end if;

         Skip_This_Entity := False;
         Get_Unique_Entity
           (New_E, Context, File, File_Entities_Cursor.Get,
            C_Header_File => C_Header_File);

         if Present (New_E) then

            --  Update the scopes stack using the reliable value provided by
            --  the low level (ie. Xref).

            if In_Ada_Lang
              and then Present (LL.Get_Scope (Get_Entity (New_E)))
            then
               --  Skip the full view of incomplete or private types because
               --  their Xref.Scope references the partial view (instead of
               --  referencing its syntax scope)

               if Is_Incomplete_Or_Private_Type (Get_Entity (New_E))
                 and then Is_Full_View (Get_Entity (New_E))
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
              and then LL.Get_Location (Get_Entity (New_E)).File /= File
              and then LL.Is_Subprogram (Get_Entity (New_E))
              and then LL.Is_Primitive (Get_Entity (New_E))
              and then Present (LL.Get_Scope (Get_Entity (New_E)))
            then
               Update_Entity
                 (New_E, LL.Get_Scope (Get_Entity (New_E)), Forced => True);
               pragma Assert (Present (New_E));
            end if;

            --  (C/C++) Locate the associated header file (if any). Currently
            --  we assume that each .c/.cpp file has a single associated
            --  header file.

            if In_CPP_Lang
              and then Get_Kind (Get_Entity (New_E)) = E_Class
              and then LL.Get_Location (Get_Entity (New_E)).File /= File
            then
               pragma Assert (C_Header_File = No_File
                 or else C_Header_File
                           = LL.Get_Location (Get_Entity (New_E)).File);
               C_Header_File := LL.Get_Location (Get_Entity (New_E)).File;

            elsif In_C_Lang
              and then LL.Get_Location (Get_Entity (New_E)).File /= File
            then
               pragma Assert (C_Header_File = No_File
                 or else C_Header_File
                           = LL.Get_Location (Get_Entity (New_E)).File);
               C_Header_File := LL.Get_Location (Get_Entity (New_E)).File;
            end if;

            --  Decorate the new entity

            if In_CPP_Lang
              and then Get_Kind (Get_Entity (New_E)) = E_Class
              and then Entities_Map.Contains
                         (LL.Get_Location (Get_Entity (New_E)))
            then
               --  No need to handle twice the same class
               Skip_This_Entity := True;

            elsif In_C_Lang
              and then Get_Kind (Get_Entity (New_E)) = E_Variable
              and then not LL.Is_Global (Get_Entity (New_E))
            then
               Skip_This_Entity := True;

            elsif In_Ada_Lang
              and then not
                Kind_In (Get_Kind (Get_Entity (New_E)), E_Variable,
                                                          E_Formal,
                                                          E_Discriminant,
                                                          E_Component)
              and then not LL.Is_Primitive (Get_Entity (New_E))
            then
               --  Skip processing the full-view of a private or incomplete
               --  type since its components are retrieved from Xref when
               --  we process its partial view.

               if Is_Incomplete_Or_Private_Type (Get_Entity (New_E))
                 and then Is_Full_View (Get_Entity (New_E))
               then
                  Skip_This_Entity := True;
               end if;

            --  Skip methods since they are entered in the tree as part of
            --  processing its class/tagged type

            elsif LL.Is_Primitive (Get_Entity (New_E)) then
               Skip_This_Entity := True;

            elsif In_Ada_Lang then

               --  An E_Variable may be in fact a component of an incomplete
               --  or private type

               if LL.Get_Kind (Get_Entity (New_E)) = E_Variable then
                  declare
                     Map_Cursor : constant EInfo_Map.Cursor :=
                                    Entities_Map.Find
                                      (LL.Get_Location (Get_Entity (New_E)));
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
              and then Present (LL.Get_Scope (Get_Entity (New_E)))
              and then LL.Is_Global (Get_Entity (New_E))
            then
               if Is_Class_Or_Record_Type (Get_Entity (New_E))
                 and then Get_Kind (Get_Entity (New_E)) /= E_Class
               then
                  --  Handle named typedef structs since the compiler
                  --  generates two entites in the LI file with the
                  --  same name.

                  declare
                     Scope_Id : constant General_Entity :=
                                  LL.Get_Scope (Get_Entity (New_E));
                  begin
                     if Context.Database.Get_Name
                          (LL.Get_Entity (Get_Entity (New_E)))
                       = Context.Database.Get_Name (Scope_Id)
                     then
                        Update_Entity (New_E, Scope_Id);
                     end if;
                  end;

               elsif Get_Kind (Get_Entity (New_E)) = E_Variable then
                  declare
                     Scope_Id   : constant General_Entity :=
                                    LL.Get_Scope (Get_Entity (New_E));
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
               if Is_New (New_E) then
                  Append_To_Map (New_E);
               end if;

               if In_Ada_Lang then
                  Append_To_Scope (Get_Entity (Current_Scope), New_E);

                  Complete_Decoration (New_E);

                  if Get_Kind (Get_Entity (New_E)) = E_Enumeration_Type then
                     Enter_Scope (New_E);

                  elsif Is_Package (Get_Entity (New_E)) then
                     Enter_Scope (New_E);
                  end if;

               --  C/C++

               else
                  if LL.Get_Location (Get_Entity (New_E)).File /= File then
                     Append_To_Scope (Get_Entity (Std_Entity), New_E);
                  else
                     Append_To_Scope (Get_Entity (Current_Scope), New_E);
                  end if;

                  Complete_Decoration (New_E);
               end if;
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      Scopes_Stack.Clear;

      return Built_Tree_Type'(Get_Entity (Std_Entity), C_Header_File);
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      EInfo_Map.Clear (Entities_Map);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -----------------
   -- Scope_Stack --
   -----------------

   package body Scopes_Stack is

      package Alloc_Entity_List is new Ada.Containers.Vectors
        (Index_Type => Natural, Element_Type => Unique_Entity_Id);
      --  procedure Free (List : in out Alloc_Entity_List.Vector);

      Std_Entity : Unique_Entity_Id;
      Stack      : Alloc_Entity_List.Vector;

      procedure Clear is
      begin
         Stack.Clear;
         Std_Entity := Unique_Entity_Allocator.No_Entity;
      end Clear;

      function Current_Scope return Unique_Entity_Id is
      begin
         return Stack.Element (0);
      end Current_Scope;

      function Current_Scope_Depth return Natural is
      begin
         return Natural (Stack.Length);
      end Current_Scope_Depth;

      procedure Enter_Scope (Scope : Unique_Entity_Id) is
      begin
         Stack.Prepend (Scope);
      end Enter_Scope;

      procedure Exit_Scope is
      begin
         Stack.Delete_First;
      end Exit_Scope;

      function Find_Entity
        (Scope : Unique_Entity_Id;
         Loc   : General_Location) return Entity_Id
      is
         Cursor : EInfo_List.Cursor;
         E      : Entity_Id;
         S      : Entity_Id := Get_Entity (Scope);

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

            exit when Get_Scope (S) = Get_Entity (Std_Entity);
            S := Get_Scope (S);
         end loop;

         return null;
      end Find_Entity;

      function In_Generic_Scope return Boolean is
         Last : constant Integer :=
           Current_Scope_Depth - 2; -- Skip standard
         use type Ada.Containers.Count_Type;
         S : Unique_Entity_Id;
      begin
         for J in 0 .. Last loop
            S := Stack.Element (Natural (J));

            if LL.Is_Generic (Get_Entity (S)) then
               return True;
            end if;
         end loop;

         return False;
      end In_Generic_Scope;

      function In_Open_Scopes (E : General_Entity) return Boolean is
         Last : constant Integer :=
           Current_Scope_Depth - 2; -- Skip standard

         use type Ada.Containers.Count_Type;
         S : Unique_Entity_Id;
      begin
         for J in 0 .. Last loop
            S := Stack.Element (Natural (J));

            if LL.Get_Entity (Get_Entity (S)) = E then
               return True;
            end if;
         end loop;

         return False;
      end In_Open_Scopes;

      procedure Register_Std_Entity (E : Unique_Entity_Id) is
      begin
         pragma Assert (Std_Entity = Unique_Entity_Allocator.No_Entity);
         Std_Entity := E;
      end Register_Std_Entity;

   end Scopes_Stack;

end Docgen3.Frontend;
