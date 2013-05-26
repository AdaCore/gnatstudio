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
with Docgen3.Utils;           use Docgen3.Utils;
with Docgen3.Errout;          use Docgen3.Errout;
with Docgen3.Time;            use Docgen3.Time;
with Language;                use Language;
with Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with Language_Handlers;       use Language_Handlers;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;
with UTF8_Utils;              use UTF8_Utils;

with Xref.Docgen;             use Xref.Docgen;

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

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id;
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
      Buffer : GNAT.Strings.String_Access;

      procedure Get_Doc (E : Entity_Id);
      --  Retrieve the documentation associated with E

      procedure Get_Source (E : Entity_Id);
      --  Retrieve the source associated with E

      -------------
      -- Get_Doc --
      -------------

      procedure Get_Doc (E : Entity_Id) is
      begin
         Set_Doc (E,
           Xref.Docgen.Get_Docgen_Documentation
             (Self =>
                General_Xref_Database_Record (Context.Database.all)'Access,
              Handler => Context.Lang_Handler,
              Buffer  => Buffer,
              Entity  => LL.Get_Entity (E)));

         --           Set_Doc (E,
         --             Db.Get_Docgen_Documentation
         --               (Handler => Lang_Handler,
         --                Buffer  => Buffer,
         --                Entity  => LL.Get_Entity (E)));
      end Get_Doc;

      ----------------
      -- Get_Source --
      ----------------

      procedure Get_Source (E : Entity_Id) is

         function Get_Source
           (Context : access constant Docgen_Context;
            Buffer : GNAT.Strings.String_Access;
            File   : Virtual_File;
            Entity : General_Entity) return Unbounded_String;
         --  Retrieve the source of Entity

         ----------------
         -- Get_Source --
         ----------------

         function Get_Source
           (Context : access constant Docgen_Context;
            Buffer : GNAT.Strings.String_Access;
            File   : Virtual_File;
            Entity : General_Entity) return Unbounded_String
         is
            Printout : Unbounded_String;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            --------
            -- CB --
            --------

            Par_Count : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);
               --  pragma Unreferenced (Entity);

               S : constant String :=
                     Buffer (Sloc_Start.Index .. Sloc_End.Index);

               --  use Basic_Types;
            begin
               --  if Entity = Block_Text then  --  identifier???
               --     return False; --  continue

               if Entity = Comment_Text then
                  return False; --  continue

               elsif Entity = Operator_Text then
                  if S = "(" then
                     if Par_Count = 0 then
                        Printout := Printout & ASCII.LF & "     ";
                     end if;

                     Par_Count := Par_Count + 1;
                  elsif S = ")" then
                     Par_Count := Par_Count - 1;
                     if Par_Count = 0 then
                        Printout := Printout & ASCII.LF & "     ";
                     end if;

                  elsif S = ";" then
                     if Par_Count = 0 then
                        return True;
                     end if;
                  end if;
               end if;

               Printout := Printout & " " & S;

               --  Parameters delimiter

               if Entity = Operator_Text and then S = ";" then
                  Printout := Printout & ASCII.LF & "       ";
               end if;

               return False;
            exception
               when E : others =>
                  Trace (Exception_Handle, E);
                  return True;
            end CB;

            --  Local variables

            Lang : constant Language_Access :=
                     Get_Language_From_File (Context.Lang_Handler, File);

         --  Start of processing for Get_Source

         begin
            Trace (Me, "Get_Source of " & (+File.Base_Name));

            declare
               Loc  : constant General_Location :=
                 Get_Location (Context.Database, Entity);

               Index         : Natural;
               Lines_Skipped : Natural;

            begin
               --  Displace the pointer to the beginning of the subprogram
               Index := Buffer'First;
               GNATCOLL.Utils.Skip_Lines
                 (Str           => Buffer.all,
                  Lines         => Loc.Line - 1,
                  Index         => Index,
                  Lines_Skipped => Lines_Skipped);

               Printout := Printout & "   ";
               Parse_Entities
                 (Lang, Buffer.all (Index .. Buffer'Last),
                  CB'Unrestricted_Access);

               return Printout;
            end;
         end Get_Source;

      begin
         if True then
            --  To be improved???
            Set_Src (E, To_Unbounded_String ("<<Get_Source disabled>>"));
            return;
         end if;

         Set_Src (E,
           Get_Source
             (Context => Context,
              Buffer  => Buffer,
              File    => File,
              Entity  => LL.Get_Entity (E)));
      end Get_Source;

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

   --  Start of processing for Add_Documentation_From_Sources

   begin
      EInfo_Vector_Sort_Loc.Sort (File_Entities.All_Entities);

      declare
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
      end;

      if False then
         if Present (File_Entities.Tree_Root) then
            Traverse_Tree (File_Entities.Tree_Root, Process_Node'Access);
         end if;

      else
         --  Retrieve the documentation from sources (if any)

         For_All (File_Entities.All_Entities, Get_Doc'Access);
         For_All (File_Entities.All_Entities, Get_Source'Access);
      end if;

      Free (Buffer);
   end Add_Documentation_From_Sources;

   ---------------------
   -- Build_File_Tree --
   ---------------------

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id
   is
      Lang         : constant Language_Access :=
                       Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Entities_Map : EInfo_Map.Map;

      use Scopes_Stack;

      procedure Append_To_Scope (Scope : Entity_Id; E : Entity_Id);
      --  Append E to the list of entities of Scope

      procedure Complete_Decoration (E : Entity_Id);
      --  Complete the decoration of entity E

      function New_Entity (E : General_Entity) return Entity_Id;
      --  Build a new entity for the Xref entity E

      procedure Update_Scopes_Stack (New_E : Entity_Id);
      --  Update the scopes stack relying on the Scope value provided by Xref

      ---------------------
      -- Append_To_Scope --
      ---------------------

      procedure Append_To_Scope (Scope : Entity_Id; E : Entity_Id) is
      begin
         File_Entities.All_Entities.Append (E);
         Entities_Map.Include (LL.Get_Location (E), E);
         Append_Entity (Scope, E);
         Set_Scope (E, Scope);
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
                  Set_Kind (Comp_E, E_Component);
                  Append_To_Scope (E, Comp_E);
               end loop;
            end;

            if Is_Tagged (E) then
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
                        Decorate_Subprogram (Meth_E);

                        Set_Scope (Meth_E, Get_Scope (E));
                        Entities_Map.Include
                          (LL.Get_Location (Meth_E), Meth_E);
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
            if Is_Record_Type (E) then
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
            end if;

         --  C or CPP

         else
            declare
               Kind : constant Entity_Kind := LL.Get_Ekind (Db, E);
            begin
               if Kind = E_Include_File then
                  return null;
               end if;
            end;
         end if;

         return Atree.New_Entity (Context, E, E_Loc);

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return null;
      end New_Entity;

      -------------------------
      -- Update_Scopes_Stack --
      -------------------------

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

               --  Push in the scopes stack the associated entity. Given
               --  that it was not found in the scopes scopes stack
               --  semantically this is not correct but we do our best!

               --  For incomplete types we push always the partial view.
               --  It is needed to handle discriminants defined in the
               --  full view.

               if Is_Incomplete_Or_Private_Type (New_E)
                 and then
                   LL.Get_Body_Loc (New_E).Line
                   < LL.Get_Location (New_E).Line
               then
                  declare
                     Partial_View : Entity_Id;

                  begin
                     Partial_View :=
                       Find_Entity
                         (Current_Scope, LL.Get_Body_Loc (New_E));
                     pragma Assert (Partial_View /= null);

                     --  Complete the decoration of these entities

                     Set_Partial_View (New_E, Partial_View);
                     Set_Full_View (Partial_View, New_E);

                     Enter_Scope (Partial_View);
                  end;

                  --  The entity of the full view of a private type is
                  --  not available in the list of entities provided by
                  --  Xref. We assume that this is the only case in which
                  --  this situation occurs but more work is needed in
                  --  this area???

               else
                  pragma Assert
                    (not Is_Incomplete_Or_Private_Type (New_E));

                  Enter_Scope (Get_Scope (New_E));
                  Set_Is_Private (New_E);
               end if;
            end if;
         end if;
      end Update_Scopes_Stack;

      --  Local variables

      Std_Entity   : constant Entity_Id :=
                       New_Internal_Entity
                         (Context => Context,
                          Name    => "Standard");
      --  This entity represents the outermost scope (ie. the standard scope).
      --  It is needed to associate some scope to generic formals of library
      --  level units.

      New_E                : Entity_Id;
      Skip_This_Entity     : Boolean := False;
      File_Entities_Cursor : Entities_In_File_Cursor;

      --  Start of processing for Build_Tree

   begin
      Set_Kind (Std_Entity, E_Package);
      Register_Std_Entity (Std_Entity);
      Enter_Scope (Std_Entity);

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
         New_E := New_Entity (File_Entities_Cursor.Get);

         if In_Ada_Lang
           and then Present (New_E)
           and then Present (LL.Get_Scope (New_E))
         then
            Update_Scopes_Stack (New_E);
         end if;

         Skip_This_Entity := False;

         if Present (New_E) then

            if not Kind_In (Get_Kind (New_E), E_Variable,
                                              E_Discriminant,
                                              E_Component)
              and then not LL.Is_Primitive (New_E)
            then
               pragma Assert
                 (not Entities_Map.Contains (LL.Get_Location (New_E)));

            --  Skip methods since they are entered in the tree as part of
            --  processing its class/tagged type

            elsif LL.Is_Primitive (New_E) then

               Skip_This_Entity := True;

            --  An E_Variable may be in fact a component of an incomplete
            --  or private type

            elsif In_Ada_Lang then
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

            elsif In_C_Lang then
               declare
                  Scope_Id   : constant General_Entity := LL.Get_Scope (New_E);
                  Map_Cursor : EInfo_Map.Cursor;
                  Prev_E     : Entity_Id;
                  use type EInfo_Map.Cursor;
               begin
                  if Present (Scope_Id) then
                     Map_Cursor :=
                       Entities_Map.Find
                         (Get_Location (Context.Database, Scope_Id));

                     --  Skip processing local variables

                     if Map_Cursor /= EInfo_Map.No_Element then
                        Prev_E := EInfo_Map.Element (Map_Cursor);

                        if LL.Is_Subprogram (Prev_E) then
                           Skip_This_Entity := True;
                        end if;

                        --  Handle fields of structs. Must use the Xref support
                        --  directly since we may have not seen yet the full
                        --  declaration of the struct.

                     elsif Context.Database.Is_Type (Scope_Id) then
                        declare
                           Kind : constant Entity_Kind :=
                             LL.Get_Ekind (Context.Database, Scope_Id);
                        begin
                           pragma Assert (Kind = E_Record_Type);
                           Skip_This_Entity := True;
                        end;
                     end if;
                  end if;
               end;
            end if;

            if not Skip_This_Entity then
               if In_C_Lang
                 and then Is_Record_Type (New_E)
               then
                  --  Handle named typedef structs since the compiler
                  --  generates two entites in the LI file with the
                  --  same name.

                  declare
                     Scope_Id : constant General_Entity :=
                       LL.Get_Scope (New_E);
                  begin
                     if LL.Is_Global (New_E)
                       and then Context.Database.Get_Name
                                  (LL.Get_Entity (New_E))
                                 = Context.Database.Get_Name (Scope_Id)
                     then
                        Free (New_E);
                        New_E :=
                          New_Entity
                            (Context => Context,
                             E       => Scope_Id,
                             Loc     => Get_Location
                                          (Context.Database, Scope_Id));
                     end if;
                  end;
               end if;

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

      return Std_Entity;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return null;
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
        (Context : access constant Docgen_Context;
         E : Entity_Id;
         S : String);
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
        (Context : access constant Docgen_Context;
         E : Entity_Id;
         S : String)
      is
         Comment : Structured_Comment := Get_Comment (E);
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
                           Current :=
                             Append_Tag
                               (Comment,
                                Tag       => To_Unbounded_String (Tag_Text),
                                Entity    => No_General_Entity,
                                Attribute => To_Unbounded_String (Text));
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
         Set_Comment (E, Comment);
      end Parse_Doc;

      ------------------------------
      -- Parse_Subprogram_Comment --
      ------------------------------

      procedure Parse_Subprogram_Comments (Subp : Entity_Id) is
         Cursor         : EInfo_List.Cursor;
         Has_Params     : Boolean;
         Param          : Entity_Id;
         Param_End_Line : Integer;
         use type Comment_Result;
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
            --  Fails: to be investigated???
            EInfo_List.Next (Cursor);

            if Get_Doc (Param) = No_Comment_Result
              or else In_C_Lang --  Unsupported feature yet???
            then
               declare
                  Comment : Structured_Comment := Get_Comment (Subp);
                  --  Avoid this extra copy using access types???

               begin
                  Append_Param_Tag
                    (Comment,
                     Entity => LL.Get_Entity (Param),
                     Param_Name =>
                       To_Unbounded_String
                         (Get_Name (Context.Database, LL.Get_Entity (Param))),
                     Text => Null_Unbounded_String);
                  Set_Comment (Subp, Comment);
               end;
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
                  --  Avoid this extra copy using access types???
                  declare
                     Comment : Structured_Comment := Get_Comment (Subp);

                  begin
                     Append_Param_Tag
                       (Comment,
                        Entity => LL.Get_Entity (Param),
                        Param_Name =>
                          To_Unbounded_String (Get_Short_Name (Param)),
                        --  To_Unbounded_String
                        --    (Get_Name (Db, LL.Get_Entity (Param))),
                        Text => Get_Doc (Param).Text);
                     Set_Comment (Subp, Comment);
                  end;
               else
                  --  Avoid this extra copy using access types???
                  declare
                     Comment : Structured_Comment := Get_Comment (Subp);

                  begin
                     Append_Param_Tag
                       (Comment,
                        Entity => LL.Get_Entity (Param),
                        Param_Name =>
                          To_Unbounded_String
                            (Get_Name
                                 (Context.Database,
                                  LL.Get_Entity (Param))),
                        Text => Null_Unbounded_String);
                     Set_Comment (Subp, Comment);
                  end;
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
         if LL.Is_Subprogram (Entity) then
            Parse_Subprogram_Comments (Entity);
            return Skip;

         elsif Get_Doc (Entity).Text /= Null_Unbounded_String then
            Set_Comment (Entity, New_Structured_Comment);
            Parse_Doc
              (Context, Entity, To_String (Get_Doc (Entity).Text));
            Set_Doc (Entity, No_Comment_Result);
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
      Tree.Tree_Root := Build_File_Tree (Context, File, Tree'Access);
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
