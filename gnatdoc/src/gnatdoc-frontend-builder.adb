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

with GNAT.HTable;
with Basic_Types;             use Basic_Types;
with GNATdoc.Utils;           use GNATdoc.Utils;
with Language.Ada;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with Xref.Docgen;             use Xref.Docgen;
with Xref;
with GNAT.IO;

package body GNATdoc.Frontend.Builder is
   Me : constant Trace_Handle := Create ("GNATdoc.1-Frontend-Builder");

   Workaround : constant Boolean := True;
   --  Variable used in assertions which identify workarounds temporarily added
   --  to this package. Setting this variable to False helps to know quickly if
   --  the workaround is still needed.

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

      procedure Append_Child_Type
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Append_Inherited_Method
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Append_Method
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Append_Parent_Type
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Append_Progenitor
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Append_To_List
        (List : access EInfo_List.Vector; E : Unique_Entity_Id);
      --  Append to List the entity E

      procedure Append_To_Enclosing_Scope
        (Scope : Unique_Entity_Id; E : Unique_Entity_Id);
      --  Append E to the list of entities of the enclosing scope of Scope

      procedure Append_To_Scope
        (Scope : Unique_Entity_Id; E : Entity_Id);
      procedure Append_To_Scope
        (Scope : Entity_Id; E : Unique_Entity_Id);
      procedure Append_To_Scope
        (Scope : Unique_Entity_Id; E : Unique_Entity_Id);
      --  Append E to the list of entities of Scope

      procedure Free (Entity : in out Unique_Entity_Id);
      --  If this is a new entity the remove it; otherwise no action is
      --  performed since there are references to it in the tree.

      function Get_End_Of_Syntax_Scope_Loc
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_Entity
        (Entity : Unique_Entity_Id) return Entity_Id;

      function Get_Full_View
        (Entity : Unique_Entity_Id) return Entity_Id;

      function Get_Kind
        (Entity : Unique_Entity_Id) return Entity_Kind;

      function Get_Language
        (Entity : Unique_Entity_Id) return Language_Access;

      function Get_LL_Alias
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_LL_Entity
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_LL_First_Private_Entity_Loc
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_LL_Location
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_LL_Scope
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_Parent
        (Entity : Unique_Entity_Id) return Entity_Id;

      function Get_Parent_Package
        (Entity : Unique_Entity_Id) return Entity_Id;

      function Get_Scope
        (Entity : Unique_Entity_Id) return Entity_Id;

      procedure Get_Unique_Entity
        (Entity  : out Unique_Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : General_Entity;
         Forced  : Boolean := False);
      --  Search for E in a hash table containing all the project entities.
      --  If found then return such entity; if not found then allocate a new
      --  unique entity for E. If Forced is True then the entity is searched
      --  and built even if it is defined in another file.

      function In_Ada_Language
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Access_Type
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Class_Or_Record_Type
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Compilation_Unit
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Concurrent_Type_Or_Object
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Container
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Decorated
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Full_View
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Global
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Incomplete_Or_Private_Type
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_New
        (Entity : Unique_Entity_Id) return Boolean;
      --  True if the tree entity was allocated when this unique Entity was
      --  built

      function Is_Generic
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Package
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Partial_View
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Primitive
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Subprogram
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Subprogram_Or_Entry
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Tagged
        (Entity : Unique_Entity_Id) return Boolean;

      function New_Internal_Entity
        (Context  : access constant Docgen_Context;
         Language : Language_Access;
         Name     : String) return Unique_Entity_Id;
      --  Allocate an internal entity. Used to build the standard entity.

      function New_Internal_Entity
        (Entity : Entity_Id) return Unique_Entity_Id;
      --  Allocate  a new internal entity. Used to build the entities
      --  associated with the full view of private and incomplete types.

      function Number_Of_Progenitors
        (Entity : Unique_Entity_Id) return Natural;

      function Present (Entity : Unique_Entity_Id) return Boolean;
      --  Return True if Entity /= No_Entity

      procedure Remove_Full_View (E : Unique_Entity_Id);
      --  Remove the full view of E and set Is_Incomplete to false

      procedure Remove_From_Scope (E : Unique_Entity_Id);
      --  Remove E from its current scope

      procedure Set_Alias
        (Entity : Unique_Entity_Id; Value : Entity_Id);

      procedure Set_In_Private_Part
        (Entity : Unique_Entity_Id);

      procedure Set_Is_Decorated
        (Entity : Unique_Entity_Id);

      procedure Set_Is_Generic_Formal
        (Entity : Unique_Entity_Id);

      procedure Set_Kind
        (Entity : Unique_Entity_Id; Value : Entity_Kind);

      procedure Set_Parent
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Set_Progenitor_As_Parent (Entity : Unique_Entity_Id);
      --  Set the unique progenitor of Entity as its parent

      procedure Set_Scope
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id);

      procedure Set_Scope
        (Entity : Unique_Entity_Id; Value : Entity_Id);

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

         procedure Append_To_Map (E : Entity_Id);
         --  Append the entity of E to Entities_Map

      private
         pragma Inline (Append_To_Map);
      end Hash_Table;
      use Hash_Table;

      ------------------------------------------
      --  Debugging routines (for use in gdb) --
      ------------------------------------------

      procedure upn (E : Unique_Entity_Id);
      --  (gdb) Prints a single tree node (full output), without printing
      --  descendants.

      procedure upnid (Unique_Id : Natural);
      --  (gdb) Search for and entity in the hash-table with Unique_Id and
      --  prints its contents. No output generated if the entity is not found.

   private
      type Unique_Entity_Info is record
         Entity : Entity_Id := Atree.No_Entity;
         Is_New : Boolean   := False;
      end record;

      pragma Inline (Append_Child_Type);
      pragma Inline (Append_Inherited_Method);
      pragma Inline (Append_Method);
      pragma Inline (Append_Parent_Type);
      pragma Inline (Append_Progenitor);
      pragma Inline (Append_To_Enclosing_Scope);
      pragma Inline (Append_To_List);
      pragma Inline (Append_To_Scope);
      pragma Inline (Get_Entity);
      pragma Inline (Get_Full_View);
      pragma Inline (Get_Kind);
      pragma Inline (Get_Language);
      pragma Inline (Get_LL_Alias);
      pragma Inline (Get_LL_Entity);
      pragma Inline (Get_LL_First_Private_Entity_Loc);
      pragma Inline (Get_LL_Location);
      pragma Inline (Get_LL_Scope);
      pragma Inline (Get_Parent);
      pragma Inline (Get_Parent_Package);
      pragma Inline (Get_Scope);
      pragma Inline (In_Ada_Language);
      pragma Inline (Is_Class_Or_Record_Type);
      pragma Inline (Is_Compilation_Unit);
      pragma Inline (Is_Concurrent_Type_Or_Object);
      pragma Inline (Is_Container);
      pragma Inline (Is_Decorated);
      pragma Inline (Is_Full_View);
      pragma Inline (Is_Global);
      pragma Inline (Is_Incomplete_Or_Private_Type);
      pragma Inline (Is_New);
      pragma Inline (Is_Package);
      pragma Inline (Is_Partial_View);
      pragma Inline (Is_Primitive);
      pragma Inline (Is_Subprogram);
      pragma Inline (Is_Subprogram_Or_Entry);
      pragma Inline (Is_Tagged);
      pragma Inline (Number_Of_Progenitors);
      pragma Inline (Present);
      pragma Inline (Set_Alias);
      pragma Inline (Set_In_Private_Part);
      pragma Inline (Set_Is_Decorated);
      pragma Inline (Set_Is_Generic_Formal);
      pragma Inline (Set_Kind);
      pragma Inline (Set_Parent);
      pragma Inline (Set_Scope);

      pragma Export (Ada, upn);
      pragma Export (Ada, upnid);
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
            Append_To_Map (Get_Entity (E));
         end Append_To_Map;

         procedure Append_To_Map (E : Entity_Id) is
         begin
            pragma Assert (Present (E));
            Entities_Map.Include (LL.Get_Location (E), E);
         end Append_To_Map;

         ---------------------
         -- Equivalent_Keys --
         ---------------------

         function Equivalent_Keys
           (Left, Right : General_Location) return Boolean
         is
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

      -----------------------
      -- Append_Child_Type --
      -----------------------

      procedure Append_Child_Type
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         LL.Append_Child_Type (Get_Entity (Entity), Get_Entity (Value));
      end Append_Child_Type;

      -----------------------------
      -- Append_Inherited_Method --
      -----------------------------

      procedure Append_Inherited_Method
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         Append_Inherited_Method (Get_Entity (Entity), Get_Entity (Value));
      end Append_Inherited_Method;

      -------------------
      -- Append_Method --
      -------------------

      procedure Append_Method
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         Append_Method (Get_Entity (Entity), Get_Entity (Value));
      end Append_Method;

      ------------------------
      -- Append_Parent_Type --
      ------------------------

      procedure Append_Parent_Type
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         LL.Append_Parent_Type (Get_Entity (Entity), Get_Entity (Value));
      end Append_Parent_Type;

      -----------------------
      -- Append_Progenitor --
      -----------------------

      procedure Append_Progenitor
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         Append_Progenitor (Get_Entity (Entity), Get_Entity (Value));
      end Append_Progenitor;

      -------------------------------
      -- Append_To_Enclosing_Scope --
      -------------------------------

      procedure Append_To_Enclosing_Scope
        (Scope : Unique_Entity_Id; E : Unique_Entity_Id)
      is
         Enclosing_Scope : constant Entity_Id :=
                             Get_Scope (Get_Entity (Scope));
      begin
         if not Get_Entities (Enclosing_Scope).Contains (Get_Entity (E)) then
            Append_To_Scope (Enclosing_Scope, Get_Entity (E));
            Set_Scope (Get_Entity (E), Enclosing_Scope);
         end if;
      end Append_To_Enclosing_Scope;

      ------------
      -- Append --
      ------------

      procedure Append_To_List
        (List : access EInfo_List.Vector;
         E    : Unique_Entity_Id) is
      begin
         pragma Assert (not List.Contains (Get_Entity (E)));
         List.Append (Get_Entity (E));
      end Append_To_List;

      ---------------------
      -- Append_To_Scope --
      ---------------------

      procedure Append_To_Scope
        (Scope : Unique_Entity_Id; E : Unique_Entity_Id) is
      begin
         Append_To_Scope (Scope, Get_Entity (E));
      end Append_To_Scope;

      procedure Append_To_Scope
        (Scope : Entity_Id; E : Unique_Entity_Id) is
      begin
         Append_To_Scope (Scope, Get_Entity (E));
         Set_Scope (Get_Entity (E), Scope);
      end Append_To_Scope;

      procedure Append_To_Scope
        (Scope : Unique_Entity_Id; E : Entity_Id) is
      begin
         Append_To_Scope (Get_Entity (Scope), E);
         Set_Scope (E, Get_Entity (Scope));
      end Append_To_Scope;

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

      ---------------------------------
      -- Get_End_Of_Syntax_Scope_Loc --
      ---------------------------------

      function Get_End_Of_Syntax_Scope_Loc
        (Entity : Unique_Entity_Id) return General_Location is
      begin
         return Get_End_Of_Syntax_Scope_Loc (Get_Entity (Entity));
      end Get_End_Of_Syntax_Scope_Loc;

      ----------------
      -- Get_Entity --
      ----------------

      function Get_Entity (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         pragma Assert (Present (Entity));
         return Entity.Entity;
      end Get_Entity;

      -------------------
      -- Get_Full_View --
      -------------------

      function Get_Full_View (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         pragma Assert (Present (Entity));
         return Get_Full_View (Entity.Entity);
      end Get_Full_View;

      --------------
      -- Get_Kind --
      --------------

      function Get_Kind (Entity : Unique_Entity_Id) return Entity_Kind is
      begin
         return Get_Kind (Get_Entity (Entity));
      end Get_Kind;

      ------------------
      -- Get_Language --
      ------------------

      function Get_Language
        (Entity : Unique_Entity_Id) return Language_Access is
      begin
         return Get_Language (Get_Entity (Entity));
      end Get_Language;

      ------------------
      -- Get_LL_Alias --
      ------------------

      function Get_LL_Alias
        (Entity : Unique_Entity_Id) return General_Entity is
      begin
         return LL.Get_Alias (Get_Entity (Entity));
      end Get_LL_Alias;

      -------------------
      -- Get_LL_Entity --
      -------------------

      function Get_LL_Entity (Entity : Unique_Entity_Id) return General_Entity
      is
      begin
         return LL.Get_Entity (Get_Entity (Entity));
      end Get_LL_Entity;

      -------------------------------------
      -- Get_LL_First_Private_Entity_Loc --
      -------------------------------------

      function Get_LL_First_Private_Entity_Loc
        (Entity : Unique_Entity_Id) return General_Location is
      begin
         return LL.Get_First_Private_Entity_Loc (Get_Entity (Entity));
      end Get_LL_First_Private_Entity_Loc;

      ---------------------
      -- Get_LL_Location --
      ---------------------

      function Get_LL_Location
        (Entity : Unique_Entity_Id) return General_Location is
      begin
         return LL.Get_Location (Get_Entity (Entity));
      end Get_LL_Location;

      ------------------
      -- Get_LL_Scope --
      ------------------

      function Get_LL_Scope (Entity : Unique_Entity_Id) return General_Entity
      is
      begin
         return LL.Get_Scope (Get_Entity (Entity));
      end Get_LL_Scope;

      ----------------
      -- Get_Parent --
      ----------------

      function Get_Parent
        (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         return Get_Parent (Get_Entity (Entity));
      end Get_Parent;

      ------------------------
      -- Get_Parent_Package --
      ------------------------

      function Get_Parent_Package
        (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         return Get_Parent_Package (Get_Entity (Entity));
      end Get_Parent_Package;

      ---------------
      -- Get_Scope --
      ---------------

      function Get_Scope
        (Entity : Unique_Entity_Id) return Entity_Id is
      begin
         return Get_Scope (Get_Entity (Entity));
      end Get_Scope;

      ---------------------
      -- In_Ada_Language --
      ---------------------

      function In_Ada_Language (Entity : Unique_Entity_Id) return Boolean is
      begin
         return In_Ada_Language (Get_Entity (Entity));
      end In_Ada_Language;

      -------------
      -- Is_Type --
      -------------

      function Is_Access_Type (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Type (Get_Entity (Entity))
           and then LL.Is_Access (Get_Entity (Entity));
      end Is_Access_Type;

      -----------------------------
      -- Is_Class_Or_Record_Type --
      -----------------------------

      function Is_Class_Or_Record_Type
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Class_Or_Record_Type (Get_Entity (Entity));
      end Is_Class_Or_Record_Type;

      -------------------------
      -- Is_Compilation_Unit --
      -------------------------

      function Is_Compilation_Unit
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return
           (Is_Package (Entity) or else Is_Subprogram (Entity))
             and then
           (Is_Global (Entity) or else Present (Get_Parent_Package (Entity)));
      end Is_Compilation_Unit;

      ----------------------------------
      -- Is_Concurrent_Type_Or_Object --
      ----------------------------------

      function Is_Concurrent_Type_Or_Object
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Concurrent_Type_Or_Object (Get_Entity (Entity));
      end Is_Concurrent_Type_Or_Object;

      ---------------
      -- Is_Global --
      ---------------

      function Is_Global (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Global (Get_Entity (Entity));
      end Is_Global;

      ------------------
      -- Is_Container --
      ------------------

      function Is_Container (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Container (Get_Entity (Entity));
      end Is_Container;

      ------------------
      -- Is_Decorated --
      ------------------

      function Is_Decorated
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Decorated (Get_Entity (Entity));
      end Is_Decorated;

      ------------------
      -- Is_Full_View --
      ------------------

      function Is_Full_View
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Full_View (Get_Entity (Entity));
      end Is_Full_View;

      ----------------
      -- Is_Generic --
      ----------------

      function Is_Generic
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Generic (Get_Entity (Entity));
      end Is_Generic;

      -----------------------------------
      -- Is_Incomplete_Or_Private_Type --
      -----------------------------------

      function Is_Incomplete_Or_Private_Type
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Incomplete (Get_Entity (Entity))
           or else Is_Private (Get_Entity (Entity));
      end Is_Incomplete_Or_Private_Type;

      ------------
      -- Is_New --
      ------------

      function Is_New (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Entity.Is_New;
      end Is_New;

      ----------------
      -- Is_Package --
      ----------------

      function Is_Package
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Package (Get_Entity (Entity));
      end Is_Package;

      ---------------------
      -- Is_Partial_View --
      ---------------------

      function Is_Partial_View
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Partial_View (Get_Entity (Entity));
      end Is_Partial_View;

      ------------------
      -- Is_Primitive --
      ------------------

      function Is_Primitive (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Primitive (Get_Entity (Entity));
      end Is_Primitive;

      -------------------
      -- Is_Subprogram --
      -------------------

      function Is_Subprogram
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Subprogram (Get_Entity (Entity));
      end Is_Subprogram;

      ----------------------------
      -- Is_Subprogram_Or_Entry --
      ----------------------------

      function Is_Subprogram_Or_Entry
        (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Subprogram_Or_Entry (Get_Entity (Entity));
      end Is_Subprogram_Or_Entry;

      ---------------
      -- Is_Tagged --
      ---------------

      function Is_Tagged (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Tagged (Get_Entity (Entity));
      end Is_Tagged;

      -----------------------
      -- Get_Unique_Entity --
      -----------------------

      procedure Get_Unique_Entity
        (Entity  : out Unique_Entity_Id;
         Context : access constant Docgen_Context;
         File    : Virtual_File;
         E       : General_Entity;
         Forced  : Boolean := False)

      is
         Db    : General_Xref_Database renames Context.Database;
         E_Loc : constant General_Location := Get_Location (Db, E);

         Lang        : constant Language_Access :=
                         Get_Language_From_File (Context.Lang_Handler, File);
         In_Ada_Lang : constant Boolean :=
                         Lang.all in Language.Ada.Ada_Language'Class;

         function Build_New_Entity return Unique_Entity_Id;
         --  Local routine which factorizes code used to allocate a new
         --  entity

         function Full_View_Needed (New_E : Entity_Id) return Boolean;
         --  Evaluate if New_E requires a full view

         function Build_New_Entity return Unique_Entity_Id is
            New_E : Entity_Id := New_Entity (Context, Lang, E, E_Loc);
         begin
            if No (New_E) then
               return No_Entity;
            else
               if Is_Package (New_E)
                 and then Present (LL.Get_Parent_Package (New_E))
               then
                  Set_Parent_Package (New_E,
                    Get_Unique_Entity
                      (Context, File, LL.Get_Parent_Package (New_E)));
               end if;

               if Full_View_Needed (New_E) then
                  declare
                     Partial_View  : constant Entity_Id := New_E;

                     Full_View_Loc : constant General_Location :=
                                       LL.Get_Body_Loc (Partial_View);

                     Full_View     : Entity_Id :=
                                       Find_Unique_Entity (Full_View_Loc);

                  begin
                     if No (Full_View) then
                        Full_View :=
                          New_Entity (Context, Lang, E, Full_View_Loc);
                     end if;

                     --  Xref does not help us to differentiate if New_E is a
                     --  private type, an incomplete declaration, or a formal
                     --  of a subprogram (because in this latter case Body_Loc
                     --  references the same formal in the subprogram body).
                     --  For this reasons at this stage the best action we can
                     --  do is to decorate the entity as "incomplete". At later
                     --  stages the entity will be fully decorated (and this
                     --  value will be reset in subprogram formals).

                     Set_Is_Incomplete (New_E);

                     --  For private types the entity associated with the full
                     --  view is not available available in Xref since the
                     --  compiler does not generate it; by contrast, for
                     --  incomplete types the compiler generates two entities.

                     Set_Full_View (New_E, Full_View);

                     --  Link the full view with its partial view

                     Set_Partial_View (Get_Full_View (New_E), New_E);

                     --  Adding a minimum high level decoration to the full
                     --  view. For incomplete types this value may be updated
                     --  later (when the type declaration is processed).

                     Set_Kind (Get_Full_View (New_E), Get_Kind (New_E));
                  end;

               --  The Xref service Child_Types returns direct references
               --  to the full view of the child types. For homoneneity in
               --  creation of entities we build here the partial view and
               --  full view and we return the reference to the partial view.

               elsif Present (LL.Get_Body_Loc (New_E))
                 and then LL.Is_Type (New_E)
                 and then LL.Get_Location (New_E).File
                            = LL.Get_Body_Loc (New_E).File
                 and then LL.Get_Location (New_E).Line
                            > LL.Get_Body_Loc (New_E).Line
               then
                  declare
                     Full_View        : constant Entity_Id := New_E;

                     Partial_View_Loc : constant General_Location :=
                                          LL.Get_Body_Loc (New_E);
                     Partial_View     : Entity_Id :=
                                          Find_Unique_Entity
                                            (Partial_View_Loc);
                  begin
                     if No (Partial_View) then
                        Partial_View :=
                          New_Entity (Context, Lang, E, Partial_View_Loc);
                     end if;

                     Set_Is_Incomplete (Partial_View);
                     Set_Full_View (Partial_View, Full_View);
                     Set_Partial_View (Full_View, Partial_View);

                     New_E := Partial_View;
                  end;
               end if;

               return
                 new Unique_Entity_Info'(Entity => New_E,
                                         Is_New => True);
            end if;
         end Build_New_Entity;

         ----------------------
         -- Full_View_Needed --
         ----------------------

         function Full_View_Needed (New_E : Entity_Id) return Boolean is
         begin
            if No (LL.Get_Body_Loc (New_E)) then
               return False;

            elsif Is_Concurrent_Type_Or_Object (New_E) then
               return
                 LL.Get_Location (New_E).File = LL.Get_Body_Loc (New_E).File
                 and then
                   LL.Get_Location (New_E).Line < LL.Get_Body_Loc (New_E).Line;

            elsif LL.Is_Type (New_E)
              or else Get_Kind (New_E) = E_Variable
            then
               return
                 LL.Get_Location (New_E).File /= LL.Get_Body_Loc (New_E).File
                 or else
                   LL.Get_Location (New_E).Line < LL.Get_Body_Loc (New_E).Line;

            else
               return False;
            end if;
         end Full_View_Needed;

         --  Local variables

         Is_Prim : constant Boolean := Is_Primitive_Of (Db, E)'Length /= 0;
         --  Avoid calling twice this service???

      begin
         Entity := null;

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
               Prev_E : constant Entity_Id := Find_Unique_Entity (E_Loc);

            begin
               if Present (Prev_E) then
                  Entity :=
                    new Unique_Entity_Info'(Entity  => Prev_E,
                                            Is_New  => False);
               else
                  Entity := Build_New_Entity;
               end if;

               return;
            end;

         --  C / C++

         else
            if E_Loc.File /= File
              and then not Forced
            then
               return;
            end if;

            declare
               Kind   : constant Entity_Kind :=
                          LL.Get_Ekind (Db, E, In_Ada_Lang => False);
               Prev_E : Entity_Id;

            begin
               if Kind = E_Include_File
                 or else Kind = E_Unknown
               then
                  return;
               end if;

               --  Before creating the entity we search for it in the hash
               --  table (to avoid duplicating it!)

               Prev_E := Find_Unique_Entity (E_Loc);

               if Present (Prev_E) then
                  Entity :=
                    new Unique_Entity_Info'(Entity => Prev_E,
                                            Is_New => False);
               else
                  Entity := Build_New_Entity;

                  if Present (Entity)
                    and then
                      LL.Get_Location (Get_Entity (Entity)).File /= File
                  then
                     Set_Ref_File (Get_Entity (Entity), File);
                  end if;
               end if;

               return;
            end;
         end if;

      exception
         when E : others =>
            Trace (Me, E);
            raise;
      end Get_Unique_Entity;

      -------------------------
      -- New_Internal_Entity --
      -------------------------

      function New_Internal_Entity
        (Entity : Entity_Id) return Unique_Entity_Id
      is
      begin
         return
           new Unique_Entity_Info'
                 (Entity => Entity,
                  Is_New => True);
      end New_Internal_Entity;

      function New_Internal_Entity
        (Context  : access constant Docgen_Context;
         Language : Language_Access;
         Name     : String) return Unique_Entity_Id
      is
      begin
         return
            new Unique_Entity_Info'
                  (Entity => New_Internal_Entity
                               (Context  => Context,
                                Language => Language,
                                Name     => Name),
                   Is_New => True);
      end New_Internal_Entity;

      ---------------------------
      -- Number_Of_Progenitors --
      ---------------------------

      function Number_Of_Progenitors
        (Entity : Unique_Entity_Id) return Natural is
      begin
         return Natural (Get_Progenitors (Get_Entity (Entity)).all.Length);
      end Number_Of_Progenitors;

      -------------
      -- Present --
      -------------

      function Present (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Entity /= No_Entity
           and then Present (Entity.Entity);
      end Present;

      ----------------------
      -- Remove_Full_View --
      ----------------------

      procedure Remove_Full_View (E : Unique_Entity_Id) is
      begin
         Set_Is_Incomplete (Get_Entity (E), False);
         Remove_Full_View (Get_Entity (E));
      end Remove_Full_View;

      -----------------------
      -- Remove_From_Scope --
      -----------------------

      procedure Remove_From_Scope (E : Unique_Entity_Id) is
      begin
         Remove_From_Scope (Get_Entity (E));
         Set_Scope (Get_Entity (E), Atree.No_Entity);
      end Remove_From_Scope;

      ---------------
      -- Set_Alias --
      ---------------

      procedure Set_Alias
        (Entity : Unique_Entity_Id; Value : Entity_Id) is
      begin
         Set_Alias (Get_Entity (Entity), Value);
      end Set_Alias;

      -------------------------
      -- Set_In_Private_Part --
      -------------------------

      procedure Set_In_Private_Part
        (Entity : Unique_Entity_Id) is
      begin
         Set_In_Private_Part (Get_Entity (Entity));
      end Set_In_Private_Part;

      ----------------------
      -- Set_Is_Decorated --
      ----------------------

      procedure Set_Is_Decorated
        (Entity : Unique_Entity_Id) is
      begin
         Set_Is_Decorated (Get_Entity (Entity));
      end Set_Is_Decorated;

      ---------------------------
      -- Set_Is_Generic_Formal --
      ---------------------------

      procedure Set_Is_Generic_Formal
        (Entity : Unique_Entity_Id) is
      begin
         Set_Is_Generic_Formal (Get_Entity (Entity));
      end Set_Is_Generic_Formal;

      --------------
      -- Set_Kind --
      --------------

      procedure Set_Kind
        (Entity : Unique_Entity_Id; Value : Entity_Kind) is
      begin
         Set_Kind (Get_Entity (Entity), Value);
      end Set_Kind;

      ----------------
      -- Set_Parent --
      ----------------

      procedure Set_Parent
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         Set_Parent (Get_Entity (Entity), Get_Entity (Value));
      end Set_Parent;

      ------------------------------
      -- Set_Progenitor_As_Parent --
      ------------------------------

      procedure Set_Progenitor_As_Parent (Entity : Unique_Entity_Id) is
      begin
         Set_Parent (Get_Entity (Entity),
           Get_Progenitors (Get_Entity (Entity)).First_Element);
         Get_Progenitors (Get_Entity (Entity)).Delete_First;
      end Set_Progenitor_As_Parent;

      ---------------
      -- Set_Scope --
      ---------------

      procedure Set_Scope
        (Entity : Unique_Entity_Id; Value : Unique_Entity_Id) is
      begin
         Set_Scope (Get_Entity (Entity), Get_Entity (Value));
      end Set_Scope;

      ---------------
      -- Set_Scope --
      ---------------

      procedure Set_Scope
        (Entity : Unique_Entity_Id; Value : Entity_Id) is
      begin
         Set_Scope (Get_Entity (Entity), Value);
      end Set_Scope;

      ---------------------------------------------------------------
      --                   Debugging routines                      --
      ---------------------------------------------------------------

      --------
      -- pn --
      --------

      procedure upn (E : Unique_Entity_Id) is
      begin
         if Present (Get_Entity (E)) then
            Atree.pn (Get_Entity (E));
            GNAT.IO.Put_Line ("Is_New : " & E.Is_New'Img);
         end if;
      end upn;

      ----------
      -- pnid --
      ----------

      procedure upnid (Unique_Id : Natural) is
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
      end upnid;

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

      function Enclosing_Generic_Scope return Unique_Entity_Id;
      --  Return the innermost generic scope (or No_Entity if we are not in
      --  a generic scope)

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
      --  Return true if some enclosing scope is generic

      procedure Register_Std_Entity (E : Unique_Entity_Id);
      --  Register in the package the entity used to represent the standard
      --  entity. Needed internally to identify the outermost scope.

   end Scopes_Stack;

   ---------------------
   -- Build_File_Tree --
   ---------------------

   function Build_File_Tree
     (Context       : access constant Docgen_Context;
      File          : Virtual_File;
      File_Entities : access Tree_Type) return Entity_Id
   is
      Lang          : constant Language_Access :=
                        Get_Language_From_File (Context.Lang_Handler, File);
      In_Ada_Lang   : constant Boolean :=
                        Lang.all in Language.Ada.Ada_Language'Class;
      Std_Entity    : constant Unique_Entity_Id :=
                        New_Internal_Entity
                          (Context  => Context,
                           Language => Lang,
                           Name     => Std_Entity_Name);
      use Scopes_Stack;

      procedure Append_To_File_Entities (E : Unique_Entity_Id);
      --  Append E to File_Entities.All_Entities

      procedure Complete_Decoration (E : Unique_Entity_Id);
      --  Complete the decoration of entity E

      procedure Set_Scope (New_E : Unique_Entity_Id);
      --  Set the scope of the entity using the current scope (if required)

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id);
      --  Update the scopes stack using the reliable value provided by Xref
      --  (if available)

      -----------------------------
      -- Append_To_File_Entities --
      -----------------------------

      procedure Append_To_File_Entities (E : Unique_Entity_Id) is
      begin
         if not File_Entities.All_Entities.Contains (Get_Entity (E)) then
            Append_To_List (File_Entities.All_Entities'Access, E);
         end if;
      end Append_To_File_Entities;

      -------------------------
      -- Complete_Decoration --
      -------------------------

      procedure Complete_Decoration (E : Unique_Entity_Id) is

         procedure Decorate_Generic_Formals (E : Unique_Entity_Id);
         --  Complete the decoration of a subprogram entity

         procedure Decorate_Record_Type (E : Unique_Entity_Id);
         --  Complete the decoration of a record type entity

         procedure Decorate_Subprogram_Formals (E : Unique_Entity_Id);
         --  Complete the decoration of a subprogram entity

         ------------------------------
         -- Decorate_Generic_Formals --
         ------------------------------

         procedure Decorate_Generic_Formals (E : Unique_Entity_Id) is
            Formals : constant Xref.Entity_Array :=
                       Formal_Parameters (Context.Database, Get_LL_Entity (E));
            Formal  : Unique_Entity_Id;

         begin
            Enter_Scope (E);

            for J in Formals'Range loop
               Get_Unique_Entity
                 (Formal, Context, File, Formals (J),
                  Forced => True);

               --  Generic formals have been already (erroneously) processed
               --  because we could not identify them as formals. Now we
               --  move them to their correct scope and complete their
               --  decoration.

               pragma Assert (not Is_New (Formal));
               Remove_From_Scope (Formal);

               Set_Is_Generic_Formal (Formal);
               Set_Scope (Formal, E);

               --  Adding minimum decoration to undecorated generic formals

               if Get_Kind (Formal) = E_Unknown then
                  Set_Kind (Formal, E_Generic_Formal);
               end if;

               Append_To_Scope (Current_Scope, Formal);

               Set_Is_Decorated (Formal);
            end loop;

            Exit_Scope;
         end Decorate_Generic_Formals;

         --------------------------
         -- Decorate_Record_Type --
         --------------------------

         procedure Decorate_Record_Type (E : Unique_Entity_Id) is

            procedure Append_Parent_And_Progenitors
              (Parents  : Xref.Entity_Array);

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : General_Entity) return Boolean;
            --  Return true if primitive Prim of tagged type Typ has been
            --  inherited from some parent or progenitor type

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
                  Append_Parent_Type (E, Parent);

                  --  The list of parents returned by Xref is not ordered and
                  --  hence it does not help to differentiate the parent type
                  --  from the progenitors.

                  if In_Ada_Lang then
                     if Get_Kind (Parent) /= E_Interface then
                        Set_Parent (E, Parent);
                     else
                        Append_Progenitor (E, Parent);
                     end if;
                  end if;

                  if Is_New (Parent) then
                     Append_To_Map (Parent);

                     if Present (Get_Full_View (Parent)) then
                        Append_To_Map (Get_Full_View (Parent));
                     end if;
                  end if;
               end loop;
            end Append_Parent_And_Progenitors;

            ----------------------------
            -- Is_Inherited_Primitive --
            ----------------------------

            function Is_Inherited_Primitive
              (Typ  : Entity_Id;
               Prim : General_Entity) return Boolean
            is
               function Check_Primitives
                 (Typ : Entity_Id) return Boolean;
               --  Return true if Prim is a primitive of Typ

               function Check_Progenitors
                 (Typ : Entity_Id) return Boolean;
               --  Return True if Prim is a primitive defined in some
               --  progenitor of Typ

               ----------------------
               -- Check_Primitives --
               ----------------------

               function Check_Primitives
                 (Typ : Entity_Id) return Boolean
               is
                  Cursor : EInfo_List.Cursor;
                  E      : Entity_Id;

               begin
                  Cursor := Get_Methods (Typ).First;
                  while EInfo_List.Has_Element (Cursor) loop
                     E := EInfo_List.Element (Cursor);

                     if LL.Get_Entity (E) = Prim then
                        return True;
                     end if;

                     EInfo_List.Next (Cursor);
                  end loop;

                  return False;
               end Check_Primitives;

               -----------------------
               -- Check_Progenitors --
               -----------------------

               function Check_Progenitors
                 (Typ : Entity_Id) return Boolean
               is
                  Cursor     : EInfo_List.Cursor;
                  Progenitor : Entity_Id;

               begin
                  Cursor := Get_Progenitors (Typ).First;
                  while EInfo_List.Has_Element (Cursor) loop
                     Progenitor := EInfo_List.Element (Cursor);

                     if Check_Primitives (Progenitor) then
                        return True;
                     end if;

                     EInfo_List.Next (Cursor);
                  end loop;

                  return False;
               end Check_Progenitors;

            begin
               if Get_Kind (Typ) = E_Interface then
                  return Check_Progenitors (Typ);

               else
                  declare
                     Parent : Entity_Id;
                  begin
                     Parent := Get_Parent (Typ);
                     while Present (Parent) loop
                        if Check_Primitives (Parent) then
                           return True;
                        end if;

                        Parent := Get_Parent (Parent);
                     end loop;

                     return False;
                  end;
               end if;
            end Is_Inherited_Primitive;

         --  Start of processing for Decorate_Record_Type

         begin
            if In_Ada_Lang and then Get_Kind (E) /= E_Interface then
               declare
                  Discrim : constant Xref.Entity_Array :=
                              Discriminants
                                (Context.Database, Get_LL_Entity (E));
                  Entity  : Unique_Entity_Id;
               begin
                  for J in Discrim'Range loop
                     Get_Unique_Entity (Entity, Context, File, Discrim (J));

                     --  If the entity is not available that means that
                     --  this is an incomplete type whose discriminants
                     --  are defined in the package body.

                     if Entity = null then
                        null;

                     elsif Is_New (Entity) then
                        --  For incomplete types whose discriminant is
                        --  repeated in the partial and full-view the
                        --  compiler generates two entities and we must
                        --  handle just one.

                        if not
                          Get_Entities (Get_Entity (E)).Contains
                            (Get_Entity (Entity))
                        then
                           Set_Kind (Entity, E_Discriminant);
                           Append_To_Map (Entity);
                           Set_Is_Decorated (Entity);

                           pragma Assert (not Is_Full_View (Entity));

                           --  For partial and incomplete types in the ALI file
                           --  in their full view we don't have available their
                           --  discriminants.

                           if Is_Partial_View (Entity) then
                              Append_To_Scope (E, Entity);

                              --  If the discriminant is visible in the partial
                              --  and full view of E then append its full view
                              --  to the full view of E and we remove it from
                              --  the scope of E

                              if Is_Partial_View (E) then
                                 Set_Kind
                                   (Get_Full_View (Entity), E_Discriminant);
                                 Get_Entities
                                   (Get_Full_View (E)).Append
                                     (Get_Full_View (Entity));
                                 Append_To_Scope (E, Get_Full_View (Entity));
                                 Append_To_Map (Get_Full_View (Entity));

                                 Set_Is_Decorated (Get_Full_View (Entity));
                              end if;

                              Remove_Full_View (Entity);

                           --  Unknown discriminant of a private or limited
                           --  type

                           elsif Is_Partial_View (E) then
                              Set_Has_Unknown_Discriminants (Get_Entity (E));
                              Append_To_Scope (Get_Full_View (E), Entity);

                           else
                              Append_To_Scope (E, Entity);
                           end if;
                        end if;

                     --  For incomplete types Xref provides all the
                     --  discriminants to the incomplete view and the full
                     --  view and hence they are already decorated. This
                     --  Xref behavior involves that more work is needed
                     --  in the frontend to have a more precise decoration
                     --  of the incomplete and full view???

                     else
                        pragma Assert (Is_Full_View (E));
                        pragma Assert (Is_Decorated (Entity));
                        pragma Assert (Get_Kind (Entity) = E_Discriminant);
                        Append_To_Scope (E, Entity);
                     end if;
                  end loop;
               end;
            end if;

            --  Check_Record_Components

            if Get_Kind (E) /= E_Interface then
               declare
                  Components : constant Xref.Entity_Array :=
                                 Fields (Context.Database, Get_LL_Entity (E));
                  Entity     : Unique_Entity_Id;

               begin
                  --  For now skip processing the components of an incomplete
                  --  type declaration whose full view is defined in the
                  --  package body since Xref provides weird components.
                  --  Seems a bug in Xref but more investigation needed???

                  if LL.Is_Type (Get_Entity (E))
                    and then Is_Partial_View (E)
                    and then
                      LL.Get_Location (Get_Full_View (Get_Entity (E))).File
                        /= LL.Get_Location (Get_Entity (E)).File
                  then
                     null;
                  else
                     for J in Components'Range loop
                        Get_Unique_Entity
                          (Entity, Context, File, Components (J));

                        --  If the entity is not available that means that
                        --  this is an incomplete type whose components are
                        --  defined in the package body.

                        if Entity = null then
                           null;

                        --  Workaround Xref problem: for incomplete types with
                        --  discriminants in the partial and full view, Xref
                        --  returns discriminants in the list of fields.

                        elsif Get_Kind (Entity) = E_Discriminant then
                           null;

                        elsif Is_New (Entity) then

                           --  In C++ we have here formals of primitives???
                           Set_Kind (Entity, E_Component);

                           if Is_Partial_View (E) then
                              Append_To_Scope
                                (Get_Full_View (Get_Entity (E)), Entity);

                           else
                              Append_To_Scope (E, Entity);
                           end if;

                           Append_To_File_Entities (Entity);
                           Append_To_Map (Entity);

                           Set_Is_Decorated (Entity);

                           --  For incomplete types Xref provides all the
                           --  discriminants to the incomplete view and the
                           --  full view and hence they are already decorated.
                           --  This Xref behavior involves that more work is
                           --  needed in the frontend to have a more precise
                           --  decoration of the incomplete and full view???

                        else
                           pragma Assert (Is_Decorated (Entity));
                           pragma Assert (Get_Kind (Entity) = E_Component);
                           Append_To_Scope (E, Entity);
                        end if;
                     end loop;
                  end if;
               end;

               pragma Assert
                 (not Has_Duplicated_Entities
                        (Get_Entities (Get_Entity (E)).all));
               if Is_Partial_View (E) then
                  pragma Assert
                    (not Has_Duplicated_Entities
                          (Get_Entities (Get_Full_View (Get_Entity (E))).all));
               end if;
            end if;

            Append_Parent_And_Progenitors
              (Xref.Parent_Types
                 (Self      => Context.Database,
                  Entity    => Get_LL_Entity (E),
                  Recursive => False));

            if In_Ada_Language (E) then
               if No (Get_Parent (E))
                 and then Number_Of_Progenitors (E) = 1
               then
                  Set_Progenitor_As_Parent (E);
               end if;
            end if;

            declare
               Childs : constant Xref.Entity_Array :=
                          Child_Types
                           (Context.Database, Get_LL_Entity (E),
                            Recursive => False);
               Child  : Unique_Entity_Id;
               Loc    : General_Location;

            begin
               for J in Childs'Range loop

                  --  Do not add as a child type the second entity generated
                  --  by the compiler for named typedef structs (the compiler
                  --  generates two entites in the LI file with the same name)

                  if In_Ada_Language (E)
                    or else not
                      LL.Is_Self_Referenced_Type
                        (Db   => Context.Database,
                         E    => Childs (J),
                         Lang => Get_Language (E))
                  then
                     Get_Unique_Entity
                       (Child, Context, File, Childs (J), Forced => True);

                     Loc := LL.Get_Location (Get_Entity (Child));

                     --  Avoid problems with wrong Xref decoration that I can
                     --  reproduce with gnatcoll-refcount-weakref.ads. To
                     --  be investigated???

                     if not Is_Class_Or_Record_Type (Child) then
                        Free (Child);

                     --  Avoid adding to the tree entities defined in package
                     --  bodies since they cannot be fully decorated and
                     --  hence cause problems to the backend (for example,
                     --  we cannot set their scope!)

                     elsif not Is_Spec_File (Context.Kernel, Loc.File) then
                        Free (Child);

                     else
                        Append_Child_Type (E, Child);

                        if Is_New (Child) then
                           Append_To_Map (Child);

                           if Present (Get_Full_View (Child)) then
                              Append_To_Map (Get_Full_View (Child));
                           end if;
                        end if;
                     end if;
                  end if;
               end loop;
            end;

            if Is_Tagged (E)
              or else Get_Kind (E) = E_Class
            then
               declare
                  All_Methods : constant Xref.Entity_Array :=
                                  Methods
                                    (Context.Database, Get_LL_Entity (E),
                                     Include_Inherited => True);

                  Method : Unique_Entity_Id;
               begin
                  for J in All_Methods'Range loop
                     Get_Unique_Entity
                       (Method, Context, File, All_Methods (J),
                        Forced => True);

                     if Is_Inherited_Primitive
                         (Get_Entity (E), All_Methods (J))
                     then
                        if not Is_Partial_View (E) then
                           Append_Inherited_Method (E, Method);
                        else
                           Append_Inherited_Method
                             (Get_Full_View (Get_Entity (E)),
                              Get_Entity (Method));
                        end if;

                     elsif In_Ada_Language (Method) then

                        --  If Xref does not have available the scope of this
                        --  method it means that it is a primitive defined in
                        --  a file which is not directly part of this project
                        --  (that is, an entity defined in the runtime of the
                        --  compiler or in a library). In such case we assume
                        --  that it is an inherited primitive.

                        if No (Get_LL_Scope (Method))
                          or else Get_LL_Scope (Method) /= Get_LL_Scope (E)
                        then
                           --  For inherited primitives defined in other
                           --  files/scopes we cannot set their scope.

                           Decorate_Subprogram_Formals (Method);

                           if not Is_Partial_View (E) then
                              Append_Inherited_Method (E, Method);
                           else
                              Append_Inherited_Method
                                (Get_Full_View (Get_Entity (E)),
                                 Get_Entity (Method));
                           end if;

                        else
                           Append_To_Enclosing_Scope (E, Method);
                           Append_To_File_Entities (Method);

                           Decorate_Subprogram_Formals (Method);

                           if not Is_Partial_View (E) then
                              Append_Method (E, Method);
                           else
                              Append_Method
                                (Get_Full_View (Get_Entity (E)),
                                 Get_Entity (Method));
                           end if;

                           Set_Is_Decorated (Method);
                        end if;

                        Append_To_Map (Method);
                     else
                        if Get_LL_Scope (Method) = Get_LL_Entity (E) then
                           Append_To_Scope (E, Method);
                           Append_To_File_Entities (Method);

                           Decorate_Subprogram_Formals (Method);
                           Append_Method (E, Method);

                        --  For inherited primitives defined in other
                        --  scopes we cannot set their scope.

                        else
                           Decorate_Subprogram_Formals (Method);
                           Append_Inherited_Method (E, Method);
                        end if;

                        Append_To_Map (Method);
                     end if;
                  end loop;
               end;
            end if;
         end Decorate_Record_Type;

         ---------------------------------
         -- Decorate_Subprogram_Formals --
         ---------------------------------

         procedure Decorate_Subprogram_Formals (E : Unique_Entity_Id) is
         begin
            --  No extra action needed if the formals are already present

            if not EInfo_List.Is_Empty (Get_Entities (Get_Entity (E)).all) then
               return;
            end if;

            Enter_Scope (E);

            declare
               Formals : constant Xref.Parameter_Array :=
                           Parameters (Context.Database, Get_LL_Entity (E));
               Formal  : Unique_Entity_Id;

            begin
               for J in Formals'Range loop

                  --  Handle weird case found processing the file gimple.h of
                  --  the gcc sources: the entity associated with a function is
                  --  also associated with one of its formals. It seems a bug
                  --  in the generated LI file. To be investigated???

                  if Get_LL_Entity (E) = Formals (J).Parameter then
                     null;

                  else
                     Get_Unique_Entity
                       (Formal, Context, File, Formals (J).Parameter,
                        Forced => True);

                     if Present (Formal)
                       and then (Is_New (Formal) or else In_Generic_Scope)
                     then
                        pragma Assert
                          (LL.Get_Entity (Get_Entity (Formal))
                           = Formals (J).Parameter);

                        --  Correct previous wrong decoration (done by
                        --  Atree.New_Internal_Entity).

                        Remove_Full_View (Formal);

                        --  Complete decoration

                        Set_Kind (Formal, E_Formal);
                        Set_Scope (Formal, E);

                        Append_To_Scope (Current_Scope, Formal);
                        Append_To_File_Entities (Formal);
                        Append_To_Map (Formal);

                        Set_Is_Decorated (Formal);
                     end if;
                  end if;
               end loop;
            end;

            Exit_Scope;
         end Decorate_Subprogram_Formals;

      --  Start of processing for Complete_Decoration

      begin
         if Is_Container (E) then
            if Is_Class_Or_Record_Type (E) then
               Decorate_Record_Type (E);

            --  For concurrent types reuse the routine which processes record
            --  types to collect their progenitors (if any)

            elsif Is_Concurrent_Type_Or_Object (E) then
               Decorate_Record_Type (E);

            elsif Is_Generic (E) then
               Decorate_Generic_Formals (E);

               if Is_Subprogram_Or_Entry (E) then
                  Decorate_Subprogram_Formals (E);
               end if;

            elsif Is_Subprogram_Or_Entry (E) or else Is_Generic (E) then
               if Is_Subprogram (E)
                 and then Present (Get_LL_Alias (E))
               then
                  declare
                     Alias : Entity_Id;

                  begin
                     --  Search for the alias in the hash table

                     Alias :=
                       Find_Unique_Entity
                         (Get_Location (Context.Database, Get_LL_Alias (E)));

                     --  If not found then force the creation of a new entity

                     if No (Alias) then
                        declare
                           New_E : Unique_Entity_Id;
                        begin
                           Get_Unique_Entity
                             (New_E,
                              Context,
                              Get_Location
                                (Context.Database, Get_LL_Alias (E)).File,
                              Get_LL_Alias (E),
                              Forced => True);
                           Complete_Decoration (New_E);
                           Append_To_Map (New_E);

                           Alias := Get_Entity (New_E);
                        end;
                     end if;

                     Set_Alias (E, Alias);
                  end;
               end if;

               Decorate_Subprogram_Formals (E);
            end if;

         elsif Get_Kind (E) = E_Interface then
            Decorate_Record_Type (E);

         --  Decorate access to subprogram types

         elsif Is_Access_Type (E) then
            Decorate_Subprogram_Formals (E);
         end if;

         if Is_Partial_View (E) then
            declare
               Full_View : Unique_Entity_Id;
            begin
               Full_View := New_Internal_Entity (Get_Full_View (E));
               Append_To_Map (Full_View);

               --  For private types the entity associated with the full
               --  view is not available available in the ALI file and hence
               --  we must append it now to the scope; for incomplete type
               --  declarations the ALI file may have two entities and hence
               --  the full view will be fully decorated later.

               Append_To_Scope (Current_Scope, Full_View);
               Set_Is_Decorated (Full_View);
            end;
         end if;
      end Complete_Decoration;

      ---------------
      -- Set_Scope --
      ---------------

      procedure Set_Scope (New_E : Unique_Entity_Id) is
         Scope_Id : Entity_Id;
         Id : constant Integer := Get_Unique_Id (Get_Entity (New_E));
         S  : constant Unique_Entity_Id := Current_Scope;
      begin
         pragma Assert (In_Ada_Lang);
         pragma Assert (Id /= -1);
         pragma Assert (Present (S));

         --  No action needed if this attribute is already set

         if Present (Get_Scope (New_E)) then
            return;

         --  We cannot use the location provided by Xref when the entity is
         --  declared in a generic package since Xref references the scope
         --  enclosing the generic package (which is wrong!). More work
         --  needed in this area???

         elsif In_Generic_Scope then
            pragma Assert (Workaround);
            return;

         --  Skip the full view of incomplete or private types because their
         --  Xref.Scope references the partial view (instead of referencing
         --  their syntax scope)

         elsif Is_Incomplete_Or_Private_Type (New_E)
           and then Is_Full_View (New_E)
         then
            pragma Assert (Workaround);
            return;

         --  If Xref does not provide the scope we do our best and assume that
         --  this entity is defined in the current scope.

         --  This case can be reproduced when the package has a separate
         --  compilation unit (reproducible using test  013-GE-Bodies)

         elsif No (Get_LL_Scope (New_E)) then
            pragma Assert (Workaround);
            Set_Scope (New_E, Current_Scope);

         elsif Get_LL_Entity (Current_Scope) = Get_LL_Scope (New_E) then
            Set_Scope (New_E, Current_Scope);

         else
            Scope_Id :=
              Find_Entity
                (Current_Scope,
                 Get_Location
                   (Context.Database, Get_LL_Scope (New_E)));

            if Present (Scope_Id) then
               Set_Scope (New_E, Scope_Id);

            --  No information available: we do our best and assume that
            --  this entity is defined in the current scope.

            else
               pragma Assert (Workaround);
               Set_Scope (New_E, Current_Scope);
            end if;
         end if;
      end Set_Scope;

      -------------------------
      -- Update_Scopes_Stack --
      -------------------------

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id) is
      begin
         pragma Assert (In_Ada_Lang);

         --  We do not use such value when the entity is declared in a generic
         --  package since Xref references the scope enclosing the generic
         --  package (which is wrong!)

         if In_Generic_Scope then
            declare
               Loc : constant General_Location :=
                       Get_LL_Location (New_E);

               Scope         : Unique_Entity_Id;
               End_Scope_Loc : General_Location;
            begin
               Scope := Enclosing_Generic_Scope;
               End_Scope_Loc := Get_End_Of_Syntax_Scope_Loc (Scope);

               while Loc.Line > End_Scope_Loc.Line loop
                  while Current_Scope /= Scope loop
                     Exit_Scope;
                  end loop;

                  Exit_Scope;

                  exit when not In_Generic_Scope;

                  Scope := Enclosing_Generic_Scope;
                  End_Scope_Loc := Get_End_Of_Syntax_Scope_Loc (Scope);
               end loop;
            end;

         --  Skip updating the scopes stack using the full view of incomplete
         --  or private types because their Xref.Scope references the partial
         --  view (instead of referencing their syntax scope).

         elsif Is_Incomplete_Or_Private_Type (New_E)
           and then Is_Full_View (New_E)
         then
            return;

         --  Update the scopes stack using the reliable value provided by Xref.
         --  We skip updating it when such value is not found in the enclosing
         --  scopes since this entity may be a primitive inherited from other
         --  package.

         elsif Present (Get_LL_Scope (New_E))
           and then In_Open_Scopes (Get_LL_Scope (New_E))
         then
            while Get_LL_Scope (New_E)
              /= Get_LL_Entity (Current_Scope)
            loop
               Exit_Scope;
            end loop;
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
      Is_Large_File        : Boolean := False;

   --  Start of processing for Build_File_Tree

   begin
      Set_Kind (Std_Entity, E_Package);
      Register_Std_Entity (Std_Entity);
      Enter_Scope (Std_Entity);

      File_Entities_Cursor := Context.Database.Entities_In_File (File);
      while not At_End (File_Entities_Cursor) loop
         Total_Entities_Count := Total_Entities_Count + 1;
         File_Entities_Cursor.Next;
      end loop;

      --  For large files emit a warning since the user may think that the tool
      --  entered into a never-ending loop while it its processing the file.

      if Total_Entities_Count > 3000 then
         if not Context.Options.Quiet_Mode then
            GNAT.IO.Put_Line
              ("warning: large file " & (+File.Base_Name));
            Is_Large_File := True;
         end if;
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

               if Get_Kind (New_E) /= E_Formal then
                  Append_To_Scope (Current_Scope, New_E);
                  Append_To_File_Entities (New_E);
                  Set_Is_Decorated (New_E);

                  Append_To_Map (New_E);
               end if;

               if Is_Compilation_Unit (New_E) then
                  Enter_Scope (New_E);
                  exit;
               end if;
            end if;
         end loop;
      end if;

      --  Process all its entities

      while not At_End (File_Entities_Cursor) loop
         Entities_Count := Entities_Count + 1;

         if Is_Large_File
           and then not Context.Options.Quiet_Mode
           and then Entities_Count mod 300 = 0
         then
            GNAT.IO.Put_Line
              ("   "
               & (+File.Base_Name)
               & ":"
               & To_String (Entities_Count)
               & "/"
               & To_String (Total_Entities_Count));
         end if;

         Skip_This_Entity := False;
         Get_Unique_Entity
           (New_E, Context, File, File_Entities_Cursor.Get);

         if Present (New_E) then

            --  Decorate the new entity

            if In_Ada_Lang then

               --  Do not update the scope with discriminants of concurrent
               --  types since Xref sets their scope to the enclosing package.
               --  Need to investigate it???

               if Get_Kind (New_E) = E_Discriminant
                 and then Is_Concurrent_Type_Or_Object (Current_Scope)
               then
                  null;
               else
                  Update_Scopes_Stack (New_E);
               end if;

               Set_Scope (New_E);

               declare
                  In_Scope_With_Private_Entities : constant Boolean :=
                    (Is_Package (Current_Scope)
                       or else Is_Concurrent_Type_Or_Object (Current_Scope))
                    and then
                      Present
                        (Get_LL_First_Private_Entity_Loc (Current_Scope));
               begin
                  if In_Scope_With_Private_Entities
                    and then
                      Get_LL_Location (New_E).Line >=
                        Get_LL_First_Private_Entity_Loc (Current_Scope).Line
                  then
                     Set_In_Private_Part (New_E);
                  end if;
               end;

               if not Is_New (New_E)
                 and then Kind_In (Get_Kind (New_E), E_Formal,
                                                     E_Discriminant,
                                                     E_Component)
               then
                  Skip_This_Entity := True;

               elsif not Is_Primitive (New_E) then

                  --  Skip processing the full-view of a private or incomplete
                  --  type since its components are retrieved from Xref when
                  --  we process its partial view.

                  if Is_Incomplete_Or_Private_Type (New_E)
                    and then Is_Full_View (New_E)
                  then
                     Skip_This_Entity := True;
                  end if;

               --  Skip methods since they are entered in the tree as part of
               --  processing its tagged type

               elsif Is_Primitive (New_E) then
                  Skip_This_Entity := True;

               --  An E_Variable may be in fact a component of an incomplete
               --  or private type

               elsif Get_Kind (New_E) = E_Variable then
                  declare
                     Prev_E : constant Entity_Id :=
                                Find_Unique_Entity
                                  (LL.Get_Location (Get_Entity (New_E)));
                  begin
                     if Present (Prev_E) then
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

            --  C/C++

            else
               if not Is_New (New_E)
                 and then Kind_In (Get_Kind (New_E), E_Formal,
                                                     E_Component)
               then
                  Skip_This_Entity := True;

               elsif Get_Kind (New_E) = E_Variable
                 and then not Is_Global (New_E)
               then
                  Skip_This_Entity := True;

               --  Skip methods since they are entered in the tree as part of
               --  processing its class/tagged type

               elsif Is_Primitive (New_E) then
                  Skip_This_Entity := True;

               elsif Present (Get_LL_Scope (New_E))
                 and then Is_Global (New_E)
               then
                  --  Handle named typedef structs since the compiler generates
                  --  two entites in the LI file with the same name: decorate
                  --  the attribute Alias of the second entity referencing the
                  --  first one.

                  if Is_Class_Or_Record_Type (New_E)
                    and then Get_Kind (New_E) /= E_Class
                  then
                     declare
                        Scope_Id : constant General_Entity :=
                                     Get_LL_Scope (New_E);
                     begin
                        if Context.Database.Get_Name (Get_LL_Entity (New_E))
                          = Context.Database.Get_Name (Scope_Id)
                        then
                           declare
                              Prev_E : constant Entity_Id :=
                                Find_Unique_Entity
                                  (Get_Location (Context.Database, Scope_Id));
                           begin
                              pragma Assert (Present (Prev_E));
                              Set_Alias (New_E, Prev_E);
                           end;
                        end if;
                     end;

                  elsif Get_Kind (New_E) = E_Variable then
                     declare
                        Scope_Id : constant General_Entity :=
                                     Get_LL_Scope (New_E);
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
            end if;

            if Skip_This_Entity then
               Free (New_E);

            else
               if Is_New (New_E) then
                  Append_To_Map (New_E);
               end if;

               Append_To_File_Entities (New_E);

               --  Full views were unconditionally added to the scope as part
               --  of processing their partial view (since for private types
               --  the compiler does not generate two entities in the ALI
               --  file). Hence we avoid adding them twice to their scope.

               if not Is_Full_View (New_E) then
                  Append_To_Scope (Current_Scope, New_E);
               end if;

               if not Is_Decorated (New_E) then
                  Complete_Decoration (New_E);
                  Set_Is_Decorated (New_E);
               end if;

               if In_Ada_Lang then
                  if Get_Kind (New_E) = E_Enumeration_Type then
                     Enter_Scope (New_E);

                  elsif Is_Concurrent_Type_Or_Object (New_E) then
                     Enter_Scope (New_E);

                  elsif Is_Package (New_E) then
                     Enter_Scope (New_E);
                  end if;
               end if;
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      --  Exit all the scopes: required to ensure that we complete the
      --  decoration of all the private entities

      while not Is_Standard_Entity (Get_Entity (Current_Scope)) loop
         Exit_Scope;
      end loop;

      Scopes_Stack.Clear;

      return Get_Entity (Std_Entity);
   exception
      when E : others =>
         Trace (Me, E);
         raise;
   end Build_File_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      EInfo_Map.Clear (Entities_Map);
   end Initialize;

   ------------------------
   -- Find_Unique_Entity --
   ------------------------

   function Find_Unique_Entity
      (Location      : General_Location;
       In_References : Boolean := False) return Entity_Id
   is
      Map_Cursor : constant EInfo_Map.Cursor := Entities_Map.Find (Location);
      use type EInfo_Map.Cursor;
   begin
      if Map_Cursor /= EInfo_Map.No_Element then
         return EInfo_Map.Element (Map_Cursor);
      end if;

      --  If not found then search for an entity which is referenced from
      --  this location

      if In_References then
         declare
            Cursor : EInfo_Map.Cursor := Entities_Map.First;
            E      : Entity_Id;
         begin
            while EInfo_Map.Has_Element (Cursor) loop
               E := EInfo_Map.Element (Cursor);

               if LL.Is_Type (E)
                 and then LL.Has_Reference (E, Location)
               then
                  return E;
               end if;

               EInfo_Map.Next (Cursor);
            end loop;
         end;
      end if;

      return Atree.No_Entity;
   end Find_Unique_Entity;

   ------------------------
   -- Find_Unique_Entity --
   ------------------------

   function Find_Unique_Entity (Full_Name : String) return Entity_Id
   is
      Cursor : EInfo_Map.Cursor := Entities_Map.First;
      E      : Entity_Id;
   begin
      while EInfo_Map.Has_Element (Cursor) loop
         E := EInfo_Map.Element (Cursor);

         if Get_Full_Name (E) = Full_Name then
            return E;
         end if;

         EInfo_Map.Next (Cursor);
      end loop;

      return Atree.No_Entity;
   end Find_Unique_Entity;

   -----------------------
   -- Get_Unique_Entity --
   -----------------------

   function Get_Unique_Entity
     (Context : access constant Docgen_Context;
      File    : Virtual_File;
      E       : General_Entity) return Entity_Id
   is
      Loc    : constant General_Location := Get_Location (Context.Database, E);
      New_E  : Unique_Entity_Id;
      Result : Entity_Id;
   begin
      pragma Assert (Present (E));

      Result := Find_Unique_Entity (Loc);

      if Present (Result) then
         return Result;
      else
         Get_Unique_Entity (New_E, Context, File, E, Forced => True);
         Append_To_Map (New_E);
         return Get_Entity (New_E);
      end if;
   end Get_Unique_Entity;

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

      function Enclosing_Generic_Scope return Unique_Entity_Id is
         Last : constant Integer :=
           Current_Scope_Depth - 2; -- Skip standard
         use type Ada.Containers.Count_Type;
         S : Unique_Entity_Id;
      begin
         for J in 0 .. Last loop
            S := Stack.Element (Natural (J));

            if LL.Is_Generic (Get_Entity (S)) then
               return S;
            end if;
         end loop;

         return Unique_Entity_Allocator.No_Entity;
      end Enclosing_Generic_Scope;

      procedure Enter_Scope (Scope : Unique_Entity_Id) is
      begin
         Stack.Prepend (Scope);
      end Enter_Scope;

      ----------------
      -- Exit_Scope --
      ----------------

      procedure Exit_Scope is

         procedure Complete_Private_Tagged_Types_Decoration
           (Scope : Unique_Entity_Id);
         --  Append to the partial view all its primitives defined in the
         --  public part.

         procedure Complete_Private_Tagged_Types_Decoration
           (Scope : Unique_Entity_Id)
         is
            Cursor : EInfo_List.Cursor;
            E      : Entity_Id;

         begin
            Cursor := Get_Entities (Get_Entity (Scope)).First;
            while EInfo_List.Has_Element (Cursor) loop
               E := EInfo_List.Element (Cursor);

               if LL.Is_Type (E)
                 and then Is_Tagged (E)
                 and then Is_Partial_View (E)
               then
                  declare
                     P_Cursor : EInfo_List.Cursor;
                     Prim     : Entity_Id;
                  begin
                     P_Cursor :=
                       Get_Inherited_Methods (Get_Full_View (E)).First;
                     while EInfo_List.Has_Element (P_Cursor) loop
                        Prim := EInfo_List.Element (P_Cursor);

                        if not In_Private_Part (Prim) then
                           Append_Inherited_Method (E, Prim);
                        end if;

                        EInfo_List.Next (P_Cursor);
                     end loop;
                  end;

                  declare
                     P_Cursor : EInfo_List.Cursor;
                     Prim     : Entity_Id;
                  begin
                     P_Cursor :=
                       Get_Methods (Get_Full_View (E)).First;
                     while EInfo_List.Has_Element (P_Cursor) loop
                        Prim := EInfo_List.Element (P_Cursor);

                        if not In_Private_Part (Prim) then
                           Append_Method (E, Prim);
                        end if;

                        EInfo_List.Next (P_Cursor);
                     end loop;
                  end;
               end if;

               EInfo_List.Next (Cursor);
            end loop;
         end Complete_Private_Tagged_Types_Decoration;

      begin
         if Is_Package (Current_Scope)
           and then
             Present (Get_LL_First_Private_Entity_Loc (Current_Scope))
         then
            EInfo_Vector_Sort_Loc.Sort
              (Get_Entities (Get_Entity (Current_Scope)).all);

            Complete_Private_Tagged_Types_Decoration (Current_Scope);
         end if;

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

            if Get_LL_Entity (S) = E then
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

end GNATdoc.Frontend.Builder;
