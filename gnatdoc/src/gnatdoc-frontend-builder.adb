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
        (Scope : Unique_Entity_Id; E : Unique_Entity_Id);
      --  Append E to the list of entities of Scope

      procedure Free (Entity : in out Unique_Entity_Id);
      --  If this is a new entity the remove it; otherwise no action is
      --  performed since there are references to it in the tree.

      function Get_End_Of_Syntax_Scope_Loc
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_Entity
        (Entity : Unique_Entity_Id) return Entity_Id;

      function Get_Kind
        (Entity : Unique_Entity_Id) return Entity_Kind;

      function Get_Language
        (Entity : Unique_Entity_Id) return Language_Access;

      function Get_LL_Entity
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_LL_First_Private_Entity_Loc
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_LL_Full_View
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_LL_Location
        (Entity : Unique_Entity_Id) return General_Location;

      function Get_LL_Scope
        (Entity : Unique_Entity_Id) return General_Entity;

      function Get_Parent
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

      function Is_Primitive
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Subprogram
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Tagged_Type
        (Entity : Unique_Entity_Id) return Boolean;

      function Is_Type
        (Entity : Unique_Entity_Id) return Boolean;

      function New_Internal_Entity
        (Context  : access constant Docgen_Context;
         Language : Language_Access;
         Name     : String) return Unique_Entity_Id;
      --  Allocate an internal entity. Used to build the standard entity.

      function Number_Of_Progenitors
        (Entity : Unique_Entity_Id) return Natural;

      function Present (Entity : Unique_Entity_Id) return Boolean;
      --  Return True if Entity /= No_Entity

      procedure Remove_From_Scope (E : Unique_Entity_Id);
      --  Remove E from its current scope

      procedure Set_Alias
        (Entity : Unique_Entity_Id; Value : Entity_Id);

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
      pragma Inline (Get_Kind);
      pragma Inline (Get_Language);
      pragma Inline (Get_LL_Entity);
      pragma Inline (Get_LL_First_Private_Entity_Loc);
      pragma Inline (Get_LL_Full_View);
      pragma Inline (Get_LL_Location);
      pragma Inline (Get_LL_Scope);
      pragma Inline (Get_Parent);
      pragma Inline (Get_Scope);
      pragma Inline (In_Ada_Language);
      pragma Inline (Is_Class_Or_Record_Type);
      pragma Inline (Is_Container);
      pragma Inline (Is_Decorated);
      pragma Inline (Is_Full_View);
      pragma Inline (Is_Global);
      pragma Inline (Is_Incomplete_Or_Private_Type);
      pragma Inline (Is_New);
      pragma Inline (Is_Package);
      pragma Inline (Is_Primitive);
      pragma Inline (Is_Subprogram);
      pragma Inline (Is_Tagged_Type);
      pragma Inline (Is_Type);
      pragma Inline (Number_Of_Progenitors);
      pragma Inline (Present);
      pragma Inline (Set_Alias);
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
            Entities_Map.Include
              (LL.Get_Location (Get_Entity (E)), Get_Entity (E));
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
         Append_To_Scope (Enclosing_Scope, Get_Entity (E));
         Set_Scope (Get_Entity (E), Enclosing_Scope);
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
         Append_To_Scope (Get_Entity (Scope), Get_Entity (E));
         Set_Scope (Get_Entity (E), Get_Entity (Scope));
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

      ----------------------
      -- Get_LL_Full_View --
      ----------------------

      function Get_LL_Full_View
        (Entity : Unique_Entity_Id) return General_Entity is
      begin
         return LL.Get_Full_View (Get_Entity (Entity));
      end Get_LL_Full_View;

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
         return Is_Incomplete_Or_Private_Type (Get_Entity (Entity));
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
         return LL.Is_Subprogram (Get_Entity (Entity));
      end Is_Subprogram;

      --------------------
      -- Is_Tagged_Type --
      --------------------

      function Is_Tagged_Type (Entity : Unique_Entity_Id) return Boolean is
      begin
         return Is_Tagged_Type (Get_Entity (Entity));
      end Is_Tagged_Type;

      -------------
      -- Is_Type --
      -------------

      function Is_Type (Entity : Unique_Entity_Id) return Boolean is
      begin
         return LL.Is_Type (Get_Entity (Entity));
      end Is_Type;

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

         function Build_New_Entity return Unique_Entity_Id is
            Entity : constant Entity_Id :=
                       New_Entity (Context, Lang, E, E_Loc);
         begin
            if No (Entity) then
               return No_Entity;
            else
               return
                 new Unique_Entity_Info'(Entity => Entity,
                                         Is_New => True);
            end if;
         end Build_New_Entity;

         --  Local variables

         Is_Prim : constant Boolean := Present (Is_Primitive_Of (Db, E));
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

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id);
      --  Update the top of the scope stack (if required)

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
                     pragma Assert (Is_New (Entity));

                     Set_Kind (Entity, E_Discriminant);
                     Append_To_Scope (E, Entity);
                     Append_To_Map (Entity);
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
                  for J in Components'Range loop
                     Get_Unique_Entity (Entity, Context, File, Components (J));
                     pragma Assert (Is_New (Entity));

                     --  In C++ we have here formals of primitives???
                     Set_Kind (Entity, E_Component);

                     Append_To_Scope (E, Entity);
                     Append_To_File_Entities (Entity);
                     Append_To_Map (Entity);
                  end loop;
               end;
            end if;

            Append_Parent_And_Progenitors
              (Xref.Parent_Types
                 (Self      => Context.Database,
                  Entity    => Get_LL_Entity (E),
                  Recursive => False));

            if In_Ada_Language (E) then

               --  Add information available in the full view (if the entity
               --  of its full view is available; see the comment describing
               --  this problem in gnatdoc-atree.adb???)

               if Is_Incomplete_Or_Private_Type (E)
                 and then Present (Get_LL_Full_View (E))
               then
                  Append_Parent_And_Progenitors
                    (Parent_Types
                       (Context.Database, Get_LL_Full_View (E),
                        Recursive => False));
               end if;

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

                     --  Avoid problems with wrong Xref decoration that I can
                     --  reproduces with gnatcoll-refcount-weakref.ads. To
                     --  be investigated???

                     if not Is_Class_Or_Record_Type (Child) then
                        Free (Child);

                     else
                        Append_Child_Type (E, Child);

                        if Is_New (Child) then
                           Append_To_Map (Child);
                        end if;
                     end if;
                  end if;
               end loop;
            end;

            if Is_Tagged_Type (E)
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
                        Append_Inherited_Method (E, Method);

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
                           Append_Inherited_Method (E, Method);

                        else
                           Append_To_Enclosing_Scope (E, Method);
                           Append_To_File_Entities (Method);

                           Decorate_Subprogram_Formals (Method);
                           Append_Method (E, Method);
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
            --  No extra action needed if we are processing the formals of the
            --  body

            if not Is_New (E) then
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

                        Set_Kind (Formal, E_Formal);
                        Set_Scope (Formal, E);

                        Append_To_Scope (Current_Scope, Formal);
                        Append_To_File_Entities (Formal);

                        Append_To_Map (Formal);
                        --  Local variables defined in the body of this
                        --  subprogram.
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

            elsif Is_Generic (E) then
               Decorate_Generic_Formals (E);

               if Is_Subprogram (E) then
                  Decorate_Subprogram_Formals (E);
               end if;

            --  Although formals are available in the list of entities of the
            --  file we are traversing, it is not easy to identify and set
            --  the scope of formals just traversing these entities since
            --  some entities do not have its Xref.Scope entity available.

            elsif Is_Subprogram (E) or else Is_Generic (E) then
               Decorate_Subprogram_Formals (E);
            end if;

         elsif Get_Kind (E) = E_Interface then
            Decorate_Record_Type (E);

         --  Decorate access to subprogram types

         elsif Is_Access_Type (E) then
            Decorate_Subprogram_Formals (E);
         end if;
      end Complete_Decoration;

      -------------------------
      -- Update_Scopes_Stack --
      -------------------------

      procedure Update_Scopes_Stack (New_E : Unique_Entity_Id) is
         Scope_Id : Entity_Id;

      begin
         pragma Assert (In_Ada_Lang);

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
         end if;

         --  Update the scopes stack using the reliable value provided by
         --  Xref (if available)

         --  We do not use such value when the entity is declared in a
         --  generic package since Xref references the scope enclosing
         --  the generic package (which is wrong!)

         if In_Generic_Scope then
            return;
         end if;

         --  Skip the full view of incomplete or private types
         --  because their Xref.Scope references the partial
         --  view (instead of referencing its syntax scope)

         if Is_Incomplete_Or_Private_Type (New_E)
           and then Is_Full_View (New_E)
         then
            return;
         end if;

         if Present (Get_LL_Scope (New_E)) then
            if In_Open_Scopes (Get_LL_Scope (New_E)) then
               while Get_LL_Scope (New_E)
                 /= Get_LL_Entity (Current_Scope)
               loop
                  Exit_Scope;
               end loop;

            elsif Is_Type (New_E) then
               Scope_Id :=
                 Find_Entity
                   (Current_Scope,
                    Get_Location
                      (Context.Database, Get_LL_Scope (New_E)));

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

      New_E                : Unique_Entity_Id;
      Skip_This_Entity     : Boolean := False;
      File_Entities_Cursor : Entities_In_File_Cursor;

      Total_Entities_Count : Natural := 0;
      Entities_Count       : Natural := 0;

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

      --  Temporarily disable construction of large trees (until we improve
      --  the performance!). For example, sqlite3.c has 14860 entities and
      --  requires 46 minutes to build the tree in my virtual machine???

      if Total_Entities_Count > 3000 then
         Trace (Me,
           ">> Build_File_Tree (skipped): "
           & Total_Entities_Count'Img
           & " entities");

         Scopes_Stack.Clear;
         return Atree.No_Entity;
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

                  Append_To_Map (New_E);
               end if;

               --  Avoid spurious entity GNATCOLL.Any_Types (procedure???)

               if Is_Container (New_E)
                 and then Is_Global (New_E)
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

         if not Context.Options.Quiet_Mode
           and then Entities_Count mod 75 = 0
         then
            GNAT.IO.Put_Line
              (+File.Base_Name
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
               Update_Scopes_Stack (New_E);

               if not Context.Options.Show_Private
                 and then Is_Package (Current_Scope)
                 and then
                   Present (Get_LL_First_Private_Entity_Loc (Current_Scope))
                 and then
                   Get_LL_Location (New_E).Line >=
                   Get_LL_First_Private_Entity_Loc (Current_Scope).Line
               then
                  Skip_This_Entity := True;

               elsif not Is_New (New_E)
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
               Append_To_Scope (Current_Scope, New_E);

               if not Is_Decorated (New_E) then
                  Complete_Decoration (New_E);
                  Set_Is_Decorated (New_E);
               end if;

               if In_Ada_Lang then
                  if Get_Kind (New_E) = E_Enumeration_Type then
                     Enter_Scope (New_E);

                  elsif Is_Package (New_E) then
                     Enter_Scope (New_E);
                  end if;
               end if;
            end if;
         end if;

         File_Entities_Cursor.Next;
      end loop;

      Scopes_Stack.Clear;

      return Get_Entity (Std_Entity);
   exception
      when E : others =>
         Trace (Me, E);
         return Atree.No_Entity;
   end Build_File_Tree;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      EInfo_Map.Clear (Entities_Map);
   end Initialize;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Unique_Entity (Location : General_Location) return Entity_Id
   is
      Map_Cursor : constant EInfo_Map.Cursor := Entities_Map.Find (Location);
      use type EInfo_Map.Cursor;
   begin
      if Map_Cursor /= EInfo_Map.No_Element then
         return EInfo_Map.Element (Map_Cursor);
      else
         return Atree.No_Entity;
      end if;
   end Find_Unique_Entity;

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
