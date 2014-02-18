------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2014, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;

with GNAT.HTable;
with GNATdoc.Utils;               use GNATdoc.Utils;

with Basic_Types;                 use Basic_Types;
with Language.Ada;
with Language.C;
with Language.Tree;               use Language.Tree;
with Language.Tree.Database;      use Language.Tree.Database;
with GNATCOLL.Traces;             use GNATCOLL.Traces;

with GNAT.IO; --  For output of debugging routines

package body GNATdoc.Atree is
   Me : constant Trace_Handle := Create ("DOCGEN.ATREE");
   Enhancements : constant Boolean := False;

   Unique_Id : Natural := 0;
   --  Internal counter used to associate an unique identifier to all the
   --  nodes.

   Disable_Free : constant Boolean := True;
   --  Value used to temporarily disable free of entities???

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Append_Direct_Derivation
     (E : Entity_Id; Value : Entity_Id);
   pragma Inline (Append_Direct_Derivation);

   function Contains
     (Container : EInfo_List.Vector;
      Entity    : Entity_Id) return Boolean;
   --  Return True if the container has an Entity whose location matches the
   --  location of Entity.

   function Get_Subprograms_And_Entries
     (E : Entity_Id) return EInfo_List.Vector;
   --  Applicable to record types, concurrent types and concurrent objects

   function Internal_New_Entity
     (Context     : access constant Docgen_Context;
      Lang        : Language_Access;
      E           : General_Entity;
      Loc         : General_Location;
      Name        : String := "";
      Is_Internal : Boolean := False) return Entity_Id;
   --  Internal subprogram which factorizes the code needed by routines
   --  New_Entity and New_Internal_Entity to create a new entity.

   function LL_Get_First_Private_Entity_Loc
     (E : Entity_Id) return General_Location;
   pragma Inline (LL_Get_First_Private_Entity_Loc);

   function LL_Get_Full_Name
     (E : Entity_Id) return String;
   pragma Inline (LL_Get_Full_Name);

   function LL_Get_Kind (E : Entity_Id) return Entity_Kind;
   pragma Inline (LL_Get_Kind);

   function LL_Is_Generic (E : Entity_Id) return Boolean;
   pragma Inline (LL_Is_Generic);

   package Hash_Table is
      function Hash
        (Key : General_Location) return Ada.Containers.Hash_Type;

      function Equivalent_Keys
        (Left, Right : General_Location) return Boolean;

      package Ref_Map is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => General_Location,
         Element_Type    => Ref_Info,
         Hash            => Hash,
         Equivalent_Keys => Equivalent_Keys);

      References_Map : Ref_Map.Map;

      procedure Append_To_Map (Ref : Ref_Info);
      --  Append the entity of E to Entities_Map

   private
      pragma Inline (Append_To_Map);
   end Hash_Table;
   use Hash_Table;

   package body Hash_Table is

      -------------------
      -- Append_To_Map --
      -------------------

      procedure Append_To_Map (Ref : Ref_Info) is
      begin
         References_Map.Include (Ref.Loc, Ref);
      end Append_To_Map;

      ---------------------
      -- Equivalent_Keys --
      ---------------------

      function Equivalent_Keys
        (Left, Right : General_Location) return Boolean is
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
   -- Append_Derivation --
   -----------------------

   procedure Append_Direct_Derivation (E : Entity_Id; Value : Entity_Id) is
   begin
      if not Get_Direct_Derivations (E).Contains (Value) then
         Get_Direct_Derivations (E).Append (Value);
      end if;
   end Append_Direct_Derivation;

   ---------------------------
   -- Append_Generic_Formal --
   ---------------------------

   procedure Append_Generic_Formal (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (Is_Generic (E));
      pragma Assert (Is_Generic_Formal (Value));

      Get_Generic_Formals (E).Append (Value);
   end Append_Generic_Formal;

   -----------------------------
   -- Append_Inherited_Method --
   -----------------------------

   procedure Append_Inherited_Method
     (E : Entity_Id; Value : Entity_Id) is
   begin
      --  Replace this conditional by an assertion???

      if not Get_Inherited_Methods (E).Contains (Value) then
         Get_Inherited_Methods (E).Append (Value);
      end if;
   end Append_Inherited_Method;

   -------------------
   -- Append_Method --
   -------------------

   procedure Append_Method (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (not Get_Methods (E).Contains (Value));
      Get_Methods (E).Append (Value);
   end Append_Method;

   -----------------------
   -- Append_Progenitor --
   -----------------------

   procedure Append_Progenitor (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (not Get_Progenitors (E).Contains (Value));
      Get_Progenitors (E).Append (Value);
   end Append_Progenitor;

   ---------------------
   -- Append_To_Scope --
   ---------------------

   procedure Append_To_Scope (E : Entity_Id; Value : Entity_Id) is

      function Check_Unique return Boolean;
      --  Check that Value is not added twice to the list of entities of E

      function Check_Unique return Boolean is
      begin
         for Entity of Get_Entities (E).all loop
            if Entity = Value then
               return False;
            end if;
         end loop;

         return True;
      end Check_Unique;

   begin
      pragma Assert (No (Get_Scope (Value)));
      pragma Assert (not Get_Entities (E).Contains (Value));
      pragma Assert (Check_Unique);
      Get_Entities (E).Append (Value);
      Atree.Set_Scope (E => Value, Value => E);

      if Present (Get_Full_View (Value)) then
         if Present (Get_Scope (Get_Full_View (Value))) then
            pragma Assert
              (not In_Same_File (Value, Get_Full_View (Value))
                 or else Get_Scope (Get_Full_View (Value)) = E);

         --  Taft ammendment

         elsif not In_Same_File (Value, Get_Full_View (Value)) then
            null;

         else
            Append_To_Scope (E, Get_Full_View (Value));
            Set_Scope (Get_Full_View (Value), E);
         end if;
      end if;
   end Append_To_Scope;

   ------------------------
   -- Append_Unique_Elmt --
   ------------------------

   procedure Append_Unique_Elmt
     (Container : in out EInfo_List.Vector;
      Entity    : Entity_Id) is
   begin
      if not Contains (Container, Entity) then
         Container.Append (Entity);
      end if;
   end Append_Unique_Elmt;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : EInfo_List.Vector;
      Entity    : Entity_Id) return Boolean
   is
      Loc : constant General_Location := LL.Get_Location (Entity);

   begin
      if Present (Loc) then
         for Entity of Container loop
            if LL.Get_Location (Entity) = Loc then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Contains;

   -------------------
   -- Delete_Entity --
   -------------------

   procedure Delete_Entity
     (List   : in out EInfo_List.Vector;
      Entity : General_Entity)
   is
      Cursor : EInfo_List.Cursor;
      E      : Entity_Id;

   begin
      if not EInfo_List.Has_Element (List.First) then
         return;
      end if;

      Cursor := List.First;
      while EInfo_List.Has_Element (Cursor) loop
         E := EInfo_List.Element (Cursor);

         if LL.Get_Entity (E) = Entity then
            EInfo_List.Delete (List, Cursor);
            return;
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      raise Not_Found;
   end Delete_Entity;

   -------------------
   -- Delete_Entity --
   -------------------

   procedure Delete_Entity
     (List   : in out EInfo_List.Vector;
      Entity : Entity_Id)
   is
      Cursor : EInfo_List.Cursor;
      E      : Entity_Id;

   begin
      if not EInfo_List.Has_Element (List.First) then
         return;
      end if;

      Cursor := List.First;
      while EInfo_List.Has_Element (Cursor) loop
         E := EInfo_List.Element (Cursor);

         if E = Entity then
            EInfo_List.Delete (List, Cursor);
            return;
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      raise Not_Found;
   end Delete_Entity;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (List   : EInfo_List.Vector;
      Entity : General_Entity) return Entity_Id
   is
   begin
      if not EInfo_List.Has_Element (List.First) then
         return No_Entity;
      end if;

      for E of List loop
         if LL.Get_Entity (E) = Entity then
            return E;
         end if;
      end loop;

      return No_Entity;
   end Find_Entity;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (List : EInfo_List.Vector;
      Name : String) return Entity_Id
   is
      Cursor : EInfo_List.Cursor;
      E      : Entity_Id;

      Is_Expanded_Name : Boolean := False;

   begin
      if not EInfo_List.Has_Element (List.First) then
         return null;
      end if;

      for J in Name'Range loop
         if Name (J) = '.' then
            Is_Expanded_Name := True;
            exit;
         end if;
      end loop;

      Cursor := List.First;

      if Is_Expanded_Name then
         while EInfo_List.Has_Element (Cursor) loop
            E := EInfo_List.Element (Cursor);

            --  We use substrings to match the name since Name may not be
            --  the full name of the entity but a partial name. For example,
            --  we may have available the Name "b.c" for an entity with full
            --  name "a.b.c" in an scope that has an use-clause for "a".

            declare
               Full_Name : constant String := Get_Full_Name (E);
               Last  : constant Natural := Full_Name'Last;
               First : constant Integer := Full_Name'Last - Name'Length + 1;

            begin
               if First in Full_Name'Range
                 and then Full_Name (First .. Last) = Name
               then
                  return E;
               end if;
            end;

            EInfo_List.Next (Cursor);
         end loop;

      else
         while EInfo_List.Has_Element (Cursor) loop
            E := EInfo_List.Element (Cursor);

            if Get_Short_Name (E) = Name then
               return E;
            end if;

            EInfo_List.Next (Cursor);
         end loop;
      end if;

      return No_Entity;
   end Find_Entity;

   -----------------
   -- Find_Entity --
   -----------------

   function Find_Entity
     (Location : General_Location) return Entity_Id
   is
      Map_Cursor : constant Ref_Map.Cursor := References_Map.Find (Location);
      use type Ref_Map.Cursor;
   begin
      if Map_Cursor /= Ref_Map.No_Element then
         return Ref_Map.Element (Map_Cursor).Entity;
      else
         return No_Entity;
      end if;
   end Find_Entity;

   -------------
   -- For_All --
   -------------

   procedure For_All
     (Vector  : in out EInfo_List.Vector;
      Process : access procedure (E_Info : Entity_Id))
   is
      Cursor  : EInfo_List.Cursor;

   begin
      Cursor := Vector.First;
      while EInfo_List.Has_Element (Cursor) loop
         Process (EInfo_List.Element (Cursor));
         EInfo_List.Next (Cursor);
      end loop;
   end For_All;

   ----------
   -- Free --
   ----------

   procedure Internal_Free is
     new Ada.Unchecked_Deallocation (Entity_Info_Record, Entity_Id);

   procedure Free (E : in out Entity_Id) is
   begin
      if Disable_Free then
         E := null;
      else
         Internal_Free (E);
      end if;
   end Free;

   procedure Free (List : in out EInfo_List.Vector) is

      procedure EInfo_List_Free
        (EInfo : in out Entity_Id);
      --  Free memory used by Entity Info

      procedure EInfo_List_Free
        (EInfo : in out Entity_Id)
      is
      begin
         if No (EInfo) then
            return;
         end if;

         Internal_Free (EInfo);
      end EInfo_List_Free;

   begin
      if Disable_Free then
         List := EInfo_List.Empty_Vector;
      else
         for J in List.First_Index .. List.Last_Index loop
            List.Update_Element (J, EInfo_List_Free'Access);
         end loop;

         List.Clear;
      end if;
   end Free;

   ---------------
   -- Get_Alias --
   ---------------

   function Get_Alias (E : Entity_Id) return Entity_Id is
   begin
      return E.Alias;
   end Get_Alias;

   -----------------
   -- Get_Comment --
   -----------------

   function Get_Comment (E : Entity_Id) return Structured_Comment is
   begin
      return E.Comment;
   end Get_Comment;

   --------------------
   -- Get_Components --
   --------------------

   function Get_Components
     (E : Entity_Id) return EInfo_List.Vector
   is
      Cursor : EInfo_List.Cursor;
      Result : EInfo_List.Vector;
      Entity : Entity_Id;

   begin
      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         Entity := EInfo_List.Element (Cursor);

         if Get_Kind (Entity) = E_Component then
            Result.Append (Entity);
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      return Result;
   end Get_Components;

   ---------------------
   -- Get_Derivations --
   ---------------------

   function Get_Direct_Derivations
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      --  For backward compatibility, for private types, direct derivations
      --  are only stored in the partial view???

      if Is_Full_View (E) then
         return Get_Partial_View (E).Direct_Derivations'Access;
      else
         return E.Direct_Derivations'Access;
      end if;
   end Get_Direct_Derivations;

   -----------------------
   -- Get_Discriminants --
   -----------------------

   function Get_Discriminants
     (E : Entity_Id) return EInfo_List.Vector
   is
      Cursor : EInfo_List.Cursor;
      Result : EInfo_List.Vector;
      Entity : Entity_Id;

   begin
      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         Entity := EInfo_List.Element (Cursor);

         if Get_Kind (Entity) = E_Discriminant then
            Result.Append (Entity);
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      return Result;
   end Get_Discriminants;

   -------------
   -- Get_Doc --
   -------------

   function Get_Doc (E : Entity_Id) return Comment_Result is
   begin
      return E.Doc;
   end Get_Doc;

   -------------------
   -- Get_Doc_After --
   -------------------

   function Get_Doc_After (E : Entity_Id) return Comment_Result is
   begin
      return E.Doc_After;
   end Get_Doc_After;

   --------------------
   -- Get_Doc_Before --
   --------------------

   function Get_Doc_Before (E : Entity_Id) return Comment_Result is
   begin
      return E.Doc_Before;
   end Get_Doc_Before;

   ---------------------------------
   -- Get_End_Of_Syntax_Scope_Loc --
   ---------------------------------

   function Get_End_Of_Syntax_Scope_Loc
     (E : Entity_Id) return General_Location is
   begin
      return E.End_Of_Syntax_Scope_Loc;
   end Get_End_Of_Syntax_Scope_Loc;

   -----------------------------------------
   -- Get_End_Of_Profile_Location_In_Body --
   -----------------------------------------

   function Get_End_Of_Profile_Location_In_Body
     (E : Entity_Id) return General_Location is
   begin
      return E.End_Of_Profile_Location_In_Body;
   end Get_End_Of_Profile_Location_In_Body;

   ------------------
   -- Get_Entities --
   ------------------

   function Get_Entities (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Entities'Access;
   end Get_Entities;

   -----------------
   -- Get_Entries --
   -----------------

   function Get_Entries
     (E : Entity_Id) return EInfo_List.Vector
   is
      Cursor : EInfo_List.Cursor;
      Result : EInfo_List.Vector;
      Entity : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type_Or_Object (E));

      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         Entity := EInfo_List.Element (Cursor);

         if Get_Kind (Entity) = E_Entry then
            Result.Append (Entity);
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      return Result;
   end Get_Entries;

   -------------------
   -- Get_Error_Msg --
   -------------------

   function Get_Error_Msg (E : Entity_Id) return Unbounded_String is
   begin
      return E.Error_Msg;
   end Get_Error_Msg;

   ----------------------------------
   -- Get_First_Private_Entity_Loc --
   ----------------------------------

   function Get_First_Private_Entity_Loc
     (E : Entity_Id) return General_Location is
   begin
      --  If the location of the first private entity is provided by the
      --  compiler then return that value; otherwise return the value
      --  computed by the frontend.

      --  We tried to improve the frontend to compute exactly the same
      --  location computed by the compiler but this approach was discarded
      --  because when there are pragmas located between the reserved word
      --  "private" and the first private declaration then it is not clear
      --  the location computed by the compiler.

      if Present (LL_Get_First_Private_Entity_Loc (E)) then
         return LL_Get_First_Private_Entity_Loc (E);
      else
         return E.First_Private_Entity_Loc;
      end if;
   end Get_First_Private_Entity_Loc;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name (E : Entity_Id) return String is
      Full_Name  : Unbounded_String;
      Scope      : Entity_Id := Get_Scope (E);
      Prev_Scope : Entity_Id;

      function In_Neverending_Loop return Boolean;
      --  Return True if the computation enters into a never ending loop

      Parents : array (1 .. 50) of Entity_Id;
      P_Count : Natural := 0;

      function In_Neverending_Loop return Boolean is
      begin
         for J in 1 .. P_Count loop
            if Parents (J) = Scope then
               pragma Assert (False);
               return True;
            end if;
         end loop;

         P_Count := P_Count + 1;
         Parents (P_Count) := Scope;

         return False;
      end In_Neverending_Loop;

   begin
      --  Workaround missing value of LL.Scope() in subprogram which causes
      --  wrong computation of the full name.

      if No (LL.Get_Scope (E))
        and then Is_Subprogram (E)
        and then Is_Expanded_Name (LL_Get_Full_Name (E))
      then
         return LL_Get_Full_Name (E);
      end if;

      Set_Unbounded_String (Full_Name, Get_Short_Name (E));

      Prev_Scope := E;
      while Present (Scope)
        and then not Is_Standard_Entity (Scope)
      loop
         if In_Neverending_Loop then
            return To_String (Full_Name);
         end if;

         --  ---
         Full_Name  := Get_Short_Name (Scope) & "." & Full_Name;
         Prev_Scope := Scope;
         Scope      := Get_Scope (Scope);
      end loop;

      Scope := Prev_Scope;
      while Present (Get_Parent_Package (Scope)) loop
         Scope := Get_Parent_Package (Scope);

         if In_Neverending_Loop then
            return To_String (Full_Name);
         end if;

         Full_Name := Get_Short_Name (Scope) & "." & Full_Name;
      end loop;

      return To_String (Full_Name);
   end Get_Full_Name;

   -------------------
   -- Get_Full_View --
   -------------------

   function Get_Full_View (E : Entity_Id) return Entity_Id is
   begin
      return E.Full_View;
   end Get_Full_View;

   -----------------------------
   -- Get_Generic_Formals_Loc --
   -----------------------------

   function Get_Generic_Formals_Loc
     (E : Entity_Id) return General_Location is
   begin
      return E.Generic_Formals_Loc;
   end Get_Generic_Formals_Loc;

   -------------------------
   -- Get_Generic_Formals --
   -------------------------

   function Get_Generic_Formals
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      pragma Assert (Is_Generic (E));
      return E.Generic_Formals'Access;
   end Get_Generic_Formals;

   ----------------------
   -- Get_IDepth_Level --
   ----------------------

   function Get_IDepth_Level (E : Entity_Id) return Natural is
   begin
      return E.Idepth_Level;
   end Get_IDepth_Level;

   ---------------------------
   -- Get_Inherited_Methods --
   ---------------------------

   function Get_Inherited_Methods
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Inherited_Methods'Access;
   end Get_Inherited_Methods;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (E : Entity_Id) return Entity_Kind is
   begin
      return E.Kind;
   end Get_Kind;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language (E : Entity_Id) return Language_Access is
   begin
      return E.Language;
   end Get_Language;

   -----------------
   -- Get_Methods --
   -----------------

   function Get_Methods (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Methods'Access;
   end Get_Methods;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (E : Entity_Id) return Entity_Id is
   begin
      --  For backward compatibility we temporarily return the parent of
      --  the full view since parents are currently only stored in partial
      --  views.

      if not Enhancements
        and then Is_Partial_View (E)
      then
         return Get_Parent (Get_Full_View (E));
      else
         return E.Parent;
      end if;
   end Get_Parent;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Package
     (E : Entity_Id) return Entity_Id is
   begin
      return E.Parent_Package;
   end Get_Parent_Package;

   ----------------------
   -- Get_Partial_View --
   ----------------------

   function Get_Partial_View (E : Entity_Id) return Entity_Id is
   begin
      return E.Partial_View;
   end Get_Partial_View;

   ---------------------
   -- Get_Progenitors --
   ---------------------

   function Get_Progenitors
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Progenitors'Access;
   end Get_Progenitors;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope (E : Entity_Id) return Entity_Id is
   begin
      return E.Scope;
   end Get_Scope;

   --------------------
   -- Get_Short_Name --
   --------------------

   function Get_Short_Name (E : Entity_Id) return String is
   begin
      return Get (E.Short_Name).all;
   end Get_Short_Name;

   -------------
   -- Get_Src --
   -------------

   function Get_Src (E : Entity_Id) return Unbounded_String is
   begin
      return E.Src;
   end Get_Src;

   ---------------------
   -- Get_Subprograms --
   ---------------------

   function Get_Subprograms
     (E : Entity_Id) return EInfo_List.Vector
   is
      Cursor : EInfo_List.Cursor;
      Result : EInfo_List.Vector;
      Entity : Entity_Id;

   begin
      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         Entity := EInfo_List.Element (Cursor);

         if Is_Subprogram (Entity) then
            Result.Append (Entity);
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      return Result;
   end Get_Subprograms;

   ---------------------------------
   -- Get_Subprograms_And_Entries --
   ---------------------------------

   function Get_Subprograms_And_Entries
     (E : Entity_Id) return EInfo_List.Vector
   is
      Cursor : EInfo_List.Cursor;
      Result : EInfo_List.Vector;
      Entity : Entity_Id;

   begin
      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         Entity := EInfo_List.Element (Cursor);

         if Is_Subprogram_Or_Entry (Entity) then
            Result.Append (Entity);
         end if;

         EInfo_List.Next (Cursor);
      end loop;

      return Result;
   end Get_Subprograms_And_Entries;

   -------------------
   -- Get_Unique_Id --
   -------------------

   function Get_Unique_Id (E : Entity_Id) return Natural is
   begin
      return E.Id;
   end Get_Unique_Id;

   -----------------------------
   -- Has_Duplicated_Entities --
   -----------------------------

   function Has_Duplicated_Entities
     (List : EInfo_List.Vector) return Boolean
   is
      Aux_List : EInfo_List.Vector;
      Cursor   : EInfo_List.Cursor;
      Prev_E   : Entity_Id;
      E        : Entity_Id;

   begin
      Cursor := List.First;
      while EInfo_List.Has_Element (Cursor) loop
         Aux_List.Append (EInfo_List.Element (Cursor));
         EInfo_List.Next (Cursor);
      end loop;

      EInfo_Vector_Sort_Loc.Sort (Aux_List);

      Cursor := Aux_List.First;

      if EInfo_List.Has_Element (Cursor) then
         Prev_E := EInfo_List.Element (Cursor);
         EInfo_List.Next (Cursor);

         while EInfo_List.Has_Element (Cursor) loop
            E := EInfo_List.Element (Cursor);

            if LL.Get_Location (Prev_E) = LL.Get_Location (E) then
               Aux_List.Clear;
               return True;
            end if;

            Prev_E := E;
            EInfo_List.Next (Cursor);
         end loop;
      end if;

      Aux_List.Clear;
      return False;
   end Has_Duplicated_Entities;

   -----------------
   -- Has_Formals --
   -----------------

   function Has_Formals (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Subprogram_Or_Entry (E));
      return Present (Get_Entities (E));
   end Has_Formals;

   ---------------------
   -- Has_Parent_Type --
   ---------------------

   function Has_Parent_Type
     (E : Entity_Id; Parent : Entity_Id) return Boolean is
   begin
      for P of LL.Get_Parent_Types (E).all loop
         if LL.Get_Location (P) = LL.Get_Location (Parent) then
            return True;
         end if;
      end loop;

      return False;
   end Has_Parent_Type;

   ------------------------
   -- Has_Private_Parent --
   ------------------------

   function Has_Private_Parent (E : Entity_Id) return Boolean is
   begin
      return E.Has_Private_Parent;
   end Has_Private_Parent;

   ------------------------------
   -- Has_Unknown_Discriminant --
   ------------------------------

   function Has_Unknown_Discriminants (E : Entity_Id) return Boolean is
   begin
      return E.Has_Unknown_Discriminants;
   end Has_Unknown_Discriminants;

   ---------------------
   -- In_Ada_Language --
   ---------------------

   function In_Ada_Language
     (E : Entity_Id) return Boolean is
   begin
      return Get_Language (E).all in Language.Ada.Ada_Language'Class;
   end In_Ada_Language;

   -------------------
   -- In_C_Language --
   -------------------

   function In_C_Or_CPP_Language
     (E : Entity_Id) return Boolean is
   begin
      return Get_Language (E).all in Language.C.C_Language'Class;
   end In_C_Or_CPP_Language;

   ---------------------
   -- In_Private_Part --
   ---------------------

   function In_Private_Part
     (E : Entity_Id) return Boolean is
   begin
      return E.In_Private_Part;
   end In_Private_Part;

   ------------------
   -- In_Same_File --
   ------------------

   function In_Same_File (E1, E2 : Entity_Id) return Boolean is
   begin
      pragma Assert (Present (E1));
      pragma Assert (Present (E2));

      return LL.Get_Location (E1).File = LL.Get_Location (E2).File;
   end In_Same_File;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Unique_Id := 0;
   end Initialize;

   --  Initialize internal state

   -------------------------
   -- Internal_New_Entity --
   -------------------------

   --  (Debugging) Suppose you find that entity 12345 is messed up. You might
   --  want to find the code that created that entity. The ways to do this is
   --  to set a conditional breakpoint:
   --     (gdb) break neb if id = 12345
   --  and run GNATdoc again from the beginning.

   procedure neb (Id : Natural);
   pragma Export (Ada, neb);

   procedure nee (Id : Natural);
   pragma Export (Ada, nee);

   procedure neb (Id : Natural) is
      pragma Unreferenced (Id);
   begin
      null;
   end neb;

   procedure nee (Id : Natural) is
      pragma Unreferenced (Id);
   begin
      null;
   end nee;

   procedure New_Entity_Begin (Id : Natural) renames neb;
   --  (gdb) Routine called at the beginning of the construction of the new
   --  entity with unique identifier Id

   procedure New_Entity_End (Id : Natural) renames nee;
   --  (gdb) Routine called at the end of the construction of the new entity
   --  with unique identifier Id

   function Internal_New_Entity
     (Context : access constant Docgen_Context;
      Lang    : Language_Access;
      E       : General_Entity;
      Loc     : General_Location;
      Name    : String := "";
      Is_Internal : Boolean := False) return Entity_Id
   is
      Db : General_Xref_Database renames Context.Database;

      procedure Complete_Decoration (New_E : Entity_Id);
      --  Complete the decoration of the Xref components. The decoration of
      --  other high-level components is done while traversing the tree since
      --  they require context information.

      ----------------------------
      -- Complete_Decoration --
      ----------------------------

      procedure Complete_Decoration
        (New_E : Entity_Id)
      is
         E : General_Entity renames New_E.Xref.Entity;

      begin
         --  Stage 1: Complete decoration of low-level attributes.

         New_E.Xref.Scope_E := Caller_At_Declaration (Db, E);

         if Is_Package (New_E) then
            New_E.Xref.Parent_Package := Xref.Parent_Package (Db, E);
         end if;

         New_E.Xref.Alias := Xref.Renaming_Of (Db, E);

         --  Protect GNATdoc against wrong information in the ALI file.
         --  We should investigate the compiler???

         if Present (LL.Get_Alias (New_E)) then
            declare
               Alias_Loc : General_Location;
            begin
               Alias_Loc := Get_Location (Db, LL.Get_Alias (New_E));

               if not Is_Spec_File (Context.Kernel, Alias_Loc.File) then
                  New_E.Xref.Alias := No_General_Entity;
               end if;
            end;
         end if;

         New_E.Xref.Etype    := Get_Type_Of (Db, E);
         New_E.Xref.Body_Loc := Get_Body (Db, E);

         New_E.Xref.Is_Type  := Xref.Is_Type (Db, E);

         --  Ada single tasks are not types (they are objects) but we handle
         --  them as tasks for homogeneity in the gnatdoc frontend. We cannot
         --  do the same here for single protected objects because they are
         --  decorated by Xref as E_Variable???

         if In_Ada_Language (New_E)
           and then not LL.Is_Type (New_E)
           and then Get_Kind (New_E) = E_Single_Task
         then
            New_E.Xref.Is_Type := True;
         end if;

         New_E.Xref.Is_Global    := Xref.Is_Global (Db, E);
         New_E.Xref.Is_Container := Xref.Is_Container (Db, E);
         New_E.Xref.Is_Abstract  := Xref.Is_Abstract (Db, E);

         if LL.Is_Type (New_E) then
            New_E.Xref.Is_Array  := Xref.Is_Array (Db, E);
            New_E.Xref.Is_Predef := Xref.Is_Predefined_Entity (Db, E);
            New_E.Xref.Is_Access := Xref.Is_Access (Db, E);

            if LL.Is_Access (New_E) then
               New_E.Xref.Pointed_Type := Xref.Pointed_Type (Db, E);

               --  Xref does not provide the expected info???
               --  pragma Assert
               --    (New_E.Xref.Pointed_Type /= No_General_Type);
            end if;
         else
            New_E.Xref.Is_Generic    := Is_Generic (Db, E);
            New_E.Xref.Is_Subprogram := Is_Subprogram (Db, E);
            New_E.Xref.Instance_Of   := Instance_Of (Db, E);

            if New_E.Xref.Is_Subprogram then
               New_E.Xref.Is_Abstract := Xref.Is_Abstract (Db, E);

               if Is_Primitive_Of (Db, E)'Length /= 0 then
                  New_E.Xref.Is_Primitive := True;
               end if;
            end if;
         end if;

         --  Stage 2: Add decoration of a few high-level components. The
         --  decoration of other high-level components is done while
         --  traversing the tree since they require context information.

         if LL.Is_Type (New_E) then
            if Is_Class_Or_Record_Type (New_E)
              or else Is_Concurrent_Type_Or_Object (New_E)
            then
               New_E.Xref.Has_Methods := Db.Has_Methods (E);

               if In_Ada_Language (New_E) then

                  --  Tasks and protected objects are decorated as Is_Tagged
                  --  since they may cover interface types.

                  if Get_Kind (New_E) = E_Interface
                    or else Is_Concurrent_Type_Or_Object (New_E)
                  then
                     Set_Is_Tagged (New_E);

                  else
                     --  Xref-bug: Xref.Has_Methods() is not reliable:
                     --  for Incomplete_Types I found that Xref reports
                     --  Has_Methods()=True for types that have no methods.
                     --  Similarly, for tagged limited records with primitives
                     --  it Xref.Has_Methods() returns False???

                     declare
                        All_Methods : constant Xref.Entity_Array :=
                          Methods (Db, E, Include_Inherited => True);
                     begin
                        if All_Methods'Length > 0 then
                           Set_Is_Tagged (New_E);
                           Set_Kind (New_E, E_Tagged_Record_Type);

                        --  last try

                        else
                           declare
                              Parents : constant Xref.Entity_Array :=
                                Parent_Types (Db, E, Recursive => False);
                              Has_Progenitors : constant Boolean :=
                                Parents'Length > 1;
                           begin
                              if Has_Progenitors then
                                 Set_Is_Tagged (New_E);
                                 Set_Kind (New_E, E_Tagged_Record_Type);
                              end if;
                           end;
                        end if;
                     end;
                  end if;
               end if;
            end if;

         elsif New_E.Xref.Is_Subprogram then

            --  (Xref): The value available through Xref.Get_Type is the same
            --  value returned by Xref.Returned_Type

            if Present (New_E.Xref.Etype) then
               Set_Kind (New_E, E_Function);
            end if;
         end if;

         --  Store all the references to types; required to support clickable
         --  types in the html output

         if LL.Is_Type (New_E) then
            declare
               Cursor : Entity_Reference_Iterator;
               Info   : Ref_Info;
               Ref    : General_Entity_Reference;

            begin
               Find_All_References
                 (Db, Cursor, LL.Get_Entity (New_E), Include_All => True);
               while not At_End (Cursor) loop
                  Ref  := Get (Cursor);
                  Info :=
                    Ref_Info'(Entity => New_E,
                              Ref    => Ref,
                              Loc    => Get_Location (Ref));

                  New_E.Xref.References.Append (Info);
                  Append_To_Map (Info);

                  Next (Cursor);
               end loop;

               Destroy (Cursor);
            end;
         end if;

         --  Store the location of the end of scope. For subprogram specs
         --  this information is not provided by Xref but it is needed by the
         --  frontend of Docgen to retrieve comments located after the spec;
         --  hence this attribute is currently set as part of retrieving the
         --  source of the subprogram specification.

         if Is_Package (New_E)
           or else Is_Concurrent_Type_Or_Object (New_E)
         then
            declare
               Cursor : Entity_Reference_Iterator;
               Ref    : General_Entity_Reference;
            begin
               Find_All_References
                 (Db, Cursor, LL.Get_Entity (New_E), Include_All => True);
               while not At_End (Cursor) loop
                  Ref := Get (Cursor);

                  if Get_Display_Kind (Ref) = "end of spec" then
                     exit;
                  elsif Get_Display_Kind (Ref) = "private part" then
                     New_E.Xref.First_Private_Entity_Loc := Get_Location (Ref);
                  end if;

                  Next (Cursor);
               end loop;

               Destroy (Cursor);
            end;
         end if;

      exception
         when E : others =>
            Trace (Me, E);
            raise;
      end Complete_Decoration;

      --  Local variables

      In_Ada_Lang : constant Boolean :=
                      Lang.all in Language.Ada.Ada_Language'Class;
      Q_Name : constant String :=
                 (if Name /= "" then Name else Qualified_Name (Db, E));
      S_Name : constant String :=
                 (if Name /= "" then Name else Get_Name (Db, E));
      Kind   : constant Entity_Kind :=
                 (if Present (E) then LL.Get_Ekind (Db, E, In_Ada_Lang)
                                 else E_Unknown);
      New_E  : Entity_Id;

      Xref_Loc : General_Location := Loc;
      --  Local variable used to workaround the wrong decoration of location

   --  Start of processing for Internal_New_Entity

   begin
      Unique_Id := Unique_Id + 1;
      New_Entity_Begin (Unique_Id);

      --  Workaround wrong decoration of Xref.Location which causes crash
      --  in the backend. To be investigated???

      if Xref_Loc.Column < 0 then
         Xref_Loc.Column := 1;
      end if;

      --  Initially E.Kind and E.Xref.Ekind are initialized with the same
      --  values. However, E.Kind may be decorated with other values at later
      --  stages based on the context. For example, an E_Variable may be
      --  redecorated as E_Formal (see docgen-frontend.adb)

      --  Similarly, E.File and E.Xref.Loc.File are initialized with the same
      --  values. However, for C/C++ entities defined in header files, E.File
      --  is updated to reference the corresponding .c (or .cpp) file.

      New_E :=
        new Entity_Info_Record'
          (Id       => Unique_Id,
           Language => Lang,
           Xref => Xref_Info'(
             Alias            => No_General_Entity,
             Body_Loc         => No_Location,
             Ekind            => Kind,
             Entity           => E,
             Etype            => No_General_Entity,

             First_Private_Entity_Loc  => No_Location,

             Instance_Of      => No_General_Entity,
             Loc              => Xref_Loc,
             Pointed_Type     => No_General_Entity,

             Scope_E          => No_General_Entity,
             Parent_Package   => No_General_Entity,

             Has_Methods   => False,

             Is_Abstract   => False,
             Is_Access     => False,
             Is_Array      => False,
             Is_Container  => False,
             Is_Global     => False,
             Is_Predef     => False,
             Is_Primitive  => False,
             Is_Subprogram => False,
             Is_Type       => False,
             Is_Generic    => False,

             Parent_Types  => <>,
             Child_Types   => <>,
             References    => <>),

           Full_Name       => Context.Kernel.Symbols.Find (Q_Name),
           Short_Name      => Context.Kernel.Symbols.Find (S_Name),
           Alias           => No_Entity,
           Kind            => Kind,

           Scope           => No_Entity,
           Parent_Package  => No_Entity,

           End_Of_Syntax_Scope_Loc => No_Location,
           End_Of_Profile_Location_In_Body => No_Location,
           Generic_Formals_Loc => No_Location,
           First_Private_Entity_Loc => No_Location,

           Has_Private_Parent => False,
           Has_Unknown_Discriminants => False,

           Has_Incomplete_Decoration => False,
           Is_Decorated              => False,

           In_Private_Part    => False,

           Is_Alias          => False,
           Is_Generic_Formal => False,
           Is_Internal       => Is_Internal,
           Is_Subtype        => False,
           Is_Tagged_Type    => False,
           Is_Incomplete     => False,
           Is_Private        => False,
           Idepth_Level      => 0,

           Doc_After         => No_Comment_Result,
           Doc_Before        => No_Comment_Result,

           Doc               => No_Comment_Result,
           Comment           => No_Structured_Comment,

           Full_View         => No_Entity,
           Partial_View      => No_Entity,

           Src               => Null_Unbounded_String,

           Entities           => <>,
           Generic_Formals    => <>,
           Inherited_Methods  => <>,
           Methods            => <>,
           Parent             => null,
           Progenitors        => <>,
           Direct_Derivations => <>,
           Error_Msg          => Null_Unbounded_String);

      --  Do not perform the full decoration of the entity for auxiliary
      --  entities created by the frontend (for example, the "standard"
      --  entity).

      if Present (E) and then not Is_Internal then
         Complete_Decoration (New_E);
      end if;

      New_Entity_End (New_E.Id);
      return New_E;
   end Internal_New_Entity;

   --------------
   -- Is_Alias --
   --------------

   function Is_Alias (E : Entity_Id) return Boolean is
   begin
      return E.Is_Alias;
   end Is_Alias;

   -------------------------
   -- Is_Compilation_Unit --
   -------------------------

   function Is_Compilation_Unit
     (E : Entity_Id) return Boolean is
   begin
      --  The Scope attribute is not available in entities of packages which
      --  are not part of the project (for example, entities defined in the
      --  runtime of the compiler)

      return Present (Get_Scope (E))
        and then Is_Standard_Entity (Get_Scope (E))
        and then (Is_Subprogram (E) or else Is_Package (E));
   end Is_Compilation_Unit;

   --------------------
   -- Is_Record_Type --
   --------------------

   function Is_Record_Type (E : Entity_Id) return Boolean is
   begin
      return Kind_In (LL_Get_Kind (E), E_Abstract_Record_Type,
                                       E_Record_Type)
        or else Kind_In (Get_Kind (E), E_Tagged_Record_Type,
                                       E_Interface);
   end Is_Record_Type;

   -----------------------------
   -- Is_Class_Or_Record_Type --
   -----------------------------

   function Is_Class_Or_Record_Type (E : Entity_Id) return Boolean is
   begin
      return Kind_In (LL_Get_Kind (E), E_Abstract_Record_Type,
                                       E_Record_Type)
        or else Kind_In (Get_Kind (E), E_Tagged_Record_Type,
                                       E_Interface,
                                       E_Class,
                                       E_Class_Wide_Type);
   end Is_Class_Or_Record_Type;

   ----------------------------------
   -- Is_Concurrent_Type_Or_Object --
   ----------------------------------

   function Is_Concurrent_Type_Or_Object
     (E : Entity_Id) return Boolean is
   begin
      return Kind_In (Get_Kind (E), E_Task_Type, E_Protected_Type)
        or else Kind_In (Get_Kind (E), E_Single_Task, E_Single_Protected);
   end Is_Concurrent_Type_Or_Object;

   ------------------
   -- Is_Decorated --
   ------------------

   function Is_Decorated (E : Entity_Id) return Boolean is
   begin
      return E.Is_Decorated;
   end Is_Decorated;

   ------------------
   -- Is_Full_View --
   ------------------

   function Is_Full_View (E : Entity_Id) return Boolean is
   begin
      return Present (Get_Partial_View (E));
   end Is_Full_View;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic (E : Entity_Id) return Boolean is
   begin
      --  We cannot rely only on the value provided by Xref since the value
      --  LL.Is_Generic is false in internal entities generated by the
      --  frontend to workaround missing entities associated with generics.

      return LL_Is_Generic (E)
        or else Kind_In (Get_Kind (E), E_Generic_Package,
                                       E_Generic_Function,
                                       E_Generic_Procedure);
   end Is_Generic;

   -----------------------
   -- Is_Generic_Formal --
   -----------------------

   function Is_Generic_Formal (E : Entity_Id) return Boolean is
   begin
      return E.Is_Generic_Formal;
   end Is_Generic_Formal;

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (E : Entity_Id) return Boolean is
   begin
      return E.Is_Incomplete;
   end Is_Incomplete;

   -----------------
   -- Is_Internal --
   -----------------

   function Is_Internal (E : Entity_Id) return Boolean is
   begin
      return E.Is_Internal;
   end Is_Internal;

   ----------------
   -- Is_Package --
   ----------------

   function Is_Package (E : Entity_Id) return Boolean is
   begin
      return
        Kind_In (LL_Get_Kind (E), E_Package,
                                  E_Generic_Package)
          or else
            (not Is_Standard_Entity (E)
               and then Kind_In (Get_Kind (E), E_Package,
                                               E_Generic_Package));
   end Is_Package;

   ---------------------
   -- Is_Partial_View --
   ---------------------

   function Is_Partial_View (E : Entity_Id) return Boolean is
   begin
      return Present (Get_Full_View (E));
   end Is_Partial_View;

   ----------------
   -- Is_Private --
   ----------------

   function Is_Private (E : Entity_Id) return Boolean is
   begin
      return E.Is_Private;
   end Is_Private;

   ------------------------
   -- Is_Standard_Entity --
   ------------------------

   function Is_Standard_Entity
     (E : Entity_Id) return Boolean is
   begin
      return E.Is_Internal
        and then Get_Short_Name (E) = Std_Entity_Name;
   end Is_Standard_Entity;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (E : Entity_Id) return Boolean is
   begin
      --  Xref has attribute Is_Subprogram in single task declarations and
      --  entries; we must explicitly exclude such cases to avoid confusing
      --  the frontend.

      return Get_Kind (E) = E_Procedure
        or else Get_Kind (E) = E_Function
        or else (E.Xref.Is_Subprogram
                  and then Get_Kind (E) /= E_Single_Task
                  and then Get_Kind (E) /= E_Entry);
   end Is_Subprogram;

   ----------------------------
   -- Is_Subprogram_Or_Entry --
   ----------------------------

   function Is_Subprogram_Or_Entry (E : Entity_Id) return Boolean is
   begin
      return Is_Subprogram (E) or else Get_Kind (E) = E_Entry;
   end Is_Subprogram_Or_Entry;

   ----------------
   -- Is_Subtype --
   ----------------

   function Is_Subtype (E : Entity_Id) return Boolean is
   begin
      return E.Is_Subtype;
   end Is_Subtype;

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged (E : Entity_Id) return Boolean is
   begin
      return E.Is_Tagged_Type;
   end Is_Tagged;

   -------------
   -- Kind_In --
   -------------

   function Kind_In
     (K  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind) return Boolean
   is
   begin
      return K = V1
        or else K = V2;
   end Kind_In;

   function Kind_In
     (K  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind) return Boolean
   is
   begin
      return K = V1
        or else K = V2
        or else K = V3;
   end Kind_In;

   function Kind_In
     (K  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind) return Boolean
   is
   begin
      return K = V1
        or else K = V2
        or else K = V3
        or else K = V4;
   end Kind_In;

   -------------------------
   -- Less_Than_Full_Name --
   -------------------------

   function Less_Than_Full_Name (Left, Right : Entity_Id) return Boolean is
   begin
      return To_Lower (Get (Left.Full_Name).all)
        < To_Lower (Get (Right.Full_Name).all);
   end Less_Than_Full_Name;

   --------------------------
   -- Less_Than_Short_Name --
   --------------------------

   function Less_Than_Short_Name (Left, Right : Entity_Id) return Boolean is
      Left_Lower  : constant String := To_Lower (Get (Left.Short_Name).all);
      Right_Lower : constant String := To_Lower (Get (Right.Short_Name).all);
   begin
      if Left_Lower = Right_Lower then
         return Less_Than_Loc (Left, Right);
      else
         return Left_Lower < Right_Lower;
      end if;
   end Less_Than_Short_Name;

   -------------------
   -- Less_Than_Loc --
   -------------------

   function Less_Than_Loc (Left, Right : Entity_Id) return Boolean is
      Left_Loc  : constant General_Location := LL.Get_Location (Left);
      Right_Loc : constant General_Location := LL.Get_Location (Right);
   begin
      if Left_Loc.File /= Right_Loc.File then
         return Base_Name (Left_Loc.File) < Base_Name (Right_Loc.File);
      elsif Left_Loc.Line < Right_Loc.Line then
         return True;
      else
         return Left_Loc.Line = Right_Loc.Line
           and then Left_Loc.Column < Right_Loc.Column;
      end if;
   end Less_Than_Loc;

   ------------------------
   -- Less_Than_Body_Loc --
   ------------------------

   function Less_Than_Body_Loc (Left, Right : Entity_Id) return Boolean is
      pragma Assert (Present (LL.Get_Body_Loc (Left)));
      pragma Assert (Present (LL.Get_Body_Loc (Right)));

      Left_Loc  : constant General_Location := LL.Get_Body_Loc (Left);
      Right_Loc : constant General_Location := LL.Get_Body_Loc (Right);
   begin
      if Left_Loc.File /= Right_Loc.File then
         return Base_Name (Left_Loc.File) < Base_Name (Right_Loc.File);
      elsif Left_Loc.Line < Right_Loc.Line then
         return True;
      else
         return Left_Loc.Line = Right_Loc.Line
           and then Left_Loc.Column < Right_Loc.Column;
      end if;
   end Less_Than_Body_Loc;

   -------------------------------------
   -- LL_Get_First_Private_Entity_Loc --
   -------------------------------------

   function LL_Get_First_Private_Entity_Loc
     (E : Entity_Id) return General_Location is
   begin
      return E.Xref.First_Private_Entity_Loc;
   end LL_Get_First_Private_Entity_Loc;

   ----------------------
   -- LL_Get_Full_Name --
   ----------------------

   function LL_Get_Full_Name (E : Entity_Id) return String is
   begin
      return Get (E.Full_Name).all;
   end LL_Get_Full_Name;

   -----------------
   -- LL_Get_Kind --
   -----------------

   function LL_Get_Kind (E : Entity_Id) return Entity_Kind is
   begin
      return E.Xref.Ekind;
   end LL_Get_Kind;

   -------------------
   -- LL_Is_Generic --
   -------------------

   function LL_Is_Generic (E : Entity_Id) return Boolean is
   begin
      return E.Xref.Is_Generic;
   end LL_Is_Generic;

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (Context  : access constant Docgen_Context;
      Language : Language_Access;
      E        : General_Entity;
      Loc      : General_Location) return Entity_Id is
   begin
      return
        Internal_New_Entity
          (Context => Context,
           Lang    => Language,
           E       => E,
           Loc     => Loc);
   end New_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Context  : access constant Docgen_Context;
      Language : Language_Access;
      Name     : String) return Entity_Id
   is
   begin
      return
        Internal_New_Entity
          (Context     => Context,
           Lang        => Language,
           E           => No_General_Entity,
           Loc         => No_Location,
           Name        => Name,
           Is_Internal => True);
   end New_Internal_Entity;

   --------
   -- No --
   --------

   function No (E : Entity_Id) return Boolean is
   begin
      return not Present (E);
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Entity_Id) return Boolean is
   begin
      return E /= null;
   end Present;

   function Present (List : access EInfo_List.Vector) return Boolean is
      Cursor : constant EInfo_List.Cursor := List.First;
   begin
      return EInfo_List.Has_Element (Cursor);
   end Present;

   ----------------------
   -- Remove_Full_View --
   ----------------------

   procedure Remove_Full_View (E : Entity_Id) is
   begin
      if Present (Get_Full_View (E)) then
         if Present (Get_Scope (Get_Full_View (E))) then
            Remove_From_Scope (Get_Full_View (E));
         end if;

         if Disable_Free then
            E.Full_View := null;
         else
            Free (E.Full_View);
         end if;
      end if;
   end Remove_Full_View;

   -----------------------
   -- Remove_From_Scope --
   -----------------------

   procedure Remove_From_Scope (E : Entity_Id) is
      Scope  : constant Entity_Id := Get_Scope (E);
   begin
      if No (Scope) then
         return;
      end if;

      if Get_Entities (Scope).Contains (E) then
         declare
            Cursor : EInfo_List.Cursor;
         begin
            Cursor := Get_Entities (Scope).Find (E);
            Get_Entities (Scope).Delete (Cursor);
         end;
      end if;

      if Is_Generic (Scope)
        and then Is_Generic_Formal (E)
        and then Get_Generic_Formals (Scope).Contains (E)
      then
         declare
            Cursor : EInfo_List.Cursor;
         begin
            Cursor := Get_Generic_Formals (Scope).Find (E);
            Get_Generic_Formals (Scope).Delete (Cursor);
         end;
      end if;

      Set_Scope (E, Atree.No_Entity);
   end Remove_From_Scope;

   ---------------
   -- Set_Alias --
   ---------------

   procedure Set_Alias (E : Entity_Id; Value : Entity_Id) is
   begin
      --  Using the sources of the gnat project there are wrong values
      --  in the renamings returned by Xref value. Hence for now we
      --  disable the assertion and protect GNATdoc aganinst wrong
      --  obvious values. To be investigated???

      --  pragma Assert (Get_Kind (E) = Get_Kind (Value));

      if Get_Kind (E) = Get_Kind (Value) then
         E.Alias := Value;
         Set_Is_Alias (E);
      end if;
   end Set_Alias;

   -----------------
   -- Set_Comment --
   -----------------

   procedure Set_Comment (E : Entity_Id; Value : Structured_Comment) is
   begin
      E.Comment := Value;
   end Set_Comment;

   -------------
   -- Set_Doc --
   -------------

   procedure Set_Doc (E : Entity_Id; Value : Comment_Result) is
   begin
      E.Doc := Value;
   end Set_Doc;

   -------------------
   -- Set_Doc_After --
   -------------------

   procedure Set_Doc_After (E : Entity_Id; Value : Comment_Result) is
   begin
      E.Doc_After := Value;
   end Set_Doc_After;

   --------------------
   -- Set_Doc_Before --
   --------------------

   procedure Set_Doc_Before (E : Entity_Id; Value : Comment_Result) is
   begin
      E.Doc_Before := Value;
   end Set_Doc_Before;

   ---------------------------------
   -- Set_End_Of_Syntax_Scope_Loc --
   ---------------------------------

   procedure Set_End_Of_Syntax_Scope_Loc
     (E : Entity_Id; Loc : General_Location) is
   begin
      E.End_Of_Syntax_Scope_Loc := Loc;
   end Set_End_Of_Syntax_Scope_Loc;

   -----------------------------------------
   -- Set_End_Of_Profile_Location_In_Body --
   -----------------------------------------

   procedure Set_End_Of_Profile_Location_In_Body
     (E : Entity_Id; Loc : General_Location) is
   begin
      E.End_Of_Profile_Location_In_Body := Loc;
   end Set_End_Of_Profile_Location_In_Body;

   -------------------------
   -- Set_In_Private_Part --
   -------------------------

   procedure Set_In_Private_Part (E : Entity_Id) is
   begin
      E.In_Private_Part := True;
   end Set_In_Private_Part;

   ----------------------
   -- Set_IDepth_Level --
   ----------------------

   procedure Set_IDepth_Level (E : Entity_Id) is
      P      : Entity_Id := E;
      Idepth : Natural := 0;
   begin
      if Is_Partial_View (P) then
         P := Get_Full_View (P);
      end if;

      pragma Assert (Is_Tagged (P));

      P := Get_Parent (P);
      while Present (P) loop
         Idepth := Idepth + 1;

         if Is_Partial_View (P) then
            P := Get_Full_View (P);
         end if;

         P := Get_Parent (P);
      end loop;

      E.Idepth_Level := Idepth;

      if Is_Full_View (E) then
         Get_Partial_View (E).Idepth_Level := Idepth;
      end if;
   end Set_IDepth_Level;

   -------------------
   -- Set_Error_Msg --
   -------------------

   procedure Set_Error_Msg   (E : Entity_Id; Value : Unbounded_String) is
   begin
      E.Error_Msg := Value;
   end Set_Error_Msg;

   ----------------------------------
   -- Set_First_Private_Entity_Loc --
   ----------------------------------

   procedure Set_First_Private_Entity_Loc
     (E : Entity_Id; Value : General_Location) is
   begin
      --  If the location of the first private entity is available in the
      --  database then such value is always located either in the same
      --  line computed by the GNATdoc frontend or after such location
      --  (if the sources have pragmas located after the reserved word
      --  "private").

      --  We tried to force the GNATdoc frontend to compute exactly the same
      --  location computed by the compiler but this approach was discarded
      --  because when there are pragmas located between the reserved word
      --  "private" then it is not clear the exact location computed by the
      --  compiler.

      if Present (LL_Get_First_Private_Entity_Loc (E)) then
         pragma Assert
           (LL_Get_First_Private_Entity_Loc (E).Line >= Value.Line);
      end if;

      E.First_Private_Entity_Loc := Value;
   end Set_First_Private_Entity_Loc;

   -------------------
   -- Set_Full_View --
   -------------------

   procedure Set_Full_View
     (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Full_View := Value;
   end Set_Full_View;

   -----------------------------
   -- Set_Generic_Formals_Loc --
   -----------------------------

   procedure Set_Generic_Formals_Loc
     (E : Entity_Id; Value : General_Location) is
   begin
      pragma Assert (Is_Generic (E));
      pragma Assert (No (Get_Generic_Formals_Loc (E)));
      E.Generic_Formals_Loc := Value;
   end Set_Generic_Formals_Loc;

   -----------------------------------
   -- Set_Has_Incomplete_Decoration --
   -----------------------------------

   procedure Set_Has_Incomplete_Decoration (E : Entity_Id) is
   begin
      E.Has_Incomplete_Decoration := True;
   end Set_Has_Incomplete_Decoration;

   ----------------------------
   -- Set_Has_Private_Parent --
   ----------------------------

   procedure Set_Has_Private_Parent (E : Entity_Id; Value : Boolean := True) is
   begin
      E.Has_Private_Parent := Value;
   end Set_Has_Private_Parent;

   ------------------
   -- Set_Is_Alias --
   ------------------

   procedure Set_Is_Alias (E : Entity_Id) is
   begin
      E.Is_Alias := True;
   end Set_Is_Alias;

   ----------------------
   -- Set_Is_Decorated --
   ----------------------

   procedure Set_Is_Decorated (E : Entity_Id) is
   begin
      E.Is_Decorated := True;
   end Set_Is_Decorated;

   ---------------------------
   -- Set_Is_Generic_Formal --
   ---------------------------

   procedure Set_Is_Generic_Formal (E : Entity_Id) is
   begin
      E.Is_Generic_Formal := True;
   end Set_Is_Generic_Formal;

   --------------------
   -- Set_Is_Private --
   --------------------

   procedure Set_Is_Incomplete (E : Entity_Id; Value : Boolean := True) is
   begin
      E.Is_Incomplete := Value;
   end Set_Is_Incomplete;

   --------------------
   -- Set_Is_Private --
   --------------------

   procedure Set_Is_Private (E : Entity_Id) is
   begin
      pragma Assert (not (Is_Private (E)));
      E.Is_Private := True;
   end Set_Is_Private;

   --------------------
   -- Set_Is_Subtype --
   --------------------

   procedure Set_Is_Subtype (E : Entity_Id) is
   begin
      E.Is_Subtype := True;
   end Set_Is_Subtype;

   -------------------
   -- Set_Is_Tagged --
   -------------------

   procedure Set_Is_Tagged (E : Entity_Id) is
   begin
      pragma Assert (Is_Class_Or_Record_Type (E)
        or else Is_Concurrent_Type_Or_Object (E));
      E.Is_Tagged_Type := True;
   end Set_Is_Tagged;

   -----------------------------------
   -- Set_Has_Unknown_Discriminants --
   -----------------------------------

   procedure Set_Has_Unknown_Discriminants (E : Entity_Id) is
   begin
      E.Has_Unknown_Discriminants := True;
   end Set_Has_Unknown_Discriminants;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (E : Entity_Id; Value : Entity_Kind) is
   begin
      E.Kind := Value;
   end Set_Kind;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (E : Entity_Id; Value : Entity_Id)
   is
   begin
      pragma Assert (Value /= E); --  Avoid circularity
      pragma Assert (LL.Is_Type (Value));
      E.Parent := Value;

      --  If the parent is not fully decorated (because it is an entity defined
      --  in the runtime of the compiler) we complete its decoration.

      if Is_Tagged (E) and then not Is_Tagged (E.Parent) then
         Set_Is_Tagged (E.Parent);
      end if;

      --  For backward compatibility, for private types, direct derivations are
      --  only attached to their partial view. This must be improved???

      declare
         Parent : constant Entity_Id :=
           (if not Is_Full_View (E.Parent) then E.Parent
            else Get_Partial_View (E.Parent));
         Derived : constant Entity_Id :=
           (if not Is_Full_View (E) then E
            else Get_Partial_View (E));
      begin
         if not Get_Direct_Derivations (Parent).Contains (Derived) then
            Append_Direct_Derivation (Parent, Derived);
         end if;
      end;
   end Set_Parent;

   ------------------------
   -- Set_Parent_Package --
   ------------------------

   procedure Set_Parent_Package
     (E : Entity_Id; Value : Entity_Id)
   is
   begin
      pragma Assert (Is_Package (E));
      E.Parent_Package := Value;
   end Set_Parent_Package;

   ----------------------
   -- Set_Partial_View --
   ----------------------

   procedure Set_Partial_View
     (E : Entity_Id; Value : Entity_Id)
   is
   begin
      E.Partial_View := Value;
   end Set_Partial_View;

   ---------------
   -- Set_Scope --
   ---------------

   procedure Set_Scope (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (Value /= E); --  Avoid circularity
      E.Scope := Value;

      if Present (Get_Full_View (E))
        and then In_Same_File (E, Get_Full_View (E))
      then
         Set_Scope (Get_Full_View (E), Value);
      end if;
   end Set_Scope;

   --------------------
   -- Set_Short_Name --
   --------------------

   procedure Set_Short_Name
     (Context : access constant Docgen_Context;
      E       : Entity_Id;
      Value   : String) is
   begin
      E.Short_Name := Context.Kernel.Symbols.Find (Value);
   end Set_Short_Name;

   -------------
   -- Set_Src --
   -------------

   procedure Set_Src (E : Entity_Id; Value : Unbounded_String) is
      Low  : Natural := 1;
      High : Natural := Length (Value);

      type T_Direction is (Forward, Backward);

      procedure Skip
        (C         : Character;
         Index     : in out Natural;
         Direction : T_Direction);
      --  Diplace Index in the specified direction skipping occurrences of C

      procedure Skip_Empty_Lines
        (Index     : in out Natural;
         Direction : T_Direction);
      --  Displace Index in the specified direction skipping empty lines

      ----------
      -- Skip --
      ----------

      procedure Skip
        (C : Character; Index : in out Natural; Direction : T_Direction)
      is
         Increment : constant Integer :=
                       (if Direction = Forward then 1 else -1);
      begin
         while Element (Value, Index) = C and then Index < High loop
            Index := Index + Increment;
         end loop;
      end Skip;

      ----------------------
      -- Skip_Empty_Lines --
      ----------------------

      procedure Skip_Empty_Lines
        (Index     : in out Natural;
         Direction : T_Direction)
      is
         J : Natural;
      begin
         Skip (ASCII.LF, Index, Direction);

         J := Index;
         while Element (Value, J) = ' ' and then J < High loop
            Skip (' ', J, Direction);

            if Element (Value, J) = ASCII.LF then
               Skip (ASCII.LF, J, Direction);
               Index := J;
            end if;
         end loop;
      end Skip_Empty_Lines;

   begin
      pragma Assert (Present (Value));

      --  Filter empty lines located at the beginning and end of Value

      Skip_Empty_Lines (Low, Forward);
      Skip_Empty_Lines (High, Backward);

      pragma Assert (No (Get_Src (E)));
      E.Src := Unbounded_Slice (Value, Low, High);
   end Set_Src;

   -------------------
   -- Traverse_Tree --
   -------------------

   procedure Traverse_Tree
     (Root    : Entity_Id;
      Process : access function
                          (Entity      : Entity_Id;
                           Scope_Level : Natural) return Traverse_Result)
   is
      procedure Do_Process (E_Info : Entity_Id; Scope_Level : Integer);
      procedure Do_Process (E_Info : Entity_Id; Scope_Level : Integer) is
         Cursor : EInfo_List.Cursor;
         Result : Traverse_Result;
      begin
         Result := Process (E_Info, Scope_Level);

         if Result = OK then
            if Is_Generic (E_Info)
              and then Present (Get_Generic_Formals (E_Info))
            then
               for Current of Get_Generic_Formals (E_Info).all loop
                  Do_Process (Current, Scope_Level + 1);
               end loop;
            end if;

            Cursor := Get_Entities (E_Info).First;
            while EInfo_List.Has_Element (Cursor) loop
               Do_Process (EInfo_List.Element (Cursor), Scope_Level + 1);

               EInfo_List.Next (Cursor);
            end loop;

            --  (Ada) Do not process primitives of tagged types since they are
            --  defined in the scope of the tagged type (and hence they would
            --  be erroneously processed twice!)

            if In_Ada_Language (E_Info) then
               if Get_Kind (E_Info) = E_Class then
                  Cursor := Get_Methods (E_Info).First;
                  while EInfo_List.Has_Element (Cursor) loop
                     Do_Process (EInfo_List.Element (Cursor), Scope_Level + 1);

                     EInfo_List.Next (Cursor);
                  end loop;
               end if;
            end if;
         end if;
      end Do_Process;

   begin
      Do_Process (Root, 0);
   end Traverse_Tree;

   -----------------------
   -- Package Low Level --
   -----------------------

   package body LL is

      procedure Append_Child_Type (E : Entity_Id; Value : Entity_Id) is
      begin
         if not E.Xref.Child_Types.Contains (Value) then
            E.Xref.Child_Types.Append (Value);
         end if;
      end Append_Child_Type;

      procedure Append_Parent_Type (E : Entity_Id; Value : Entity_Id) is
      begin
         pragma Assert (not Has_Parent_Type (E, Value));
         pragma Assert (not E.Xref.Parent_Types.Contains (Value));

         E.Xref.Parent_Types.Append (Value);
      end Append_Parent_Type;

      function Get_Alias (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Alias;
      end Get_Alias;

      function Get_Body_Loc (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Body_Loc;
      end Get_Body_Loc;

      function Get_Child_Types
        (E : Entity_Id) return access EInfo_List.Vector is
      begin
         return E.Xref.Child_Types'Access;
      end Get_Child_Types;

      function Get_Entity (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Entity;
      end Get_Entity;

      function Get_Instance_Of
        (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Instance_Of;
      end Get_Instance_Of;

      function Get_Location (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Loc;
      end Get_Location;

      function Get_Parent_Package (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Parent_Package;
      end Get_Parent_Package;

      function Get_Parent_Types
        (E : Entity_Id) return access EInfo_List.Vector is
      begin
         return E.Xref.Parent_Types'Access;
      end Get_Parent_Types;

      function Get_Pointed_Type (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Pointed_Type;
      end Get_Pointed_Type;

      function Get_Scope (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Scope_E;
      end Get_Scope;

      function Has_Methods (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Has_Methods;
      end Has_Methods;

      function Is_Abstract (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Abstract;
      end Is_Abstract;

      function Is_Access (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Access;
      end Is_Access;

      function Is_Array (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Array;
      end Is_Array;

      function Is_Global (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Global;
      end Is_Global;

      function Is_Predef (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Predef;
      end Is_Predef;

      function Is_Primitive (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Primitive;
      end Is_Primitive;

      ------------------------
      -- Is_Self_Referenced --
      ------------------------

      function Is_Self_Referenced_Type
        (Db   : General_Xref_Database;
         E    : General_Entity;
         Lang : Language_Access) return Boolean
      is
         In_C_Or_CPP_Lang : constant Boolean :=
                              Lang.all in Language.C.C_Language'Class;
      begin
         return
           In_C_Or_CPP_Lang
             and then Present (E)
             and then Db.Is_Type (E)
             and then Db.Is_Container (E)
              --  The value returned by Xref.Is_Container() is not
              --  fully reliable in Ada (for example, not set for
              --  e_class_wide_type???)
             and then Db.Is_Global (E)
             and then LL.Get_Ekind (Db, E, In_Ada_Lang => False)
                        = E_Record_Type
             and then Db.Get_Name (Db.Caller_At_Declaration (E))
                        = Db.Get_Name (E);
      end Is_Self_Referenced_Type;

      -------------
      -- Is_Type --
      -------------

      function Is_Type (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Type;
      end Is_Type;

      ---------------
      -- Get_Ekind --
      ---------------
      --  Source: gnatcoll-xref-database.adb
      --       at gnatlib/src/generated

      function Get_Ekind
        (Db          : General_Xref_Database;
         E           : General_Entity;
         In_Ada_Lang : Boolean) return Entity_Kind
      is
         Kind : constant String := Get_Display_Kind (Db, E);

      begin
         --  Variables, fields and parameters

         if Kind = "array"
           or else Kind = "boolean"
           or else Kind = "class wide"
           or else Kind = "decimal fixed point"
           or else Kind = "enumeration"
           or else Kind = "fixed point"
           or else Kind = "floating point"
           or else Kind = "integer"
           or else Kind = "pointer" -- out mode parameter
           or else Kind = "private object"
           or else Kind = "record"
           or else Kind = "string"
           or else Kind = "unsigned integer"
           or else Kind = "protected object"
         then
            return E_Variable;

         elsif Kind = "named number" then
            return E_Named_Number; -- constant

         elsif Kind = "abstract record type" then
            return E_Abstract_Record_Type;

         elsif Kind = "abstract procedure" then
            return E_Abstract_Procedure;

         elsif Kind = "abstract function" then
            return E_Abstract_Function;

         elsif Kind = "access type" then
            return E_Access_Type;

         elsif Kind = "array type" then
            return E_Array_Type;

         elsif Kind = "boolean type" then
            return E_Boolean_Type;

         elsif Kind = "class wide type" then
            return E_Class_Wide_Type;

         elsif Kind = "decimal fixed point type" then
            return E_Decimal_Fixed_Point_Type;

         elsif Kind = "enumeration type" then
            return E_Enumeration_Type;

         elsif Kind = "enumeration literal" then
            return E_Enumeration_Literal;

         elsif Kind = "entry" then
            return E_Entry;

         elsif Kind = "fixed point type" then
            return E_Floating_Point_Type;

         elsif Kind = "floating point type" then
            return E_Floating_Point_Type;

         elsif Kind = "function" then
            return E_Function;

         elsif Kind = "generic formal" then
            --  In practice this value is never returned by Xref. Xref bug???
            return E_Generic_Formal;

         elsif Kind = "generic package" then
            return E_Generic_Package;

         elsif Kind = "generic procedure" then
            return E_Generic_Procedure;

         elsif Kind = "generic function" then
            return E_Generic_Procedure;

         elsif Kind = "interface" then
            return E_Interface;  -- Interface type ???

         elsif Kind = "integer type"
           or else Kind = "unsigned integer type"
         then
            return E_Integer_Type;

         elsif Kind = "package" then
            return E_Package;

         elsif Kind = "procedure" then
            return E_Procedure;

         elsif Kind = "protected type" then
            return E_Protected_Type;

         elsif Kind = "record type" then
            return E_Record_Type;

         elsif Kind = "string type" then
            return E_String_Type;

         elsif Kind = "task" then
            return E_Single_Task;

         elsif Kind = "task type" then
            return E_Task_Type;

         elsif Kind = "exception" then
            return E_Exception;

            --  C/C++

         elsif Kind = "macro" then
            return E_Macro;

         elsif Kind = "function macro" then
            return E_Function_Macro;

         elsif Kind = "class" then
            return E_Class;

         elsif Kind = "class instance" then
            return E_Class_Instance;

         elsif Kind = "include file" then
            return E_Include_File;

         elsif Kind = "loop label"
           or else Kind = "block label"
           or else Kind = "statement label"
           or else Kind = "statement"
         then
            --  Should not be found in Ada since we are processing only
            --  specifications (bodies are not handled yet???)

            if In_Ada_Lang then
               pragma Assert (False);
            end if;

            return E_Unknown;

         else
            pragma Assert (False);
            return E_Unknown;
         end if;
      end Get_Ekind;

      procedure Set_Location
        (E : Entity_Id; Value : General_Location)
      is
         Xref_Loc : General_Location := Value;
      begin
         --  Workaround wrong decoration of Xref.Location which causes crash
         --  in the backend. To be investigated???

         if Xref_Loc.Column < 0 then
            Xref_Loc.Column := 1;
         end if;

         E.Xref.Loc := Xref_Loc;
      end Set_Location;

   end LL;

   -------------------------------------------------------------------------
   --                 Debugging routines for use in gdb                   --
   -------------------------------------------------------------------------

   -----------------------
   -- Register_Database --
   -----------------------

   Db : General_Xref_Database;

   procedure Register_Database (Database : General_Xref_Database) is
   begin
      Db := Database;
   end Register_Database;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (E              : Entity_Id;
      Prefix         : String := "";
      With_Full_Loc  : Boolean := False;
      With_Src       : Boolean := False;
      With_Doc       : Boolean := False;
      With_Errors    : Boolean := False;
      With_Unique_Id : Boolean := False;
      Reliable_Mode  : Boolean := True) return String
   is
      LL_Prefix : constant String := "xref: ";
      Printout  : aliased Unbounded_String;

      procedure Append_Entities
        (Vector : access EInfo_List.Vector;
         Header : String;
         Prefix : String);
      --  Append to Printout Header followed by all the entities in Vector
      --  (Prefix added before each entity)

      procedure Append_Entity
        (Prefix         : String;
         Entity         : Entity_Id;
         With_Full_Name : Boolean := False);
      --  Append to Printout "Prefix [Entity.Id] Entity.Name [Entity.Location]"

      procedure Append_Line (Text : String);
      --  Append to Printout "Prefix Text ASCII.LF"

      procedure Append_Line_Without_Prefix (Text : String);
      --  Append to Printout "Text & ASCII.LF"

      ---------------------
      -- Append_Entities --
      ---------------------

      procedure Append_Entities
        (Vector : access EInfo_List.Vector;
         Header : String;
         Prefix : String)
      is
         Cursor : EInfo_List.Cursor;

      begin
         Cursor := Vector.First;

         if EInfo_List.Has_Element (Cursor) then
            Append_Line (Header);
         end if;

         while EInfo_List.Has_Element (Cursor) loop
            Append_Entity
              (Prefix         => Prefix,
               Entity         => EInfo_List.Element (Cursor),
               With_Full_Name => True);
            EInfo_List.Next (Cursor);
         end loop;
      end Append_Entities;

      -------------------
      -- Append_Entity --
      -------------------

      procedure Append_Entity
        (Prefix         : String;
         Entity         : Entity_Id;
         With_Full_Name : Boolean := False)
      is
         Name : constant String :=
                  (if With_Full_Name then Get_Full_Name (Entity)
                                     else Get_Short_Name (Entity));
         UID  : constant String :=
                  (if not With_Unique_Id then ""
                   else "[" & To_String (Get_Unique_Id (Entity)) & "] ");

         In_Private : constant String :=
                        (if not In_Private_Part (Entity) then ""
                         else " (private)");
      begin
         Append_Line
           (Prefix
            & UID
            & Name
            & " ["
            & Image (LL.Get_Location (Entity))
            & "]"
            & In_Private);
      end Append_Entity;

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Prefix & Text & ASCII.LF;
      end Append_Line;

      --------------------------------
      -- Append_Line_Without_Prefix --
      --------------------------------

      procedure Append_Line_Without_Prefix (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line_Without_Prefix;

   --  Start of processing for To_String

   begin
      pragma Assert (Db /= null);

      if No (E) then
         return "";
      end if;

      declare
         UID : constant String :=
                 (if With_Unique_Id then To_String (E.Id) & ": "
                                    else "");
      begin
         --  The internally generated Standard entity is not fully decorated
         --  and hence we generate a minimum output.

         if Is_Standard_Entity (E) then
            Append_Line
              ("*** "
               & UID
               & Get_Short_Name (E)
               & " ("
               & Get_Kind (E)'Img
               & ")");

            return To_String (Printout);
         end if;

         Append_Line
           ("*** "
            & UID
            & Get_Short_Name (E)
            & " ("
            & Get_Kind (E)'Img
            & ":"
            & LL_Get_Kind (E)'Img
            & ")");
      end;

      --  Synthesized attributes

      Append_Line ("Full_Name: " & Get_Full_Name (E));

      if Present (Get_Parent_Package (E)) then
         Append_Entity ("Parent_Package: ", Get_Parent_Package (E));
      end if;

      if Present (Get_Scope (E)) then
         Append_Entity ("Scope: ", Get_Scope (E));
      end if;

      if Present (Get_End_Of_Syntax_Scope_Loc (E)) then
         if Reliable_Mode
           and then not Enhancements
           and then
             (Get_Kind (E) = E_Formal
                or else Get_Kind (E) = E_Variable
                or else Get_Kind (E) = E_Component
                or else Is_Generic_Formal (E)
                or else Get_Kind (E) = E_Generic_Formal
                or else Get_Kind (E) = E_Access_Type
                or else (LL.Is_Type (E)
                          and then not Is_Record_Type (E)
                          and then not Is_Concurrent_Type_Or_Object (E))
                or else Present (LL.Get_Instance_Of (E)))
         then
            null;

         elsif Is_Subprogram_Or_Entry (E) then
            Append_Line
              ("End_Of_Profile_Location: "
               & Image (Get_End_Of_Profile_Location (E)));
         else
            Append_Line
              ("End_Of_Syntax_Scope_Loc: "
               & Image (Get_End_Of_Syntax_Scope_Loc (E)));
         end if;
      end if;

      if Present (Get_Generic_Formals_Loc (E)) then
         Append_Line
           ("Generic_Formals_Location:"
            & Image (Get_Generic_Formals_Loc (E)));
      end if;

      if Present (Get_End_Of_Profile_Location_In_Body (E)) then
         Append_Line
           ("End_Of_Profile_Location_In_Body: "
            & Image (Get_End_Of_Profile_Location_In_Body (E)));
      end if;

      if Present (Get_Alias (E)) then
         Append_Entity ("Alias: ", Get_Alias (E));
      end if;

      if Present (Get_Parent (E)) then
         Append_Entity ("Parent: ", Get_Parent (E));
      end if;

      if Has_Private_Parent (E) then
         Append_Line ("Has_Private_Parent");
      end if;

      if Has_Unknown_Discriminants (E) then
         Append_Line ("Has_Unknown_Discriminants");
      end if;

      if In_Private_Part (E) then
         Append_Line ("In_Private_Part");
      end if;

      if Enhancements and then Is_Alias (E) then
         Append_Line ("Is_Alias");
      end if;

      if Is_Decorated (E) then
         Append_Line ("Is_Decorated");
      end if;

      if not Reliable_Mode
        and then E.Has_Incomplete_Decoration
      then
         Append_Line ("Has_Incomplete_Decoration");
      end if;

      if Is_Incomplete (E) then
         Append_Line ("Is_Incomplete");
      end if;

      if Is_Private (E) then
         Append_Line ("Is_Private");
      end if;

      if Is_Partial_View (E) then
         Append_Line
           ("Is_Partial_View "
            & "(Full_View_Loc: "
            & Image (LL.Get_Location (Get_Full_View (E)))
            & ")");

         if not Reliable_Mode then
            Append_Entity ("Full_View: ", Get_Full_View (E));
         end if;

      elsif Is_Full_View (E) then
         if Present (Get_Partial_View (E)) then
            Append_Line
              ("Is_Full_View "
               & "(Partial_View_Loc: "
               & Image (LL.Get_Location (Get_Partial_View (E)))
               & ")");

            if not Reliable_Mode then
               Append_Entity ("Partial_View: ", Get_Partial_View (E));
            end if;

         else
            Append_Line
              ("Is_Full_View");
         end if;
      end if;

      if Is_Subtype (E) then
         Append_Line ("Is_Subtype");
      end if;

      if Is_Tagged (E) then
         Append_Line ("Is_Tagged");

         if Enhancements then
            Append_Line ("  IDepth_Level:" & Get_IDepth_Level (E)'Img);
         end if;
      end if;

      if Is_Generic_Formal (E) then
         Append_Line ("Is_Generic_Formal");
      end if;

      if Is_Subprogram (E) then
         Append_Line ("Is_Subprogram");
      end if;

      --  Display record type discriminants, components, entries and
      --  subprograms

      if Is_Class_Or_Record_Type (E)
        or else Is_Concurrent_Type_Or_Object (E)
      then
         declare
            Discr : aliased EInfo_List.Vector :=
                      Get_Discriminants (E);
            Comp  : aliased EInfo_List.Vector :=
                      Get_Components (E);
            Ops   : aliased EInfo_List.Vector :=
                      Get_Subprograms_And_Entries (E);
         begin
            if Is_Tagged (E) then
               Append_Entities
                 (Vector => Get_Progenitors (E),
                  Header => "Progenitors",
                  Prefix => " - ");
               Append_Entities
                 (Vector => Get_Direct_Derivations (E),
                  Header => "Derivations",
                  Prefix => " - ");
            end if;

            Append_Entities
              (Vector => Discr'Access,
               Header => "Discriminants",
               Prefix => " - ");
            Append_Entities
              (Vector => Comp'Access,
               Header => "Components",
               Prefix => " - ");
            Append_Entities
              (Vector => Ops'Access,
               Header => "Subprograms and entries",
               Prefix => " - ");
         end;
      end if;

      --  Output information retrieved from Xref

      if With_Full_Loc then
         Append_Line
           (LL_Prefix
            & "Loc: "
            & (+LL.Get_Location (E).File.Dir_Name)
            & Image (LL.Get_Location (E)));
      else
         Append_Line
           (LL_Prefix
            & "Loc: "
            & Image (LL.Get_Location (E)));
      end if;

      if not Reliable_Mode then
         Append_Line
           (LL_Prefix
            & "Full_Name: "
            & LL_Get_Full_Name (E));
      end if;

      if Present (LL.Get_Body_Loc (E)) then
         Append_Line
           (LL_Prefix
            & "Body_Loc: " & Image (LL.Get_Body_Loc (E)));
      end if;

      if Enhancements
        and then Present (Get_First_Private_Entity_Loc (E))
      then
         Append_Line
           ("Private_Loc: "
            & Image (Get_First_Private_Entity_Loc (E)));
      end if;

      if Present (LL_Get_First_Private_Entity_Loc (E)) then
         Append_Line
           (LL_Prefix
            & "First_Private_Entity_Loc: "
            & Image (LL_Get_First_Private_Entity_Loc (E)));
      end if;

      declare
         End_Of_Scope_Loc : constant General_Location :=
           End_Of_Scope (Db, LL.Get_Entity (E));

      begin
         if Present (End_Of_Scope_Loc) then
            Append_Line
              (LL_Prefix
               & "End_Of_Scope_Loc: " & Image (End_Of_Scope_Loc));
         end if;
      end;

      if not Reliable_Mode
       and then Present (LL.Get_Parent_Package (E))
      then
         Append_Line
           (LL_Prefix
            & "Parent_Package: "
            & Get_Name (Db, LL.Get_Parent_Package (E))
            & " ["
            & Image (Get_Location (Db, LL.Get_Parent_Package (E)))
            & "]");
      end if;

      if Present (LL.Get_Scope (E)) then
         Append_Line
           (LL_Prefix
            & "Scope: "
            & Get_Name (Db, LL.Get_Scope (E))
            & " ["
            & Image (Get_Location (Db, LL.Get_Scope (E)))
            & "]");
      else
         Append_Line
           (LL_Prefix
            & "Scope: Unknown");
      end if;

      if Present (E.Xref.Etype) then
         Append_Line
           (LL_Prefix
            & "Etype: " & Get_Name (Db, E.Xref.Etype)
            & " [" & Image (Db, E.Xref.Etype) & "]");
      end if;

      if LL.Is_Access (E)
        and then Present (LL.Get_Pointed_Type (E))
      then
         Append_Line
           (LL_Prefix
            & "Pointed type: " & Get_Name (Db, LL.Get_Pointed_Type (E))
            & " [" & Image (Db, LL.Get_Pointed_Type (E)) & "]");
      end if;

      if Present (LL.Get_Alias (E)) then
         Append_Line
           (LL_Prefix
            & "Alias: " & Get_Name (Db, LL.Get_Alias (E))
            & " [" & Image (Db, LL.Get_Alias (E)) & "]");
      end if;

      if LL.Is_Abstract (E) then
         Append_Line
           (LL_Prefix
            & " Is_Abstract");
      end if;

      if LL.Is_Access (E) then
         Append_Line
           (LL_Prefix
            & " Is_Access");
      end if;

      if LL.Is_Array (E) then
         Append_Line
           (LL_Prefix
            & " Is_Array");
      end if;

      if E.Xref.Is_Container then
         Append_Line
           (LL_Prefix
            & " Is_Container");
      end if;

      if LL.Is_Global (E) then
         Append_Line
           (LL_Prefix
            & " Is_Global");
      end if;

      if LL.Is_Predef (E) then
         Append_Line
           (LL_Prefix
            & " Is_Predef");
      end if;

      if LL.Is_Type (E) then
         Append_Line
           (LL_Prefix
            & " Is_Type");
      end if;

      if LL.Has_Methods (E) then
         Append_Line
           (LL_Prefix
            & " Has_Methods");
      end if;

      --  Display record type components and dispatching primitives (methods)

      if Is_Class_Or_Record_Type (E)
        or else Is_Concurrent_Type_Or_Object (E)
      then
         Append_Entities
           (Vector => LL.Get_Parent_Types (E),
            Header => LL_Prefix & " Parent types",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => LL.Get_Child_Types (E),
            Header => LL_Prefix & " Child types",
            Prefix => LL_Prefix & " - ");

         if Is_Class_Or_Record_Type (E) then
            Append_Entities
              (Vector => Get_Entities (E),
               Header => LL_Prefix & " Components:",
               Prefix => LL_Prefix & " - ");
         else
            Append_Entities
              (Vector => Get_Entities (E),
               Header => LL_Prefix & " Entities:",
               Prefix => LL_Prefix & " - ");
         end if;

         Append_Entities
           (Vector => Get_Inherited_Methods (E),
            Header => LL_Prefix & " Inherited methods:",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => Get_Methods (E),
            Header => LL_Prefix & " Methods:",
            Prefix => LL_Prefix & " - ");
      end if;

      if E.Xref.Is_Subprogram then
         Append_Line (LL_Prefix & " Is_Subprogram");
      end if;

      if Is_Generic (E) then
         if Present (Get_Generic_Formals (E)) then
            declare
               Cursor : EInfo_List.Cursor;
               Formal : Entity_Id;

            begin
               Append_Line (LL_Prefix & " Generic Formals:");

               Cursor := Get_Generic_Formals (E).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Formal := EInfo_List.Element (Cursor);
                  pragma Assert (Is_Generic_Formal (Formal));

                  declare
                     UID : constant String :=
                             (if With_Unique_Id
                              then To_String (Formal.Id) & ": "
                              else "");
                  begin
                     Append_Line
                       (LL_Prefix
                        & " - "
                        & UID
                        & Image (LL.Get_Location (Formal))
                        & ":"
                        & Get_Short_Name (Formal));
                  end;

                  EInfo_List.Next (Cursor);
               end loop;
            end;
         end if;
      end if;

      if E.Xref.Is_Subprogram then
         if Has_Formals (E) then
            declare
               Cursor : EInfo_List.Cursor;
               Formal : Entity_Id;

            begin
               Append_Line (LL_Prefix & " Formals:");

               Cursor := Get_Entities (E).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Formal := EInfo_List.Element (Cursor);

                  declare
                     UID : constant String :=
                             (if With_Unique_Id
                              then To_String (Formal.Id) & ": "
                              else "");
                     Suffix : constant String :=
                       (if Get_Kind (Formal) = E_Formal then ""
                           else Get_Kind (Formal)'Img & " ???");
                  begin
                     Append_Line
                       (LL_Prefix
                        & " - "
                        & UID
                        & Image (LL.Get_Location (Formal))
                        & ":"
                        & Get_Short_Name (Formal)
                        & Suffix);
                  end;

                  EInfo_List.Next (Cursor);
               end loop;
            end;
         end if;
      end if;

      if LL.Is_Primitive (E) then
         Append_Line
           (LL_Prefix
            & " Is_Primitive");
      end if;

      if LL_Is_Generic (E) then
         Append_Line
           (LL_Prefix
            & " Is_Generic");

      elsif Present (LL.Get_Instance_Of (E)) then
         Append_Line
           (LL_Prefix
            & "Instance_Of: "
            & Image (Get_Location (Db, LL.Get_Instance_Of (E)))
            & ":"
            & Get_Name (Db, LL.Get_Instance_Of (E)));
      end if;

      if not Reliable_Mode
        and then LL.Is_Type (E)
      then
         declare
            Cursor : Ref_List.Cursor;
         begin
            Append_Line (LL_Prefix & "References:");

            Cursor := E.Xref.References.First;
            while Ref_List.Has_Element (Cursor) loop
               Append_Line
                 (LL_Prefix
                  & "- "
                  & Image (Ref_List.Element (Cursor).Loc));
               Ref_List.Next (Cursor);
            end loop;
         end;
      end if;

      if With_Src then
         Append_Line ("Partial View Src:");
         Append_Line (To_String (Get_Src (E)));

         if Present (Get_Full_View (E)) then
            Append_Line ("Full View Src:");
            Append_Line (To_String (Get_Src (Get_Full_View (E))));
         end if;
      end if;

      if With_Doc then
         if Present (Get_Doc_Before (E)) then
            Append_Line
              ("Doc_Before.Line:" & Get_Doc_Before (E).Start_Line'Img);
            Append_Line
              ("Doc_Before.Text: " & To_String (Get_Doc_Before (E).Text));
         end if;

         if Present (Get_Doc_After (E)) then
            Append_Line
              ("Doc_After.Line:" & Get_Doc_After (E).Start_Line'Img);
            Append_Line
              ("Doc_After.Text: " & To_String (Get_Doc_After (E).Text));
         end if;

         if Present (Get_Doc (E)) then
            Append_Line ("Doc.Line:" & Get_Doc (E).Start_Line'Img);
            Append_Line ("Doc.Text: " & To_String (Get_Doc (E).Text));
         end if;

         if Present (Get_Comment (E)) then
            Append_Line ("Structured Comment:");

            --  Append the comment avoiding the duplicate addition of the
            --  prefix to the output

            Append_Line_Without_Prefix
              (Ada.Strings.Unbounded.To_String
                 (To_Unbounded_String (Get_Comment (E), Prefix => Prefix)));
         end if;
      end if;

      if With_Errors
        and then Present (Get_Error_Msg (E))
      then
         Append_Line (To_String (Get_Error_Msg (E)));
      end if;

      return To_String (Printout);
   end To_String;

   --  **************************************************************
   --                         Debugging Routines
   --  **************************************************************

   ----------
   -- name --
   ----------

   function name
     (Db : General_Xref_Database;
      E  : General_Entity) return String is
   begin
      return Get_Name (Db, E);
   end name;

   --------
   -- pl --
   --------

   procedure pl (E : Entity_Id) is
      Cursor : EInfo_List.Cursor;
   begin
      GNAT.IO.Put ("List of entities of ");
      pns (E);

      Cursor := Get_Entities (E).First;
      while EInfo_List.Has_Element (Cursor) loop
         pns (EInfo_List.Element (Cursor));
         EInfo_List.Next (Cursor);
      end loop;
   end pl;

   ----------
   -- ploc --
   ----------

   procedure ploc (E : Entity_Id) is
   begin
      if Present (LL.Get_Location (E)) then
         GNAT.IO.Put_Line
           ((+LL.Get_Location (E).File.Dir_Name)
             & Image (LL.Get_Location (E)));
      else
         GNAT.IO.Put_Line ("<no location>");
      end if;
   end ploc;

   --------
   -- pn --
   --------

   procedure pn (E : Entity_Id) is
   begin
      --  Reliable_Mode is set to False to display all the information of the
      --  entity (including unreliable LL attributes) when debugging.

      GNAT.IO.Put_Line
        (To_String (E,
           With_Src => True,
           With_Doc => True,
           With_Full_Loc => True,
           With_Unique_Id => True,
           Reliable_Mode => False));
   end pn;

   ---------
   -- pns --
   ---------

   procedure pns (E : Entity_Id) is
   begin
      if No (E) then
         GNAT.IO.Put_Line ("<No entity>");
      else
         GNAT.IO.Put_Line
           ("["
            & To_String (Get_Unique_Id (E))
            & "] "
            & Get_Short_Name (E) & " "
            & Image (LL.Get_Location (E)));
      end if;
   end pns;

   procedure pns (Db : General_Xref_Database; E : General_Entity) is
   begin
      if No (E) then
         GNAT.IO.Put_Line ("<No entity>");
      else
         declare
            Loc  : constant General_Location := Get_Location (Db, E);
            Name : constant String := Get_Name (Db, E);
         begin
            GNAT.IO.Put_Line (Name & " " & Image (Loc));
         end;
      end if;
   end pns;

   ----------
   -- pnsb --
   ----------

   procedure pnsb (E : Entity_Id) is
   begin
      if No (E) then
         GNAT.IO.Put_Line ("<No entity>");
      else
         GNAT.IO.Put_Line
           ("["
            & To_String (Get_Unique_Id (E))
            & "] "
            & Get_Short_Name (E) & " "
            & Image (LL.Get_Body_Loc (E)));
      end if;
   end pnsb;

   --------
   -- pv --
   --------

   procedure pv (V : EInfo_List.Vector) is
      Cursor  : EInfo_List.Cursor;

   begin
      Cursor := V.First;
      while EInfo_List.Has_Element (Cursor) loop
         pns (EInfo_List.Element (Cursor));
         EInfo_List.Next (Cursor);
      end loop;
   end pv;

   procedure pv (Db : General_Xref_Database; V : Xref.Entity_Array) is
   begin
      for J in V'Range loop
         pns (Db, V (J));
      end loop;
   end pv;

   ---------
   -- pvb --
   ---------

   procedure pvb (V : EInfo_List.Vector) is
      Cursor  : EInfo_List.Cursor;

   begin
      Cursor := V.First;
      while EInfo_List.Has_Element (Cursor) loop
         pnsb (EInfo_List.Element (Cursor));
         EInfo_List.Next (Cursor);
      end loop;
   end pvb;

end GNATdoc.Atree;
