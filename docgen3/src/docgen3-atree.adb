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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;     use Ada.Characters.Handling;

with Docgen3.Utils;               use Docgen3.Utils;
with Language.Ada;
with Language.C;
with Language.Tree;               use Language.Tree;
with Language.Tree.Database;      use Language.Tree.Database;
with Traces;                      use Traces;

with GNAT.IO; --  For output of debugging routines

package body Docgen3.Atree is

   Unique_Id : Natural := 0;
   --  Internal counter used to associate an unique identifier to all the
   --  nodes.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Contains
     (Container : EInfo_List.Vector;
      Entity    : Entity_Id) return Boolean;
   --  Return True if the container has an Entity whose location matches the
   --  location of Entity.

   function Internal_New_Entity
     (Context     : access constant Docgen_Context;
      Lang        : Language_Access;
      E           : General_Entity;
      Loc         : General_Location;
      Name        : String := "";
      Is_Internal : Boolean := False) return Entity_Id;
   --  Internal subprogram which factorizes the code needed by routines
   --  New_Entity and New_Internal_Entity to create a new entity.

   procedure LL_Set_Full_View (E : Entity_Id; Value : General_Entity);

   procedure Set_Is_Incomplete_Or_Private_Type (E : Entity_Id);
   --  Set to true field E.Is_Incomplete_Or_Private_Type

   -----------------------
   -- Append_Derivation --
   -----------------------

   procedure Append_Derivation (E : Entity_Id; Value : Entity_Id) is
   begin
      if not Contains (E.Derivations, Value) then
         E.Derivations.Append (Value);
      end if;
   end Append_Derivation;

   -------------------------
   -- Append_Discriminant --
   -------------------------

   procedure Append_Discriminant (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Discriminants.Append (Value);
   end Append_Discriminant;

   -----------------------------
   -- Append_Inherited_Method --
   -----------------------------

   procedure Append_Inherited_Method
     (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (not Contains (E.Methods, Value));
      E.Inherited_Methods.Append (Value);
   end Append_Inherited_Method;

   -------------------
   -- Append_Method --
   -------------------

   procedure Append_Method (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Methods.Append (Value);
   end Append_Method;

   -----------------------
   -- Append_Progenitor --
   -----------------------

   procedure Append_Progenitor (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (not E.Progenitors.Contains (Value));
      E.Progenitors.Append (Value);
   end Append_Progenitor;

   ---------------------
   -- Append_To_Scope --
   ---------------------

   procedure Append_To_Scope (E : Entity_Id; Value : Entity_Id) is

      function Check_Unique return Boolean;
      --  Check that Value is not added twice to the list of entities of E

      function Check_Unique return Boolean is
         Cursor : EInfo_List.Cursor;
      begin
         Cursor := Get_Entities (E).First;
         while EInfo_List.Has_Element (Cursor) loop
            if EInfo_List.Element (Cursor) = E then
               return False;
            end if;

            EInfo_List.Next (Cursor);
         end loop;

         return True;
      end Check_Unique;

   begin
      pragma Assert (Check_Unique);
      E.Entities.Append (Value);
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
      Loc     : constant General_Location := LL.Get_Location (Entity);
      Cursor  : EInfo_List.Cursor;

   begin
      if Loc /= No_Location then
         Cursor := Container.First;
         while EInfo_List.Has_Element (Cursor) loop
            if LL.Get_Location (EInfo_List.Element (Cursor)) = Loc then
               return True;
            end if;

            EInfo_List.Next (Cursor);
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
      Cursor : EInfo_List.Cursor;
      E      : Entity_Id;

   begin
      if not EInfo_List.Has_Element (List.First) then
         return No_Entity;
      end if;

      Cursor := List.First;
      while EInfo_List.Has_Element (Cursor) loop
         E := EInfo_List.Element (Cursor);

         if LL.Get_Entity (E) = Entity then
            return E;
         end if;

         EInfo_List.Next (Cursor);
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
      Internal_Free (E);
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
      for J in List.First_Index .. List.Last_Index loop
         List.Update_Element (J, EInfo_List_Free'Access);
      end loop;

      List.Clear;
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

   ---------------------
   -- Get_Derivations --
   ---------------------

   function Get_Derivations
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Derivations'Access;
   end Get_Derivations;

   -----------------------
   -- Get_Discriminants --
   -----------------------

   function Get_Discriminants
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Discriminants'Access;
   end Get_Discriminants;

   -------------
   -- Get_Doc --
   -------------

   function Get_Doc (E : Entity_Id) return Comment_Result is
   begin
      return E.Doc;
   end Get_Doc;

   function Get_End_Of_Syntax_Scope_Loc
     (E : Entity_Id) return General_Location is
   begin
      return E.End_Of_Syntax_Scope_Loc;
   end Get_End_Of_Syntax_Scope_Loc;

   ------------------
   -- Get_Entities --
   ------------------

   function Get_Entities (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Entities'Access;
   end Get_Entities;

   -------------------
   -- Get_Error_Msg --
   -------------------

   function Get_Error_Msg (E : Entity_Id) return Unbounded_String is
   begin
      return E.Error_Msg;
   end Get_Error_Msg;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name (E : Entity_Id) return String is
   begin
      return Get (E.Full_Name).all;
   end Get_Full_Name;

   ---------------------------
   -- Get_Full_View_Comment --
   ---------------------------

   function Get_Full_View_Comment (E : Entity_Id) return Structured_Comment is
   begin
      return E.Full_View_Comment;
   end Get_Full_View_Comment;

   -----------------------
   -- Get_Full_View_Doc --
   -----------------------

   function Get_Full_View_Doc (E : Entity_Id) return Comment_Result is
   begin
      return E.Full_View_Doc;
   end Get_Full_View_Doc;

   -----------------------
   -- Get_Full_View_Src --
   -----------------------

   function Get_Full_View_Src (E : Entity_Id) return Unbounded_String is
   begin
      return E.Full_View_Src;
   end Get_Full_View_Src;

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
      return E.Parent;
   end Get_Parent;

   ---------------------
   -- Get_Progenitors --
   ---------------------

   function Get_Progenitors
     (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Progenitors'Access;
   end Get_Progenitors;

   ------------------
   -- Get_Ref_File --
   ------------------

   function Get_Ref_File
     (E : Entity_Id) return Virtual_File is
   begin
      return E.Ref_File;
   end Get_Ref_File;

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

   function Get_Short_Name   (E : Entity_Id) return String is
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

      EInfo_Vector_Sort_Full.Sort (Aux_List);

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
   --  and run GPS again from the beginning.

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

         New_E.Xref.Scope_E   := Caller_At_Declaration (Db, E);
         New_E.Xref.Scope_Loc := Get_Location (Db, New_E.Xref.Scope_E);

         New_E.Xref.Alias     := Xref.Renaming_Of (Db, E);
         New_E.Xref.Etype     := Get_Type_Of (Db, E);
         New_E.Xref.Body_Loc  := Get_Body (Db, E);

         New_E.Xref.End_Of_Scope_Loc := End_Of_Scope (Db, E);

         New_E.Xref.Is_Type   := Xref.Is_Type (Db, E);

         --  (Ada) Interfaces are NOT decorated as types by Xref???

         if In_Ada_Language (New_E)
           and then not New_E.Xref.Is_Type
           and then Get_Kind (New_E) = E_Interface
         then
            New_E.Xref.Is_Type := True;
         end if;

         New_E.Xref.Is_Global    := Xref.Is_Global (Db, E);
         New_E.Xref.Is_Container := Xref.Is_Container (Db, E);
         New_E.Xref.Is_Abstract  := Xref.Is_Abstract (Db, E);

         if New_E.Xref.Is_Type then
            New_E.Xref.Is_Array      := Xref.Is_Array (Db, E);
            New_E.Xref.Is_Predef     := Xref.Is_Predefined_Entity (Db, E);
            New_E.Xref.Is_Access     := Xref.Is_Access (Db, E);

            if New_E.Xref.Is_Access then
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

               if Present (Is_Primitive_Of (Db, E)) then
                  New_E.Xref.Is_Primitive := True;
               end if;
            end if;
         end if;

         --  Stage 2: Add decoration of a few high-level components. The
         --  decoration of other high-level components is done while
         --  traversing the tree since they require context information.

         if New_E.Xref.Is_Type then

            if No (New_E.Xref.Etype)
              and then Present (New_E.Xref.Body_Loc)
            then
               Set_Is_Incomplete_Or_Private_Type (New_E);
            end if;

            if Is_Class_Or_Record_Type (New_E) then
               New_E.Xref.Has_Methods := Db.Has_Methods (E);

               if In_Ada_Language (New_E) then
                  if Get_Kind (New_E) = E_Interface then
                     Set_Is_Tagged_Type (New_E);

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
                           Set_Is_Tagged_Type (New_E);
                           Set_Kind (New_E, E_Tagged_Record_Type);

                        --  last try

                        else
                           declare
                              Parents : constant Xref.Entity_Array :=
                                Parent_Types (Db, E, Recursive => False);
                           begin
                              if Parents'Length > 0 then
                                 Set_Is_Tagged_Type (New_E);
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

         if Present (LL.Get_Body_Loc (New_E))
           and then (LL.Is_Type (New_E) or else Get_Kind (New_E) = E_Variable)
         then
            if (LL.Get_Body_Loc (New_E).File
                  = LL.Get_Location (New_E).File)
              and then (LL.Get_Body_Loc (New_E).Line
                          > LL.Get_Location (New_E).Line)
            then
               Set_Is_Partial_View (New_E);

               --  The entity associated with the full view is not always
               --  available (since the compiler does not generate it). Unclear
               --  if this is a bug in the compiler. To be investigated???

               --  In such cases Xref returns the entity associated with the
               --  partial view when we request the entity of its full view.

               declare
                  Full_View_E : General_Entity;

               begin
                  Full_View_E :=
                    Xref.Get_Entity
                      (Db   => Context.Database,
                       Name => Get_Short_Name (New_E),
                       Loc  => LL.Get_Body_Loc (New_E));

                  if Full_View_E /= LL.Get_Entity (New_E) then
                     LL_Set_Full_View (New_E, Full_View_E);
                  end if;
               end;
            end if;
         end if;

         --  Store the location of the end of scope. For subprogram specs
         --  this information is not provided by Xref but it is needed by the
         --  frontend of Docgen to retrieve comments located after the spec;
         --  hence this attribute is currently set as part of retrieving the
         --  source of the subprogram specification.

         if Is_Package (New_E) then
            declare
               Cursor : Entity_Reference_Iterator;
               Ref    : General_Entity_Reference;
            begin
               Find_All_References
                 (Db, Cursor, LL.Get_Entity (New_E), Include_All => True);
               while not At_End (Cursor) loop
                  Ref := Get (Cursor);

                  if Get_Display_Kind (Ref) = "end of spec" then
                     New_E.End_Of_Syntax_Scope_Loc := Get_Location (Ref);
                     exit;
                  end if;

                  Next (Cursor);
               end loop;

               Destroy (Cursor);
            end;

         elsif not LL.Is_Subprogram (New_E) then
            New_E.End_Of_Syntax_Scope_Loc := LL.Get_End_Of_Scope_Loc (New_E);
         end if;

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return;
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

   --  Start of processing for Internal_New_Entity

   begin
      Unique_Id := Unique_Id + 1;
      New_Entity_Begin (Unique_Id);

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
           Ref_File => Loc.File,
           Xref => Xref_Info'(
             Alias            => No_General_Entity,
             Body_Loc         => No_Location,
             Ekind            => Kind,
             End_Of_Scope_Loc => No_Location,
             Entity           => E,
             Etype            => No_General_Entity,
             Full_View        => No_General_Entity,
             Instance_Of      => No_General_Entity,
             Loc              => Loc,
             Pointed_Type     => No_General_Entity,
             Scope_E          => No_General_Entity,
             Scope_Loc        => No_Location,

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
             Child_Types   => <>),

           Full_Name       => Context.Kernel.Symbols.Find (Q_Name),
           Short_Name      => Context.Kernel.Symbols.Find (S_Name),
           Alias           => No_Entity,
           Scope           => No_Entity,
           Kind            => Kind,
           End_Of_Syntax_Scope_Loc => No_Location,

           Is_Doc_From_Body  => False,
           Is_Generic_Formal => False,
           Is_Incomplete_Or_Private_Type => False,
           Is_Internal       => Is_Internal,
           Is_Tagged_Type    => False,
           Is_Private        => False,
           Is_Partial_View   => False,
           Idepth_Level      => 0,
           --  Inheritance depth level of the tagged type.

           Doc               => No_Comment_Result,
           Comment           => No_Structured_Comment,

           Full_View_Doc     => No_Comment_Result,
           Full_View_Comment => No_Structured_Comment,

           Src               => Null_Unbounded_String,
           Full_View_Src     => Null_Unbounded_String,

           Discriminants     => <>,
           Entities          => <>,
           Inherited_Methods => <>,
           Methods           => <>,
           Parent            => null,
           Progenitors       => <>,
           Derivations       => <>,
           Error_Msg         => Null_Unbounded_String);

      --  Do not perform the full decoration of the entity for auxiliary
      --  entities created by the frontend (for example, the "standard"
      --  entity).

      if Present (E) then
         Complete_Decoration (New_E);
      end if;

      New_Entity_End (New_E.Id);
      return New_E;
   end Internal_New_Entity;

   -----------------------------
   -- Is_Class_Or_Record_Type --
   -----------------------------

   function Is_Class_Or_Record_Type (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Present (E)
        and then Get_Kind (E) /= E_Unknown);

      return E.Xref.Ekind = E_Abstract_Record_Type
        or else E.Xref.Ekind = E_Record_Type
        or else E.Kind = E_Tagged_Record_Type
        or else E.Kind = E_Interface
        or else E.Kind = E_Class
        or else E.Kind = E_Class_Wide_Type;
   end Is_Class_Or_Record_Type;

   ------------------
   -- Is_Full_View --
   ------------------

   function Is_Full_View (E : Entity_Id) return Boolean is
   begin
      return Present (LL.Get_Body_Loc (E))
        and then LL.Get_Body_Loc (E).Line
                   < LL.Get_Location (E).Line;
   end Is_Full_View;

   ----------------------
   -- Is_Doc_From_Body --
   ----------------------

   function Is_Doc_From_Body (E : Entity_Id) return Boolean is
   begin
      return E.Is_Doc_From_Body;
   end Is_Doc_From_Body;

   -----------------------
   -- Is_Generic_Formal --
   -----------------------

   function Is_Generic_Formal (E : Entity_Id) return Boolean is
   begin
      return E.Is_Generic_Formal;
   end Is_Generic_Formal;

   -----------------------------------
   -- Is_Incomplete_Or_Private_Type --
   -----------------------------------

   function Is_Incomplete_Or_Private_Type (E : Entity_Id) return Boolean is
   begin
      return E.Is_Incomplete_Or_Private_Type;
   end Is_Incomplete_Or_Private_Type;

   ----------------
   -- Is_Package --
   ----------------

   function Is_Package (E : Entity_Id) return Boolean is
   begin
      return Kind_In (LL.Get_Kind (E), E_Package,
                                       E_Generic_Package);
   end Is_Package;

   ---------------------
   -- Is_Partial_View --
   ---------------------

   function Is_Partial_View (E : Entity_Id) return Boolean is
   begin
      return E.Is_Partial_View;
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

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged_Type (E : Entity_Id) return Boolean is
   begin
      return E.Is_Tagged_Type;
   end Is_Tagged_Type;

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
   begin
      return To_Lower (Get (Left.Short_Name).all) <
        To_Lower (Get (Right.Short_Name).all);
   end Less_Than_Short_Name;

   -------------------
   -- Less_Than_Loc --
   -------------------

   function Less_Than_Loc (Left, Right : Entity_Id) return Boolean is
      Left_Loc  : constant General_Location := LL.Get_Location (Left);
      Right_Loc : constant General_Location := LL.Get_Location (Right);
   begin
      if Left_Loc.File /= Right_Loc.File then
         return False;
      elsif Left_Loc.Line < Right_Loc.Line then
         return True;
      else
         return Left_Loc.Line = Right_Loc.Line
           and then Left_Loc.Column < Right_Loc.Column;
      end if;
   end Less_Than_Loc;

   ----------------------
   -- LL_Set_Full_View --
   ----------------------

   procedure LL_Set_Full_View (E : Entity_Id; Value : General_Entity) is
   begin
      pragma Assert (Is_Partial_View (E));
      pragma Assert (No (E.Xref.Full_View));
      pragma Assert (Present (LL.Get_Entity (E)));
      pragma Assert (Value /= LL.Get_Entity (E)); --  Avoid wrong circularity
      E.Xref.Full_View := Value;
   end LL_Set_Full_View;

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

   -----------------------
   -- Remove_From_Scope --
   -----------------------

   procedure Remove_From_Scope (E : Entity_Id) is
      Scope  : constant Entity_Id := Get_Scope (E);
      Cursor : EInfo_List.Cursor;
   begin
      pragma Assert (Present (Scope));
      pragma Assert (Get_Entities (Scope).Contains (E));

      Cursor := Scope.Entities.Find (E);
      Scope.Entities.Delete (Cursor);
   end Remove_From_Scope;

   ---------------
   -- Set_Alias --
   ---------------

   procedure Set_Alias (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Alias := Value;
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

   ---------------------------------
   -- Set_End_Of_Syntax_Scope_Loc --
   ---------------------------------

   procedure Set_End_Of_Syntax_Scope_Loc
     (E : Entity_Id; Loc : General_Location) is
   begin
      E.End_Of_Syntax_Scope_Loc := Loc;
   end Set_End_Of_Syntax_Scope_Loc;

   ----------------------
   -- Set_IDepth_Level --
   ----------------------

   procedure Set_IDepth_Level (E : Entity_Id) is
      P      : Entity_Id;
      Idepth : Natural := 0;
   begin
      pragma Assert (Is_Tagged_Type (E));

      P := E.Parent;
      while Present (P) loop
         Idepth := Idepth + 1;
         P := Get_Parent (P);
      end loop;

      E.Idepth_Level := Idepth;
   end Set_IDepth_Level;

   -------------------
   -- Set_Error_Msg --
   -------------------

   procedure Set_Error_Msg   (E : Entity_Id; Value : Unbounded_String) is
   begin
      E.Error_Msg := Value;
   end Set_Error_Msg;

   ---------------------------
   -- Set_Full_View_Comment --
   ---------------------------

   procedure Set_Full_View_Comment
     (E : Entity_Id; Value : Structured_Comment) is
   begin
      E.Full_View_Comment := Value;
   end Set_Full_View_Comment;

   -----------------------
   -- Set_Full_View_Doc --
   -----------------------

   procedure Set_Full_View_Doc (E : Entity_Id; Value : Comment_Result) is
   begin
      pragma Assert
        (E.Full_View_Doc = No_Comment_Result
           or else Value = No_Comment_Result);
      E.Full_View_Doc := Value;
   end Set_Full_View_Doc;

   -----------------------
   -- Set_Full_View_Src --
   -----------------------

   procedure Set_Full_View_Src (E : Entity_Id; Value : Unbounded_String) is
   begin
      pragma Assert (E.Full_View_Src = Null_Unbounded_String);
      E.Full_View_Src := Value;
   end Set_Full_View_Src;

   --------------------------
   -- Set_Is_Doc_From_Body --
   --------------------------

   procedure Set_Is_Doc_From_Body (E : Entity_Id) is
   begin
      E.Is_Doc_From_Body := True;
   end Set_Is_Doc_From_Body;

   ---------------------------
   -- Set_Is_Generic_Formal --
   ---------------------------

   procedure Set_Is_Generic_Formal (E : Entity_Id) is
   begin
      E.Is_Generic_Formal := True;
   end Set_Is_Generic_Formal;

   ---------------------------------------
   -- Set_Is_Incomplete_Or_Private_Type --
   ---------------------------------------

   procedure Set_Is_Incomplete_Or_Private_Type (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Incomplete_Or_Private_Type));
      E.Is_Incomplete_Or_Private_Type := True;
   end Set_Is_Incomplete_Or_Private_Type;

   -------------------------
   -- Set_Is_Partial_View --
   -------------------------

   procedure Set_Is_Partial_View (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Partial_View));
      E.Is_Partial_View := True;
   end Set_Is_Partial_View;

   --------------------
   -- Set_Is_Private --
   --------------------

   procedure Set_Is_Private (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Private));
      E.Is_Private := True;
   end Set_Is_Private;

   -------------------
   -- Set_Is_Tagged --
   -------------------

   procedure Set_Is_Tagged_Type (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Tagged_Type));
      pragma Assert (Is_Class_Or_Record_Type (E));
      E.Is_Tagged_Type := True;
   end Set_Is_Tagged_Type;

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
      pragma Assert (No (E.Parent));
      pragma Assert (Value /= E); --  Avoid circularity
      E.Parent := Value;

      --  If the parent is not fully decorated (because it is an entity defined
      --  in the runtime of the compiler) we complete its decoration.

      if Is_Tagged_Type (E) and then not Is_Tagged_Type (E.Parent) then
         Set_Is_Tagged_Type (E.Parent);
      end if;

      Append_Derivation (E.Parent, E);
   end Set_Parent;

   ------------------
   -- Set_Ref_File --
   ------------------

   procedure Set_Ref_File
     (E : Entity_Id; Value : Virtual_File) is
   begin
      E.Ref_File := Value;
   end Set_Ref_File;

   ---------------
   -- Set_Scope --
   ---------------

   procedure Set_Scope (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (Value /= E); --  Avoid circularity
      E.Scope := Value;
   end Set_Scope;

   -------------
   -- Set_Src --
   -------------

   procedure Set_Src (E : Entity_Id; Value : Unbounded_String) is
   begin
      pragma Assert (E.Src = Null_Unbounded_String);
      E.Src := Value;
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
            if Is_Class_Or_Record_Type (E_Info) then
               Cursor := Get_Discriminants (E_Info).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Do_Process (EInfo_List.Element (Cursor), Scope_Level + 1);

                  EInfo_List.Next (Cursor);
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
         if not Contains (E.Xref.Child_Types, Value) then
            E.Xref.Child_Types.Append (Value);
         end if;
      end Append_Child_Type;

      procedure Append_Parent_Type (E : Entity_Id; Value : Entity_Id) is
      begin
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

      function Get_End_Of_Scope_Loc
        (E : Entity_Id) return General_Location is
      begin
         return E.Xref.End_Of_Scope_Loc;
      end Get_End_Of_Scope_Loc;

      function Get_Entity (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Entity;
      end Get_Entity;

      function Get_Full_View (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Full_View;
      end Get_Full_View;

      function Get_Instance_Of
        (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Instance_Of;
      end Get_Instance_Of;

      function Get_Kind (E : Entity_Id) return Entity_Kind is
      begin
         return E.Xref.Ekind;
      end Get_Kind;

      function Get_Location (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Loc;
      end Get_Location;

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

      function Get_Scope_Loc (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Scope_Loc;
      end Get_Scope_Loc;

      function Get_Type (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Etype;
      end Get_Type;

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

      function Is_Container (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Container;
      end Is_Container;

      function Is_Generic (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Generic;
      end Is_Generic;

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
             and then Db.Is_Global (E)
             and then LL.Get_Ekind (Db, E, In_Ada_Lang => False)
                        = E_Record_Type
             and then Db.Get_Name (Db.Caller_At_Declaration (E))
                        = Db.Get_Name (E);
      end Is_Self_Referenced_Type;

      function Is_Subprogram (E : Entity_Id) return Boolean is
      begin
         return E.Xref.Is_Subprogram;
      end Is_Subprogram;

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
            return E_Task;

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
      With_Unique_Id : Boolean := False) return String
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
      begin
         Append_Line
           (Prefix
            & UID
            & Name
            & " ["
            & Image (LL.Get_Location (Entity))
            & "]");
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

      --  Internal entity are not fully decorated (currently used only to
      --  represent the standard scope)

      elsif E.Is_Internal then
         Append_Line
           ("*** "
            & To_String (E.Id)
            & ": "
            & Get (E.Short_Name).all
            & " ("
            & E.Kind'Img
            & ")");
         return To_String (Printout);
      end if;

      declare
         UID : constant String :=
                 (if With_Unique_Id then To_String (E.Id) & ": "
                                    else "");
      begin
         Append_Line
           ("*** "
            & UID
            & Get (E.Short_Name).all
            & " ("
            & E.Kind'Img
            & ":"
            & E.Xref.Ekind'Img
            & ")");
      end;

      --  Synthesized attributes

      if Present (E.Scope) then
         Append_Entity ("Scope: ", E.Scope);
      end if;

      if E.End_Of_Syntax_Scope_Loc /= No_Location then
         Append_Line
           ("End_Of_Syntax_Scope_Loc: " & Image (E.End_Of_Syntax_Scope_Loc));
      end if;

      if Present (E.Alias) then
         Append_Entity ("Alias: ", E.Alias);
      end if;

      if Present (Get_Parent (E)) then
         Append_Entity ("Parent: ", Get_Parent (E));
      end if;

      if (Is_Class_Or_Record_Type (E) and then Is_Tagged_Type (E))
        or else Get_Kind (E) = E_Class
      then
         Append_Entities
           (Vector => Get_Progenitors (E),
            Header => "Progenitors",
            Prefix => " - ");
         Append_Entities
           (Vector => Get_Derivations (E),
            Header => "Derivations",
            Prefix => " - ");
      end if;

      if E.Is_Incomplete_Or_Private_Type then
         Append_Line (" Is_Incomplete_Or_Private_Type");
      end if;

      if E.Is_Private then
         Append_Line ("Is_Private");
      end if;

      if E.Is_Partial_View then
         Append_Line ("Is_Partial_View");
      end if;

      if E.Is_Tagged_Type then
         Append_Line ("Is_Tagged");
      end if;

      if E.Is_Generic_Formal then
         Append_Line ("Is_Generic_Formal");
      end if;

      --  Output information retrieved from Xref

      if With_Full_Loc then
         Append_Line
           (LL_Prefix
            & "Loc: "
            & (+E.Xref.Loc.File.Dir_Name)
            & Image (E.Xref.Loc));
      else
         Append_Line
           (LL_Prefix
            & "Loc: "
            & Image (E.Xref.Loc));
      end if;

      Append_Line
        (LL_Prefix
         & "Full Name: "
         & Get (E.Full_Name).all);

      if E.Xref.Body_Loc /= No_Location then
         Append_Line
           (LL_Prefix
            & "Body_Loc: " & Image (E.Xref.Body_Loc));
      end if;

      if E.Xref.End_Of_Scope_Loc /= No_Location then
         Append_Line
           (LL_Prefix
            & "End_Of_Scope_Loc: " & Image (E.Xref.End_Of_Scope_Loc));
      end if;

      if Present (LL.Get_Full_View (E)) then
         Append_Line
           (LL_Prefix
            & "Full_View: "
            & Get_Name (Db, LL.Get_Full_View (E))
            & " ["
            & Image (Db, LL.Get_Full_View (E))
            & "]");
      end if;

      if Present (E.Xref.Scope_E) then
         Append_Line
           (LL_Prefix
            & "Scope: "
            & Get_Name (Db, E.Xref.Scope_E)
            & " ["
            & Image (E.Xref.Scope_Loc)
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

      if E.Xref.Is_Access
        and then Present (E.Xref.Pointed_Type)
      then
         Append_Line
           (LL_Prefix
            & "Pointed type: " & Get_Name (Db, E.Xref.Pointed_Type)
            & " [" & Image (Db, E.Xref.Pointed_Type) & "]");
      end if;

      if Present (E.Xref.Alias) then
         Append_Line
           (LL_Prefix
            & "Alias: " & Get_Name (Db, E.Xref.Alias)
            & " [" & Image (Db, E.Xref.Alias) & "]");
      end if;

      if E.Xref.Is_Abstract then
         Append_Line
           (LL_Prefix
            & " Is_Abstract");
      end if;

      if E.Xref.Is_Access then
         Append_Line
           (LL_Prefix
            & " Is_Access");
      end if;

      if E.Xref.Is_Array then
         Append_Line
           (LL_Prefix
            & " Is_Array");
      end if;

      if E.Xref.Is_Container then
         Append_Line
           (LL_Prefix
            & " Is_Container");
      end if;

      if E.Xref.Is_Global then
         Append_Line
           (LL_Prefix
            & " Is_Global");
      end if;

      if E.Xref.Is_Predef then
         Append_Line
           (LL_Prefix
            & " Is_Predef");
      end if;

      if E.Xref.Is_Type then
         Append_Line
           (LL_Prefix
            & " Is_Type");
      end if;

      if E.Xref.Has_Methods then
         Append_Line
           (LL_Prefix
            & " Has_Methods");
      end if;

      --  Display record type components and dispatching primitives (methods)

      if Is_Class_Or_Record_Type (E)
        or else Get_Kind (E) = E_Class
      then
         Append_Entities
           (Vector => LL.Get_Parent_Types (E),
            Header => LL_Prefix & " Parent types",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => LL.Get_Child_Types (E),
            Header => LL_Prefix & " Child types",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => Get_Entities (E),
            Header => LL_Prefix & " Components:",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => Get_Inherited_Methods (E),
            Header => LL_Prefix & " Inherited methods:",
            Prefix => LL_Prefix & " - ");

         Append_Entities
           (Vector => Get_Methods (E),
            Header => LL_Prefix & " Methods:",
            Prefix => LL_Prefix & " - ");
      end if;

      if LL.Is_Subprogram (E) then
         Append_Line (LL_Prefix & " Is_Subprogram");

         declare
            Cursor : EInfo_List.Cursor;
            E_Info : Entity_Id;

         begin
            Cursor := Get_Entities (E).First;

            if EInfo_List.Has_Element (Cursor) then
               Append_Line (LL_Prefix & " Formals:");
            end if;

            while EInfo_List.Has_Element (Cursor) loop
               E_Info := EInfo_List.Element (Cursor);
               Append_Line
                 (LL_Prefix
                  & " - "
                  & Image (LL.Get_Location (E_Info))
                  & ":"
                  & Get_Short_Name (E_Info));
               EInfo_List.Next (Cursor);
            end loop;
         end;
      end if;

      if E.Xref.Is_Primitive then
         Append_Line
           (LL_Prefix
            & " Is_Primitive");
      end if;

      if E.Xref.Is_Generic then
         Append_Line
           (LL_Prefix
            & " Is_Generic");

      elsif Present (E.Xref.Instance_Of) then
         Append_Line
           (LL_Prefix
            & "Instance_Of: "
            & Image (Get_Location (Db, E.Xref.Instance_Of))
            & ":"
            & Get_Name (Db, E.Xref.Instance_Of));
      end if;

      if With_Src then
         if E.Full_View_Src = Null_Unbounded_String then
            if Get_Src (E) /= Null_Unbounded_String then
               Append_Line ("Src:");
               Append_Line (To_String (Get_Src (E)));
            end if;
         else
            Append_Line ("Partial View Src:");
            Append_Line (To_String (Get_Src (E)));
            Append_Line ("Full View Src:");
            Append_Line (To_String (Get_Full_View_Src (E)));
         end if;
      end if;

      if With_Doc then
         if E.Doc /= No_Comment_Result then
            if E.Is_Doc_From_Body then
               Append_Line ("Is_Doc_From_Body");
            end if;

            Append_Line ("Doc.Line:" & E.Doc.Start_Line'Img);
            Append_Line ("Doc.Text: " & To_String (E.Doc.Text));
         end if;

         if E.Comment /= No_Structured_Comment then
            Append_Line ("Structured Comment:");

            --  Append the comment avoiding the duplicate addition of the
            --  prefix to the output

            Append_Line_Without_Prefix
              (Ada.Strings.Unbounded.To_String
                 (To_Unbounded_String (Get_Comment (E), Prefix => Prefix)));
         end if;

         if Is_Partial_View (E) then
            if E.Full_View_Doc /= No_Comment_Result then
               Append_Line
                 ("Full_View.Doc.Line:" & E.Full_View_Doc.Start_Line'Img);
               Append_Line
                 ("Full_View.Doc.Text: " & To_String (E.Full_View_Doc.Text));
            end if;

            if E.Full_View_Comment /= No_Structured_Comment then
               Append_Line ("Full_View.Structured Comment:");

               --  Append the comment avoiding the duplicate addition of the
               --  prefix to the output

               Append_Line_Without_Prefix
                 (Ada.Strings.Unbounded.To_String
                    (To_Unbounded_String
                       (Get_Full_View_Comment (E), Prefix => Prefix)));
            end if;
         end if;
      end if;

      if With_Errors
        and then E.Error_Msg /= Null_Unbounded_String
      then
         Append_Line (To_String (E.Error_Msg));
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

   --------
   -- pn --
   --------

   procedure pn (E : Entity_Id) is
   begin
      GNAT.IO.Put_Line
        (To_String (E,
           With_Src => True,
           With_Doc => True,
           With_Full_Loc => True,
           With_Unique_Id => True));
   end pn;

   ---------
   -- pns --
   ---------

   procedure pns (E : Entity_Id) is
   begin
      GNAT.IO.Put_Line
        (Get_Unique_Id (E)'Img & ":"
         & Get_Short_Name (E) & " "
         & Image (LL.Get_Location (E)));
   end pns;

   procedure pns (Db : General_Xref_Database; E : General_Entity) is
   begin
      if Present (E) then
         declare
            Loc  : constant General_Location := Get_Location (Db, E);
            Name : constant String := Get_Name (Db, E);
         begin
            GNAT.IO.Put_Line (Name & " " & Image (Loc));
         end;
      end if;
   end pns;

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

end Docgen3.Atree;
