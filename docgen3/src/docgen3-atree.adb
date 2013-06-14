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
with Language.Cpp;
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

   function Internal_New_Entity
     (Context : access constant Docgen_Context;
      Lang    : Language_Access;
      E       : General_Entity;
      Loc     : General_Location;
      Name    : String := "") return Entity_Id;
   --  Internal subprogram which factorizes the code needed by routines
   --  New_Entity and New_Internal_Entity to create a new entity.

   procedure LL_Set_Full_View (E : Entity_Id; Value : General_Entity);

   procedure Set_Is_Incomplete_Or_Private_Type (E : Entity_Id);
   --  Set to true field E.Is_Incomplete_Or_Private_Type

   -----------------------
   -- Append_Child_Type --
   -----------------------

   procedure Append_Child_Type (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Child_Types.Append (Value);
   end Append_Child_Type;

   -------------------------
   -- Append_Discriminant --
   -------------------------

   procedure Append_Discriminant (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Discriminants.Append (Value);
   end Append_Discriminant;

   -------------------
   -- Append_Entity --
   -------------------

   procedure Append_Entity (E : Entity_Id; Value : Entity_Id) is

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
   end Append_Entity;

   -------------------
   -- Append_Method --
   -------------------

   procedure Append_Method (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Methods.Append (Value);
   end Append_Method;

   ------------------------
   -- Append_Parent_Type --
   ------------------------

   procedure Append_Parent_Type (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Parent_Types.Append (Value);
   end Append_Parent_Type;

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

   ---------------------
   -- Get_Child_Types --
   ---------------------

   function Get_Child_Types (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Child_Types'Access;
   end Get_Child_Types;

   -----------------
   -- Get_Comment --
   -----------------

   function Get_Comment (E : Entity_Id) return Structured_Comment is
   begin
      return E.Comment;
   end Get_Comment;

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

   ----------------------
   -- Get_Parent_Types --
   ----------------------

   function Get_Parent_Types (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Parent_Types'Access;
   end Get_Parent_Types;

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

   -----------------
   -- Has_Formals --
   -----------------

   function Has_Formals (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Present (E)
         and then Get_Kind (E) /= E_Unknown);

      return E.Kind = E_Procedure
        or else E.Kind = E_Abstract_Procedure
        or else E.Kind = E_Generic_Procedure
        or else E.Kind = E_Function
        or else E.Kind = E_Abstract_Function
        or else E.Kind = E_Generic_Function
        or else E.Kind = E_Entry;
   end Has_Formals;

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

   function In_C_Language
     (E : Entity_Id) return Boolean is
   begin
      return Get_Language (E).all in Language.C.C_Language'Class
               and then
             Get_Language (E).all not in Language.Cpp.Cpp_Language'Class;
   end In_C_Language;

   ---------------------
   -- In_CPP_Language --
   ---------------------

   function In_CPP_Language
     (E : Entity_Id) return Boolean is
   begin
      return Get_Language (E).all in Language.Cpp.Cpp_Language'Class;
   end In_CPP_Language;

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
      Name    : String := "") return Entity_Id
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

         New_E.Xref.Scope_E      := Caller_At_Declaration (Db, E);
         New_E.Xref.Scope_Loc    := Get_Location (Db, New_E.Xref.Scope_E);

         New_E.Xref.Etype        := Get_Type_Of (Db, E);
         New_E.Xref.Body_Loc     := Get_Body (Db, E);

         New_E.Xref.Is_Type      := Xref.Is_Type (Db, E);

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

            if New_E.Xref.Is_Subprogram then
               New_E.Xref.Is_Abstract := Xref.Is_Abstract (Db, E);

               if Is_Primitive_Of (Db, E) /= No_General_Entity then
                  New_E.Xref.Is_Primitive := True;
               end if;
            end if;
         end if;

         --  Stage 2: Add decoration of a few high-level components. The
         --  decoration of other high-level components is done while
         --  traversing the tree since they require context information.

         if New_E.Xref.Is_Type then

            if New_E.Xref.Etype = No_General_Entity
              and then New_E.Xref.Body_Loc /= No_Location
            then
               Set_Is_Incomplete_Or_Private_Type (New_E);
            end if;

            if Is_Class_Or_Record_Type (New_E) then
               New_E.Xref.Has_Methods := Db.Has_Methods (E);

               if In_Ada_Language (New_E) then
                  if Get_Kind (New_E) = E_Interface then
                     New_E.Is_Tagged := True;

                  elsif New_E.Xref.Has_Methods then
                     declare
                        All_Methods : constant Xref.Entity_Array :=
                          Methods (Db, E, Include_Inherited => True);
                     begin
                        --  Xref-bug: At least for Incomplete_Types I have
                        --  found that Xref reports Has_Methods()=True for
                        --  types that have no methods. Hence this is NOT a
                        --  reliable Xref service. A bug in Xref????

                        --  pragma Assert (All_Methods'Length > 0);

                        if All_Methods'Length > 0 then
                           Set_Is_Tagged (New_E);
                        end if;
                     end;

                  else
                     declare
                        Parents : constant Xref.Entity_Array :=
                          Parent_Types (Db, E, Recursive => False);
                     begin
                        if Parents'Length > 0 then
                           Set_Is_Tagged (New_E);
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
               LL_Set_Full_View (New_E,
                 Xref.Get_Entity
                   (Db   => Context.Database,
                    Name => Get_Short_Name (New_E),
                    Loc  => LL.Get_Body_Loc (New_E)));
            end if;
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

   --  Start of processing for New_Entity

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
             Entity        => E,
             Full_View     => No_General_Entity,
             Loc           => Loc,
             Body_Loc      => No_Location,
             Ekind         => Kind,
             Scope_E       => No_General_Entity,
             Scope_Loc     => No_Location,
             Etype         => No_General_Entity,
             Pointed_Type  => No_General_Entity,

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
             Is_Generic    => False),

           Full_Name       => Context.Kernel.Symbols.Find (Q_Name),
           Short_Name      => Context.Kernel.Symbols.Find (S_Name),
           Scope           => null,
           Kind            => Kind,

           Is_Incomplete_Or_Private_Type => False,

           Is_Tagged       => False,
           Is_Private      => False,
           Is_Partial_View => False,

           Doc             => No_Comment_Result,
           Comment         => No_Structured_Comment,

           Full_View_Doc     => No_Comment_Result,
           Full_View_Comment => No_Structured_Comment,

           Src             => Null_Unbounded_String,
           Full_View_Src   => Null_Unbounded_String,

           Discriminants   => <>,
           Entities        => <>,
           Methods         => <>,
           Parent_Types    => <>,
           Child_Types     => <>,
           Error_Msg       => Null_Unbounded_String);

      --  Do not perform the full decoration of the entity for auxiliary
      --  entities created by the frontend (for example, the "standard"
      --  entity).

      if Present (E) then
         Complete_Decoration (New_E);
      end if;

      New_Entity_End (New_E.Id);
      return New_E;
   end Internal_New_Entity;

   ------------------
   -- Is_Full_View --
   ------------------

   function Is_Full_View (E : Entity_Id) return Boolean is
   begin
      return Present (LL.Get_Body_Loc (E))
        and then LL.Get_Body_Loc (E).Line
                   < LL.Get_Location (E).Line;
   end Is_Full_View;

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

   --------------------
   -- Is_Record_Type --
   --------------------

   function Is_Class_Or_Record_Type (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Present (E)
        and then Get_Kind (E) /= E_Unknown);

      return E.Xref.Ekind = E_Abstract_Record_Type
        or else E.Xref.Ekind = E_Record_Type
        or else E.Kind = E_Tagged_Record_Type
        or else E.Kind = E_Interface
        or else E.Kind = E_Class;
   end Is_Class_Or_Record_Type;

   ---------------
   -- Is_Tagged --
   ---------------

   function Is_Tagged (E : Entity_Id) return Boolean is
   begin
      return E.Is_Tagged;
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
      pragma Assert (E.Xref.Full_View = No_General_Entity);
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
      Name     : String) return Entity_Id is
   begin
      return
        Internal_New_Entity
          (Context => Context,
           Lang    => Language,
           E       => No_General_Entity,
           Loc     => No_Location,
           Name    => Name);
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

   -------------
   -- Setters --
   -------------

   procedure Set_Comment (E : Entity_Id; Value : Structured_Comment) is
   begin
      E.Comment := Value;
   end Set_Comment;

   -------------
   -- Set_Doc --
   -------------

   procedure Set_Doc (E : Entity_Id; Value : Comment_Result) is
   begin
      pragma Assert
        (E.Doc = No_Comment_Result
           or else Value = No_Comment_Result);
      E.Doc := Value;
   end Set_Doc;

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

   procedure Set_Is_Tagged (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Tagged));
      pragma Assert (Is_Class_Or_Record_Type (E));
      E.Is_Tagged := True;
      E.Kind := E_Tagged_Record_Type;
   end Set_Is_Tagged;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (E : Entity_Id; Value : Entity_Kind) is
   begin
      E.Kind := Value;
   end Set_Kind;

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
      pragma Assert (No (E.Scope) or else E.Scope = Value);
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

      function Get_Body_Loc (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Body_Loc;
      end Get_Body_Loc;

      function Get_Entity (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Entity;
      end Get_Entity;

      function Get_Full_View (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Full_View;
      end Get_Full_View;

      function Get_Kind (E : Entity_Id) return Entity_Kind is
      begin
         return E.Xref.Ekind;
      end Get_Kind;

      function Get_Location (E : Entity_Id) return General_Location is
      begin
         return E.Xref.Loc;
      end Get_Location;

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
     (E             : Entity_Id;
      Prefix        : String := "";
      With_Full_Loc : Boolean := False;
      With_Src      : Boolean := False;
      With_Doc      : Boolean := False;
      With_Errors   : Boolean := False) return String
   is
      LL_Prefix : constant String := "xref: ";
      Printout  : aliased Unbounded_String;

      procedure Append_Line (Text : String);
      --  Append Prefix & Text & ASCII.LF to Printout

      procedure Append_Line_Without_Prefix (Text : String);
      --  Append Text & ASCII.LF to Printout

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Prefix & Text & ASCII.LF;
      end Append_Line;

      procedure Append_Line_Without_Prefix (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line_Without_Prefix;

      procedure Print_Entities
        (Vector : access EInfo_List.Vector;
         Header : String;
         Prefix : String);
      procedure Print_Entities
        (Vector : access EInfo_List.Vector;
         Header : String;
         Prefix : String)
      is
         Cursor : EInfo_List.Cursor;
         E_Info : Entity_Id;

      begin
         Cursor := Vector.First;

         if EInfo_List.Has_Element (Cursor) then
            Append_Line (Header);
         end if;

         while EInfo_List.Has_Element (Cursor) loop
            E_Info := EInfo_List.Element (Cursor);
            Append_Line
              (Prefix
               & "["
               & To_String (E_Info.Id)
               & "] "
               & Get_Short_Name (E_Info)
               & " ["
               & Image (LL.Get_Location (E_Info))
               & "]"
              );
            EInfo_List.Next (Cursor);
         end loop;
      end Print_Entities;

   --  Start of processing for To_String

   begin
      pragma Assert (Db /= null);

      if No (E) then
         return "";
      end if;

      Append_Line
        ("*** "
         & To_String (E.Id)
         & ": "
         & Get (E.Short_Name).all
         & " ("
         & E.Kind'Img
         & ":"
         & E.Xref.Ekind'Img
         & ")");

      --  Synthesized attributes

      if Present (E.Scope) then
         Append_Line
           ("Scope: ["
            & To_String (E.Scope.Id)
            & "] "
            & Get (E.Scope.Short_Name).all);
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

      if E.Is_Tagged then
         Append_Line ("Is_Tagged");
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

      if E.Xref.Scope_E /= No_General_Entity then
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

      if E.Xref.Etype /= No_General_Entity then
         Append_Line
           (LL_Prefix
            & "Etype: " & Get_Name (Db, E.Xref.Etype));
      end if;

      if E.Xref.Is_Access
        and then E.Xref.Pointed_Type /= No_General_Entity
      then
         Append_Line
           (LL_Prefix
            & "Pointed type: " & Get_Name (Db, E.Xref.Pointed_Type));
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

      --  Display record type components

      if Is_Class_Or_Record_Type (E)
        or else Get_Kind (E) = E_Class
      then
         Print_Entities
           (Vector => Get_Parent_Types (E),
            Header => LL_Prefix & " Parent types",
            Prefix => LL_Prefix & " - ");

         Print_Entities
           (Vector => Get_Child_Types (E),
            Header => LL_Prefix & " Child types",
            Prefix => LL_Prefix & " - ");

         Print_Entities
           (Vector => Get_Entities (E),
            Header => LL_Prefix & " Components:",
            Prefix => LL_Prefix & " - ");

         Print_Entities
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
      end if;

      if With_Src then
         if E.Full_View_Src = Null_Unbounded_String then
            Append_Line ("Src:");
            Append_Line (To_String (Get_Src (E)));
         else
            Append_Line ("Partial View Src:");
            Append_Line (To_String (Get_Src (E)));
            Append_Line ("Full View Src:");
            Append_Line (To_String (Get_Full_View_Src (E)));
         end if;
      end if;

      if With_Doc then
         if E.Doc /= No_Comment_Result then
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
        (To_String (E, With_Src => True, With_Doc => True,
                       With_Full_Loc => True));
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

end Docgen3.Atree;
