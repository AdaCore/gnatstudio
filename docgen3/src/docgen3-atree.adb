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
with Language;                    use Language;
with Language.Tree;               use Language.Tree;
with Language.Tree.Database;      use Language.Tree.Database;
with Traces;                      use Traces;
with GNAT.IO;

package body Docgen3.Atree is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Internal_New_Entity
     (Context : access constant Docgen_Context;
      E       : General_Entity;
      Loc     : General_Location;
      Name    : String := "") return Entity_Id;
   --  Internal subprogram which factorizes the code needed by routines
   --  New_Entity and New_Internal_Entity to create a new entity.

   procedure Set_Is_Incomplete_Or_Private_Type (E : Entity_Id);
   --  Set to true field E.Is_Incomplete_Or_Private_Type

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
   begin
      E.Entities.Append (Value);
   end Append_Entity;

   -------------------
   -- Append_Method --
   -------------------

   procedure Append_Method (E : Entity_Id; Value : Entity_Id) is
   begin
      E.Methods.Append (Value);
   end Append_Method;

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

   -------------------
   -- Get_Full_View --
   -------------------

   function Get_Full_View (E : Entity_Id) return Entity_Id is
   begin
      return E.Full_View;
   end Get_Full_View;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (E : Entity_Id) return Entity_Kind is
   begin
      return E.Kind;
   end Get_Kind;

   -----------------
   -- Get_Methods --
   -----------------

   function Get_Methods (E : Entity_Id) return access EInfo_List.Vector is
   begin
      return E.Methods'Access;
   end Get_Methods;

   ----------------------
   -- Get_Partial_View --
   ----------------------

   function Get_Partial_View (E : Entity_Id) return Entity_Id is
   begin
      return E.Partial_View;
   end Get_Partial_View;

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

   -------------------------
   -- Internal_New_Entity --
   -------------------------

   Unique_Id : Natural := 0;

   function Internal_New_Entity
     (Context : access constant Docgen_Context;
      E       : General_Entity;
      Loc     : General_Location;
      Name    : String := "") return Entity_Id
   is
      Db : General_Xref_Database renames Context.Database;

      procedure Complete_Decoration (New_E : Entity_Id);

      --------------
      -- Decorate --
      --------------

      procedure Complete_Decoration
        (New_E : Entity_Id)
      is
         E      : General_Entity renames New_E.Xref.Entity;
         E_Type : General_Entity;

      begin
         New_E.Xref.Scope_E      := Caller_At_Declaration (Db, E);
         New_E.Xref.Etype        := Get_Type_Of (Db, E);
         New_E.Xref.Body_Loc     := Get_Body (Db, E);

         New_E.Xref.Is_Type      := Xref.Is_Type (Db, E);
         New_E.Xref.Is_Global    := Xref.Is_Global (Db, E);
         New_E.Xref.Is_Container := Xref.Is_Container (Db, E);

         if not New_E.Xref.Is_Type then
            New_E.Xref.Is_Subprogram := Is_Subprogram (Db, E);

         else
            New_E.Xref.Is_Abstract   := Xref.Is_Abstract (Db, E);
            New_E.Xref.Is_Array      := Xref.Is_Array (Db, E);
            New_E.Xref.Is_Predef     := Xref.Is_Predefined_Entity (Db, E);
            New_E.Xref.Is_Access     := Xref.Is_Access (Db, E);

            if New_E.Xref.Is_Access then
               New_E.Xref.Pointed_Type := Xref.Pointed_Type (Db, E);

               --  Xref does not provide the expected info???
               --  pragma Assert
               --    (New_E.Xref.Pointed_Type /= No_General_Type);
            end if;

            if New_E.Xref.Is_Type
              and then E_Type = No_General_Entity
              and then New_E.Xref.Body_Loc /= No_Location
            --  and then Get_Name (Db, E) = Get_Name (Db, E_Scope)
            then
               Set_Is_Incomplete_Or_Private_Type (New_E);
            end if;
         end if;

         --  Complete decoration

         if New_E.Xref.Ekind = E_Package
           or else New_E.Xref.Ekind = E_Generic_Package
         then
            New_E.Is_Package := True;
            New_E.Xref.Is_Generic := Is_Generic (Db, E);

         elsif New_E.Xref.Is_Subprogram then
            New_E.Xref.Is_Abstract := Xref.Is_Abstract (Db, E);
            New_E.Xref.Is_Generic := Is_Generic (Db, E);

            if Is_Primitive_Of (Db, E) /= No_General_Entity then
               New_E.Xref.Is_Primitive := True;
            end if;

         elsif Is_Record_Type (New_E) then
            if Has_Methods (Db, E) then
               declare
                  All_Methods : constant Xref.Entity_Array :=
                    Methods (Db, E, Include_Inherited => True);
               begin
                  --  Xref-bug: At least for Incomplete_Types I have found that
                  --  Xref reports Has_Methods()=True for types that have no
                  --  methods. Hence this is NOT a reliable Xref service. A
                  --  bug in Xref????

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

      exception
         when E : others =>
            Trace (Exception_Handle, E);
            return;
      end Complete_Decoration;

      --  Local variables

      Q_Name : constant String :=
        (if Name /= "" then Name else Qualified_Name (Db, E));
      S_Name : constant String :=
        (if Name /= "" then Name else Get_Name (Db, E));
      Kind   : constant Entity_Kind :=
        (if Present (E) then LL.Get_Ekind (Db, E) else E_Unknown);

      --  Start of processing for New_Entity

      New_E : Entity_Id;
   begin
      Unique_Id := Unique_Id + 1;

      --  Initially E.Kind and E.Xref.Ekind are initialized with the same
      --  values. However, E.Kind may be decorated with other values at later
      --  stages based on the context. For example, an E_Variable may be
      --  redecorated as E_Formal (see docgen-frontend.adb)

      New_E :=
        new Entity_Info_Record'
          (Id   => Unique_Id,
           Xref => Xref_Info'(
             Entity        => E,
             Loc           => Loc,
             Body_Loc      => No_Location,
             Ekind         => Kind,
             Scope_E       => No_General_Entity,
             Etype         => No_General_Entity,
             Pointed_Type  => No_General_Entity,

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
           Partial_View    => null,
           Full_View       => null,

           Is_Incomplete_Or_Private_Type => False,

           Is_Package      => False,
           Is_Tagged       => False,
           Is_Private      => False,
           Is_Partial_View => False,
           Is_Full_View    => False,
           Is_Frozen       => False,

           Doc             => No_Comment_Result,
           Src             => Null_Unbounded_String,

           Comment         => No_Structured_Comment,
           Discriminants   => <>,
           Entities        => <>,
           Methods         => <>,
           Error_Msg       => Null_Unbounded_String);

      if Present (E) then
         Complete_Decoration (New_E);

         if Present (LL.Get_Body_Loc (New_E))
           and then (LL.Is_Type (New_E) or else Get_Kind (New_E) = E_Variable)
         then -- restrict the above to Is_Record_Type???
            if (LL.Get_Body_Loc (New_E).File
                  = LL.Get_Location (New_E).File)
              and then (LL.Get_Body_Loc (New_E).Line
                          > LL.Get_Location (New_E).Line)
            then
               Set_Is_Partial_View (New_E);
            else
               Set_Is_Full_View (New_E);
            end if;
         end if;
      end if;

      return New_E;
   end Internal_New_Entity;

   ------------------
   -- Is_Full_View --
   ------------------

   function Is_Full_View (E : Entity_Id) return Boolean is
   begin
      return E.Is_Full_View;
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
      return E.Is_Package;
   end Is_Package;

   ---------------------
   -- Is_Partial_View --
   ---------------------

   function Is_Partial_View  (E : Entity_Id) return Boolean is
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

   function Is_Record_Type (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Present (E)
        and then Get_Kind (E) /= E_Unknown);

      return E.Xref.Ekind = E_Abstract_Record_Type
        or else E.Xref.Ekind = E_Record_Type
        or else E.Kind = E_Tagged_Record_Type;
   end Is_Record_Type;

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

   ----------------
   -- New_Entity --
   ----------------

   function New_Entity
     (Context : access constant Docgen_Context;
      E       : General_Entity;
      Loc     : General_Location) return Entity_Id is
   begin
      return
        Internal_New_Entity
          (Context => Context,
           E       => E,
           Loc     => Loc);
   end New_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Context : access constant Docgen_Context;
      Name    : String) return Entity_Id is
   begin
      return
        Internal_New_Entity
          (Context => Context,
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

   -------------------
   -- Set_Full_View --
   -------------------

   procedure Set_Full_View (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (Is_Record_Type (E));
      pragma Assert (No (E.Full_View));
      E.Full_View := Value;
   end Set_Full_View;

   ---------------------------------------
   -- Set_Is_Incomplete_Or_Private_Type --
   ---------------------------------------

   procedure Set_Is_Incomplete_Or_Private_Type (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Incomplete_Or_Private_Type));
      E.Is_Incomplete_Or_Private_Type := True;
   end Set_Is_Incomplete_Or_Private_Type;

   --------------------
   -- Set_Is_Package --
   --------------------

   procedure Set_Is_Package (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Package));
      E.Is_Package := True;
   end Set_Is_Package;

   -------------------------
   -- Set_Is_Partial_View --
   -------------------------

   procedure Set_Is_Partial_View (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Partial_View));
      pragma Assert (not (E.Is_Full_View));
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
      pragma Assert (Is_Record_Type (E));
      E.Is_Tagged := True;
      E.Kind := E_Tagged_Record_Type;
   end Set_Is_Tagged;

   ----------------------
   -- Set_Is_Full_View --
   ----------------------

   procedure Set_Is_Full_View (E : Entity_Id) is
   begin
      pragma Assert (not (E.Is_Partial_View));
      pragma Assert (not (E.Is_Full_View));
      E.Is_Full_View := True;
   end Set_Is_Full_View;

   --------------
   -- Set_Kind --
   --------------

   procedure Set_Kind (E : Entity_Id; Value : Entity_Kind) is
   begin
      E.Kind := Value;
   end Set_Kind;

   ----------------------
   -- Set_Partial_View --
   ----------------------

   procedure Set_Partial_View (E : Entity_Id; Value : Entity_Id) is
   begin
      pragma Assert (No (E.Partial_View));
      E.Partial_View := Value;
   end Set_Partial_View;

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
            if Is_Record_Type (E_Info) then
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

            if Is_Tagged (E_Info) then
               Cursor := Get_Methods (E_Info).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Do_Process (EInfo_List.Element (Cursor), Scope_Level + 1);

                  EInfo_List.Next (Cursor);
               end loop;
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

      function Get_Type (E : Entity_Id) return General_Entity is
      begin
         return E.Xref.Etype;
      end Get_Type;

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
        (Db : General_Xref_Database;
         E  : General_Entity) return Entity_Kind
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

            --  Should not be found???
         elsif Kind = "loop label"
           or else Kind = "block label"
           or else Kind = "statement label"
           or else Kind = "statement"
         then
            pragma Assert (False);
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
               & Image (LL.Get_Location (E_Info))
               & ":"
               & Get_Short_Name (E_Info));
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
           ("Scope: " & Get (E.Scope.Short_Name).all);
      end if;

      if E.Is_Incomplete_Or_Private_Type then
         Append_Line (" Is_Incomplete_Or_Private_Type");
      end if;

      if Present (E.Partial_View) then
         Append_Line
           ("Partial_View: " & Get (E.Partial_View.Short_Name).all);
      end if;

      if Present (E.Full_View) then
         Append_Line
           ("Full_View: " & Get (E.Full_View.Short_Name).all);
      end if;

      if E.Is_Private then
         Append_Line ("Is_Private");
      end if;

      if E.Is_Partial_View then
         Append_Line ("Is_Partial_View");
      end if;

      if E.Is_Full_View then
         Append_Line ("Is_Full_View");
      end if;

      if E.Is_Package then
         Append_Line ("Is_Package");
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
            & "Scope: " & Get_Name (Db, E.Xref.Scope_E));
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

      if Is_Record_Type (E) then
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
      end if;

      --           Src           : Ada.Strings.Unbounded.Unbounded_String;
      --           Entities      : EInfo_List.Vector;
      --           Methods       : EInfo_List.Vector;

      if With_Errors
        and then E.Error_Msg /= Null_Unbounded_String
      then
         Append_Line (To_String (E.Error_Msg));
      end if;

      return To_String (Printout);
   end To_String;

   --------
   -- pn --
   --------

   procedure pn (E : Entity_Id) is
   begin
      GNAT.IO.Put_Line (To_String (E));
   end pn;

end Docgen3.Atree;
