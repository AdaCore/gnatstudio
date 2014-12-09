------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with String_Utils; use String_Utils;
with Libclang.File; use Libclang.File;
with Language.Libclang_Tree; use Language.Libclang_Tree;

with clang_c_Index_h; use clang_c_Index_h;
with Interfaces.C;
with GNATCOLL.Projects;
use GNATCOLL.Projects;
with Language.Libclang; use Language.Libclang;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with GPS.Editors; use GPS.Editors;
with Ada.Text_IO; use Ada.Text_IO;

package body Clang_Xref is

   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;

   function Get_Clang_Cursor (E : Clang_Entity) return Clang_Cursor;
   function Get_Clang_Cursor
     (Kernel : Core_Kernel; Loc : General_Location) return Clang_Cursor;

   function To_General_Location
     (K : Core_Kernel; Clang_Loc : Clang_Location) return General_Location;

   function Type_As_Entity
     (From_Entity : Clang_Entity; Typ : Clang_Type) return Clang_Entity;

   function Cursor_As_Entity
     (Db : Clang_Database;
      Kernel : Core_Kernel;
      Cursor : Clang_Cursor) return Clang_Entity;

   function Cursor_As_Entity
     (From_Entity : Clang_Entity; Cursor : Clang_Cursor) return Clang_Entity
   is
     (Cursor_As_Entity (From_Entity.Db,
                        From_Entity.Kernel, Cursor));

   function Methods
     (Entity : Clang_Entity) return Cursors_Vectors.Vector;

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Vectors.Vector;

   function To_Entity_Array
     (From_Entity : Clang_Entity;
      Cursors : Cursors_Vectors.Vector) return Entity_Array;

   use GNATCOLL.VFS;

   function To_General_Location
     (K : Core_Kernel; F : Virtual_File;
      Offset : Offset_T) return General_Location;

   function Find_All_References_Local
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;

   function Find_All_References_Global
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Cursor : Clang_Cursor;
      Kind : Clang_Cursor_Kind) return Cursors_Vectors.Vector
   is
      function Has_Kind (C : Clang_Cursor) return Boolean is (C.kind = Kind);
   begin
      return Get_Children (Cursor, Has_Kind'Access);
   end Get_Children;

   --------------------
   -- Get_Clang_Node --
   --------------------

   function Get_Clang_Cursor (E : Clang_Entity) return Clang_Cursor is
   begin
      return
        Clang_Node
          (E.Kernel.Get_Abstract_Tree_For_File (E.Loc.File).Node_At
           (Sloc_T'(E.Loc.Line, E.Loc.Column, 0))).Cursor;
   end Get_Clang_Cursor;

   --------------------
   -- Get_Clang_Node --
   --------------------

   function Get_Clang_Cursor
     (Kernel : Core_Kernel; Loc : General_Location) return Clang_Cursor
   is
   begin
      return
        Clang_Node
          (Kernel.Get_Abstract_Tree_For_File (Loc.File).Node_At
           (Sloc_T'(Loc.Line, Loc.Column, 0))).Cursor;
   end Get_Clang_Cursor;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Db : Clang_Database;
      General_Db : General_Xref_Database;
      Name : String;
      Loc : General_Location) return Root_Entity'Class
   is
      pragma Unreferenced (Name, General_Db);

      C : constant Clang_Cursor := clang_getCursorReferenced
        (Get_Clang_Cursor (Db.Kernel, Loc));
   begin
      if clang_Cursor_isNull (C) /= 0 then
         return No_Root_Entity;
      else
         return Cursor_As_Entity (Clang_Entity'(Db => Db,
                                                Kernel => Db.Kernel,
                                                others => <>), C);
      end if;
   end Get_Entity;

   --------------
   -- Is_Fuzzy --
   --------------

   overriding function Is_Fuzzy (Entity : Clang_Entity) return Boolean
   is (False);

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Entity : Clang_Entity) return String is
     (+Entity.Name);

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Entity : Clang_Entity) return String is
     (To_String
        (clang_getCursorKindSpelling
             (Kind (Get_Clang_Cursor (Entity)))));

   --------------------
   -- Qualified_Name --
   --------------------

   overriding function Qualified_Name
     (Entity : Clang_Entity) return String is
   begin
      return To_String (clang_getCursorUSR (Get_Clang_Cursor (Entity)));
   end Qualified_Name;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (Entity : Clang_Entity) return Integer
   is
   begin
      return Integer (clang_hashCursor (Get_Clang_Cursor (Entity)));
   end Hash;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (K : Core_Kernel; Clang_Loc : Clang_Location) return General_Location
   is
      C_File : aliased CXFile;
      use Interfaces.C;
      VFile : Virtual_File;
      Offset : aliased unsigned;

   begin
      clang_getFileLocation
        (Clang_Loc, C_File'Access, null, null, Offset'Access);
      VFile := Libclang.File.File (C_File);
      declare
         Loc : constant Editor_Location'Class :=
           K.Get_Buffer_Factory.Get
             (VFile, Open_View => False, Focus => False, Open_Buffer => True)
           .New_Location (Natural (Offset));
      begin
         return General_Location'
           (VFile, GNATCOLL.Projects.No_Project,
            Loc.Line, Loc.Column);
      end;
   end To_General_Location;

   ----------------------
   -- Cursor_As_Entity --
   ----------------------

   function Cursor_As_Entity
     (Db : Clang_Database;
      Kernel : Core_Kernel;
      Cursor : Clang_Cursor) return Clang_Entity
   is
   begin
      return Clang_Entity'
        (Db => Db,
         Name       => +Spelling (Cursor),
         Loc        =>
           To_General_Location
             (Db.Kernel,
              clang_getCursorLocation (clang_getCursorReferenced (Cursor))),
         Kernel     => Kernel,
         Has_Type_Inst => False,
         Clang_Type_Inst => <>);
   end Cursor_As_Entity;

   ---------------------
   -- Get_Declaration --
   ---------------------

   overriding function Get_Declaration
     (Entity : Clang_Entity) return General_Entity_Declaration
   is
      Cursor : Clang_Cursor;
      Def : Clang_Cursor;
      Cloc : Clang_Location;
   begin
      if Entity.Loc = No_Location then
         return No_General_Entity_Declaration;
      end if;

      Cursor := Get_Clang_Cursor (Entity);
      Def := clang_getCursorReferenced (Cursor);

      if Cursor = Def then
         Def := clang_getCanonicalCursor (Cursor);
      end if;

      Cloc := clang_getCursorLocation (Def);

      return General_Entity_Declaration'
        (Loc => To_General_Location (Entity.Kernel, Cloc),
         Name => +Spelling (Def),
         Body_Is_Full_Declaration => clang_isCursorDefinition (Def) /= 0);
   end Get_Declaration;

   ---------------------------
   -- Caller_At_Declaration --
   ---------------------------

   overriding function Caller_At_Declaration
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      pragma Unreferenced (Entity);
   begin
      return No_Root_Entity;
   end Caller_At_Declaration;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (K : Core_Kernel;
      F : Virtual_File;
      Offset : Offset_T) return General_Location
   is
      Ed : constant Editor_Buffer'Class := K.Get_Buffer_Factory.Get
        (F, Open_View => False, Focus => False, Open_Buffer => True);
      Loc : constant Editor_Location'Class :=
        Ed.New_Location (Natural (Offset));
   begin
      return
        General_Location'
          (File    => F,
           Project => GNATCOLL.Projects.No_Project,
           Line    => Loc.Line,
           Column  => Loc.Column);
   end To_General_Location;

   --------------
   -- Get_Body --
   --------------

   overriding function Get_Body
     (Entity : Clang_Entity;
      After  : General_Location := No_Location)
      return General_Location
   is

      pragma Unreferenced (After);

      Cursor : constant Clang_Cursor := Get_Clang_Cursor (Entity);
      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Entity.Kernel.Registry.Tree.Info_Set
             (Entity.Loc.File).First_Element);
      Context : Clang_Context := TU_Source.Context (F_Info.Project);
      USR : constant Symbol := Context.Sym_Table.Find
        (To_String ((clang_getCursorUSR (Cursor))));
      Entity_Body : constant Clang_Cursor :=
        clang_getCursorDefinition (Cursor);
      V : Info_Vectors;
      use type Interfaces.C.int;
      use VFS_To_Refs_Maps;
      Loc : Offset_T;
      Fil : Virtual_File := No_File;
   begin
      if clang_Cursor_isNull (Entity_Body) = 0 then
         return To_General_Location
           (Entity.Kernel, clang_getCursorLocation (Entity_Body));
      else
         for C in Context.Refs.Iterate loop
            exit when Fil /= No_File;
            if Element (C).Contains (USR) then
               V := Element (C).Element (USR);
               for I in V.Decls.First_Index .. V.Decls.Last_Index loop
                  exit when Fil /= No_File;
                  if V.Decls.Element (I).Is_Def then
                     Loc := V.Decls.Element (I).Loc;
                     Fil := Key (C);
                  end if;
               end loop;
            end if;
         end loop;
         if Loc /= 0 then
            return To_General_Location
              (Entity.Kernel, Fil, Loc);
         else
            return No_Location;
         end if;
      end if;
   end Get_Body;

   -----------------
   -- Get_Type_Of --
   -----------------

   overriding function Get_Type_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      T : constant CXType := clang_getCursorType (Get_Clang_Cursor (Entity));
   begin
      return Type_As_Entity (Entity, T);
   end Get_Type_Of;

   --------------------
   -- Type_As_Entity --
   --------------------

   function Type_As_Entity
     (From_Entity : Clang_Entity; Typ : Clang_Type) return Clang_Entity
   is
      Decl : constant Clang_Cursor := clang_getTypeDeclaration (Typ);
   begin
      return
        Clang_Entity'(From_Entity.Kernel,
                      From_Entity.Db,
                      +Spelling (Typ),
                      To_General_Location
                        (From_Entity.Kernel, clang_getCursorLocation (Decl)),
                      True, Typ);
   end Type_As_Entity;

   -------------------
   -- Returned_Type --
   -------------------

   overriding function Returned_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      Cursor : constant Clang_Cursor := Get_Clang_Cursor
        (Entity.Kernel, Entity.Get_Declaration.Loc);
      T : Clang_Type;
   begin
      if clang_getCursorKind (Cursor) = CXCursor_FunctionDecl then
         T := clang_getResultType (clang_getCursorType (Cursor));
         return Type_As_Entity (Entity, T);
      end if;
      return No_Root_Entity;
   end Returned_Type;

   --------------------
   -- Parent_Package --
   --------------------

   overriding function Parent_Package
     (Entity : Clang_Entity) return Root_Entity'Class is
      pragma Unreferenced (Entity);
   begin
      --  No packages in C/C++, maybe make it work for namespaces ???
      return No_Root_Entity;
   end Parent_Package;

   -------------
   -- As_Type --
   -------------

   function As_Type (Entity : Clang_Entity) return Clang_Type
   is
     (if Entity.Has_Type_Inst then Entity.Clang_Type_Inst
      else clang_getCursorType (Get_Clang_Cursor (Entity)));

   -------------
   -- Methods --
   -------------

   function Methods
     (Entity : Clang_Entity) return Cursors_Vectors.Vector
   is
      T : constant Clang_Type := As_Type (Entity);
      T_Decl : constant Clang_Cursor := clang_getTypeDeclaration (T);
   begin
      if T_Decl.kind in CXCursor_ClassDecl | CXCursor_StructDecl then
         return Get_Children (T_Decl, CXCursor_CXXMethod);
      else
         return Cursors_Vectors.Empty_Vector;
      end if;
   end Methods;

   ------------------
   -- Pointed_Type --
   ------------------

   overriding function Pointed_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      T : constant Clang_Type := As_Type (Entity);
      Res_T : constant Clang_Type := clang_getPointeeType (T);
   begin
      if Res_T.kind = CXType_Invalid then
         return No_Root_Entity;
      else
         return Type_As_Entity (Entity, Res_T);
      end if;
   end Pointed_Type;

   -----------------
   -- Renaming_Of --
   -----------------

   overriding function Renaming_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      T : constant Clang_Type := As_Type (Entity);
   begin
      if T.kind = CXType_Typedef then
         return Type_As_Entity (Entity, T);
      end if;

      return No_Root_Entity;
   end Renaming_Of;

   ---------------------
   -- Is_Primitive_Of --
   ---------------------

   overriding function Is_Primitive_Of
     (Entity : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if
        clang_getCursorKind (C) = CXCursor_CXXMethod
      then
         declare
            Class : constant Clang_Cursor := clang_getCursorSemanticParent (C);
            Base_Classes : Cursors_Vectors.Vector :=
              Get_Children (Class, CXCursor_CXXBaseSpecifier);
            Ret : Entity_Array (1 .. Natural (Base_Classes.Length) + 1);
         begin
            Ret (1) := new Clang_Entity'(Cursor_As_Entity (Entity, Class));
            for J in 2 .. Ret'Length loop
               Ret (J) := new Clang_Entity'
                 (Cursor_As_Entity
                    (Entity,
                     clang_getCursorReferenced (Base_Classes (J - 1))));
            end loop;
            return Ret;
         end;
      end if;
      return No_Entity_Array;
   end Is_Primitive_Of;

   -----------------
   -- Has_Methods --
   -----------------

   overriding function Has_Methods (E : Clang_Entity) return Boolean is
   begin
      return clang_getCursorKind (Get_Clang_Cursor (E)) in
        CXCursor_ClassDecl | CXCursor_ClassTemplate
          | CXCursor_ClassTemplatePartialSpecialization
            | CXCursor_StructDecl;
   end Has_Methods;

   ---------------
   -- Is_Access --
   ---------------

   overriding function Is_Access (E : Clang_Entity) return Boolean
   is
      T : constant Clang_Type := As_Type (E);
   begin
      return T.kind = CXType_Pointer
        or else (T.kind = CXType_Typedef and then
                 Libclang.Index.clang_getTypedefDeclUnderlyingType
                   (clang_getTypeDeclaration (T)).kind = CXType_Pointer);
   end Is_Access;

   -----------------
   -- Is_Abstract --
   -----------------

   overriding function Is_Abstract
     (E  : Clang_Entity) return Boolean
   is
     (for some M of Methods (E) => clang_CXXMethod_isPureVirtual (M) /= 0);

   --------------
   -- Is_Array --
   --------------

   overriding function Is_Array
     (E  : Clang_Entity) return Boolean
   is
     (As_Type (E).kind in
        CXType_ConstantArray
        | CXType_VariableArray
        | CXType_DependentSizedArray
        | CXType_IncompleteArray);

   ------------------------------
   -- Is_Printable_In_Debugger --
   ------------------------------

   overriding function Is_Printable_In_Debugger
     (E : Clang_Entity) return Boolean is
      pragma Unreferenced (E);
   begin
      --  Let's assume everything is printable in debugger for now
      return True;
   end Is_Printable_In_Debugger;

   -------------
   -- Is_Type --
   -------------

   overriding function Is_Type
     (E  : Clang_Entity) return Boolean
   is
     (E.Has_Type_Inst or else
        Get_Clang_Cursor (E).kind in
        CXCursor_StructDecl | CXCursor_ClassDecl | CXCursor_TypeAliasDecl
          | CXCursor_TypedefDecl | CXCursor_EnumDecl | CXCursor_UnionDecl);

   -------------------
   -- Is_Subprogram --
   -------------------

   overriding function Is_Subprogram
     (E  : Clang_Entity) return Boolean is
   begin
      return Get_Clang_Cursor (E).kind
      in CXCursor_FunctionDecl | CXCursor_CXXMethod
        | CXCursor_FunctionTemplate;
   end Is_Subprogram;

   ------------------
   -- Is_Container --
   ------------------

   overriding function Is_Container
     (E  : Clang_Entity) return Boolean is
   begin
      return E.Is_Array or else Get_Clang_Cursor (E).kind in
        CXCursor_StructDecl | CXCursor_ClassDecl;
   end Is_Container;

   ----------------
   -- Is_Generic --
   ----------------

   overriding function Is_Generic
     (E  : Clang_Entity) return Boolean is
   begin
      return Get_Clang_Cursor (E).kind in
        CXCursor_ClassTemplatePartialSpecialization
          | CXCursor_ClassTemplate
            | CXCursor_FunctionTemplate
              | CXCursor_TemplateRef;
   end Is_Generic;

   ---------------
   -- Is_Global --
   ---------------

   overriding function Is_Global
     (E  : Clang_Entity) return Boolean is
   begin
      return
        clang_getCursorLexicalParent
          (Get_Clang_Cursor (E)).kind = CXCursor_TranslationUnit;
   end Is_Global;

   ---------------------
   -- Is_Static_Local --
   ---------------------

   overriding function Is_Static_Local
     (E  : Clang_Entity) return Boolean is
      pragma Unreferenced (E);
   begin
      return False;
   end Is_Static_Local;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   overriding function Is_Predefined_Entity
     (E  : Clang_Entity) return Boolean is
      pragma Unreferenced (E);
   begin
      return False;
   end Is_Predefined_Entity;

   -------------------
   -- Documentation --
   -------------------

   overriding procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : Clang_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True)
   is
      pragma Unreferenced (Handler, Check_Constructs, Look_Before_First);
      Comment_Range : constant CXSourceRange :=
        clang_Cursor_getCommentRange (Get_Clang_Cursor (Entity));
      Buf : constant Editor_Buffer'Class :=
        Entity.Kernel.Get_Buffer_Factory.Get
          (Entity.Loc.File, Open_View => False, Focus => False);
      Loc_Start, Loc_End : Sloc_T;
   begin
      Loc_Start := To_Sloc_T (clang_getRangeStart (Comment_Range));
      Loc_End := To_Sloc_T (clang_getRangeEnd (Comment_Range));
      Formater.Add_Comments
        (Buf.Get_Chars (Buf.New_Location (Loc_Start.Line, Loc_Start.Column),
         Buf.New_Location (Loc_End.Line, Loc_End.Column)));
   end Documentation;

   ------------------
   -- End_Of_Scope --
   ------------------

   overriding function End_Of_Scope
     (Entity : Clang_Entity) return General_Location is
     (To_General_Location
        (Entity.Kernel,
         clang_getRangeEnd
           (clang_getCursorExtent (Get_Clang_Cursor (Entity)))));

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   overriding function Is_Parameter_Of
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if C.kind = CXCursor_ParmDecl then
         return Cursor_As_Entity
           (Entity, clang_getCursorSemanticParent (C));
      end if;
      return No_Root_Entity;
   end Is_Parameter_Of;

   ---------------
   -- Overrides --
   ---------------

   overriding function Overrides
     (Entity : Clang_Entity) return Root_Entity'Class
   is
      type Cursor_Ptr is access Clang_Cursor;
      pragma Convention (C, Cursor_Ptr);

      use Interfaces.C;

      procedure Get_Overriden_Cursors
        (Cursor : Clang_Cursor;
         Overridden : access Cursor_Ptr;
         Num_Overridden : access unsigned);
      pragma Import (C, Get_Overriden_Cursors, "clang_getOverriddenCursors");

      C : aliased Cursor_Ptr;
      Ign : aliased unsigned;

      Ret : Clang_Entity;
   begin
      Get_Overriden_Cursors
        (Get_Clang_Cursor (Entity), C'Access, Ign'Access);

      Ret := Cursor_As_Entity (Entity, C.all);

      clang_disposeOverriddenCursors (C);

      return Ret;
   end Overrides;

   -----------------
   -- Instance_Of --
   -----------------

   overriding function Instance_Of
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   -------------
   -- Methods --
   -------------

   overriding function Methods
     (Entity            : Clang_Entity;
      Include_Inherited : Boolean) return Entity_Array
   is
      pragma Unreferenced (Include_Inherited);
   begin
      return To_Entity_Array (Entity, Methods (Entity));
   end Methods;

   ---------------------
   -- To_Entity_Array --
   ---------------------

   function To_Entity_Array
     (From_Entity : Clang_Entity;
      Cursors : Cursors_Vectors.Vector) return Entity_Array
   is
      Ret : Entity_Array (1 .. Natural (Cursors.Length));
   begin
      for I in Ret'Range loop
         Ret (I) :=
           new Clang_Entity'(Cursor_As_Entity (From_Entity, Cursors (I)));
      end loop;
      return Ret;
   end To_Entity_Array;

   ------------
   -- Fields --
   ------------

   overriding function Fields
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if C.kind in CXCursor_StructDecl | CXCursor_ClassDecl
        | CXCursor_ClassTemplate
      then
         return To_Entity_Array (Entity, Get_Children (C, CXCursor_FieldDecl));
      end if;
      return No_Entity_Array;
   end Fields;

   --------------
   -- Literals --
   --------------

   overriding function Literals
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      return To_Entity_Array
        (Entity, Get_Children (C, CXCursor_EnumConstantDecl));
   end Literals;

   -----------------------
   -- Formal_Parameters --
   -----------------------

   overriding function Formal_Parameters
     (Entity            : Clang_Entity) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);
   begin
      if Entity.Is_Generic then
         return To_Entity_Array
           (Entity, Get_Children (C, CXCursor_TemplateTypeParameter));
      end if;
      return No_Entity_Array;
   end Formal_Parameters;

   ---------------------
   -- Discriminant_Of --
   ---------------------

   overriding function Discriminant_Of
     (Entity            : Clang_Entity) return Root_Entity'Class
   is
      --  Not applicable to C/C++
      (No_Root_Entity);

   -------------------
   -- Discriminants --
   -------------------

   overriding function Discriminants
     (Entity            : Clang_Entity) return Entity_Array
   is
      --  Not applicable to C/C++
     (No_Entity_Array);

   --------------------
   -- Component_Type --
   --------------------

   overriding function Component_Type
     (Entity : Clang_Entity) return Root_Entity'Class
   is
   begin
      if Entity.Is_Array then
         return Type_As_Entity
           (Entity, clang_getArrayElementType (As_Type (Entity)));
      end if;
      return No_Root_Entity;
   end Component_Type;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
     (Entity : Clang_Entity) return Entity_Array is (No_Entity_Array);

   -----------------
   -- Child_Types --
   -----------------

   overriding function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array is (No_Entity_Array);

   ------------------
   -- Parent_Types --
   ------------------

   overriding function Parent_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array
   is
      C : constant Clang_Cursor := Get_Clang_Cursor (Entity);

      --  Return an array containing all the ancestors of the types contained
      --  in the 'Entities' array
      function Children_Parent_Types
        (Entities : Entity_Array) return Entity_Array
      is
        (if Entities = No_Entity_Array
         then No_Entity_Array
         else Parent_Types
                (Entities (Entities'First).all, True)
                 & Children_Parent_Types
                     (Entities (Entities'First + 1 .. Entities'Last)));
   begin
      if C.kind in CXCursor_ClassDecl
        | CXCursor_ClassTemplate | CXCursor_StructDecl
      then
         declare
            Parents : constant Entity_Array :=
              To_Entity_Array
                (Entity, Get_Children (C, CXCursor_CXXBaseSpecifier));
         begin
            return
              (if Recursive
               --  If recursive, we want the parent types plus all their
               --  ancestors
               then Parents & Children_Parent_Types (Parents)
               --  Else, we only want the parent types
               else Parents);
         end;
      end if;
      return No_Entity_Array;
   end Parent_Types;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters
     (Entity : Clang_Entity) return Parameter_Array
   is (No_Parameters);

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   overriding function Get_All_Called_Entities
     (Entity : Clang_Entity) return Abstract_Entities_Cursor'Class
   is (No_Entities_Cursor);

   -------------------------
   -- Find_All_References --
   -------------------------

   overriding function Find_All_References
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
   begin
      if Entity.Is_Global then
         return Find_All_References_Global
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Kind);
      else
         return Find_All_References_Local
           (Entity, In_File, In_Scope, Include_Overriding, Include_Overridden,
            Include_Implicit, Include_All, Kind);
      end if;
   end Find_All_References;

   use GNATCOLL.VFS;

   type Indexer_Data is record
      Ret_Iterator  : access Clang_Reference_Iterator;
      Clang_Db      : Clang_Database;
      File          : Virtual_File;
      Sought_Cursor : Clang_Cursor;
   end record;

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data; Info : Clang_Decl_Info);
   procedure Index_Reference
     (Client_Data : in out Indexer_Data; Info : Clang_Ref_Info);

   function Abort_Query
     (Client_Data : in out Indexer_Data) return Boolean is (False);
   procedure Diagnostic
     (Client_Data : in out Indexer_Data;
      Diagnostics : Clang_Diagnostic_Set) is null;
   procedure Entered_Main_File
     (Client_Data : in out Indexer_Data; File : Virtual_File) is null;
   procedure Included_File
     (Client_Data : in out Indexer_Data;
      Included_File_Info : Clang_Included_File_Info) is null;
   procedure Started_Translation_Unit
     (Client_Data : in out Indexer_Data) is null;

   -----------------------
   -- Index_Declaration --
   -----------------------

   procedure Index_Declaration
     (Client_Data : in out Indexer_Data;
      Info        : Clang_Decl_Info)
   is
      Loc : constant Clang_Location :=
        clang_indexLoc_getCXSourceLocation (Info.loc);
   begin
      if Is_From_Main_File (Loc)
      and then
          Clang_Cursor
            (Info.entityInfo.cursor) = Client_Data.Sought_Cursor
      then
         Client_Data.Ret_Iterator.Elements.Append
           (Clang_Reference'
              (Client_Data.File, To_Offset_T (Loc), Client_Data.Clang_Db));
      end if;
   end Index_Declaration;

   ---------------------
   -- Index_Reference --
   ---------------------

   procedure Index_Reference
     (Client_Data : in out Indexer_Data;
      Info   : Clang_Ref_Info)
   is
      Loc : constant Clang_Location :=
        clang_indexLoc_getCXSourceLocation (Info.loc);
   begin
      if Is_From_Main_File (Loc) then
         if Clang_Cursor
           (Info.referencedEntity.cursor) = Client_Data.Sought_Cursor
         then
            Client_Data.Ret_Iterator.Elements.Append
              (Clang_Reference'
                 (Client_Data.File, To_Offset_T (Loc), Client_Data.Clang_Db));
         end if;
      end if;
   end Index_Reference;

   package Indexer is new Source_File_Indexer
     (Client_Data_T => Indexer_Data);

   -------------------------------
   -- Find_All_References_Local --
   -------------------------------

   function Find_All_References_Local
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Kind, In_File);

      Ret : aliased Clang_Reference_Iterator :=
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => new Clang_Entity_Ref_Vectors.Vector,
           Current_Index => 1);

      Index_Data : constant Indexer_Data := Indexer_Data'
        (Ret_Iterator  => Ret'Unchecked_Access,
         Clang_Db      => Entity.Db,
         File          => Entity.Loc.File,
         Sought_Cursor =>
           clang_getCursorReferenced (Get_Clang_Cursor (Entity)));

      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Entity.Kernel.Registry.Tree.Info_Set
             (Entity.Loc.File).First_Element);

      Index_Action : constant Clang_Index_Action :=
        TU_Source.Context (F_Info.Project).Index_Action;
   begin
      Indexer.Index_Translation_Unit
        (Index_Action, Index_Data, CXIndexOpt_IndexFunctionLocalSymbols,
         TU_Source.Translation_Unit (Entity.Kernel, Entity.Loc.File));

      return Ret;
   end Find_All_References_Local;

   --------------------------------
   -- Find_All_References_Global --
   --------------------------------

   function Find_All_References_Global
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class
   is
      pragma Unreferenced
        (In_Scope, Include_Overriding, Include_Overridden,
         Include_Implicit, Include_All, Kind);

      Cursor : constant Clang_Cursor := Get_Clang_Cursor (Entity);
      F_Info : constant File_Info'Class :=
        File_Info'Class
          (Entity.Kernel.Registry.Tree.Info_Set
             (Entity.Loc.File).First_Element);
      Context : Clang_Context := TU_Source.Context (F_Info.Project);
      USR : constant Symbol := Context.Sym_Table.Find
        (To_String ((clang_getCursorUSR (Cursor))));
      V : Info_Vectors;

      use type Interfaces.C.int;
      use VFS_To_Refs_Maps;

      Ret : constant Clang_Reference_Iterator :=
        Clang_Reference_Iterator'
          (Entity        => Entity,
           Db            => Entity.Db,
           Elements      => new Clang_Entity_Ref_Vectors.Vector,
           Current_Index => 1);

      procedure Find_References_In_File_Map
        (F : Virtual_File; M : Sym_To_Loc_Map);

      procedure Find_References_In_File_Map
        (F : Virtual_File; M : Sym_To_Loc_Map) is
      begin
         if M.Contains (USR) then
            V := M.Element (USR);
            for I in V.Decls.First_Index .. V.Decls.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (File     => F,
                     Offset   => V.Decls.Element (I).Loc,
                     Clang_Db => Entity.Db));
            end loop;
            for I in V.Refs.First_Index .. V.Refs.Last_Index loop
               Ret.Elements.Append
                 (Clang_Reference'
                    (File     => F,
                     Offset   => V.Refs.Element (I).Loc,
                     Clang_Db => Entity.Db));
            end loop;
         end if;
      end Find_References_In_File_Map;
   begin
      if In_File /= No_File then
         Find_References_In_File_Map
           (In_File, Context.Refs.Element (In_File));
      else
         for C in Context.Refs.Iterate loop
            Find_References_In_File_Map (Key (C), Element (C));
         end loop;
      end if;

      return Ret;
   end Find_All_References_Global;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Ref : Clang_Reference) return Root_Entity'Class
   is
   begin
      Put_Line ("<++++ In Clang_Reference Get_Entity ++++>");
      return
        Get_Entity
          (Ref.Clang_Db,
           Ref.Clang_Db.Kernel.Databases, "",
           Ref.Get_Location);
   end Get_Entity;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Ref : Clang_Reference) return General_Location is
   begin
      return To_General_Location (Ref.Clang_Db.Kernel, Ref.File, Ref.Offset);
   end Get_Location;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Clang_Reference_Iterator) return Boolean
   is
   begin
      return Iter.Current_Index > Natural (Iter.Elements.Length);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Clang_Reference_Iterator)
   is
   begin
      Iter.Current_Index := Iter.Current_Index + 1;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Clang_Reference_Iterator) return Root_Entity_Reference'Class
   is
   begin
      if not Iter.At_End then
         return Iter.Elements.Element (Iter.Current_Index);
      end if;
      return No_Root_Entity_Reference;
   end Get;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Iter : Clang_Reference_Iterator) return Root_Entity'Class
   is
   begin
      return Iter.Entity;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iter : in out Clang_Reference_Iterator) is
   begin
      Free (Iter.Elements);
   end Destroy;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Iter : Clang_Reference_Iterator) return Integer is
   begin
      return Iter.Current_Index;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Iter : Clang_Reference_Iterator) return Integer
   is
     (Integer (Iter.Elements.Length));

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Ref  : Clang_Reference) return String is
     (Ref.Get_Entity.Get_Display_Kind);

end Clang_Xref;
