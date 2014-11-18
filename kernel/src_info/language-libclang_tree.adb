------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Language.Libclang; use Language.Libclang;
with Libclang.File; use Libclang.File;
with clang_c_Index_h; use clang_c_Index_h;
with Interfaces.C; use Interfaces.C;
with String_Utils;

package body Language.Libclang_Tree is

   function Node_From_Cursor
     (C : Clang_Cursor; N : Clang_Node) return Semantic_Node'Class;

   function To_Sloc_T (Arg : CXSourceLocation) return Sloc_T;

   function Filter_Children (C : Clang_Cursor) return Boolean;

   ------------
   -- Filter --
   ------------

   function Filter_Children (C : Clang_Cursor) return Boolean
   is
      K : constant Clang_Cursor_Kind := Kind (C);
   begin
      if K in CXCursor_UnionDecl | CXCursor_StructDecl
        and then Spelling (C) = ""
      then
         return False;
      end if;
      return True;
   end Filter_Children;

   ----------------------
   -- Node_From_Cursor --
   ----------------------

   function Node_From_Cursor
     (C : Clang_Cursor; N : Clang_Node) return Semantic_Node'Class
   is
     (if C = No_Cursor
      then No_Semantic_Node
      else Clang_Node'(N.Kernel, C, N.Ref_File));

   ---------------
   -- To_Sloc_T --
   ---------------

   function To_Sloc_T (Arg : CXSourceLocation) return Sloc_T is

      procedure clang_getSpellingLocation
        (location : CXSourceLocation;
         file : access CXFile;
         line : access unsigned;
         column : access unsigned;
         offset : access unsigned);

      pragma Import (C, clang_getSpellingLocation,
                     "clang_getSpellingLocation");

      Line, Column, Offset : aliased unsigned;
      File : aliased CXFile;
   begin
      clang_getSpellingLocation
        (Arg, File'Access, Line'Access, Column'Access, Offset'Access);

      return Sloc_T'(Natural (Line), Visible_Column_Type (Column),
                     Natural (Offset));
   end To_Sloc_T;

   ------------------------------------
   -- Clang_Tree_Provider primitives --
   ------------------------------------

   ------------
   -- Create --
   ------------

   function Create (K : Core_Kernel) return Semantic_Tree_Provider_Access
   is
   begin
      return new Clang_Tree_Provider'(Kernel => K);
   end Create;

   -----------------------
   -- Get_Tree_For_File --
   -----------------------

   overriding function Get_Tree_For_File
     (Self : Clang_Tree_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
   is
   begin
      return Abstract_Clang_Tree'
        (Self.Kernel,
         File,
         Translation_Unit (Self.Kernel, File));
   end Get_Tree_For_File;

   ------------------------------------
   -- Abstract_Clang_Tree primitives --
   ------------------------------------

   ----------------
   -- Root_Nodes --
   ----------------

   overriding function Root_Nodes
     (Self : Abstract_Clang_Tree) return Semantic_Node_Array'Class
   is
   begin
      return
        Clang_Node_Array'
          (Cursors_Holders.To_Holder
             (Toplevel_Nodes (Self.Tu, Filter_Children'Access)),
           Self.Kernel, Self.File);
   end Root_Nodes;

   -------------------
   -- Root_Iterator --
   -------------------

   overriding function Root_Iterator
     (Self : Abstract_Clang_Tree) return Semantic_Tree_Iterator'Class
   is
      Initial_List : Clang_Iterator_Lists.List;
      Initial_Children : constant Cursors_Vectors.Vector :=
        Get_Children (Root_Cursor (Self.Tu));
   begin
      for I in 1 .. Initial_Children.Length loop
         Initial_List.Append (Initial_Children.Element (Natural (I)));
      end loop;

      return Clang_Tree_Iterator'
        (Self.Kernel, Self.File, Initial_List, Initial_List.First, False);
   end Root_Iterator;

   -------------
   -- Node_At --
   -------------

   overriding function Node_At
     (Self : Abstract_Clang_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class
   is
      pragma Unreferenced (Category_Filter);
      Clang_Loc : constant Clang_Location := clang_getLocation
        (Self.Tu,
         File
           (Self.Tu, String (Self.File.Full_Name.all)),
         unsigned (Sloc.Line), unsigned (Sloc.Column));
   begin
      return Clang_Node'(Self.Kernel,
                         clang_getCursor (Self.Tu, Clang_Loc),
                         Self.File);
   end Node_At;

   overriding function File
     (Self : Abstract_Clang_Tree) return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Self.File;
   end File;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Self : Abstract_Clang_Tree)
   is
      TU : Clang_Translation_Unit :=
        Translation_Unit (Self.Kernel, Self.File, Reparse => True);
      pragma Unreferenced (TU);
   begin
      null;
   end Update;

   ---------------------------
   -- Clang_Node primitives --
   ---------------------------

   type Clang_Cursor_Kind_To_Category_Array is
     array (unsigned range 1 .. 600) of Language_Category;

   Clang_Cursor_Kind_To_Category :
   constant Clang_Cursor_Kind_To_Category_Array :=
     (CXCursor_StructDecl => Cat_Structure,
      CXCursor_UnionDecl => Cat_Union,
      CXCursor_UnexposedDecl => Cat_Type,
      CXCursor_ClassDecl => Cat_Class,
      CXCursor_EnumDecl => Cat_Type,
      CXCursor_FieldDecl => Cat_Field,
      CXCursor_EnumConstantDecl => Cat_Field,
      CXCursor_FunctionDecl => Cat_Function,
      CXCursor_VarDecl => Cat_Variable,
      CXCursor_ParmDecl => Cat_Parameter,
      CXCursor_TypedefDecl => Cat_Type,
      CXCursor_CXXMethod => Cat_Method,
      CXCursor_Namespace => Cat_Namespace,
      CXCursor_Constructor => Cat_Constructor,
      CXCursor_Destructor => Cat_Destructor,
      CXCursor_ConversionFunction => Cat_Function,
      CXCursor_FunctionTemplate => Cat_Function,
      CXCursor_ClassTemplate => Cat_Class,
      CXCursor_ClassTemplatePartialSpecialization => Cat_Class,
      CXCursor_NamespaceAlias => Cat_Namespace,
      CXCursor_UsingDirective => Cat_Use,
      CXCursor_UsingDeclaration => Cat_Use,
      CXCursor_TypeAliasDecl => Cat_Type,
      others => Cat_Unknown);

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Clang_Node) return Language_Category is
   begin
      return Clang_Cursor_Kind_To_Category (Kind (Self.Cursor));
   end Category;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : Clang_Node) return Semantic_Node'Class
   is
   begin
      return Node_From_Cursor
           (clang_getCursorSemanticParent (Self.Cursor), Self);
   end Parent;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self : Clang_Node) return Semantic_Node_Array'Class
   is
      C : constant Cursors_Vectors.Vector := Get_Children
        (Self.Cursor);
   begin
      if Kind (Self.Cursor) = CXCursor_TypedefDecl
        and then C.Length = 1
        and then
          Kind (C.Element (1)) in CXCursor_StructDecl | CXCursor_UnionDecl
      then
         if Spelling (C.Element (1)) = "" then
            return Clang_Node_Array'
              (Cursors_Holders.To_Holder
                 (Get_Children (C.Element (1))),
               Self.Kernel, Self.File);
         else
            return Clang_Node_Array'
              (Cursors_Holders.To_Holder (Cursors_Vectors.Empty_Vector),
               Self.Kernel, Self.File);
         end if;
      end if;
      return Clang_Node_Array'
        (Cursors_Holders.To_Holder (C),
         Self.Kernel,
         Self.File);
   end Children;

   -----------------
   -- First_Child --
   -----------------

   overriding function First_Child
     (Self : Clang_Node) return Semantic_Node'Class is
   begin
      return Self.Children.Get (1);
   end First_Child;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Clang_Node) return GNATCOLL.Symbols.Symbol is
   begin
      return Self.Kernel.Symbols.Find (Spelling (Self.Cursor));
   end Name;

   -------------
   -- Profile --
   -------------

   overriding function Profile
     (Self : Clang_Node) return String
   is
      K : constant Clang_Cursor_Kind := Kind (Self.Cursor);
   begin
      if K in CXCursor_FunctionDecl | CXCursor_FunctionTemplate
        | CXCursor_ConversionFunction
      then
         declare
            Profile : constant String := Display_Name (Self.Cursor);
            Name : constant String := GNATCOLL.Symbols.Get (Self.Name).all;
         begin
            return Profile (Name'Length + 1 .. Profile'Last);
         end;
      elsif K = CXCursor_FieldDecl then
         return Spelling (clang_getCursorType (Self.Cursor));
      end if;

      return "";
   end Profile;

   -----------------
   -- Counterpart --
   -----------------

   overriding function Definition
     (Self : Clang_Node) return Semantic_Node'Class
   is
   begin
      return Node_From_Cursor (clang_getCursorDefinition (Self.Cursor), Self);
   end Definition;

   --------------
   -- Sloc_Def --
   --------------

   overriding function Sloc_Def
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T (clang_getCursorLocation (Self.Cursor));
   end Sloc_Def;

   ----------------
   -- Sloc_Start --
   ----------------

   overriding function Sloc_Start
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T
        (clang_getRangeStart (clang_getCursorExtent (Self.Cursor)));
   end Sloc_Start;

   --------------
   -- Sloc_End --
   --------------

   overriding function Sloc_End
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T
        (clang_getRangeEnd (clang_getCursorExtent (Self.Cursor)));
   end Sloc_End;

   --------------
   -- Get_Hash --
   --------------

   overriding function Get_Hash
     (Self : Clang_Node) return Hash_Type is
   begin
      return Hash_Type (clang_hashCursor (Self.Cursor));
   end Get_Hash;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Clang_Node) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Ref_File;
   end File;

   --------------------
   -- Is_Declaration --
   --------------------

   overriding function Is_Declaration
     (Self : Clang_Node) return Boolean is
   begin
      return Self.Category /= Cat_Unknown
        and then (clang_isCursorDefinition (Self.Cursor) = 0);
   end Is_Declaration;

   ----------------
   -- Visibility --
   ----------------

   overriding function Visibility
     (Self : Clang_Node) return Semantic_Node_Visibility is
      pragma Unreferenced (Self);
   begin
      --  TODO: At a later stage figure that out for C++ class members
      return Visibility_Public;
   end Visibility;

   ---------------
   -- Unique_Id --
   ---------------

   overriding function Unique_Id
     (Self : Clang_Node) return String is
   begin
      return To_String (clang_getCursorUSR (Self.Cursor));
   end Unique_Id;

   ----------
   -- Info --
   ----------

   overriding function Info
     (Self : Clang_Node) return Semantic_Node_Info
   is
      use String_Utils;
   begin
      return A : Semantic_Node_Info do
         A := (Category   => Self.Category,
               Name       => Self.Name,
               Profile    => +Self.Profile,
               Unique_Id  => +Self.Unique_Id,
               Is_Decl    => Self.Is_Declaration,
               Visibility => Self.Visibility,
               Sloc_Start => Self.Sloc_Start,
               Sloc_Def   => Self.Sloc_Def);
      end return;
   end Info;

   ------------------------
   -- Documentation_Body --
   ------------------------

   overriding function Documentation_Body
     (Self : Clang_Node) return String is
   begin
      return To_String (clang_Cursor_getRawCommentText (Self.Cursor));
   end Documentation_Body;

   --------------------------
   -- Documentation_Header --
   --------------------------

   overriding function Documentation_Header
     (Self : Clang_Node) return String is
   begin
      return To_String (clang_Cursor_getBriefCommentText (Self.Cursor));
   end Documentation_Header;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Clang_Tree_Iterator)
   is
      use Clang_Iterator_Lists;
      Vec : constant Cursors_Vectors.Vector :=
        Get_Children (Element (It.Current_Cursor));
      Cursor_Before : constant Clang_Iterator_Lists.Cursor :=
        Next (It.Current_Cursor);
   begin
      for I in 1 .. Vec.Length loop
         It.Elements.Insert (Cursor_Before, Vec.Element (Natural (I)));
      end loop;
      Next (It.Current_Cursor);
   end Next;

   -------------
   -- Element --
   -------------

   overriding function Element
     (It : Clang_Tree_Iterator) return Semantic_Node'Class
   is
      use Clang_Iterator_Lists;
   begin
      return (if not Has_Element (It)
              then No_Semantic_Node
              else Clang_Node'(It.Kernel,
                Element (It.Current_Cursor), It.File));
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (It : Clang_Tree_Iterator) return Boolean
   is
      use Clang_Iterator_Lists;
   begin
      return not (It.Current_Cursor = Clang_Iterator_Lists.No_Element
                  or else Element (It.Current_Cursor) = No_Cursor);
   end Has_Element;

end Language.Libclang_Tree;
