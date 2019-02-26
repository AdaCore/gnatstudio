------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with Ada.Containers.Generic_Array_Sort;

with Interfaces.C;                      use Interfaces.C;
with Libclang.Task_Parser_Pool;         use Libclang.Task_Parser_Pool;
with String_Utils;

package body Language.Libclang_Tree is

   type Update_Async_Record is new Parse_Callback with record
      Kernel : Core_Kernel;
   end record;

   overriding procedure Call
     (Self : access Update_Async_Record;
      File : GNATCOLL.VFS.Virtual_File;
      TU   : Clang_Translation_Unit);

   function Node_From_Cursor
     (C : Clang_Cursor; N : Clang_Node) return Semantic_Node'Class;

   ----------
   -- Call --
   ----------

   overriding procedure Call
     (Self : access Update_Async_Record;
      File : GNATCOLL.VFS.Virtual_File;
      TU   : Clang_Translation_Unit)
   is
      pragma Unreferenced (TU);
   begin
      Self.Kernel.Semantic_Tree_Updated (File);
   end Call;

   ------------
   -- Filter --
   ------------

   function Filter_Children (C : Clang_Cursor) return Boolean
   is
     (not (Kind (C) in CXCursor_UnionDecl | CXCursor_StructDecl
           and then Spelling (C) = ""));

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
                     Offset_T (Offset));
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
     (Self    : in out Clang_Tree_Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
   is
      pragma Unreferenced (Context);
   begin
      return Abstract_Clang_Tree'(Self.Kernel, File);
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
      Initial_List : Clang_Iterator_Lists_Ref.Holder :=
        Clang_Iterator_Lists_Ref.To_Holder
          (Clang_Iterator_Lists.Empty_List);

      Initial_Children : constant Cursors_Arrays.Array_Type :=
        Toplevel_Nodes (Self.Tu, Filter_Children'Access);
   begin
      for Cursor of Initial_Children loop
         Initial_List.Reference.Append (Cursor);
      end loop;

      declare
         It : constant Clang_Iterator_Lists.Cursor :=
           Initial_List.Reference.First;
      begin
         return Clang_Tree_Iterator'
           (Self.Kernel, Self.File, Initial_List, It, False);
      end;
   end Root_Iterator;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Abstract_Clang_Tree) return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Self.File;
   end File;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Self : in out Abstract_Clang_Tree) is
      TU : Clang_Translation_Unit :=
        Translation_Unit (Self.Kernel, Self.File, Reparse => True);
      pragma Unreferenced (TU);
   begin
      null;
   end Update;

   ------------------
   -- Update_Async --
   ------------------

   overriding procedure Update_Async (Self : in out Abstract_Clang_Tree) is
      Callback : Parse_Callback_Access := new Update_Async_Record'
        (Parse_Callback with Kernel => Self.Kernel);
   begin
      Enqueue_Translation_Unit
        (Kernel   => Self.Kernel,
         File     => Self.File,
         Reparse  => True,
         Callback => Callback);
   end Update_Async;

   ---------------------------
   -- Clang_Node primitives --
   ---------------------------

   type Clang_Cursor_Kind_To_Category_Array is
     array (CXCursor_MIN_VALUE .. CXCursor_MAX_VALUE) of Language_Category;

   Clang_Cursor_Kind_To_Category :
   constant Clang_Cursor_Kind_To_Category_Array :=
     (CXCursor_StructDecl => Cat_Structure,
      CXCursor_UnionDecl => Cat_Union,
      CXCursor_UnexposedDecl => Cat_Type,
      CXCursor_ClassDecl => Cat_Class,
      CXCursor_EnumDecl => Cat_Type,
      CXCursor_FieldDecl => Cat_Field,
      CXCursor_CompoundStmt => Cat_Declare_Block,
      CXCursor_EnumConstantDecl => Cat_Field,
      CXCursor_FunctionDecl => Cat_Function,
      CXCursor_CXXMethod => Cat_Method,
      CXCursor_VarDecl => Cat_Variable,
      CXCursor_ParmDecl => Cat_Parameter,
      CXCursor_TypedefDecl => Cat_Type,
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
      CXCursor_IfStmt => Cat_If_Statement,
      CXCursor_ForStmt => Cat_Loop_Statement,
      CXCursor_WhileStmt => Cat_Loop_Statement,
      CXCursor_DoStmt => Cat_Loop_Statement,
      CXCursor_CaseStmt => Cat_Case_Statement,
      others => Cat_Unknown);

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Clang_Node) return Language_Category
   is
      C_Cat : constant unsigned := (Kind (Self.Cursor));
   begin
      return Clang_Cursor_Kind_To_Category (C_Cat);
   end Category;

   -------------
   -- Node_At --
   -------------

   overriding function Node_At
     (Self : Abstract_Clang_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class
   is
      Line_Offset : constant Natural :=
        Self.Kernel.Get_Buffer_Factory.Get
          (Self.File, Open_View => False, Focus => False, Open_Buffer => True)
        .New_Location (Sloc.Line, Sloc.Column).Line_Offset;

      Top_Cursor : constant Clang_Cursor :=
        Cursor_At (Self.Tu, Self.File, Sloc.Line, Line_Offset + 1);
      Cursor : Clang_Cursor := Top_Cursor;
      Parent : Clang_Cursor;

      use Cursors_Arrays;

      function In_Range (Containing : Clang_Cursor) return Boolean
      is (In_Range (Top_Cursor, Containing));

      function Next_Child (C : Clang_Cursor) return Array_Type
      is (Get_Children (C, In_Range'Access));

      function Children_Chain (C : Clang_Cursor) return Array_Type;
      function Children_Chain (C : Clang_Cursor) return Array_Type
      is
         Child : constant Array_Type := Next_Child (C);
      begin
         return (if Child = Empty_Array then Child
                 else Child & Children_Chain (Child (1)));
      end Children_Chain;

      function Fullfills_Filter (C : Clang_Cursor) return Boolean
      is
        (Is_In (Clang_Cursor_Kind_To_Category (C.kind), Category_Filter));

   begin
      --  If we are in a case in which we don't have a filter, let's try to use
      --  clang's lexical parent first.
      --  NOTE: The only reason we do that is for potential performance gains,
      --  but their need has not been proven yet. It might be better to have an
      --  unified algorithm

      if Category_Filter = Null_Category_Array then
         Parent := Lexical_Parent (Cursor);
         if Parent /= No_Cursor then
            return Clang_Node'(Self.Kernel, Parent, Self.File);
         end if;
      end if;

      --  Get the topmost cursor just before translation unit
      loop
         Parent := Semantic_Parent (Cursor);
         exit when Parent = No_Cursor
           or else Parent.kind = CXCursor_TranslationUnit;
         Cursor := Parent;
      end loop;

      --  Semantic parent did jump to a non lexical containing cursor
      if not In_Range (Sought     => Top_Cursor,
                       Containing => Cursor)
      then
         Cursor := Root_Cursor (Self.Tu);
      end if;

      --  Now explore down to cursor, and keep the list
      declare
         C1 : constant Array_Type := Cursor & Children_Chain (Cursor);
         C  : constant Array_Type
           := (if Category_Filter /= Null_Category_Array
               then Filter (C1, Fullfills_Filter'Access)
               else C1);
      begin
         return (if C = Empty_Array then No_Semantic_Node
                 else Clang_Node'(Self.Kernel, C (C'Last), Self.File));
      end;

   end Node_At;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : Clang_Node) return Semantic_Node'Class
   is
   begin
      return Node_From_Cursor
        (Semantic_Parent (Self.Cursor), Self);
   end Parent;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self : Clang_Node)
      return Semantic_Node_Array'Class
   is
      C : constant Cursors_Arrays.Array_Type := Get_Children (Self.Cursor);
   begin
      if Kind (Self.Cursor) = CXCursor_TypedefDecl
        and then C'Length = 1
        and then
          Kind (C (1)) in CXCursor_StructDecl | CXCursor_UnionDecl
      then
         if Spelling (C (1)) = "" then
            return Clang_Node_Array'
              (Cursors_Holders.To_Holder
                 (Get_Children (C (1))),
               Self.Kernel, Self.File);
         else
            return Clang_Node_Array'
              (Cursors_Holders.To_Holder (Cursors_Arrays.Empty_Array),
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
      if Kind (Self.Cursor) = CXCursor_TranslationUnit then

         --  If the cursor is the translation unit, then spelling will be the
         --  full file path. In that case we want only the file name

         declare
            Path        : constant String := Spelling (Self.Cursor);
            Slash_Index : Natural := Path'Last;
            Dot_Index   : Natural := Path'Last;
            use String_Utils;
         begin
            Skip_To_Char (Path, Slash_Index, '/', -1);
            Skip_To_Char (Path, Dot_Index, '.', -1);
            return Self.Kernel.Symbols.Find
              (Path (Slash_Index + 1 .. Dot_Index - 1));
         end;
      else
         return Self.Kernel.Symbols.Find (Spelling (Self.Cursor));
      end if;
   end Name;

   -------------
   -- Profile --
   -------------

   overriding function Profile
     (Self             : Clang_Node;
      Show_Param_Names : Boolean := True) return Symbol
   is
      --  ??? Does libclang supports not showing the parameter names
      pragma Unreferenced (Show_Param_Names);
      K : constant Clang_Cursor_Kind := Kind (Self.Cursor);
   begin
      if Is_Function (K) or else Is_Object_Type (K) or else Is_Type (K) then
         declare
            Profile : constant String := Display_Name (Self.Cursor);
            Name : constant String := GNATCOLL.Symbols.Get (Self.Name).all;
         begin
            if Name'Length < Profile'Length then
               return Self.Kernel.Symbols.Find
                 (Profile (Name'Length + 1 .. Profile'Last));
            else
               return Empty_String;
            end if;
         end;
      elsif K in CXCursor_FieldDecl | CXCursor_VarDecl then
         return Self.Kernel.Symbols.Find (Spelling (Get_Type (Self.Cursor)));
      end if;

      return Empty_String;
   end Profile;

   ----------------
   -- Definition --
   ----------------

   overriding function Definition
     (Self : Clang_Node) return Semantic_Node'Class
   is
   begin
      return Node_From_Cursor (Definition (Self.Cursor), Self);
   end Definition;

   --------------
   -- Sloc_Def --
   --------------

   overriding function Sloc_Def
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T (Location (Self.Cursor));
   end Sloc_Def;

   ----------------
   -- Sloc_Start --
   ----------------

   overriding function Sloc_Start
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T
        (Range_Start (Extent (Self.Cursor)));
   end Sloc_Start;

   --------------
   -- Sloc_End --
   --------------

   overriding function Sloc_End
     (Self : Clang_Node) return Sloc_T is
   begin
      return To_Sloc_T
        (Range_End (Extent (Self.Cursor)));
   end Sloc_End;

   --------------
   -- Get_Hash --
   --------------

   overriding function Get_Hash
     (Self : Clang_Node) return Hash_Type is
   begin
      return Hash (Self.Cursor);
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
        and then (Is_Definition (Self.Cursor));
   end Is_Declaration;

   ----------------
   -- Visibility --
   ----------------

   overriding function Visibility
     (Self : Clang_Node) return Construct_Visibility is
      pragma Unreferenced (Self);
   begin
      --  TODO: At a later stage figure that out for C++ class members
      return Visibility_Public;
   end Visibility;

   ---------------
   -- Unique_Id --
   ---------------

   overriding function Unique_Id (Self : Clang_Node) return Symbol is
   begin
      return Self.Kernel.Symbols.Find (USR (Self.Cursor));
   end Unique_Id;

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
      Vec : constant Cursors_Arrays.Array_Type :=
        Get_Children (Element (It.Current_Cursor));
      Cursor_Before : constant Clang_Iterator_Lists.Cursor :=
        Next (It.Current_Cursor);
   begin
      for Cursor of Vec loop
         It.Elements.Reference.Insert (Cursor_Before, Cursor);
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
      Not_Empty_List_Cursor : constant Boolean :=
        It.Current_Cursor /= Clang_Iterator_Lists.No_Element;
      Not_Empty_Clang_Cursor : constant Boolean :=
        Not_Empty_List_Cursor
        and then Element (It.Current_Cursor) /= No_Cursor;
   begin
      return Not_Empty_Clang_Cursor;
   end Has_Element;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (Self : in out Clang_Node_Array;
      Less_Than : access function (L, R : Semantic_Node'Class) return Boolean)
   is
      function "<" (L, R : Clang_Cursor) return Boolean
      is (Less_Than
          (Semantic_Node'Class (Clang_Node'(Self.Kernel, L, Self.File)),
           Semantic_Node'Class (Clang_Node'(Self.Kernel, R, Self.File))));

      procedure Gen_Sort is new Ada.Containers.Generic_Array_Sort
        (Positive, Clang_Cursor, Cursors_Arrays.Array_Type);

   begin
      Gen_Sort (Self.Nodes.Reference);
   end Sort;

end Language.Libclang_Tree;
