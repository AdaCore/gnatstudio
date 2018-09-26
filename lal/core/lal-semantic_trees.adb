------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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
with Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded;
with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

with GNATCOLL.Symbols;
with GNATCOLL.Xref;              use GNATCOLL.Xref;

with Xref;
with Language.Tree;
with Langkit_Support.Iterators;
with Langkit_Support.Slocs;
with Langkit_Support.Tree_Traversal_Iterator;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Iterators;

with GPS.Editors;

package body LAL.Semantic_Trees is

   function Is_Exposed (Element : Ada_Node) return Boolean;
   --  Only nodes of a few kinds are exposed over Semantic_Trees interface.
   --  This function detects if given node should be exposed or not.

   function Is_Local (Element : Ada_Node) return Boolean;
   --  Return True if given Element is nested in a subprogram/task body.

   type Ada_Node_Kind_Array is array
     (Positive range <>, Positive range <>) of Ada_Node_Kind_Type;

   function In_Context
     (Element  : Ada_Node;
      Contexts : Ada_Node_Kind_Array) return Boolean;
   --  Check if Element.Parent.Kind = Context(Cntext'Last) then go to the
   --  parent and continue recursively

   With_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_With_Clause, Ada_Name_List));
   --  Context where name occurs inside with-clause

   Use_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_Use_Package_Clause, Ada_Name_List),
      2 => (Ada_Use_Type_Clause, Ada_Name_List));
   --  Context where name occurs inside use-clause

   Param_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_Param_Spec, Ada_Defining_Name_List, Ada_Defining_Name));
   --  Context where name occurs inside subprogram parameter

   Field_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_Component_Decl, Ada_Defining_Name_List, Ada_Defining_Name));
   --  Context where name occurs inside record component declaration

   Object_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_Object_Decl, Ada_Defining_Name_List, Ada_Defining_Name),
      2 => (Ada_Number_Decl, Ada_Defining_Name_List, Ada_Defining_Name),
      3 => (Ada_Exception_Decl, Ada_Defining_Name_List, Ada_Defining_Name));
   --  Context where name occurs inside object declaration

   Discriminant_Context : constant Ada_Node_Kind_Array :=
     (1 => (Ada_Discriminant_Spec, Ada_Defining_Name_List, Ada_Defining_Name));
   --  Context where name occurs inside discriminant declaration

   package Trees is
      --  Implementation of Semantic_Tree

      type Tree is new Semantic_Tree with record
         Kernel   : GPS.Core_Kernels.Core_Kernel;
         Context  : Libadalang.Analysis.Analysis_Context;
         File     : GNATCOLL.VFS.Virtual_File;
         Unit     : Libadalang.Analysis.Analysis_Unit;
         Provider : access constant LAL.Semantic_Trees.Provider;
      end record;

      overriding function Root_Iterator
        (Self : Tree) return Semantic_Tree_Iterator'Class;

      overriding function Root_Nodes
        (Self : Tree) return Semantic_Node_Array'Class;

      overriding function Node_At
        (Self            : Tree;
         Sloc            : Language.Sloc_T;
         Category_Filter : Language.Tree.Category_Array :=
           Language.Tree.Null_Category_Array) return Semantic_Node'Class;

      overriding function File (Self : Tree) return GNATCOLL.VFS.Virtual_File;

      overriding procedure Update (Self : in out Tree);

      overriding procedure Update_Async (Self : in out Tree);

      overriding function Is_Ready (Self : Tree) return Boolean is (True);

   end Trees;

   package Nodes is
      --  Implementation of Semantic_Node

      type Node is new Semantic_Node with record
         Provider : access constant LAL.Semantic_Trees.Provider;
         Ada_Node : Libadalang.Analysis.Ada_Node;
      end record;

      overriding function Is_Valid (Self : Node) return Boolean;

      overriding function Category
        (Self : Node) return Language.Language_Category;

      overriding function Is_Declaration (Self : Node) return Boolean;

      overriding function Children
        (Self : Node) return Semantic_Node_Array'Class;

      overriding function First_Child (Self : Node) return Semantic_Node'Class;

      overriding function Parent (Self : Node) return Semantic_Node'Class;

      overriding function Name (Self : Node) return GNATCOLL.Symbols.Symbol;

      overriding function Sloc_Start (Self : Node) return Language.Sloc_T;

      overriding function Sloc_Def (Self : Node) return Language.Sloc_T;

      overriding function Sloc_End (Self : Node) return Language.Sloc_T;

      overriding function Profile
        (Self             : Node;
         Show_Param_Names : Boolean := True)
         return GNATCOLL.Symbols.Symbol;

      overriding function Definition (Self : Node) return Semantic_Node'Class;

      overriding function Get_Hash
        (Self : Node) return Ada.Containers.Hash_Type;

      overriding function File (Self : Node) return GNATCOLL.VFS.Virtual_File;

      overriding function Unique_Id
        (Self : Node) return GNATCOLL.Symbols.Symbol;

      overriding function Visibility
        (Self : Node) return Language.Construct_Visibility;

      overriding function Documentation_Body (Self : Node) return String;

      overriding function Documentation_Header (Self : Node) return String;
   end Nodes;

   package Node_Arrays is
      --  Implementation of Semantic_Node_Array

      type Nodes_Array is array (Positive range <>) of Nodes.Node;

      type Node_Array (Length : Natural) is new Semantic_Node_Array with record
         Data : Nodes_Array (1 .. Length);
      end record;

      overriding procedure Sort
        (Self      : in out Node_Array;
         Less_Than : access
           function (L, R : Semantic_Node'Class) return Boolean);

      overriding function Get
        (Self : Node_Array; Index : Positive) return Semantic_Node'Class;

      overriding function Length (Self : Node_Array) return Natural;

   end Node_Arrays;

   package Iterators is
      --  Implementation of Semantic_Tree_Iterator

      package Holders is new Ada.Containers.Indefinite_Holders
        (Element_Type => Libadalang.Iterators.Traverse_Iterator'Class,
         "="          => Libadalang.Iterators."=");

      type Iterator is limited new Semantic_Tree_Iterator with record
         Done    : Boolean := True;
         Node    : Nodes.Node;
         Cursor  : Holders.Holder;
      end record;

      overriding procedure Next (Self : in out Iterator);

      overriding function Element (Self : Iterator) return Semantic_Node'Class;

      overriding function Has_Element (Self : Iterator) return Boolean;
   end Iterators;

   package body Iterators is

      overriding procedure Next (Self : in out Iterator) is
      begin
         Self.Done := not Self.Cursor.Reference.Next (Self.Node.Ada_Node);
      end Next;

      overriding function Element
        (Self : Iterator) return Semantic_Node'Class is
      begin
         return Self.Node;
      end Element;

      overriding function Has_Element (Self : Iterator) return Boolean is
      begin
         return not Self.Done;
      end Has_Element;

   end Iterators;

   package Immediate_Iterators is
      --  This iterator doesn't travel under exposed nodes.
      --  But this rule doesn't count root of the subtree:
      --  Immediate children of root are traversed, even if root exposed.

      type Immediate_Iterator is
        new Libadalang.Iterators.Ada_Node_Iterators.Iterator
      with private;
      --  This iterator provides access to nodes immediate reacheble
      --  from given root over unexposed nodes.

      function Children (Node : Ada_Node) return Immediate_Iterator;
      --  Return children of the Node and children of unexposed children, etc.

   private

      type Node_Wrapper is record
         Root : Ada_Node;
         --  Root of some subtree
         Node : Ada_Node;
      end record;

      No_Node_Wrapper : constant Node_Wrapper := (No_Ada_Node, No_Ada_Node);

      function Get_Child (N : Node_Wrapper; J : Natural) return Node_Wrapper;
      function Get_Parent (N : Node_Wrapper) return Node_Wrapper;
      function First (N : Node_Wrapper) return Natural is
        (N.Node.First_Child_Index);
      function Last (N : Node_Wrapper) return Natural;

      type Node_Wrapper_Array is array (Positive range <>) of Node_Wrapper;

      package Wrapper_Iterators is new Langkit_Support.Iterators
        (Node_Wrapper, Node_Wrapper_Array);

      package Iterators is
        new Langkit_Support.Tree_Traversal_Iterator
          (Node_Type         => Node_Wrapper,
           Node_Array        => Node_Wrapper_Array,
           No_Node           => No_Node_Wrapper,
           Get_Child         => Get_Child,
           Get_Parent        => Get_Parent,
           First_Child_Index => First,
           Last_Child_Index  => Last,
           Iterators         => Wrapper_Iterators);

      type Immediate_Iterator is
        new Libadalang.Iterators.Ada_Node_Iterators.Iterator
      with record
         Plain : Iterators.Traverse_Iterator;
      end record;

      overriding function Next
        (Self    : in out Immediate_Iterator;
         Element : out Ada_Node) return Boolean;

   end Immediate_Iterators;

   package Exposed_Iterators is
      --  This iterator is a filter over its parent iterator.
      --  It filters unexposed nodes out and provides only exposed nodes.

      type Exposed_Iterator
       (Parent : access Libadalang.Iterators.Ada_Node_Iterators.Iterator'Class)
         is new Libadalang.Iterators.Ada_Node_Iterators.Iterator
           with null record;

      overriding function Next
        (Self    : in out Exposed_Iterator;
         Element : out Ada_Node) return Boolean;

   end Exposed_Iterators;

   package body Exposed_Iterators is

      overriding function Next
        (Self    : in out Exposed_Iterator;
         Element : out Ada_Node) return Boolean is
      begin
         while Self.Parent.Next (Element) loop
            if Is_Exposed (Element) then
               return True;
            end if;
         end loop;

         return False;
      end Next;

   end Exposed_Iterators;

   package body Immediate_Iterators is

      --------------
      -- Children --
      --------------

      function Children (Node : Ada_Node) return Immediate_Iterator is
         Root : constant Node_Wrapper := (Node, Node);
         Skip : Ada_Node;
         Ok   : Boolean;
         pragma Unreferenced (Ok);
      begin
         return Result : Immediate_Iterator do
            Iterators.Create_Tree_Iterator (Root, Result.Plain);
            Ok := Result.Next (Skip);
            --  The Plain iterator always return the Root at first step.
            --  Skip it right now.
         end return;
      end Children;

      ---------------
      -- Get_Child --
      ---------------

      function Get_Child (N : Node_Wrapper; J : Natural) return Node_Wrapper is
         Result : constant Ada_Node := N.Node.Child (J);
      begin
         if Result = No_Ada_Node then
            return No_Node_Wrapper;
         else
            return (N.Root, Result);
         end if;
      end Get_Child;

      ----------------
      -- Get_Parent --
      ----------------

      function Get_Parent (N : Node_Wrapper) return Node_Wrapper is
      begin
         return (N.Root, N.Node.Parent);
      end Get_Parent;

      ----------
      -- Last --
      ----------

      function Last (N : Node_Wrapper) return Natural is
      begin
         if N.Node = N.Root or else not Is_Exposed (N.Node) then
            return N.Node.Last_Child_Index;
         else
            return 0;
         end if;
      end Last;

      ----------
      -- Next --
      ----------

      overriding function Next
        (Self    : in out Immediate_Iterator;
         Element : out Ada_Node) return Boolean
      is
         Wrapper : Node_Wrapper;
      begin
         return Result : constant Boolean := Self.Plain.Next (Wrapper) do
            Element := Wrapper.Node;
         end return;
      end Next;

   end Immediate_Iterators;

   package body Node_Arrays is

      ----------
      -- Sort --
      ----------

      overriding procedure Sort
        (Self      : in out Node_Array;
         Less_Than : access
           function (L, R : Semantic_Node'Class) return Boolean)
      is
         function Less (L, R : Nodes.Node) return Boolean;

         ----------
         -- Less --
         ----------

         function Less (L, R : Nodes.Node) return Boolean is
         begin
            return Less_Than (L, R);
         end Less;

         procedure Do_Sort is new Ada.Containers.Generic_Array_Sort
           (Index_Type   => Positive,
            Element_Type => Nodes.Node,
            Array_Type   => Nodes_Array,
            "<"          => Less);
      begin
         Do_Sort (Self.Data);
      end Sort;

      ---------
      -- Get --
      ---------

      overriding function Get
        (Self : Node_Array; Index : Positive) return Semantic_Node'Class is
      begin
         return Self.Data (Index);
      end Get;

      ------------
      -- Length --
      ------------

      overriding function Length (Self : Node_Array) return Natural is
      begin
         return Self.Length;
      end Length;

   end Node_Arrays;

   package body Nodes is

      function Line
        (Token : Libadalang.Common.Token_Reference)
         return Langkit_Support.Slocs.Line_Number is
           (Langkit_Support.Slocs.Start_Sloc
              (Libadalang.Common.Sloc_Range
                   (Libadalang.Common.Data (Token))).Line);

      --------------
      -- Is_Valid --
      --------------

      overriding function Is_Valid (Self : Node) return Boolean is
      begin
         return Self.Ada_Node /= No_Ada_Node;
      end Is_Valid;

      --------------
      -- Category --
      --------------

      overriding function Category
        (Self : Node) return Language.Language_Category is
      begin
         case Self.Ada_Node.Kind is
            when Ada_Dotted_Name | Ada_Identifier =>
               if In_Context (Self.Ada_Node, With_Context) then
                  return Language.Cat_With;
               elsif In_Context (Self.Ada_Node, Use_Context) then
                  return Language.Cat_Use;
               elsif In_Context (Self.Ada_Node, Param_Context) then
                  return Language.Cat_Parameter;
               elsif In_Context (Self.Ada_Node, Field_Context) then
                  return Language.Cat_Field;
               elsif In_Context (Self.Ada_Node, Discriminant_Context) then
                  return Language.Cat_Discriminant;
               elsif In_Context (Self.Ada_Node, Object_Context) then
                  if Is_Local (Self.Ada_Node) then
                     return Language.Cat_Local_Variable;
                  else
                     return Language.Cat_Variable;
                  end if;
               end if;
            when Ada_Generic_Package_Decl |
                 Ada_Generic_Package_Instantiation |
                 Ada_Generic_Package_Renaming_Decl |
                 Ada_Package_Body |
                 Ada_Package_Decl |
                 Ada_Package_Renaming_Decl =>
               return Language.Cat_Package;
            when Ada_Abstract_Subp_Decl |
                 Ada_Formal_Subp_Decl |
                 Ada_Subp_Decl |
                 Ada_Subp_Renaming_Decl =>
               declare
                  Node : constant Classic_Subp_Decl :=
                    Self.Ada_Node.As_Classic_Subp_Decl;
               begin
                  if Node.F_Subp_Spec.F_Subp_Returns = No_Ada_Node then
                     return Language.Cat_Procedure;
                  else
                     return Language.Cat_Function;
                  end if;
               end;
            when Ada_Subp_Body =>
               declare
                  Node : constant Subp_Body := Self.Ada_Node.As_Subp_Body;
               begin
                  if Node.F_Subp_Spec.F_Subp_Returns = No_Ada_Node then
                     return Language.Cat_Procedure;
                  else
                     return Language.Cat_Function;
                  end if;
               end;
            when Ada_Generic_Subp_Instantiation =>
               if Self.Ada_Node.As_Generic_Subp_Instantiation.F_Kind in
                 Ada_Subp_Kind_Procedure
               then
                  return Language.Cat_Procedure;
               else
                  return Language.Cat_Function;
               end if;
            when Ada_Generic_Subp_Renaming_Decl =>
               if Self.Ada_Node.As_Generic_Subp_Renaming_Decl.F_Kind in
                 Ada_Subp_Kind_Procedure
               then
                  return Language.Cat_Procedure;
               else
                  return Language.Cat_Function;
               end if;
            when Ada_Generic_Subp_Decl =>
               declare
                  Node : constant Generic_Subp_Decl :=
                    Self.Ada_Node.As_Generic_Subp_Decl;
               begin
                  if Node.F_Subp_Decl.F_Subp_Spec.F_Subp_Returns
                       = No_Ada_Node
                  then
                     return Language.Cat_Procedure;
                  else
                     return Language.Cat_Function;
                  end if;
               end;
            when Ada_Null_Subp_Decl =>
               return Language.Cat_Procedure;
            when Ada_Expr_Function =>
               return Language.Cat_Function;
            when Ada_Enum_Literal_Decl =>
               return Language.Cat_Literal;
            when Ada_Protected_Body |
                 Ada_Protected_Type_Decl |
                 Ada_Single_Protected_Decl =>
               return Language.Cat_Protected;
            when Ada_Entry_Decl =>
               return Language.Cat_Entry;
            when Ada_Type_Decl =>
               --  TODO: we need to return next values also:
               --  Language.Cat_Class;
               --  Language.Cat_Structure;
                  return Language.Cat_Type;
            when Ada_Subtype_Decl =>
               return Language.Cat_Subtype;
            when Ada_If_Stmt =>
               return Language.Cat_If_Statement;
            when Ada_Loop_Stmt =>
               return Language.Cat_Loop_Statement;
            when Ada_Case_Stmt =>
               return Language.Cat_Case_Statement;
            when Ada_Begin_Block =>
               return Language.Cat_Simple_Block;
            when Ada_Decl_Block =>
               return Language.Cat_Declare_Block;
            when Ada_Pragma_Node =>
               return Language.Cat_Pragma;
            when Ada_Extended_Return_Stmt =>
               return Language.Cat_Return_Block;
            when Ada_For_Loop_Spec =>
               return Language.Cat_Local_Variable;
            when Ada_Exception_Handler =>
               return Language.Cat_Exception_Handler;
            when Ada_Attribute_Def_Clause =>
               return Language.Cat_Representation_Clause;
            when Ada_Aspect_Spec =>
               return Language.Cat_Aspect;
            when Ada_Variant_Part =>
               return Language.Cat_Case_Inside_Record;
            when Ada_Single_Task_Decl | Ada_Task_Body | Ada_Task_Type_Decl =>
               return Language.Cat_Task;
            when others =>
               null;
         end case;

         raise Constraint_Error;
      end Category;

      --------------------
      -- Is_Declaration --
      --------------------

      overriding function Is_Declaration (Self : Node) return Boolean is
      begin
         case Self.Ada_Node.Kind is
            when Ada_Generic_Package_Decl |
                 Ada_Generic_Package_Instantiation |
                 Ada_Generic_Package_Renaming_Decl |
                 Ada_Package_Decl |
                 Ada_Package_Renaming_Decl |

                 Ada_Abstract_Subp_Decl |
                 Ada_Formal_Subp_Decl |
                 Ada_Subp_Decl |
                 Ada_Subp_Renaming_Decl |
                 Ada_Generic_Subp_Instantiation |
                 Ada_Generic_Subp_Renaming_Decl |
                 Ada_Generic_Subp_Decl |
                 Ada_Null_Subp_Decl |
                 Ada_Expr_Function |
                 Ada_Protected_Type_Decl |
                 Ada_Single_Protected_Decl |
                 Ada_Entry_Decl |
                 Ada_Type_Decl |
                 Ada_Single_Task_Decl |
                 Ada_Task_Type_Decl =>
               return True;
            when others =>
               return False;
         end case;
      end Is_Declaration;

      --------------
      -- Children --
      --------------

      overriding function Children
        (Self : Node) return Semantic_Node_Array'Class
      is
         Immediate : aliased Immediate_Iterators.Immediate_Iterator :=
           Immediate_Iterators.Children (Self.Ada_Node);
         Exposed   : Exposed_Iterators.Exposed_Iterator (Immediate'Access);
         Vector : constant Libadalang.Analysis.Ada_Node_Array :=
           Exposed.Consume;
      begin
         return Result : Node_Arrays.Node_Array (Vector'Length) do
            for J in Vector'Range loop
               Result.Data (J) := (Provider => Self.Provider,
                                   Ada_Node => Vector (J));
            end loop;
         end return;
      end Children;

      -----------------
      -- First_Child --
      -----------------

      overriding function First_Child
        (Self : Node) return Semantic_Node'Class
      is
         Immediate : aliased Immediate_Iterators.Immediate_Iterator :=
           Immediate_Iterators.Children (Self.Ada_Node);
         Exposed   : Exposed_Iterators.Exposed_Iterator (Immediate'Access);
         Result    : Libadalang.Analysis.Ada_Node;
      begin
         if Exposed.Next (Result) then
            return Node'(Provider => Self.Provider, Ada_Node => Result);
         else
            return No_Semantic_Node;
         end if;
      end First_Child;

      ------------
      -- Parent --
      ------------

      overriding function Parent (Self : Node) return Semantic_Node'Class is
         Result : Libadalang.Analysis.Ada_Node := Self.Ada_Node.Parent;
      begin
         while Result /= No_Ada_Node and then not Is_Exposed (Result) loop
            Result := Result.Parent;
         end loop;

         if Result = No_Ada_Node then
            return No_Semantic_Node;
         else
            return Node'(Provider => Self.Provider, Ada_Node => Result);
         end if;
      end Parent;

      ----------
      -- Name --
      ----------

      overriding function Name (Self : Node) return GNATCOLL.Symbols.Symbol is

         function To_Symbol
           (Node : Libadalang.Analysis.Ada_Node'Class)
            return GNATCOLL.Symbols.Symbol;
         --  Convert Node of a name to Symbol

         ---------------
         -- To_Symbol --
         ---------------

         function To_Symbol
           (Node : Libadalang.Analysis.Ada_Node'Class)
            return GNATCOLL.Symbols.Symbol is
         begin
            if Node.Is_Null then
               --  In case of incomplete/invalid source some names are
               --  unavailable, so Node could be null.
               return GNATCOLL.Symbols.No_Symbol;
            end if;

            case Node.Kind is
               when Ada_Identifier | Ada_String_Literal | Ada_Char_Literal =>
                  declare
                     Token : constant Token_Reference := Node.Token_Start;
                  begin
                     return Self.Provider.Kernel.Symbols.Find
                       (Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
                          (Text (Token)));
                  end;

               when Ada_Dotted_Name =>
                  declare
                     use Ada.Strings.Unbounded;
                     Image : Unbounded_String;
                  begin
                     for Token of Node.Token_Range loop
                        if not Is_Trivia (Token) then
                           Append
                             (Image,
                              Ada.Strings.UTF_Encoding.Wide_Wide_Strings.Encode
                                (Text (Token)));
                        end if;
                     end loop;

                     return Self.Provider.Kernel.Symbols.Find
                       (To_String (Image));
                  end;

               when Ada_Defining_Name =>
                  return To_Symbol (Node.As_Defining_Name.F_Name);

               when others =>
                  raise Constraint_Error;
            end case;
         end To_Symbol;

         Result : GNATCOLL.Symbols.Symbol;
      begin
         case Self.Ada_Node.Kind is
            when Ada_Identifier | Ada_Dotted_Name =>
               Result := To_Symbol (Self.Ada_Node);

            when Ada_Abstract_Subp_Decl |
                 Ada_Entry_Decl |
                 Ada_Enum_Literal_Decl |
                 Ada_Expr_Function |
                 Ada_Formal_Subp_Decl |
                 Ada_Generic_Package_Decl |
                 Ada_Generic_Package_Instantiation |
                 Ada_Generic_Package_Renaming_Decl |
                 Ada_Generic_Subp_Decl |
                 Ada_Generic_Subp_Instantiation |
                 Ada_Generic_Subp_Renaming_Decl |
                 Ada_Null_Subp_Decl |
                 Ada_Package_Body |
                 Ada_Package_Decl |
                 Ada_Package_Renaming_Decl |
                 Ada_Protected_Body |
                 Ada_Protected_Type_Decl |
                 Ada_Single_Protected_Decl |
                 Ada_Single_Task_Decl |
                 Ada_Subp_Body |
                 Ada_Subp_Decl |
                 Ada_Subp_Renaming_Decl |
                 Ada_Subtype_Decl |
                 Ada_Task_Body |
                 Ada_Task_Type_Decl |
                 Ada_Type_Decl =>

               Result := To_Symbol
                 (Self.Ada_Node.As_Basic_Decl.P_Defining_Name);

            when Ada_Pragma_Node =>
               Result := To_Symbol (Self.Ada_Node.As_Pragma_Node.F_Id);

            when Ada_For_Loop_Spec =>
               Result := To_Symbol
                 (Self.Ada_Node.As_For_Loop_Spec.F_Var_Decl.F_Id);

            when Ada_Aspect_Spec |
                 Ada_Attribute_Def_Clause |
                 Ada_Block_Stmt |
                 Ada_Case_Stmt |
                 Ada_Exception_Handler |
                 Ada_Extended_Return_Stmt |
                 Ada_If_Stmt |
                 Ada_Loop_Stmt |
                 Ada_Variant_Part =>
               null;

            when others =>
               null;
         end case;

         return Result;
      end Name;

      ----------------
      -- Sloc_Start --
      ----------------

      overriding function Sloc_Start (Self : Node) return Language.Sloc_T is
         use type Langkit_Support.Slocs.Line_Number;

         S : constant Langkit_Support.Slocs.Source_Location_Range :=
           Self.Ada_Node.Sloc_Range;

         Step : constant Libadalang.Common.Token_Reference :=
           Libadalang.Common.Previous
             (Self.Ada_Node.Token_Start, Exclude_Trivia => True);

         Result : Language.Sloc_T :=
           (Natural (S.Start_Line), Visible_Column (S.Start_Column), 0);
      begin
         if Step = Libadalang.Common.No_Token
           or else Line (Step) /= Line (Self.Ada_Node.Token_Start)
         then
            Result.Column := 1;
         end if;

         return Result;
      end Sloc_Start;

      --------------
      -- Sloc_Def --
      --------------

      overriding function Sloc_Def (Self : Node) return Language.Sloc_T is
         pragma Unreferenced (Self);
      begin
         --  TODO: implement Sloc_Def
         return (0, 0, 0);
      end Sloc_Def;

      --------------
      -- Sloc_End --
      --------------

      overriding function Sloc_End (Self : Node) return Language.Sloc_T is
         use type Langkit_Support.Slocs.Line_Number;

         function Last_Column_In_Line
           (Token : Libadalang.Common.Token_Reference) return Visible_Column;

         function Last_Column_In_Line
           (Token : Libadalang.Common.Token_Reference) return Visible_Column
         is
            Next  : Token_Reference := Token;
         begin
            while Next /= Libadalang.Common.No_Token loop
               declare
                  Value : constant Token_Data_Type := Data (Next);
                  Span  : constant Langkit_Support.Slocs.Source_Location_Range
                    := Sloc_Range (Value);
               begin
                  if Kind (Value) in Libadalang.Common.Ada_Whitespace
                    and then Span.Start_Line /= Span.End_Line
                  then
                     return Visible_Column (Span.Start_Column);
                  end if;

                  Next := Libadalang.Common.Next (Next);
               end;
            end loop;

            return 0;
         end Last_Column_In_Line;

         S : constant Langkit_Support.Slocs.Source_Location_Range :=
           Self.Ada_Node.Sloc_Range;

         Step : constant Libadalang.Common.Token_Reference :=
           Libadalang.Common.Next
             (Self.Ada_Node.Token_End, Exclude_Trivia => True);

         Result : Language.Sloc_T :=
           (Natural (S.End_Line), Visible_Column (S.End_Column), 0);
      begin
         if Step = Libadalang.Common.No_Token
           or else Line (Step) /= Line (Self.Ada_Node.Token_Start)
         then
            Result.Column := Last_Column_In_Line (Self.Ada_Node.Token_End);
         end if;

         return Result;
      end Sloc_End;

      -------------
      -- Profile --
      -------------

      overriding function Profile
        (Self             : Node;
         Show_Param_Names : Boolean := True)
         return GNATCOLL.Symbols.Symbol
      is

         function To_Profile (Node : Subp_Spec'Class) return String;

         function To_Profile (Node : Subp_Spec'Class) return String is
            use Ada.Strings.Unbounded;
            Result  : Unbounded_String;
            Params  : constant Param_Spec_Array := Node.P_Params;
            Returns : constant Type_Expr := Node.F_Subp_Returns;
         begin
            if Params'Length > 0 then
               Append (Result, "(");
            end if;

            for Param of Params loop
               declare
                  Names : constant Defining_Name_List := Param.F_Ids;
                  Init  : constant Expr := Param.F_Default_Expr;
                  Item  : Unbounded_String;
               begin
                  Append (Item, " :");

                  if Show_Param_Names then
                     case Param.F_Mode is
                        when Ada_Mode_Default | Ada_Mode_In =>
                           Append (Item, " in ");
                        when Ada_Mode_In_Out =>
                           Append (Item, " in out ");
                        when Ada_Mode_Out =>
                           Append (Item, " out ");
                     end case;
                  end if;

                  Append (Item, Param.F_Type_Expr.String_Text);

                  if not Init.Is_Null then
                     Append (Result, " := ");
                     Append (Item, Init.String_Text);
                  end if;

                  for J in
                    Names.First_Child_Index .. Names.Last_Child_Index
                  loop
                     if Length (Result) /= 1 then
                        Append (Result, "; ");
                     end if;

                     Append (Result, Names.Child (J).String_Text);
                     Append (Result, Item);
                     Item := Null_Unbounded_String;
                  end loop;

               end;
            end loop;

            if Params'Length > 0 then
               Append (Result, ")");
            end if;

            if not Returns.Is_Null then
               Append (Result, " return ");
               Append (Result, Returns.String_Text);
            end if;

            return To_String (Result);
         end To_Profile;

         pragma Unreferenced (Show_Param_Names);
      begin
         case Self.Ada_Node.Kind is
            when Ada_Classic_Subp_Decl =>
               return Self.Provider.Kernel.Symbols.Find
                 (To_Profile
                    (Self.Ada_Node.As_Classic_Subp_Decl.F_Subp_Spec));
            when Ada_Base_Subp_Body =>
               return Self.Provider.Kernel.Symbols.Find
                 (To_Profile
                    (Self.Ada_Node.As_Base_Subp_Body.F_Subp_Spec));
            when Ada_Generic_Subp_Decl  =>
               return Self.Provider.Kernel.Symbols.Find
                 (To_Profile
                    (Self.Ada_Node.As_Generic_Subp_Decl.
                         F_Subp_Decl.F_Subp_Spec));
            when others =>
               return GNATCOLL.Symbols.No_Symbol;
         end case;

      end Profile;

      ----------------
      -- Definition --
      ----------------

      overriding function Definition
        (Self : Node) return Semantic_Node'Class
      is
         pragma Unreferenced (Self);
      begin
         --  TODO: implement Definition
         return No_Semantic_Node;
      end Definition;

      --------------
      -- Get_Hash --
      --------------

      overriding function Get_Hash
        (Self : Node) return Ada.Containers.Hash_Type
      is
         use Ada.Containers;
         S : constant Langkit_Support.Slocs.Source_Location_Range :=
           Self.Ada_Node.Sloc_Range;
         From : constant Hash_Type :=
           Hash_Type (S.Start_Column) * 2017 + Hash_Type (S.Start_Line);
         To   : constant Hash_Type :=
           Hash_Type (S.End_Column) * 2017 + Hash_Type (S.End_Line);
      begin
         return 997 * From + To;
      end Get_Hash;

      ----------
      -- File --
      ----------

      overriding function File
        (Self : Node) return GNATCOLL.VFS.Virtual_File
      is
         Unit      : constant Libadalang.Analysis.Analysis_Unit :=
           Libadalang.Analysis.Unit (Self.Ada_Node);
         File_Name : constant String :=
           Libadalang.Analysis.Get_Filename (Unit);
      begin
         return GNATCOLL.VFS.Create
           (GNATCOLL.VFS.Filesystem_String (File_Name));
      end File;

      ---------------
      -- Unique_Id --
      ---------------

      overriding function Unique_Id
        (Self : Node) return GNATCOLL.Symbols.Symbol
      is
         Image : String := Ada.Containers.Hash_Type'Image (Self.Get_Hash);
      begin
         Image (Image'First) := 'U';

         return Self.Provider.Kernel.Symbols.Find (Image);
      end Unique_Id;

      ----------------
      -- Visibility --
      ----------------

      overriding function Visibility
        (Self : Node) return Language.Construct_Visibility is
         pragma Unreferenced (Self);
      begin
         --  TODO: implement Visibility
         return Language.Visibility_Public;
      end Visibility;

      ------------------------
      -- Documentation_Body --
      ------------------------

      overriding function Documentation_Body (Self : Node) return String is
         Formater : aliased Language.Profile_Formaters.Profile_Formater'Class
           := Self.Provider.Formater.all;

         Start : constant Language.Sloc_T := Self.Sloc_Start;
         Loc : constant Xref.General_Location :=
           (File         => Self.File,
            Project_Path => <>,
            Line         => Start.Line,
            Column       => Start.Column);
         Entity : constant Xref.Root_Entity'Class :=
           Self.Provider.Kernel.Databases.Get_Entity
             (Name => GNATCOLL.Symbols.Get (Self.Name).all,
              Loc  => Loc);
      begin
         Xref.Documentation
           (Handler           => Self.Provider.Kernel.Lang_Handler,
            Entity            => Entity,
            Formater          => Formater'Access,
            Look_Before_First => Self.Provider.Doc_Search_Before_First);

         return Formater.Get_Text;
      end Documentation_Body;

      --------------------------
      -- Documentation_Header --
      --------------------------

      overriding function Documentation_Header (Self : Node) return String is
      begin
         return "<b>" & GNATCOLL.Symbols.Get (Self.Name).all & "</b>";
      end Documentation_Header;

   end Nodes;

   package body Trees is

      -------------------
      -- Root_Iterator --
      -------------------

      overriding function Root_Iterator
        (Self : Tree) return Semantic_Tree_Iterator'Class
      is
         Root : constant Ada_Node := Self.Unit.Root;

      begin
         if Root = No_Ada_Node then
            return No_Semantic_Tree.Root_Iterator;
         end if;

         return Result : Iterators.Iterator :=
           (Cursor => Iterators.Holders.To_Holder
              (Libadalang.Iterators.Traverse_Iterator
                   (Libadalang.Iterators.Find (Root, Is_Exposed'Access))),
            others => <>)
         do
            Result.Done := not Result.Cursor.Reference.Next
              (Result.Node.Ada_Node);
            Result.Node.Provider := Self.Provider;
         end return;
      end Root_Iterator;

      ----------------
      -- Root_Nodes --
      ----------------

      overriding function Root_Nodes
        (Self : Tree) return Semantic_Node_Array'Class
      is
         Root : constant Ada_Node := Self.Unit.Root;

      begin
         if Root = No_Ada_Node then
            return No_Semantic_Tree.Root_Nodes;
         end if;

         declare
            Immediate : aliased Immediate_Iterators.Immediate_Iterator :=
              Immediate_Iterators.Children (Root);
            Exposed   : Exposed_Iterators.Exposed_Iterator (Immediate'Access);
            Vector : constant Libadalang.Analysis.Ada_Node_Array :=
              Exposed.Consume;
         begin
            return Result : Node_Arrays.Node_Array (Vector'Length) do
               for J in Vector'Range loop
                  Result.Data (J) := (Provider => Self.Provider,
                                      Ada_Node => Vector (J));
               end loop;
            end return;
         end;
      end Root_Nodes;

      -------------
      -- Node_At --
      -------------

      overriding function Node_At
        (Self            : Tree;
         Sloc            : Language.Sloc_T;
         Category_Filter : Language.Tree.Category_Array :=
           Language.Tree.Null_Category_Array) return Semantic_Node'Class
      is
         use type Language.Language_Category;
         use Langkit_Support.Slocs;

         function Adjust_Source_Location
           (Loc : Source_Location) return Source_Location;
         --  All spaces on the starting line of some compound node
         --  are considered to be part of the node itself.
         --  So we make Loc correction here to find correct node:
         --  When Loc is not inside a token then shift Loc to next token
         --  if the token is on the same line.
         --  The same is for trailing spaces.

         ----------------------------
         -- Adjust_Source_Location --
         ----------------------------

         function Adjust_Source_Location
           (Loc : Source_Location) return Source_Location
         is
            Step   : Libadalang.Common.Token_Reference;
            Result : Source_Location;
            Token  : constant Libadalang.Common.Token_Reference :=
              Self.Unit.Lookup_Token (Loc);

         begin
            if Token = Libadalang.Common.No_Token then
               return No_Source_Location;
            end if;

            Step := Libadalang.Common.Next (Token, Exclude_Trivia => True);

            if Step /= Libadalang.Common.No_Token then
               Result := Start_Sloc (Libadalang.Common.Sloc_Range
                                       (Libadalang.Common.Data (Step)));

               if Result.Line = Loc.Line then
                  return Result;
               end if;
            end if;

            Step := Libadalang.Common.Previous
              (Token, Exclude_Trivia => True);

            if Step /= Libadalang.Common.No_Token then
               Result := Start_Sloc (Libadalang.Common.Sloc_Range
                                       (Libadalang.Common.Data (Step)));

               if Result.Line = Loc.Line then
                  return Result;
               end if;
            end if;

            return Loc;
         end Adjust_Source_Location;

         Root : constant Ada_Node := Self.Unit.Root;
         Loc  : Source_Location :=
           (Line_Number (Sloc.Line), Column_Number (Sloc.Column));
         Node : Libadalang.Analysis.Ada_Node := No_Ada_Node;

      begin
         if Root /= No_Ada_Node then
            Loc := Adjust_Source_Location (Loc);
            Node := Libadalang.Analysis.Lookup (Root, Loc);
         end if;

         while Node /= No_Ada_Node loop
            if Is_Exposed (Node) then
               declare
                  Result : constant Nodes.Node :=
                    (Provider => Self.Provider, Ada_Node => Node);
                  Category : constant Language.Language_Category :=
                    Result.Category;
               begin
                  --  Apply Category_Filter if present
                  if Category_Filter'Length = 0 or else
                    (for some Element of Category_Filter => Element = Category)
                  then
                     return Result;
                  end if;
               end;
            end if;

            Node := Node.Parent;
         end loop;

         return No_Semantic_Node;
      end Node_At;

      ----------
      -- File --
      ----------

      overriding function File
        (Self : Tree) return GNATCOLL.VFS.Virtual_File is
      begin
         return Self.File;
      end File;

      ------------
      -- Update --
      ------------

      overriding procedure Update (Self : in out Tree) is
         use type GPS.Editors.Editor_Buffer'Class;

         Name   : constant GNATCOLL.VFS.Filesystem_String :=
           Self.File.Full_Name;

         Buffer : constant GPS.Editors.Editor_Buffer'Class :=
           Self.Kernel.Get_Buffer_Factory.Get
             (Self.File, False, False, False, False);
      begin
         if Buffer = GPS.Editors.Nil_Editor_Buffer then
            Self.Unit :=
              Libadalang.Analysis.Get_From_File
                (Context     => Self.Context,
                 Filename    => String (Name),
                 Reparse     => True);
         else
            Self.Unit :=
              Libadalang.Analysis.Get_From_Buffer
                (Context     => Self.Context,
                 Filename    => String (Name),
                 Buffer      => Buffer.Get_Chars,
                 Charset     => "UTF-8");
         end if;
      end Update;

      ------------------
      -- Update_Async --
      ------------------

      overriding procedure Update_Async (Self : in out Tree) is
      begin
         Self.Update;
         Self.Kernel.Semantic_Tree_Updated (Self.File);
      end Update_Async;

   end Trees;

   -----------------------
   -- Get_Tree_For_File --
   -----------------------

   overriding function Get_Tree_For_File
     (Self    : in out Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
   is
      pragma Unreferenced (Context);

      Name   : constant GNATCOLL.VFS.Filesystem_String := File.Full_Name;

      Result : Trees.Tree :=
        Trees.Tree'(Kernel   => Self.Kernel,
                    Context  => Self.Context,
                    File     => File,
                    Unit     => <>,
                    Provider => Self'Unchecked_Access);  --  We have only
      --  one Provider and its lifespan is the same as GPS instance, so it's
      --  save to get access to it.

   begin
      if Libadalang.Analysis.Has_Unit (Self.Context, String (Name)) then
         Result.Unit :=
           Libadalang.Analysis.Get_From_File
             (Context     => Self.Context,
              Filename    => String (Name));

      else
         Result.Update;
      end if;

      return Result;
   end Get_Tree_For_File;

   ----------------
   -- In_Context --
   ----------------

   function In_Context
     (Element  : Ada_Node;
      Contexts : Ada_Node_Kind_Array) return Boolean
   is
      Node  : Ada_Node;
      Index : Natural;
   begin
      for J in Contexts'Range (1) loop
         Node  := Element.Parent;
         Index := Contexts'Last (2);

         while Node /= No_Ada_Node
           and then Index >= Contexts'First (2)
           and then Node.Kind = Contexts (J, Index)
         loop
            Node := Node.Parent;
            Index := Index - 1;
         end loop;

         exit when Index < Contexts'First (2);
      end loop;

      return Index < Contexts'First (2);
   end In_Context;

   ----------------
   -- Is_Exposed --
   ----------------

   function Is_Exposed (Element : Ada_Node) return Boolean is
   begin
      case Element.Kind is
         when Ada_Dotted_Name | Ada_Identifier =>
            return In_Context (Element, With_Context)
              or else In_Context (Element, Use_Context)
              or else In_Context (Element, Param_Context)
              or else In_Context (Element, Field_Context)
              or else In_Context (Element, Object_Context)
              or else In_Context (Element, Discriminant_Context);

         when Ada_Abstract_Subp_Decl |
              Ada_Aspect_Spec |
              Ada_Attribute_Def_Clause |
              Ada_Block_Stmt |
              Ada_Case_Stmt |
              Ada_Entry_Decl |
              Ada_Enum_Literal_Decl |
              Ada_Exception_Handler |
              Ada_Expr_Function |
              Ada_Extended_Return_Stmt |
              Ada_For_Loop_Spec |
              Ada_Formal_Subp_Decl |
              Ada_Generic_Package_Decl |
              Ada_Generic_Package_Instantiation |
              Ada_Generic_Package_Renaming_Decl |
              Ada_Generic_Subp_Decl |
              Ada_Generic_Subp_Instantiation |
              Ada_Generic_Subp_Renaming_Decl |
              Ada_If_Stmt |
              Ada_Loop_Stmt |
              Ada_Null_Subp_Decl |
              Ada_Package_Body |
              Ada_Package_Decl |
              Ada_Package_Renaming_Decl |
              Ada_Pragma_Node |
              Ada_Protected_Body |
              Ada_Protected_Type_Decl |
              Ada_Single_Protected_Decl |
              Ada_Single_Task_Decl |
              Ada_Subp_Body |
              Ada_Subp_Decl |
              Ada_Subp_Renaming_Decl |
              Ada_Subtype_Decl |
              Ada_Task_Body |
              Ada_Task_Type_Decl |
              Ada_Type_Decl |
              Ada_Variant_Part =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Exposed;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Element : Ada_Node) return Boolean is
      Next : Ada_Node := Element;
   begin
      while Next /= No_Ada_Node loop
         case Next.Kind is
            when Ada_Subp_Body | Ada_Task_Body =>
               return True;
            when others =>
               null;
         end case;

         Next := Next.Parent;
      end loop;

      return False;
   end Is_Local;

end LAL.Semantic_Trees;
