------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2016, AdaCore                     --
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

--  This package provides an abstract syntax tree, and the related parser
--  Its aim is to provide unparsing and source modification abilities
--  It is at the moment solely focused on subprogram specifications, but is
--  meant to be extended to a bigger subset of the Ada grammar as the need
--  arises.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Doubly_Linked_Lists;
with Language; use Language;
with GPS.Editors; use GPS.Editors;
with Xref; use Xref;
with GPS.Core_Kernels; use GPS.Core_Kernels;
with Ada.Finalization;

package GPS.Syntax_Tree is
   pragma Elaborate_Body;

   function Contains
     (Start_Loc, End_Loc, Loc : Editor_Location'Class) return Boolean;

   Parser_Exception : exception;
   --  An exception in the parser

   type Editor_Location_Access is access all Editor_Location'Class;

   ---------------
   -- AST Utils --
   ---------------

   type Args_Mode_Kind is (Kind_In, Kind_Out, Kind_InOut);

   type Access_Modifier_Type is
     (Modifier_All, Modifier_Constant, Modifier_None);
   type Subprogram_Kind is (Kind_Function, Kind_Procedure);

   type AST_Node_Record is tagged;
   type AST_Node is access all AST_Node_Record'Class;
   type Subprogram_Specification is tagged;

   --------------------------------------------
   -- AST Node definition : Base node record --
   --------------------------------------------

   type AST_Node_Record is abstract tagged record
      Start_Loc : Editor_Location_Access;
      End_Loc   : Editor_Location_Access;
      Kernel    : Core_Kernel;
   end record;
   --  The base type for an AST node

   procedure Copy_Structure (Source : AST_Node_Record'Class;
                             Dest : in out AST_Node_Record'Class);
   --  Copies the structure of an the Source node to the Dest node, while
   --  keeping Dest's source locations

   function Unparse
     (Node : AST_Node_Record) return String is abstract;
   --  Unparse Node to a String representation

   function Same (Left, Right : AST_Node_Record'Class) return Boolean
   is
     (Left = Right
      and then Left.Start_Loc = Right.Start_Loc
      and then Left.End_Loc   = Right.End_Loc);
   --  Equality function to check that not only are Left and Right structurally
   --  equivalent, but that they also have the same source locations

   function Buffer (Node : AST_Node_Record) return Editor_Buffer'Class is
     (Node.Start_Loc.Buffer);
   --  Get the Editor_Buffer to which Node belongs

   procedure Hard_Replace (Node : AST_Node_Record'Class);
   --  Replace the in editor content of Node with the unparsed representation
   --  of Node. If Node hasn't been modified since its creation, it won't do
   --  anything.

   function Contains
     (Node : AST_Node_Record; Loc : Editor_Location'Class) return Boolean;
   --  Returns true if the bounds of Node in the source contains Loc

   -------------------------------------------
   -- AST Node definition : Type_Expression --
   -------------------------------------------

   type Type_Expression_Record is abstract
     new AST_Node_Record with null record;
   type Type_Expression_Access is access all Type_Expression_Record'Class;

   type Type_Expression is new Ada.Finalization.Controlled with record
      Contents : Type_Expression_Access;
   end record;
   --  Type representing an AST Node corresponding to a Type Expression.
   --  The base Type_Expression_Record is wrapped into this controlled type
   --  so that we can store instances of this type directly and have value
   --  semantics for our AST (eg. copying an AST Node is always deep copy).

   overriding procedure Adjust (Object : in out Type_Expression);

   Null_Type_Expression : Type_Expression :=
     (Ada.Finalization.Controlled with Contents => null);

   --------------------------
   -- Tokenizer definition --
   --------------------------

   package Tok is
      Tokenizer_Exception : exception;
      --  Exception arising in the tokenizer

      type Token_List;

      type Tokenizer is tagged record
         Tokens         : access Token_List;
         Src            : access String;
         Start_Location : Editor_Location_Access;
         Kernel         : Core_Kernel;
      end record;
      --  A Tokenizer takes in Ada source code in the form of a string and
      --  enables the user to get the syntactic tokens that form the source.
      --  This is actually a thin wrapper around Ada_Analyzer, to enable us
      --  to do some lookahead and such things with minimal fuss.

      type Token is record
         Entity : Language_Entity;
         Sloc_Start : Source_Location;
         Sloc_End : Source_Location;
         Tokenizr : Tokenizer;
      end record;
      --  Type representing an individual token.

      function Create
        (Editor : Editor_Buffer'Class;
         Start_Location : Editor_Location'Class;
         End_Location : Editor_Location'Class;
         Kernel : Core_Kernel) return Tokenizer;
      --  Tokenizer constructor. Will take in an Editor Buffer and the start
      --  and end locations, and return a Tokenizer for the corresponding
      --  source code.

      function Get_String (Tok : Token) return String;
      --  Get the source string corresponding to the token Tok.

      function Get (T : Tokenizer; I : Natural) return String;
      --  Get the source string at position I on the Tokenizer stack of tokens.

      function Get_Token (T : Tokenizer; I : Natural) return Token;
      --  Get the token at position I on the Tokenizer stack of tokens.

      function Has (T : Tokenizer; I : Natural) return Boolean;
      --  Returns True if the tokenizer has a number of remaining tokens
      --  superior or equal to I.

      procedure Pop (T : in out Tokenizer; I : Natural);
      --  Pop I tokens from the top of the stack.

      function Get_Comment (T : Tokenizer) return Token;
      --  Returns the first token if it is a comment, else raise an exception

      function Is_Comment (T : Tokenizer) return Boolean;
      --  Returns True if the token on top of the stack is a comment

      function Start_Loc (Tok : Token) return Editor_Location_Access;
      --  Returns the start location of Tok

      function End_Loc (Tok : Token) return Editor_Location_Access;
      --  Returns the end location of Tok

      package Token_Lists is new Ada.Containers.Doubly_Linked_Lists (Token);
      type Token_List is new Token_Lists.List with null record;

   end Tok;

   use Tok;

   -------------------------------------
   -- Generic operations on AST Nodes --
   -------------------------------------

   generic
      type AST_Subnode is new AST_Node_Record with private;
      with package List is
        new Ada.Containers.Doubly_Linked_Lists (AST_Subnode);
   package Gen_Ops is

      type Parse_Proc_Type is access procedure
          (T : in out Tokenizer;
           Node : out AST_Subnode;
           Has_Node : out Boolean);

      type Parse_List_Proc_Type is
        access procedure
          (T : in out Tokenizer; Node : in out List.List);

      function Parse_List
        (T : in out Tokenizer;
         Sep : String;
         El_Parser : Parse_Proc_Type) return List.List;
      --  Parse a List of nodes of type AST_Subnode. Each node is parsed
      --  with the procedure El_Parser, and separated by the separator Sep.

      function Parse_Lists
        (T : in out Tokenizer;
         Sep : String;
         Els_Parser : Parse_List_Proc_Type) return List.List;
      --  Parse a List of list of nodes of type AST_Subnode with the procedure
      --  Els_Parser. Each individual list is parsed with the procedure
      --  Els_Parser, and concatenated to the return list.

      function Unparse
        (Node_List : List.List; Sep : String) return String;
      --  Give a String representation of a node list,corresponding to
      --  Unparse called on each Node, concatenated together and separated by
      --  Sep.

   end Gen_Ops;

   --------------------------------------
   -- AST Node definition : Identifier --
   --------------------------------------

   type Identifier is new AST_Node_Record with record
      Name : Unbounded_String;
   end record;
   --  AST_Node representing an Identifier in the source.

   overriding function Unparse
     (Node : Identifier) return String;

   overriding function "="
     (Left, Right : Identifier) return Boolean
   is
      (Left.Name = Right.Name);

   package Id_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Identifier);

   Null_Identifier : Identifier :=
     (null, null, null, Null_Unbounded_String);

   -----------------------------------
   -- AST Node definition : Comment --
   -----------------------------------

   type Node_Comment is new AST_Node_Record with record
      Comment : Unbounded_String := Null_Unbounded_String;
   end record;
   --  AST_Node representing a Comment in the source.
   --  TODO : Should this be an AST_Node ?

   overriding function Unparse
     (Node : Node_Comment) return String;

   Null_Comment : Node_Comment :=
     Node_Comment'(Comment => Null_Unbounded_String, others => <>);

   package Comments_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Node_Comment);

   ------------------------------------------------
   -- AST Node definition : Subprogram_Arguments --
   ------------------------------------------------

   type Subprogram_Arguments is new AST_Node_Record with record
      Names          : Id_Lists.List; --  Names of the arguments
      Mode           : Args_Mode_Kind; --  Mode : in, out, or in out
      Args_Type      : Type_Expression; --  Type of the arguments
      Null_Exclusion : Boolean := False; --  If the args are "not null"

      Comments        : Comments_Lists.List := Comments_Lists.Empty_List;
      --  List of comments associated with the arguments in the source

      Colon_Location : Editor_Location_Access;
      --  Location of the colon in the source. Used to see if arguments in a
      --  spec are aligned or not.
   end record;
   --  AST_Node representing a group of subprogram arguments in a function
   --  specification, of the form "A,B,C : Integer"

   function Unparse_Type
     (Node : Subprogram_Arguments) return String;
   --  Unparse only the type part of an instance of Subprogram_Arguments

   overriding function Unparse
     (Node : Subprogram_Arguments) return String;

   overriding function "="
     (Left, Right : Subprogram_Arguments) return Boolean;

   function Has_Comments (Node : Subprogram_Arguments) return Boolean
   is
     (not Node.Comments.Is_Empty);

   package Args_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Subprogram_Arguments);

   ----------------------------------------------------
   -- AST Node definition : Subprogram_Specification --
   ----------------------------------------------------

   type Subprogram_Specification is new AST_Node_Record with record
      Args     : Args_Lists.List; --  List of Subprogram_Arguments
      Ret_Type : Type_Expression; --  Return type, if function
      Args_Profile_End_Loc : Editor_Location_Access;
      --  Location of the end of the args profile (closing parenthesis)
   end record;

   overriding function Unparse
     (Node : Subprogram_Specification) return String;

   function Is_Args_Layout_Vertical
     (Subp : Subprogram_Specification) return Boolean;
   --  Returns true if each group of args is on a separate line, like
   --  function Test (A, B : Foo;
   --                 C : Bar) return FooBar

   function Is_Args_Layout_Aligned
     (Subp : Subprogram_Specification) return Boolean;
   --  Returns true if each group of args is aligned, that is if their colon
   --  is on the same colon, like so :
   --  function Test (A, B : Foo;
   --                 C    : Bar) return FooBar

   overriding function "="
     (Left, Right : Subprogram_Specification) return Boolean;

   function Unparse_Spec
     (Node : Subprogram_Specification;
      With_Parentheses : Boolean := True;
      Vertical : Boolean := False;
      Align_Params : Boolean := False) return String;
   --  Specific function to unparse a Subprogram_Specification, allowing one
   --  to specify the way the specification should be formatted.
   --  * If Vertical is true, each group of parameters will be on its own line
   --  * If Align_Params is true, groups of params will be aligned as defined
   --    in the doc of Is_Args_Layout_Aligned

   Null_Subprogram_Specification : Subprogram_Specification;

   -------------------------------------------------
   -- AST Node definition : Subprogram_Definition --
   -------------------------------------------------

   type Subprogram_Definition is new AST_Node_Record with record
      Name : Identifier;
      Kind : Subprogram_Kind;
      Is_Overriding : Boolean := False;
      Spec : Subprogram_Specification;
   end record;
   --  AST_Node representing a complete program definition.

   type Subprogram_Definition_Access is access all Subprogram_Definition;

   overriding function "=" (Left, Right : Subprogram_Definition) return Boolean
   is
     (Left.Name = Right.Name
      and then Left.Kind = Right.Kind
      and then Left.Is_Overriding = Right.Is_Overriding
      and then Left.Spec = Right.Spec);

   function Get_Entity_Location
     (Node : Subprogram_Definition) return General_Location;
   --  Returns the location of the Entity => the start location of the Name
   --  node

   function Get_Entity
     (Node : Subprogram_Definition) return General_Entity;
   --  Returns the General_Entity instance corresponding to this subprogram

   function Is_Body (Node : Subprogram_Definition) return Boolean;
   --  Returns true if the Subprogram_Definition instance is part of a body

   function Is_Declaration (Node : Subprogram_Definition) return Boolean;
   --  Returns true if the Subprogram_Definition instance is part of a
   --  declaration

   function Get_Counterpart
     (Node : Subprogram_Definition;
      Factory : Editor_Buffer_Factory_Access)
      return Subprogram_Definition'Class;
   --  If the instance corresponds to a body, returns the instance
   --  corresponding to its declaration, and vice versa.

   overriding function Unparse
     (Node : Subprogram_Definition) return String;

   function Get_Subprogram_At
     (Editor : Editor_Buffer'Class;
      Kernel : Core_Kernel;
      Location : Editor_Location'Class) return Subprogram_Definition;
   --  Parses and create a Subprogram_Definition instance from the Editor and
   --  location given as parameters.

   Null_Subprogram_Definition : Subprogram_Definition;

   ----------------------------------------
   -- AST Node definition : Node_Subtype --
   ----------------------------------------

   type Node_Subtype is new Type_Expression_Record with record
      Type_Name : Unbounded_String;
      Class_Wide : Boolean := False;
   end record;
   type Node_Subtype_Access is access all Node_Subtype;

   overriding function "="
     (Left, Right : Node_Subtype) return Boolean
   is
     (Left.Type_Name = Right.Type_Name
      and then Left.Class_Wide = Right.Class_Wide);

   overriding function Unparse
     (Node : Node_Subtype) return String;

   --------------------------------------------
   -- AST Node definition : Node_Access_Type --
   --------------------------------------------

   type Node_Access_Type is new Type_Expression_Record with record
      Access_To : Node_Subtype;
      Null_Exclusion : Boolean := False;
      General_Access_Modifier : Access_Modifier_Type;
   end record;

   overriding function "=" (Left, Right : Node_Access_Type) return Boolean
   is
     (Left.Access_To = Right.Access_To
      and then Left.Null_Exclusion = Right.Null_Exclusion
      and then Left.General_Access_Modifier = Right.General_Access_Modifier);

   overriding function Unparse
     (Node : Node_Access_Type) return String;

   -------------------------------------------------------
   -- AST Node definition : Node_Access_Subprogram_Type --
   -------------------------------------------------------

   type Node_Access_Subprogram_Type is new Type_Expression_Record with record
      Is_Protected : Boolean;
      Kind : Subprogram_Kind;
      Spec : Subprogram_Specification;
   end record;

   overriding function "="
     (Left, Right : Node_Access_Subprogram_Type) return Boolean
   is
     (Left.Is_Protected = Right.Is_Protected
      and then Left.Kind = Right.Kind
      and then Left.Spec = Right.Spec);

   overriding function Unparse
     (Node : Node_Access_Subprogram_Type) return String;

end GPS.Syntax_Tree;
