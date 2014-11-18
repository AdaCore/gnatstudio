------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

--  This package defines the abstract interface for a language tree, which
--  is used thorougly across GPS. The aim is to replace the old ad-hoc trees
--  (constructs for example) that we had before, eventually by wrapping them.
--  The idea is that you can provide a semantic interface to a new language by
--  implementing this interface, and registering a tree provider in the kernel.
--  This tree provides mainly syntactic information about the code, but it is
--  not impossible that this interface might someday be extended to provide
--  semantic abilities. The reason this has not yet been done is because the
--  XRef interface is supposed to provide the semantic abilities, but we might
--  want to bridge across the two at some point.
--
--  The abstract interface is composed of a few interface types, that each
--  provide a different layer into the tree.
--
--  * The first layer is the tree provider, which provides the basic ability
--    of getting the corresponding tree for a file. It is not meant to be
--    used directly by the client, which will rather go through the kernel
--    interface.
--
--  * The second layer is the tree, which provides the ability to explore
--    the nodes of the tree, and to ask the tree to update itself after the
--    file has changed.
--
--  * The third layer is the tree node, with which you can query information
--    about the syntactic structure of the tree. An important point is that
--    thoses nodes are not meant to be kept across reparse, and doing so
--    will result in undefined behavior !
--
--  * The fourth layer, meant to provide a way around the fact that nodes
--    are not meant to be kept, is the node information, that will provide
--    you with some static information about the node, that you can keep as
--    long as you want.
--
--  An important thing about this interface is that it is meant to be used in
--  a value paradigm rather than in a reference paradigm: Nodes don't survive
--  a reparse, but they have an unique_id that you can use to keep identity
--  across reparses, and which is what is used in the GPS outline.

with Language.Tree; use Language.Tree;
with Ada.Containers; use Ada.Containers;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Language.Abstract_Language_Tree is

   ----------------------
   -- Type definitions --
   ----------------------

   type Sloc_T is record
      Line   : Natural;
      --  Line in the file, starting at 1
      Column : Visible_Column_Type;
      --  Column in the file, starting at 1
      Index  : Natural;
      --  Absolute index in the file
   end record;
   --  This is just a simple source location type, that is meant to use real
   --  columns rather than character offsets, unlike the Source_Location type

   No_Sloc_T : constant Sloc_T := (0, 0, 0);

   function "<" (Left, Right : Sloc_T) return Boolean is
      (Left.Index < Right.Index);

   type Semantic_Tree_Provider is interface;
   type Semantic_Tree_Provider_Access
   is access all Semantic_Tree_Provider'Class;
   --  This type represents a tree factory that will provide you with the right
   --  tree for a given file. It is the point of dispatch of the API, since
   --  different providers will provide you with different trees, adapted to
   --  the language they were implemented for.

   type Semantic_Tree is interface;
   type Semantic_Tree_Access is not null access all Semantic_Tree'Class;
   --  This type represent the tree for a given file.

   type Semantic_Node is interface;
   type Semantic_Node_Access is not null access all Semantic_Node'Class;
   --  This type represents a node in the tree.

   type Semantic_Node_Array is interface;
   --  This type represents a collection of nodes. This was introduced so that
   --  the underlying representation of a node list doesn't matter much. The
   --  starting index is 1

   type Semantic_Tree_Iterator is interface;

   subtype Semantic_Node_Visibility is Language.Construct_Visibility;

   type Semantic_Node_Info is record
      Category   : Language_Category;
      --  The category/kind of this node
      Name       : GNATCOLL.Symbols.Symbol;
      --  The name of this node if applicable
      Profile    : Unbounded_String;
      --  The profile of this node, if this node is a subprogram
      --  declaration/body.
      Unique_Id  : Unbounded_String;
      --  The unique Id of this node
      Is_Decl    : Boolean;
      --  Wether this node represents a declaration
      Visibility : Semantic_Node_Visibility;
      --  Visibility of this node at a package level
      Sloc_Start : Sloc_T;
      --  The starting source location of this node
      Sloc_Def   : Sloc_T;
      --  The source location where the defining identifier of this node is, if
      --  there is one
   end record;
   --  This type represents the static information about a node.

   No_Node_Info : Semantic_Node_Info :=
     (Cat_Unknown, No_Symbol, Null_Unbounded_String,
      Null_Unbounded_String, False, Visibility_Public, (0, 0, 0), (0, 0, 0));

   ----------------------------------------
   -- Primitives for Semantic_Node_Array --
   ----------------------------------------

   function Get
     (Self : Semantic_Node_Array; Index : Positive) return Semantic_Node'Class
      is abstract;
   --  Returns the element at index Index in the node array, with the starting
   --  index being 1

   function Length
     (Self : Semantic_Node_Array) return Natural
      is abstract;
   --  Returns the total length of the array

   -------------------------------------------
   -- Primitives for Semantic_Tree_Provider --
   -------------------------------------------

   function Get_Tree_For_File
     (Self : Semantic_Tree_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
      is abstract;
   --  Get the tree for the given file. This function should not be called
   --  outside of GPS kernel

   ----------------------------------
   -- Primitives for Semantic_Tree --
   ----------------------------------

   function Root_Iterator
     (Self : Semantic_Tree) return Semantic_Tree_Iterator'Class is abstract;

   function Root_Nodes
     (Self : Semantic_Tree) return Semantic_Node_Array'Class
      is abstract;
   --  Returns the forest of nodes at the root of this tree. For example,
   --  in Ada this will be with/use statements, and a package/subprogram/..
   --  declaration

   function Node_At
     (Self : Semantic_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class is abstract;
   --  Returns the most precise enclosing node at location which satisfies
   --  filter

   function File
     (Self : Semantic_Tree) return GNATCOLL.VFS.Virtual_File is abstract;
   --  Returns the file for this semantic tree

   procedure Update
     (Self : Semantic_Tree) is abstract;
   --  Ask the tree to update itself to the new content of the file. This will
   --  invalidate all semantic nodes

   ----------------------------------
   -- Primitives for Semantic_Node --
   ----------------------------------

   function Is_Valid
     (SN : Semantic_Node) return Boolean is abstract;
   --  Predicate to indicate wether this node is valid. This is used to
   --  guarantee that clients will not generate invalid results on other
   --  primitives of nodes

   function Category
     (Self : Semantic_Node) return Language_Category is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Gets the category of node

   function Is_Declaration
     (Self : Semantic_Node) return Boolean
   is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Wether the node is a declaration or not

   function Children
     (Self : Semantic_Node) return Semantic_Node_Array'Class
      is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Return a collection of children of node

   function First_Child
     (Self : Semantic_Node) return Semantic_Node'Class is abstract;

   function Parent
     (Self : Semantic_Node) return Semantic_Node'Class is abstract;
   --  Return the parent of node

   function Name
     (Self : Semantic_Node) return GNATCOLL.Symbols.Symbol is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Return the name of node, if applicable

   function Sloc_Start
     (Self : Semantic_Node) return Sloc_T is abstract;
   --  Return the starting source location of node

   function Sloc_Def
     (Self : Semantic_Node) return Sloc_T is abstract;
   --  Return the source location of the defining identifier for node, if
   --  applicable

   function Sloc_End
     (Self : Semantic_Node) return Sloc_T is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Return the ending source location for node.

   function Profile
     (Self : Semantic_Node) return String is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Return the profile for node, if applicable (typically if node is a
   --  subprogram node)

   function Definition
     (Self : Semantic_Node) return Semantic_Node'Class is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Returns the counterpart of node. If node is an ada type definition with
   --  a private view, it will return the private view, and vice versa. If node
   --  is a subprogram declaration with a body, it will return the body.

   function Get_Hash
     (Self : Semantic_Node) return Hash_Type is abstract
     with Pre'Class => (Self.Is_Valid);
   --  Returns a hash for the node

   function File
     (Self : Semantic_Node) return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the file corresponding to node

   function Unique_Id
     (SN : Semantic_Node) return String is abstract;
   --  Return the unique id for this node

   function Visibility
     (SN : Semantic_Node) return Semantic_Node_Visibility is abstract;
   --  Return the visibility of this node

   function Info
     (Self : Semantic_Node) return Semantic_Node_Info is abstract;
   --  Return a static information record for this node

   function Hash
     (SN : Semantic_Node'Class) return Hash_Type is
     (SN.Get_Hash);
   --  Static non dispatching hash function for nodes

   function Documentation_Body
     (SN : Semantic_Node) return String is abstract;
   --  Returns the body for the documentation of this node, if applicable

   function Documentation_Header
     (SN : Semantic_Node) return String is abstract;
   --  Returns the documentation's header for this node, if applicable

   -------------------------------------------
   -- Primitives for Semantic_Tree_Iterator --
   -------------------------------------------

   procedure Next (It : in out Semantic_Tree_Iterator) is abstract;

   function Element
     (It : Semantic_Tree_Iterator) return Semantic_Node'Class is abstract;

   function Has_Element
     (It : Semantic_Tree_Iterator) return Boolean is abstract;

   No_Semantic_Node : constant Semantic_Node'Class;
   No_Semantic_Tree : constant Semantic_Tree'Class;
   No_Semantic_Node_Array : constant Semantic_Node_Array'Class;
   No_Semantic_Tree_Iterator : constant Semantic_Tree_Iterator'Class;

   package Sem_Tree_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Tree'Class);

   package Sem_Node_Holders is new Ada.Containers.Indefinite_Holders
     (Semantic_Node'Class);

private

   -------------------------------
   -- Dummy_Semantic_Node_Array --
   -------------------------------

   type Dummy_Semantic_Node_Array is new Semantic_Node_Array with null record;

   overriding function Get
     (Self : Dummy_Semantic_Node_Array;
      Index : Positive) return Semantic_Node'Class
   is (No_Semantic_Node);

   overriding function Length
     (Self : Dummy_Semantic_Node_Array) return Natural
      is (0);

   -------------------------
   -- Dummy_Semantic_Node --
   -------------------------

   type Dummy_Semantic_Node is new Semantic_Node with null record;

   overriding function First_Child
     (Self : Dummy_Semantic_Node) return Semantic_Node'Class
   is (No_Semantic_Node);

   function Sloc_Def
     (Self : Dummy_Semantic_Node) return Sloc_T is (0, 0, 0);

   function Documentation_Body
     (SN : Dummy_Semantic_Node) return String is ("");

   function Documentation_Header
     (SN : Dummy_Semantic_Node) return String is ("");

   overriding function Info
     (Self : Dummy_Semantic_Node) return Semantic_Node_Info
   is
     (No_Node_Info);

   overriding function Visibility
     (Self : Dummy_Semantic_Node) return Semantic_Node_Visibility
   is
      (Visibility_Public);

   overriding function Is_Valid
     (SN : Dummy_Semantic_Node) return Boolean is (False);

   overriding function Category
     (Self : Dummy_Semantic_Node) return Language_Category is (Cat_Unknown);

   overriding function Is_Declaration
     (Self : Dummy_Semantic_Node) return Boolean is (False);

   overriding function Children
     (Self : Dummy_Semantic_Node) return Semantic_Node_Array'Class
   is (No_Semantic_Node_Array);

   overriding function Parent
     (Self : Dummy_Semantic_Node) return Semantic_Node'Class
   is (No_Semantic_Node);

   overriding function Name
     (Self : Dummy_Semantic_Node) return GNATCOLL.Symbols.Symbol
   is (No_Symbol);

   overriding function Sloc_Start
     (Self : Dummy_Semantic_Node) return Sloc_T
   is (Sloc_T'(others => <>));

   overriding function Sloc_End
     (Self : Dummy_Semantic_Node) return Sloc_T
   is (Sloc_T'(others => <>));

   overriding function Profile
     (Self : Dummy_Semantic_Node) return String is ("");

   overriding function Definition
     (Self : Dummy_Semantic_Node) return Semantic_Node'Class is
      (No_Semantic_Node);

   overriding function Get_Hash
     (Self : Dummy_Semantic_Node) return Hash_Type is (0);

   overriding function File
     (Self : Dummy_Semantic_Node) return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.No_File);

   overriding function Unique_Id
     (Self : Dummy_Semantic_Node) return String is ("");

   ----------------------------------
   -- Dummy_Semantic_Tree_Iterator --
   ----------------------------------

   type Dummy_Semantic_Tree_Iterator
   is new Semantic_Tree_Iterator with null record;

   overriding procedure Next
     (It : in out Dummy_Semantic_Tree_Iterator) is null;

   overriding function Element
     (It : Dummy_Semantic_Tree_Iterator) return Semantic_Node'Class
   is (No_Semantic_Node);

   overriding function Has_Element
     (It : Dummy_Semantic_Tree_Iterator) return Boolean is (False);

   -------------------------
   -- Dummy_Semantic_Tree --
   -------------------------

   type Dummy_Semantic_Tree is new Semantic_Tree with null record;

   function Root_Iterator
     (Self : Dummy_Semantic_Tree) return Semantic_Tree_Iterator'Class is
      (No_Semantic_Tree_Iterator);

   function Root_Nodes
     (Self : Dummy_Semantic_Tree) return Semantic_Node_Array'Class
      is (No_Semantic_Node_Array);

   function Node_At
     (Self : Dummy_Semantic_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class is (No_Semantic_Node);

   function File
     (Self : Dummy_Semantic_Tree) return GNATCOLL.VFS.Virtual_File is
     (GNATCOLL.VFS.No_File);

   overriding procedure Update (Self : Dummy_Semantic_Tree) is null;

   --------------------
   -- Null constants --
   --------------------

   No_Semantic_Node : constant Semantic_Node'Class :=
     Dummy_Semantic_Node'(others => <>);

   No_Semantic_Tree : constant Semantic_Tree'Class :=
     Dummy_Semantic_Tree'(others => <>);

   No_Semantic_Node_Array : constant Semantic_Node_Array'Class :=
     Dummy_Semantic_Node_Array'(others => <>);

   No_Semantic_Tree_Iterator : constant Semantic_Tree_Iterator'Class :=
     Dummy_Semantic_Tree_Iterator'(others => <>);

end Language.Abstract_Language_Tree;
