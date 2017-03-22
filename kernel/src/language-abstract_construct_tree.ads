------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Vectors;

with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language.Tree;                   use Language.Tree;
with Language.Tree.Database;          use Language.Tree.Database;

with GPS.Kernel;                      use GPS.Kernel;

package Language.Abstract_Construct_Tree is

   --  This is the concrete implementation of the abstract language tree, based
   --  on the constructs engine. It is currently being used for every language,
   --  and is the default language tree that GPS kernel will return. The
   --  creation of the abstract API was meant to abstract the constructs behind
   --  a common interface, that would make more sense to implement for other
   --  languages
   --  See Language.Abstract_Language_Tree for a description of the interface

   ----------------------
   -- Type definitions --
   ----------------------

   type Construct_Tree_Provider is new Semantic_Tree_Provider with record
      Kernel    : Kernel_Handle;
   end record;
   type Construct_Tree_Provider_Access is access all Construct_Tree_Provider;

   type Abstract_Construct_Tree is new Semantic_Tree with record
      Construct_File : Structured_File_Access;
      Kernel         : Kernel_Handle;
   end record;
   type Construct_Tree_Access is access all Abstract_Construct_Tree;

   type Construct_Node is new Semantic_Node with record
      Construct_File : Structured_File_Access;
      Entity         : Entity_Access;
      Kernel         : Kernel_Handle;
   end record;

   type Abstract_Construct_Tree_Iterator
   is new Semantic_Tree_Iterator with record
      It : Construct_Tree_Iterator;
      Kernel : Kernel_Handle;
      Construct_File : Structured_File_Access;
   end record;

   overriding procedure Next (Self : in out Abstract_Construct_Tree_Iterator);

   overriding function Element
     (Self : Abstract_Construct_Tree_Iterator) return Semantic_Node'Class;

   overriding function Has_Element
     (Self : Abstract_Construct_Tree_Iterator) return Boolean;

   type Construct_Node_Access is access all Construct_Node;

   ----------------------------------------
   -- Construct_Tree_Provider primitives --
   ----------------------------------------

   function Create (K : Kernel_Handle) return Semantic_Tree_Provider_Access;

   overriding function Get_Tree_For_File
     (Self    : Construct_Tree_Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class;

   overriding function Root_Nodes
     (Self : Abstract_Construct_Tree) return Semantic_Node_Array'Class;

   overriding function Root_Iterator
     (Self : Abstract_Construct_Tree) return Semantic_Tree_Iterator'Class;

   overriding function Node_At
     (Self : Abstract_Construct_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class;

   overriding function File
     (Self : Abstract_Construct_Tree) return GNATCOLL.VFS.Virtual_File;

   overriding procedure Update (Self : Abstract_Construct_Tree);

   overriding procedure Update_Async (Self : Abstract_Construct_Tree);

   overriding function Is_Ready (Self : Abstract_Construct_Tree)
     return Boolean is (True);

   overriding function Category
     (Self : Construct_Node) return Language_Category;

   overriding function Parent
     (Self : Construct_Node) return Semantic_Node'Class;

   overriding function Children
     (Self : Construct_Node)
      return Semantic_Node_Array'Class;

   overriding function First_Child
     (Self : Construct_Node) return Semantic_Node'Class;

   overriding function Name
     (Self : Construct_Node) return GNATCOLL.Symbols.Symbol;

   overriding function Profile
     (Self : Construct_Node;
      Show_Param_Names : Boolean) return GNATCOLL.Symbols.Symbol;

   overriding function Is_Valid
     (Self : Construct_Node) return Boolean is (True);

   overriding function Definition
     (Self : Construct_Node) return Semantic_Node'Class;

   overriding function Sloc_Def
     (Self : Construct_Node) return Sloc_T;

   overriding function Sloc_Start
     (Self : Construct_Node) return Sloc_T;

   overriding function Sloc_End
     (Self : Construct_Node) return Sloc_T;

   overriding function Get_Hash
     (Self : Construct_Node) return Hash_Type;

   overriding function File
     (Self : Construct_Node) return GNATCOLL.VFS.Virtual_File;

   overriding function Is_Declaration
     (Self : Construct_Node) return Boolean;

   overriding function Visibility
     (Self : Construct_Node) return Construct_Visibility;

   overriding function Unique_Id
     (Self : Construct_Node) return GNATCOLL.Symbols.Symbol;

   overriding function Documentation_Body
     (Self : Construct_Node) return String;

   overriding function Documentation_Header
     (Self : Construct_Node) return String;

private

   --------------------------------------------------------------------------
   -- Private definition of Construct_Node_Array and associated primitives --
   --------------------------------------------------------------------------

   package Construct_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Construct_Node);

   type Construct_Node_Array is new Semantic_Node_Array with record
      Nodes : Construct_Node_Vectors.Vector;
   end record;

   overriding function Get
     (Self : Construct_Node_Array; Index : Positive) return Semantic_Node'Class
   is
      (Self.Nodes.Element (Index));

   overriding function Length (Self : Construct_Node_Array) return Natural is
     (Natural (Self.Nodes.Length));

   overriding procedure Sort
     (Self : in out Construct_Node_Array;
      Less_Than : access function (L, R : Semantic_Node'Class) return Boolean);

end Language.Abstract_Construct_Tree;
