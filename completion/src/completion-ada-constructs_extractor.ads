-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Provides a completer working on language constructs for Ada

with Ada.Containers.Doubly_Linked_Lists; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with Language.Tree;              use Language.Tree;
with Language.Tree.Database;     use Language.Tree.Database;
with Ada_Semantic_Tree.List_Resolver;
use Ada_Semantic_Tree.List_Resolver;
with Ada_Semantic_Tree.Declarations;  use Ada_Semantic_Tree.Declarations;

private with Completion.History;

package Completion.Ada.Constructs_Extractor is

   type Construct_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on the constructs found in a given file

   function New_Construct_Completion_Resolver
     (Construct_Db   : Construct_Database_Access;
      Current_File   : Virtual_File;
      Current_Buffer : String_Access)
      return Completion_Resolver_Access;
   --  Create a new resolver, based on a construct tree, and the current
   --  analyzed file.

   overriding
   procedure Get_Completion_Root
     (Resolver : access Construct_Completion_Resolver;
      Offset   : Integer;
      Context  : Completion_Context;
      Result   : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Construct_Completion_Resolver) return String;
   --  See inherited documentation

   procedure Free (This : in out Construct_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private

   package Tree_List is new Doubly_Linked_Lists (Construct_Tree);

   use Tree_List;
   use Completion.History;

   type Construct_Completion_Resolver is new Completion_Resolver with record
      Construct_Db    : Construct_Database_Access;
      Current_File    : Structured_File_Access;
      Current_Buffer  : String_Access;
   end record;

   ------------------------------------------
   -- Stored_Construct_Completion_Proposal --
   ------------------------------------------

   type Stored_Construct_Completion_Proposal is new Stored_Proposal with record
      Persistent_Entity : Entity_Persistent_Access;
      Is_All            : Boolean;
      Is_In_Call        : Boolean;
      Actual_Params     : Actual_Parameter_Resolver_Access;
   end record;

   overriding
   function Equal
     (Left  : Stored_Construct_Completion_Proposal;
      Right : Stored_Proposal'Class)
      return Boolean;

   overriding
   function From_Stored_Proposal
     (Stored  : Stored_Construct_Completion_Proposal;
      Manager : Completion_Manager_Access;
      Context : Completion_Context)
      return Completion_Proposal_Access;

   overriding
   procedure Free
     (Stored : in out Stored_Construct_Completion_Proposal);

   -----------------------------------
   -- Construct_Completion_Proposal --
   -----------------------------------

   type Construct_Completion_Proposal is new Storable_Proposal
   with record
      Tree_Node     : Construct_Tree_Iterator;
      File          : Structured_File_Access;
      Is_All        : Boolean := False;
      Actual_Params : Actual_Parameter_Resolver_Access;
      Is_In_Call    : Boolean;
   end record;

   overriding
   function To_Completion_Id
     (Proposal : Construct_Completion_Proposal) return Completion_Id;
   --  See inherited documentation

   overriding
   function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   overriding
   function Get_Label
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   overriding
   function Get_Caret_Offset
     (Proposal : Construct_Completion_Proposal)
      return Basic_Types.Character_Offset_Type;
   --  See inherited documentation

   overriding
   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category;
   --  See inherited documentation

   overriding
   function Get_Visibility
     (Proposal : Construct_Completion_Proposal) return Construct_Visibility;
   --  See inherited documentation

   overriding
   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   overriding
   function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location;
   --  See inherited documentation

   overriding
   function Match
     (Proposal : Construct_Completion_Proposal;
      Context  : Completion_Context;
      Offset   : Integer) return Boolean;
   --  See inherited documentation

   overriding
   procedure Free (Proposal : in out Construct_Completion_Proposal);
   --  See inherited documentation

   overriding
   function To_Stored_Proposal
     (Proposal : Construct_Completion_Proposal)
      return Stored_Proposal_Access;
   --  See inherited documentation

   --------------------------
   -- Completion_Iterators --
   --------------------------

   -- Entity_Iterator --

   type Construct_Db_Wrapper is new Completion_List_Pckg.Virtual_List_Component
   with record
      Context  : Visibility_Context;
      Resolver : Completion_Resolver_Access;
      List     : Declaration_List;
   end record;

   overriding
   procedure Free (This : in out Construct_Db_Wrapper);

   type Iteration_Stage is (Initial_File, Parent_File, Database);

   type Construct_Tree_Iterator_Array_Access is
     access all Construct_Tree_Iterator_Array;

   procedure Free (This : in out Construct_Tree_Iterator_Array_Access);
   --  Free the data associated to this array

   type Formal_Parameter_Array_Access is access all Formal_Parameter_Array;

   procedure Free is new Standard.Ada.Unchecked_Deallocation
     (Formal_Parameter_Array, Formal_Parameter_Array_Access);

   type Construct_Iterator_Wrapper is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record
      Context  : Visibility_Context;
      Resolver : Completion_Resolver_Access;
      Iter     : Declaration_Iterator;

      Current_Decl : Declaration_View := Null_Declaration_View;

      --  This is used when completing with possible parameters
      Params_Array : Formal_Parameter_Array_Access;
      Params_It : Integer;
   end record;

   function First
     (Db_Construct : Construct_Db_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Construct_Iterator_Wrapper) return Boolean;
   --  See inherited documentation

   function Is_Valid (It : Construct_Iterator_Wrapper) return Boolean;
   --  Return true if the iterator is OK to be returned to the user, which
   --  means that either it points on an expected value or it's at end.

   procedure Next (It : in out Construct_Iterator_Wrapper);
   --  See inherited documentation

   function Get
     (This : Construct_Iterator_Wrapper) return Completion_Proposal'Class;
   --  See inherited documentation

   procedure Free (This : in out Construct_Iterator_Wrapper);
   --  Free the data associated to the wrapper

end Completion.Ada.Constructs_Extractor;
