-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;

package Completion.Ada.Constructs_Extractor is

   type Construct_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on the constructs found in a given file

   function New_Construct_Completion_Resolver
     (Construct_Db   : Construct_Database_Access;
      Current_File   : Virtual_File;
      Current_Buffer : String_Access)
      return Construct_Completion_Resolver;
   --  Create a new resolver, based on a construct tree, and the current
   --  analyzed file.

   procedure Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List);
   --  See inherited documentation

   procedure Free (This : in out Construct_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private

   package Tree_List is new Doubly_Linked_Lists (Construct_Tree);

   use Tree_List;

   type Construct_Completion_Resolver is new Completion_Resolver with record
      Construct_Db    : Construct_Database_Access;
      Current_File    : Structured_File_Access;
      Current_Buffer  : String_Access;

      Tree_Collection : Tree_List.List;
      --  This is the list of the tree that have to be freed when the resolver
      --  is freed.
   end record;

   type Construct_Completion_Proposal is new Completion_Proposal with record
      Tree_Node            : Construct_Tree_Iterator;
      File                 : Structured_File_Access;
      Is_All               : Boolean := False;
      Params_In_Expression : Integer := 0;
      Tree                 : Construct_Tree;
      Filter               : Possibilities_Filter;
      Buffer               : String_Access;
   end record;

   function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category;
   --  See inherited documentation

   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location;
   --  See inherited documentation

   procedure Get_Composition
     (Proposal   : Construct_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List);
   --  See inherited documentation

   function Get_Number_Of_Parameters
     (Proposal : Construct_Completion_Proposal) return Natural;
   --  See inherited documentation

   procedure Append_Expression
     (Proposal             : in out Construct_Completion_Proposal;
      Number_Of_Parameters : Natural);
   --  See inherited documentation

   procedure Free (Proposal : in out Construct_Completion_Proposal);
   --  See inherited documentation

   --------------------------
   -- Completion_Iterators --
   --------------------------

   -- Entity_Iterator --

   type Construct_Db_Wrapper is new Completion_List_Pckg.Virtual_List_Component
   with record
      First_File   : Structured_File_Access;
      First_Buffer : String_Access;
      Construct_Db : Construct_Database_Access;
      Name         : String_Access;
      Is_Partial   : Boolean;
      Offset       : Integer;
      Resolver     : access Construct_Completion_Resolver'Class;
      Filter       : Possibilities_Filter;
   end record;

   overriding
   procedure Free (This : in out Construct_Db_Wrapper);

   type Iteration_Stage is (Initial_File, Parent_File, Database);

   type Construct_Tree_Iterator_Array_Access is
     access all Construct_Tree_Iterator_Array;

   procedure Free (This : in out Construct_Tree_Iterator_Array_Access);
   --  Free the data associated to this array

   type Construct_Iterator_Wrapper is new
     Completion_List_Pckg.Virtual_List_Component_Iterator
   with record

      --  Common data

      Resolver   : access Construct_Completion_Resolver'Class;
      Stage      : Iteration_Stage := Initial_File;
      Name       : String_Access;
      Is_Partial : Boolean;
      Filter     : Possibilities_Filter;

      --  Data needed by the first completion stage (current file)

      First_Buffer       : String_Access;
      First_Tree         : Construct_Tree;
      Visible_Constructs : Construct_Tree_Iterator_Array_Access;
      Visible_Index      : Integer;

      --  Data needed by the second completion stage (parent files)

      First_File   : Structured_File_Access;
      Current_File : Structured_File_Access;
      Current_It   : Construct_Tree_Iterator := Null_Construct_Tree_Iterator;

      --  Data needed by the third completion stage (database)

      Construct_Db : Construct_Database_Access;
      Db_Iterator  : Construct_Db_Iterator;

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
