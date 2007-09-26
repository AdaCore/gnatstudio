-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2007, AdaCore                    --
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

--  This package provides a way of iterating over the contents of an entity,
--  taking into account various semantic information such as the fact that it
--  has several declarations parts (package spec & body), the visibility...

with Ada.Unchecked_Deallocation;

with GNAT.Strings;           use GNAT.Strings;

with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Units;        use Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Visibility;   use Ada_Semantic_Tree.Visibility;

package Ada_Semantic_Tree.Entity_Iteration is

   type Semantic_Tree_Iterator is private;
   --  This iterator is a special kind of iterator able to iterate over the
   --  contents of a element, with semantic knowledge. The precise definition
   --  dependends on the construct cell used to initialize the iterator:
   --
   --  Cat_Package
   --    * Child packages
   --    * Contents of the package

   type Semantic_Kind is (None, All_Access, Prefix_Notation);
   --  When a semantic information is found, it can be either from a "simple"
   --  reference, or through a particular construct such as the "all" keyword,
   --  which has to be handled in a particlar way, or an Ada 2005 prefix
   --  notation.

   type Semantic_Information is record
      Entity : Entity_Access;
      Kind   : Semantic_Kind := None;
   end record;
   --  This type wrapps an entity access with contexctual semantic information
   --  extracted from the reference found

   type References_To_Follow_Type is (Dereferences, Returned_Type);
   --  This type is used to keep track of references to break, for example to
   --  avoid to propose all to a access type that is already dereferenced.

   type References_To_Follow_Array is array
     (References_To_Follow_Type) of Boolean;

   All_References : References_To_Follow_Array := (others => True);

   function To_Semantic_Tree_Iterator
     (Info                 : Semantic_Information;
      From_Visibility      : Visibility_Context;
      References_To_Follow : References_To_Follow_Array := All_References;
      Excluded_Entities    : Excluded_Stack_Type := Null_Excluded_Stack)
      return Semantic_Tree_Iterator;
   --  Create a new iterator.
   --  If Follow_Referenced_Entities is true, then the contents of the
   --  potential referenced entity (e.g. the type of a variable, the parent
   --  of a tagged type) will be iterated as well.

   function At_End (It : Semantic_Tree_Iterator) return Boolean;
   --  Return true if there is no more entities to extract.

   procedure Next (It : in out Semantic_Tree_Iterator);
   --  Moves the cursor to the next entity

   function Is_Valid (It : Semantic_Tree_Iterator) return Boolean;
   --  Return true if the iterator is in a valid state.

   function Get (It : Semantic_Tree_Iterator) return Semantic_Information;
   --  Return the semantic information pointed by this iterator

   procedure Free (It : in out Semantic_Tree_Iterator);
   --  Free the data associated to this iterator.

   Null_Semantic_Tree_Iterator : constant Semantic_Tree_Iterator;

private

   type Semantic_Tree_Iterator_Step is
     (All_Access,
      Referenced_Entity,
      Referenced_Entity_From_Package,
      Child_Packages,
      Contents,
      Package_Body_Contents,
      Package_Spec_Contents,
      Tagged_Type_Contents,
      Finished);

   type Full_Construct_Cell is record
      It     : Construct_Tree_Iterator;
      Tree   : Construct_Tree;
      Buffer : String_Access;
   end record;

   type Semantic_Tree_Iterator_Access is access all Semantic_Tree_Iterator;

   type Semantic_Tree_Iterator is record
      Step : Semantic_Tree_Iterator_Step;
      Db   : Construct_Database_Access;
      References_To_Follow : References_To_Follow_Array := All_References;

      Root_Entity : Semantic_Information;

      Initial_File : Structured_File_Access;
      --  Very initial file for the iterator (could come from a parent
      --  iterator).

      Is_All : Boolean := False;

      Step_Has_Started : Boolean := False;
      --  Notes if the current step has started or not.

      Buffer : String_Access;

      Current_File      : Structured_File_Access;
      Current_Construct : Construct_Tree_Iterator;
      Current_Tree      : Construct_Tree := Null_Construct_Tree;

      Current_Kind : Semantic_Kind;

      Excluded_Entities : Excluded_Stack_Type := Null_Excluded_Stack;
      --  Since iteration is working on non compilable files, there might be
      --  cirtularities in the references. We don't want to enter in an
      --  infinite loop in this case, so we maitain a list of constructs that
      --  have been found during the process, in order to avoid cycling.

      From_Visibility : Visibility_Context;

      --  for Referenced_Entity step

      Decl_List      : Declaration_List := Null_Declaration_List;
      Decl_It        : Declaration_Iterator;
      Sub_It         : Semantic_Tree_Iterator_Access;
      Cut_Access     : Boolean;
      Cut_Subprogram : Boolean;

      --  for Contents step

      Content_It : Construct_Tree_Iterator := Null_Construct_Tree_Iterator;

      Package_Body      : Construct_Tree_Iterator :=
        Null_Construct_Tree_Iterator;
      Package_Body_File : Structured_File_Access;
      Package_Body_Tree : Construct_Tree;
      Package_Relation  : Location_Relation := Full_Spec_Hierarchy;

      --  for Child_Packages step

      Child_Pckg_It : Unit_Iterator;

      --  for Tagged_Type_Contents step

      Parents        : Entity_Array_Access;
      Parent_It      : Integer;
      Parent_Entity  : Entity_Access;
      Parent_Field   : Construct_Tree_Iterator;
      Parent_File    : Structured_File_Access;

      Dotted_Subprograms       : Entity_Persistent_Array_Access;
      Dotted_Subprograms_Index : Integer;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Semantic_Tree_Iterator, Semantic_Tree_Iterator_Access);

   Null_Semantic_Tree_Iterator : constant Semantic_Tree_Iterator :=
     (Step => Finished, others => <>);

end Ada_Semantic_Tree.Entity_Iteration;
