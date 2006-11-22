-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
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

package Language.Tree.Ada is

   type Ada_Construct_Tree is private;
   --  This type contains ada-specific information, extending a generic
   --  construct tree. This structure contains only the ada-specific part of
   --  the information, and has to be used with the construct tree from which
   --  it has been created.

   Null_Ada_Construct_Tree : constant Ada_Construct_Tree;

   procedure Free
     (Tree : Construct_Tree; Ada_Tree : in out Ada_Construct_Tree);
   --  Free the data associated to an ada construct tree access. The
   --  Construct_Tree given in parameter will not be freed, but is needed in
   --  order to identify clearly the contents referenced by the Ada_Tree.

   function Generate_Ada_Construct_Tree
     (Tree     : Construct_Tree;
      Buffer   : String_Access) return Ada_Construct_Tree;
   --  Return the ada construct tree corresponding to this construct tree.

   procedure Generate_Ada_Construct_Trees
     (Spec_Tree, Body_Tree         : Construct_Tree;
      Spec_Buffer, Body_Buffer     : String_Access;
      Spec_Ada_Tree, Body_Ada_Tree : out Ada_Construct_Tree);
   --  Generates the ada constructs trees for the two trees given in parameter,
   --  assuming that Spec_Tree is the specification of Body_Tree. References
   --  will be made between the two trees when appropriate.

   function Get_Spec
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Construct_Cell_Access;
   --  Return the specification of the given iterator if any, or the iterator
   --  itself otherwise.

   function Get_First_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Construct_Cell_Access;
   --  Return the first body found in the tree

   function Get_Second_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Construct_Cell_Access;
   --  Return the second body found in the tree

   function Get_Most_Complete_View
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Construct_Cell_Access;
   --  Return the most complete view accessible for the iterator given in
   --  parameter.

   function Is_Most_Complete_View
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Boolean;
   --  Return false if we have build a most complete view of the entity pointed
   --  by the iterator, false otherwise.

   function Is_First_Occurence
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Boolean;
   --  Return True if the iterator given in parameter is the first known
   --  occurence, according to the trees.

   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Ada_Tree   : Ada_Construct_Tree;
      Offset     : Natural;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Construct_Tree_Iterator_Array;
   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Ada_Tree   : Ada_Construct_Tree;
      From       : Construct_Tree_Iterator;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Construct_Tree_Iterator_Array;
   --  Return the closest entity (closest visibility-wise) from the position
   --  given in parameter, for the given name.

   function Get_Visible_Constructs
     (Tree         : Construct_Tree;
      Ada_Tree     : Ada_Construct_Tree;
      Start_Entity : Construct_Tree_Iterator;
      Id           : Composite_Identifier;
      Use_Wise     : Boolean := True)
      return Construct_Tree_Iterator_Array;
   function Get_Visible_Constructs
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Offset   : Natural;
      Id       : Composite_Identifier;
      Use_Wise : Boolean := True)
      return Construct_Tree_Iterator_Array;
   --  Return the first construct visible with the given name from the given
   --  offset. Null_Tree_Iterator if none. This function is sensitive to
   --  private parts, and to use clauses.

   function Is_Enum_Type
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Boolean;
   --  Return tree if the construct given in parameter is a type enumeration

   type Ada_Tree_Language is new Tree_Language with private;
   --  This type provides an ada implementation to the language-specific tree
   --  functionalitites.

   function Get_Language
     (Tree : access Ada_Tree_Language) return Language_Access;
   --  See inherited documentation

   function Get_Parent_Tree
     (Lang       : access Ada_Tree_Language;
      Left_Tree  : Construct_Tree;
      Right_Tree : Construct_Tree) return Get_Parent_Tree_Result;
   --  Return true if Child is a child unit of parent, false otherwise.

   function Get_Public_Tree
     (Lang      : access Ada_Tree_Language;
      Full_Tree : access Construct_Tree;
      Free_Tree : Boolean) return Construct_Tree;
   --  See inherited documentation

   function Get_Unit_Construct
     (Lang : access Ada_Tree_Language;
      Tree : Construct_Tree) return Construct_Tree_Iterator;
   --  Return the unit construct of the tree given in parameter

   function Get_Unit_Name
     (Lang : access Ada_Tree_Language;
      Tree : Construct_Tree) return Composite_Identifier;
   --  See inherited documentation

   function Get_Name_Index
     (Lang      : access Ada_Tree_Language;
      Construct : Simple_Construct_Information) return String;
   --  See inherited documentation

   function Compare_Entities
     (Lang                      : access Ada_Tree_Language;
      Left_Iter, Right_Iter     : Construct_Tree_Iterator;
      Left_Tree, Right_Tree     : Construct_Tree;
      Left_Buffer, Right_Buffer : String_Access)  return General_Order;
   --  See inherited documentation. If the two cells points to two parts of
   --  the same entity, this will return Equals.

   Ada_Tree_Lang : constant Tree_Language_Access;

   function Is_Body_Of (Body_Tree, Spec_Tree : Construct_Tree) return Boolean;
   --  Return True if Body_Tree is indeed the body of Spec_Tree

private

   type Ada_Construct_Relation is record
      Spec : Construct_Cell_Access;

      First_Body : Construct_Cell_Access;
      --  If the entity is a subprogram, this stores the position of its body.
      --  If it's a type, it stores the position of the declaration (the public
      --  declaration in case there is a full one in the private one as well).

      Second_Body : Construct_Cell_Access;
      --  Only used in the construct is a type. Stores the position of the
      --  full declaration in the private part.
   end record;

   type Relation_Ptr is access all Ada_Construct_Relation;

   type Ada_Construct_Tree_Information is record
      Ada_Relation : Relation_Ptr;
   end record;

   type Ada_Construct_Tree_Array is array
     (Positive range <>) of Ada_Construct_Tree_Information;

   type Ada_Construct_Tree is access all Ada_Construct_Tree_Array;

   Null_Ada_Construct_Tree : constant Ada_Construct_Tree := null;

   type Ada_Tree_Language is new Tree_Language with null record;

   Ada_Tree_Lang : constant Tree_Language_Access := new Ada_Tree_Language;

end Language.Tree.Ada;
