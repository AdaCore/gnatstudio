-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                      --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Language; use Language;

package Language.Tree is

   function Contains (Scope, Item : Construct_Access) return Boolean;
   --  Returns true if Item is contained by Scope, false otherwise.

   --------------------
   -- Construct_Tree --
   --------------------

   type Construct_Tree (<>) is private;

   Null_Construct_Tree : constant Construct_Tree;

   type Construct_Tree_Access is access all Construct_Tree;

   procedure Free (Tree : in out Construct_Tree_Access);
   --  Free the data associated to a construct tree access

   function To_Construct_Tree
     (Language       : Language_Access;
      Buffer         : String;
      List           : Construct_List;
      Compute_Scopes : Boolean := True)
      return Construct_Tree;
   --  Return the construct tree corresponding to the construct list given in
   --  parameter. If Compute_Scopes is True, the we will make an other pass
   --  trying to associate the different parts of the same construct (e.g.
   --  body and spec of a function, public and private part of a type...).
   --  This is possibly time consuming, and if not needed, can be deactivated
   --  by the user.

   -----------------------------
   -- Construct_Tree_Iterator --
   -----------------------------

   type Construct_Tree_Iterator is private;
   --  This type is used to iterate over a construct tree

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator;

   function First (Tree : Construct_Tree) return Construct_Tree_Iterator;
   --  Return the first element of a construct tree, typically the firt item
   --  of the source

   function Get_Parent_Scope
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator;
   --  Return the parent scope of the given iterator, Null_Construct_Tree if
   --  none.

   function Get_Construct (Iter : Construct_Tree_Iterator)
      return Construct_Access;
   --  Return the construct pointed by the iterator given in parameter.

   function Get_Iterator_At
     (Tree : Construct_Tree; Line, Line_Offset : Natural)
      return Construct_Tree_Iterator;
   --  Return the closest iterator before the given position

   function Get_Child_Number (Iter : Construct_Tree_Iterator) return Natural;
   --  Return the number of childs in the construct pointed by this iterator

   type Scope_Navigation is (Jump_Over, Jump_Into);

   function Next
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator;
   --  Return the next element of the tree. If Scope_Policy is Jump_Over, then
   --  the next function will not enter in any nested block, and will jump
   --  directly to the next element in the current scope. If there is no next
   --  element, then this function will return Null_Construct_Iterator.

   function Prev
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator;
   --  Return the previous element of the tree. If Scope_Policy is Jump_Over,
   --  then the prev function will not enter in any nested block, and will
   --  jump directly to the previous element in the current scope.
   --  If there is no previous element, then this function will return
   --  Null_Construct_Iterator.

   function Has_Children (Iter : Construct_Tree_Iterator) return Boolean;
   --  Return true if the constrcut pointed by the iterator has any child

   function Get_Last_Child
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator;
   --  Return the last child of the construct. If none, return the construct
   --  itself.

   function Is_Same_Entity
     (Tree : Construct_Tree; Iter1, Iter2 : Construct_Tree_Iterator)
      return Boolean;
   --  Return true if the two entity are the same, or two parts of the same
   --  entity (e.g. body / spec, private / public part...).

   function Encloses
     (Tree : Construct_Tree; Scope, Iter : Construct_Tree_Iterator)
      return Boolean;
   --  Return true if the construct pointed by Iter1 is enclosed by the
   --  construct enclosed by Scope.
   --  This returns always false if Scope is not actually pointing on a scope
   --  If Scope is a different part of the same scope, e.g. is Iter is on a
   --  body of a package and Scope is its spec, the function will still return
   --  true.

   function Get_Spec (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
     return Construct_Tree_Iterator;
   --  Return the specification of the given iterator if any, or the iterator
   --  given in

   function Get_First_Body
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator;
   --  Return the first body found in the tree

   function Get_Second_Body
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator;
   --  Return the second body found in the tree

   function Encloses
     (Tree              : Construct_Tree;
      Scope             : Construct_Tree_Iterator;
      Line, Line_Offset : Positive)
      return Boolean;
   function Encloses
     (Tree   : Construct_Tree;
      Scope  : Construct_Tree_Iterator;
      Offset : Positive)
      return Boolean;
   --  Same as above, but doesn't need a "real" entity, only a location

   function Get_Full_Name
     (Tree : Construct_Tree; It : Construct_Tree_Iterator) return String;
   --  Return the name of the construct given in parameter, prefixed by all the
   --  relevant enclosing units.

   --------------------------
   -- Composite_Identifier --
   --------------------------

   type Composite_Identifier (<>) is private;
   --  This type is used to store identifiers with multiple parts, e.g.
   --  A.B.

   function Length (Id : Composite_Identifier) return Natural;
   --  Return the number of items composing this identifier

   function Get_Item (Id : Composite_Identifier; Number : Natural)
     return String;
   --  Return a given item from the identifier.

   function Prepend
     (Id         : Composite_Identifier;
      Word_Begin : Natural;
      Word_End   : Natural)
     return Composite_Identifier;
   --  Return a composite identifier based on the same string, but with the
   --  index of a new element before. This function is used to construct an
   --  indentifier, and should not be called after.

   function To_Composite_Identifier (Identifier : String)
     return Composite_Identifier;
   --  Convert the given identifier to a composite identifier. This function
   --  will not necesseraly parse the whole string, but will stop as soon as
   --  the thing given is not an identifier.

   function To_String (Identifier : Composite_Identifier) return String;
   --  Return a string corresponding to the identifier given in parameter. This
   --  does not return blindly the string upon wich the identifier has been
   --  created, but remove all the irrelevant caracters (typically, blanks
   --  charaters)

   function Get_Slice
     (Identifier : Composite_Identifier; From : Natural; To : Natural)
      return Composite_Identifier;
   --  Return an identifier base on the slice of items specified in
   --  parameterers.

   type Construct_Tree_Iterator_Array is array (Natural range <>) of
     Construct_Tree_Iterator;

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array;

   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Offset     : Natural;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Construct_Tree_Iterator_Array;
   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      From       : Construct_Tree_Iterator;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False)
      return Construct_Tree_Iterator_Array;
   --  Return the closest entity (closest visibility-wise) from the position
   --  given in parameter, for the given name.

   function Get_Visible_Constructs
     (Tree         : Construct_Tree;
      Start_Entity : Construct_Tree_Iterator;
      Id           : Composite_Identifier;
      Use_Wise     : Boolean := True)
      return Construct_Tree_Iterator_Array;
   function Get_Visible_Constructs
     (Tree     : Construct_Tree;
      Offset   : Natural;
      Id       : Composite_Identifier;
      Use_Wise : Boolean := True)
      return Construct_Tree_Iterator_Array;
   --  Return the first construct visible with the given name from the given
   --  offset. Null_Tree_Iterator if none. This function is sensitive to
   --  private parts, and to use clauses.

private

   type Construct_Tree_Node is record
      Construct              : Construct_Access := null;
      Sub_Nodes_Length       : Natural := 0;
      Previous_Sibling_Index : Natural := 0;
      Parent_Index           : Natural := 0;

      Spec_Index             : Natural := 0;

      First_Body_Index       : Natural := 0;
      --  If the entity is a subprogram, this stores the position of its body.
      --  If it's a type, it stores the position of the declaration in the
      --  public part if there are two.

      Second_Body_Index      : Natural := 0;
      --  Only used in the construct is a type. Stores the position of the
      --  full declaration in the private part.
   end record;

   type Construct_Tree is array (Positive range <>) of Construct_Tree_Node;

   type Construct_Tree_Iterator is record
      Node  : Construct_Tree_Node;
      Index : Natural;
   end record;

   Null_Construct_Tree_Node : constant Construct_Tree_Node :=
     (null, 0, 0, 0, 0, 0, 0);

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator :=
     (Null_Construct_Tree_Node, 0);

   Null_Construct_Tree : constant Construct_Tree (1 .. 0) :=
     (others => Null_Construct_Tree_Node);

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array
     (1 .. 0) := (others => Null_Construct_Tree_Iterator);

   type Positions_Array is array (Natural range <>) of Integer;

   type Composite_Identifier
     (String_Length : Natural; Number_Of_Elements : Natural)
      is record
         Identifier     : String (1 .. String_Length);
         Position_Start : Positions_Array (1 .. Number_Of_Elements);
         Position_End   : Positions_Array (1 .. Number_Of_Elements);
      end record;

   function Get_Last_Relevant_Construct
     (Tree : Construct_Tree; Offset : Positive)
      return Construct_Tree_Iterator;
   --  Return the last construct represtenting the scope where the offset is.
   --  It can be either the last entity declared in the scope, or the scope
   --  itself.
   --
   --  example:
   --
   --  package A is
   --
   --     V : Integer; <- here is the last relevant construct
   --     <-  here is the offset
   --
   --  other example:
   --
   --  package A is
   --
   --     V : Integer;
   --
   --     package B is <- here is the last relevant construct
   --        V2 : Integer;
   --     end B;
   --
   --     <-  here is the offset
   --
   --  last example:
   --
   --  package A is <- here is the last relevant construct
   --
   --     <-  here is the offset

end Language.Tree;
