-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006                           --
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

with GNAT.Strings; use GNAT.Strings;

package Language.Tree is

   type Relative_Position is (Before, After, Specified, Enclosing);

   type General_Order is (Greater_Than, Lower_Than, Equals, Equivalent);

   function Contains (Scope, Item : Construct_Access) return Boolean;
   --  Returns true if Item is contained by Scope, false otherwise.

   --------------------
   -- Construct_Tree --
   --------------------

   type Construct_Tree is private;

   Null_Construct_Tree : constant Construct_Tree;

   procedure Free (Tree : in out Construct_Tree);
   --  Free the data associated to a construct tree.

   function To_Construct_Tree
     (List      : access Construct_List;
      Free_List : Boolean := False)
      return Construct_Tree;
   --  Return the construct tree corresponding to the construct list given in
   --  parameter. If Free_List is true, then the list given in parameter will
   --  be freed at the end of the process.

   function To_Construct_Tree
     (Buffer : String; Lang : access Language_Root'Class)
      return Construct_Tree;
   --  Same as above

   -----------------------------
   -- Construct_Tree_Iterator --
   -----------------------------

   type Construct_Tree_Iterator is private;
   --  This type is used to iterate over a construct tree

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator;

   function First (Tree : Construct_Tree) return Construct_Tree_Iterator;
   --  Return the first element of a construct tree, typically the first item
   --  of the source

   function Last (Tree : Construct_Tree) return Construct_Tree_Iterator;
   --  Return the last element of a construct tree.

   function Get_Parent_Scope
     (Tree : Construct_Tree;
      Iter : Construct_Tree_Iterator)
      return Construct_Tree_Iterator;
   --  Return the parent scope of the given iterator, Null_Construct_Tree if
   --  none.

   function Get_Construct
     (Iter : Construct_Tree_Iterator) return Simple_Construct_Information;
   --  Return the construct pointed by the iterator given in parameter.

   type Category_Array is array (Natural range <>) of Language_Category;
   Null_Category_Array : Category_Array (1 .. 0) := (others => Cat_Unknown);

   type Position_Type is (Start_Construct, Start_Name);

   function Get_Iterator_At
     (Tree              : Construct_Tree;
      Line, Line_Offset : Natural;
      From_Type         : Position_Type     := Start_Construct;
      Position          : Relative_Position := Specified;
      Categories_Seeked : Category_Array    := Null_Category_Array)
      return Construct_Tree_Iterator;
   --  Return the closest iterator before the given position. It's possible to
   --  look for the closest match just before or after the given position, and
   --  to reduce the scope of search to certain categories. If
   --  Categories_Seeked is null, then any category will be retrieved.
   --  From_Type determines if the search has to be done on the start of the
   --  construct or on the start of its name. This value is not used if
   --  position is 'Enclosing'.

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
   --  Return the previous element of the tree.
   --  If Scope_Policy is Jump_Over, Prev will not enter in any nested block
   --  and will jump directly to the previous element in the current scope.
   --  If there is no previous element,  this function will return
   --  Null_Construct_Iterator.

   function Has_Children (Iter : Construct_Tree_Iterator) return Boolean;
   --  Return true if the constrcut pointed by the iterator has any child

   function Get_Last_Child
     (Tree : Construct_Tree;
      Iter : Construct_Tree_Iterator) return Construct_Tree_Iterator;
   --  Return the last child of the construct. If none, return the construct
   --  itself.

   function Is_Same_Entity
     (Tree         : Construct_Tree;
      Iter1, Iter2 : Construct_Tree_Iterator) return Boolean;
   --  Return true if the two entities are either the same or two parts of the
   --  same entity (e.g. body / spec, private / public part...).

   function Encloses
     (Tree        : Construct_Tree;
      Scope, Iter : Construct_Tree_Iterator) return Boolean;
   --  Return true if the construct pointed by Iter is enclosed by the
   --  construct enclosed by Scope.
   --  This returns always false if Scope is not actually pointing on a scope
   --  If Scope is a different part of the same scope, e.g. is Iter is on a
   --  body of a package and Scope is its spec, the function will still return
   --  true.

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

   ---------------------------
   -- Construct_Cell_Access --
   ---------------------------

   type Construct_Cell_Access is private;
   --  This type is used to point to a particular node of a tree.

   Null_Construct_Cell_Access : constant Construct_Cell_Access;

   function To_Construct_Access
     (Tree : Construct_Tree; Iterator : Construct_Tree_Iterator)
      return Construct_Cell_Access;
   --  Creates a construct access

   function To_Construct_Tree_Iterator
     (Construct : Construct_Cell_Access) return Construct_Tree_Iterator;
   --  Returns the construct iterator store in this construct access

   function Get_Tree (Construct : Construct_Cell_Access) return Construct_Tree;
   --  Return the tree stored in this construct access

   --------------------------
   -- Composite_Identifier --
   --------------------------

   type Composite_Identifier (<>) is private;
   --  This type is used to store identifiers with multiple parts, e.g.
   --  A.B.

   type Composite_Identifier_Access is access all Composite_Identifier;

   procedure Free (This : in out Composite_Identifier_Access);
   --  Free the data associated to a composite identifier

   function Length (Id : Composite_Identifier) return Natural;
   --  Return the number of items composing this identifier

   function Get_Item
     (Id : Composite_Identifier; Number : Natural) return String;
   --  Return an item given its identifier

   function Prepend
     (Id         : Composite_Identifier;
      Word_Begin : Natural;
      Word_End   : Natural)
      return Composite_Identifier;
   --  Return a composite identifier based on the same string, but with the
   --  index of a new element before. This function is used to build an
   --  indentifier and should not be called afterwards.

   function To_Composite_Identifier
     (Identifier : String) return Composite_Identifier;
   --  Convert the given identifier to a composite identifier. This function
   --  will not necesseraly parse the whole string, but will stop as soon as
   --  the thing given is not an identifier.

   function To_String (Identifier : Composite_Identifier) return String;
   --  Return a string corresponding to the identifier given in parameter. This
   --  does not return blindly the string upon wich the identifier has been
   --  created but removes all the irrelevant caracters (typically, blanks
   --  charaters)

   function Get_Slice
     (Identifier : Composite_Identifier;
      From, To   : Natural)
      return Composite_Identifier;
   --  Return an identifier bases on the slice of items specified in
   --  parameterers.

   type Construct_Tree_Iterator_Array is array (Natural range <>) of
     Construct_Tree_Iterator;

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array;

   -------------------
   -- Tree_Language --
   -------------------

   type Tree_Language is abstract tagged private;
   --  This type represents the language of a given tree. It's used to provide
   --  various language-specific capabilities on a tree.

   type Tree_Language_Access is access all Tree_Language'Class;

   function Get_Language
     (Tree : access Tree_Language) return Language_Access is abstract;
   --  Return the language associated to this tree.

   type Get_Parent_Tree_Result is (Left, Right, None);

   function Get_Parent_Tree
     (Lang       : access Tree_Language;
      Left_Tree  : Construct_Tree;
      Right_Tree : Construct_Tree)
      return Get_Parent_Tree_Result is abstract;
   --  Return Left if Left if the parent of Right, Right if it's the parent
   --  of Left, None if no parent relationship can be estalished.

   function Get_Public_Tree
     (Lang      : access Tree_Language;
      Full_Tree : access Construct_Tree;
      Free_Tree : Boolean) return Construct_Tree is abstract;
   --  Return a tree containing only the public information of the tree given
   --  in parameter. If Free_Tree is True, then Full_Tree will be freed at
   --  the end of the process.

   function Get_Unit_Construct
     (Lang : access Tree_Language;
      Tree : Construct_Tree) return Construct_Tree_Iterator is abstract;
   --  Return the construct representing the unit of the file, if such a
   --  notion exists in the target language. Otherwise, return
   --  Null_Construct_Tree_Iterator.

   function Get_Unit_Name
     (Lang : access Tree_Language;
      Tree : Construct_Tree) return Composite_Identifier is abstract;
   --  Return the identifier representing the unit of the file, if such a
   --  notion exists in the target language.

   function Get_Name_Index
     (Lang      : access Tree_Language;
      Construct : Simple_Construct_Information) return String is abstract;
   --  Return the name that should be used to index the given construct. Takes
   --  care of e.g. case handling.

   function Compare_Entities
     (Lang                      : access Tree_Language;
      Left_Iter, Right_Iter     : Construct_Tree_Iterator;
      Left_Tree, Right_Tree     : Construct_Tree;
      Left_Buffer, Right_Buffer : String_Access) return General_Order
      is abstract;
   --  Return a the order relationship between the two cell. The definition of
   --  order is language dependant, and can be based on the name, categories
   --  or structure of the two cells. This function is aimed to be used
   --  efficiently by generic sort procedures. If two entities are
   --  "equivalent", they both refer to differents views of the same entity.

private

   type Construct_Tree_Node is record
      Construct              : Simple_Construct_Information;
      Sub_Nodes_Length       : Natural := 0;
      Previous_Sibling_Index : Natural := 0;
      Parent_Index           : Natural := 0;
   end record;

   type Node_Array is array (Natural range <>) of Construct_Tree_Node;

   type Construct_Tree_Record (Length : Natural) is record
      Contents : Node_Array (1 .. Length);

      Unit_Index : Integer := -1;
      Unit_Name : Composite_Identifier_Access := null;
      --  These two fields may be set by the implementations of
      --  Get_Unit_Construct.
   end record;

   type Construct_Tree is access all Construct_Tree_Record;

   type Construct_Tree_Iterator is record
      Node  : Construct_Tree_Node;
      Index : Natural;
   end record;

   Null_Construct_Tree_Node : constant Construct_Tree_Node :=
     (Null_Simple_Construct_Info, 0, 0, 0);

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator :=
     (Null_Construct_Tree_Node, 0);

   Null_Construct_Tree : constant Construct_Tree := null;

   Null_Construct_Tree_Record : constant Construct_Tree_Record :=
     (0,
      Contents    => (others => Null_Construct_Tree_Node),
      Unit_Index  => -1,
      Unit_Name   => null);

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array
     (1 .. 0) := (others => Null_Construct_Tree_Iterator);

   type Construct_Cell_Access is record
      Tree  : Construct_Tree;
      Index : Natural := 0;
   end record;

   Null_Construct_Cell_Access : constant Construct_Cell_Access := (null, 0);

   type Positions_Array is array (Natural range <>) of Integer;

   type Composite_Identifier
     (String_Length : Natural; Number_Of_Elements : Natural)
      is record
         Identifier     : String (1 .. String_Length);
         Position_Start : Positions_Array (1 .. Number_Of_Elements);
         Position_End   : Positions_Array (1 .. Number_Of_Elements);
      end record;

   type Tree_Language is abstract tagged null record;

   function Get_Last_Relevant_Construct
     (Tree   : Construct_Tree;
      Offset : Natural)
      return Construct_Tree_Iterator;
   --  Return the last construct representing the scope where the offset is.
   --  It can be either the last entity declared in the scope, or the scope
   --  itself.
   --
   --  Example:
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
