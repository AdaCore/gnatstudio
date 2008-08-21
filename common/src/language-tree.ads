-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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

with Annotations;

package Language.Tree is

   type Relative_Position is (Before, After, Specified, Enclosing);

   function Contains (Scope, Item : Construct_Access) return Boolean;
   --  Returns true if Item is contained by Scope, false otherwise.

   type Construct_Tree_Iterator is private;

   --------------------
   -- Construct_Tree --
   --------------------

   type Construct_Tree is private;

   Null_Construct_Tree : constant Construct_Tree;

   package Tree_Annotations_Pckg is new Annotations;

   package Construct_Annotations_Pckg is new Annotations;

   procedure Free (Tree : in out Construct_Tree);
   --  Free the data associated to a construct tree. Annotation have to be
   --  freed separately.

   procedure Free_Annotations (Tree : in out Construct_Tree);
   --  Free all the annotations associated to this construct tree.

   function To_Construct_Tree
     (List      : access Construct_List;
      Free_List : Boolean := False)
      return Construct_Tree;
   --  Return the construct tree corresponding to the construct list given in
   --  parameter. If Free_List is true, then the list given in parameter will
   --  be freed at the end of the process.

   function To_Construct_Tree
     (Buffer : String;
      Lang   : access Language_Root'Class)
      return Construct_Tree;
   --  Same as above

   function Get_Annotation_Container
     (Tree : Construct_Tree)
      return access Tree_Annotations_Pckg.Annotation_Container;
   --  Return the annotation container holding annotations for the given tree.

   -----------------------------
   -- Construct_Tree_Iterator --
   -----------------------------

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator;

   overriding function "="
     (Left, Right : Construct_Tree_Iterator) return Boolean;
   function "<" (Left, Right : Construct_Tree_Iterator) return Boolean;
   function ">" (Left, Right : Construct_Tree_Iterator) return Boolean;
   function "<=" (Left, Right : Construct_Tree_Iterator) return Boolean;
   function ">=" (Left, Right : Construct_Tree_Iterator) return Boolean;
   --  Provided that the two constructs are coming from the same tree, these
   --  function are doing positional comparaison between the iterators.
   --  The result of these functions is unexpectable if the two iterators are
   --  coming from different trees.

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

   function Is_Parent_Scope
     (Scope, It : Construct_Tree_Iterator) return Boolean;
   --  Return true if Scope is the direct parent scope of It.

   function Get_Construct
     (Iter : Construct_Tree_Iterator)
      return access Simple_Construct_Information;
   --  Return the construct pointed by the iterator given in parameter.
   --  The user should not free the fields of the returned
   --  Simple_Construct_Information.The lifetime of the returned
   --  object is the one of the corresponding tree. For longer storage, the
   --  construct and its fields should be copied.

   type Category_Array is array (Natural range <>) of Language_Category;
   Null_Category_Array : Category_Array (1 .. 0) := (others => Cat_Unknown);

   type Position_Type is (Start_Construct, Start_Name);

   type Text_Location (Absolute_Offset : Boolean) is record
      case Absolute_Offset is
         when True =>
            Offset : Integer;
         when False =>
            Line, Line_Offset : Natural;
      end case;
   end record;
   --  This type is used to make comparaisons to Source_Location when only a
   --  set "Line, Column" or "Offset" is available, but not both.

   function "=" (Left : Text_Location; Right : Source_Location) return Boolean;

   function "<" (Left : Text_Location; Right : Source_Location) return Boolean;

   function "<="
     (Left : Text_Location; Right : Source_Location) return Boolean;

   function ">"
     (Left : Text_Location; Right : Source_Location) return Boolean;

   function ">="
     (Left : Text_Location; Right : Source_Location) return Boolean;

   function To_Location (Offset : Integer) return Text_Location;
   --  Return a text location for the offset given in parameter.

   function To_Location (Line, Line_Offset : Natural) return Text_Location;
   --  Return a text location for the line/column given in parameter.

   function Get_Iterator_At
     (Tree              : Construct_Tree;
      Location          : Text_Location;
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
   --  Return the number of children in the construct pointed by this iterator

   type Scope_Navigation is (Jump_Over, Jump_Into);

   function Next
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator;
   --  Return the next element of the tree. If Scope_Policy is Jump_Over, then
   --  the next function will not enter in any nested block, and will jump
   --  directly to the next element in the current scope. If there is no next
   --  element, then this function will return Null_Construct_Tree_Iterator.

   function Prev
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator;
   --  Return the previous element of the tree.
   --  If Scope_Policy is Jump_Over, Prev will not enter in any nested block
   --  and will jump directly to the previous element in the current scope.
   --  If there is no previous element,  this function will return
   --  Null_Construct_Tree_Iterator.

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
     (Scope             : Construct_Tree_Iterator;
      Line, Line_Offset : Positive)
      return Boolean;
   function Encloses
     (Scope  : Construct_Tree_Iterator;
      Offset : Positive)
       return Boolean;
   --  Same as above, but doesn't need a "real" entity, only a location

   function Get_Full_Name
     (Tree : Construct_Tree; It : Construct_Tree_Iterator) return String;
   --  Return the name of the construct given in parameter, prefixed by all the
   --  relevant enclosing units.

   function Get_Annotation_Container
     (Tree : Construct_Tree; It : Construct_Tree_Iterator)
      return access Construct_Annotations_Pckg.Annotation_Container;
   --  Return the annotation container holding annotations for this iterator.
   --  ??? Is the tree really needed here?

   function Has_Same_Scope
     (Left, Right : Construct_Tree_Iterator) return Boolean;
   --  Return True if Left and Right are on the same scope.

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

   function Is_Prefix_Of
     (Potential_Prefix, Full_Id : Composite_Identifier;
      Case_Sensitive            : Boolean)
      return Boolean;
   --  Return true if Prefix is a prefix of Full_Path. This checks categories
   --  & name.

   function Equal
     (Left, Right : Composite_Identifier; Case_Sensitive : Boolean)
     return Boolean;
   --  Return true if the two identifiers are equals, according to the case
   --  sensitivity.

   type Construct_Tree_Iterator_Array is array (Natural range <>) of
     Construct_Tree_Iterator;

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array;

   function Full_Construct_Path
     (Tree         : Construct_Tree;
      Construct_It : Construct_Tree_Iterator)
      return Construct_Tree_Iterator_Array;
   --  Return an array containing all the parents of the construct cell given
   --  in parameter.

   function Full_Construct_Path
     (Tree : Construct_Tree;
      Offset : Natural) return Construct_Tree_Iterator_Array;
   --  Return an array containing all the parents around the offset given in

   -----------------
   -- Identifiers --
   -----------------

   type Distinct_Identifier is access all String;
   --  A distinct identifier is publicly implemented as a access string. The
   --  identifier manager is responsible for ensuring that for a given string,
   --  only one access value is possible, taking into account. For example,
   --  two distinct names "Bla" and "BLA", if the language is case insensitive,
   --  should both be associated to a single string on the heap holding, e.g.
   --  "bla".

   Null_Distinct_Identifier : constant Distinct_Identifier := new String'("");

   --  ??? This could be an Ada 2005 interface
   type Identifier_Manager is abstract tagged null record;
   --  This manager is responsible for carrying unique values for identifiers.
   --  See the declaration of Distinct_Identifier for more details.

   function Get_Identifier
     (Manager : access Identifier_Manager; Name : String)
      return Distinct_Identifier is abstract;
   --  Return the unique identifier corresponding to this string.

   type Referenced_Identifiers_List is private;
   --  This is a list of referenced datas.

   Null_Referenced_Identifiers_List : constant Referenced_Identifiers_List;

   overriding function "="
     (Left, Right : Referenced_Identifiers_List) return Boolean;
   --  Return true if all the elements of the two lists are the same, and if
   --  the two lists contains the same number of elements, false otherwise.

   function "="
     (Left : Referenced_Identifiers_List; Right : Distinct_Identifier)
      return Boolean;
   --  Return true if Left contains only one element, equals to Right.

   function "="
     (Left : Distinct_Identifier; Right : Referenced_Identifiers_List)
      return Boolean;
   --  Return true if Right contains only one element, equals to Left.

   function Get_Identifier
     (It : Construct_Tree_Iterator) return Distinct_Identifier;
   --  Return the identifier stored at this iterator location.

   function Get_Referenced_Identifiers
     (It : Construct_Tree_Iterator) return Referenced_Identifiers_List;
   --  Return the referenced data list pointed by this iterator.

   function Get_Next_Referenced_Identifiers
     (Ref : Referenced_Identifiers_List) return Referenced_Identifiers_List;
   --  Return the list of referenced identifiers starting at the next element
   --  of the one given in parameter.

   function Get_Identifier
     (Ref : Referenced_Identifiers_List) return Distinct_Identifier;
   --  Return the first distinct identifier of the list.

   procedure Analyze_Constructs_Identifiers
     (Manager : access Identifier_Manager'Class;
      Tree    : Construct_Tree);
   --  Initialize the indentifier information of the contents of the tree.
   --  Get_Identifier needs to have this function called before.

   procedure Analyze_Referenced_Identifiers
     (Buffer  : String;
      Lang    : access Language_Root'Class;
      Manager : access Identifier_Manager'Class;
      Tree    : Construct_Tree);
   --  Initialize the referenced indentifier information of the contents of the
   --  tree. Get_Referenced_Identifiers needs to have this function called
   --  before.

   function Match
     (Seeked_Name, Tested_Name : Distinct_Identifier;
      Seeked_Is_Partial : Boolean)
      return Boolean;
   --  If Seeked_Is_Partial is false, return true if Seeked_Name equals
   --  Tested_Name. Otherwise, return true if Seeked_Name is a prefix of
   --  Tested_Name

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

   function Get_Name_Index
     (Lang      : access Tree_Language;
      Construct : Simple_Construct_Information) return String;
   --  Return the name that should be used to index the given construct. Takes
   --  care of e.g. case handling. Default implementation return the actual
   --  construct name.

   function Get_Documentation
     (Lang   : access Tree_Language;
      Buffer : String;
      Tree   : Construct_Tree;
      Node   : Construct_Tree_Iterator) return String;
   --  This function returns the documentation for the entity given in
   --  parameter, for the given language. By default, it computes a
   --  documentation from generic knowledge on the constructs.
   --  Language-specific computation may give more accurate information.

   type Diff_Kind is (Removed, Added, Preserved);

   type Diff_Callback is access procedure
     (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

   procedure Diff
     (Lang               : access Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback);
   --  Calls the callback on each construct, showing if it's an added, modified
   --  or unmodified construct. The default implementation calls remove on all
   --  the contents of the old tree, and add to all the contents of the new
   --  tree. The implementer may use the referenced data stored in the
   --  constructs nodes.

   type Unknown_Tree_Language is new Tree_Language with private;

   overriding function Get_Language
     (Tree : access Unknown_Tree_Language) return Language_Access;
   --  See inherited documentation

   Unknown_Tree_Lang : constant Tree_Language_Access;

private

   type Referenced_Identifiers_List_Record;

   type Access_Referenced_List is access all
     Referenced_Identifiers_List_Record;

   type Referenced_Identifiers_List is record
      Contents : Access_Referenced_List;
   end record;

   Null_Referenced_Identifiers_List : constant Referenced_Identifiers_List :=
     (Contents => null);

   type Referenced_Identifiers_List_Record is record
      Element : Distinct_Identifier;
      Next    : Referenced_Identifiers_List;
   end record;

   type Construct_Tree_Node is record
      Construct              : aliased Simple_Construct_Information;
      Sub_Nodes_Length       : Natural := 0;
      Previous_Sibling_Index : Natural := 0;
      Parent_Index           : Natural := 0;
      Annotations            : aliased
        Construct_Annotations_Pckg.Annotation_Container;
      Id                     : Distinct_Identifier;
      Referenced_Ids         : Referenced_Identifiers_List;
   end record;

   type Node_Array is array (Natural range <>) of aliased Construct_Tree_Node;

   type Construct_Tree_Record (Length : Natural) is record
      Contents     : Node_Array (1 .. Length);

      Annotations : aliased Tree_Annotations_Pckg.Annotation_Container;
   end record;

   type Construct_Tree is access all Construct_Tree_Record;

   Null_Construct_Tree_Node : aliased Construct_Tree_Node :=
     (Null_Simple_Construct_Info, 0, 0, 0,
      Construct_Annotations_Pckg.Null_Annotation_Container, null,
      (Contents => null));

   type Construct_Tree_Iterator is record
      Node  : access Construct_Tree_Node := Null_Construct_Tree_Node'Access;
      Index : Natural := 0;
   end record;

   type Construct_Index is new Natural;

   Null_Construct_Tree_Iterator : constant Construct_Tree_Iterator :=
     (Null_Construct_Tree_Node'Access, 0);

   Null_Construct_Tree : constant Construct_Tree := null;

   Null_Construct_Tree_Record : constant Construct_Tree_Record :=
     (0,
      Contents     => (others => Null_Construct_Tree_Node),
      Annotations  => Tree_Annotations_Pckg.Null_Annotation_Container);

   Null_Construct_Tree_Iterator_Array : constant Construct_Tree_Iterator_Array
     (1 .. 0) := (others => Null_Construct_Tree_Iterator);

   Null_Construct_Index : constant Construct_Index := 0;

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

   type Unknown_Tree_Language is new Tree_Language with null record;

   Unknown_Tree_Lang : constant Tree_Language_Access :=
     new Unknown_Tree_Language;

end Language.Tree;
