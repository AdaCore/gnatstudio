------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Annotations;
with Ada.Unchecked_Deallocation;

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
     (File   : GNATCOLL.VFS.Virtual_File;
      Buffer : String;
      Lang   : access Language_Root'Class)
      return Construct_Tree;
   --  Same as above

   function Get_Annotation_Container
     (Tree : Construct_Tree)
      return access Tree_Annotations_Pckg.Annotation_Container;
   pragma Inline (Get_Annotation_Container);
   --  Return the annotation container holding annotations for the given tree.

   -----------------------------
   -- Construct_Tree_Iterator --
   -----------------------------

   Null_Construct_Tree_Iterator : aliased constant Construct_Tree_Iterator;

   overriding function "="
     (Left, Right : Construct_Tree_Iterator) return Boolean;
   pragma Inline ("=");
   function "<" (Left, Right : Construct_Tree_Iterator) return Boolean;
   pragma Inline ("<");
   function ">" (Left, Right : Construct_Tree_Iterator) return Boolean;
   pragma Inline (">");
   function "<=" (Left, Right : Construct_Tree_Iterator) return Boolean;
   pragma Inline ("<=");
   function ">=" (Left, Right : Construct_Tree_Iterator) return Boolean;
   pragma Inline (">=");
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
   pragma Inline (Is_Parent_Scope);
   --  Return true if Scope is the direct parent scope of It.

   function Get_Construct
     (Iter : Construct_Tree_Iterator)
      return access Simple_Construct_Information;
   pragma Inline (Get_Construct);
   --  Return the construct pointed by the iterator given in parameter.
   --  The user should not free the fields of the returned
   --  Simple_Construct_Information. The lifetime of the returned
   --  object is the one of the corresponding tree. For longer storage, the
   --  construct and its fields should be copied.

   type Category_Array is array (Natural range <>) of Language_Category;
   Null_Category_Array : aliased constant
      Category_Array (1 .. 0) := (others => Cat_Unknown);

   Categories_For_Block_Highlighting : constant Category_Array :=
     (Cat_Package,            -- Enclosing Entities
      Cat_Namespace,
      Cat_Task,
      Cat_Procedure,
      Cat_Function,
      Cat_Method,
      Cat_Constructor,
      Cat_Destructor,
      Cat_Protected,
      Cat_Entry,
      Cat_Class,              --  Data/Types structures
      Cat_Structure,
      Cat_Case_Inside_Record,
      Cat_Union,
      Cat_Loop_Statement,     --  Constructs
      Cat_If_Statement,
      Cat_Case_Statement,
      Cat_Select_Statement,
      Cat_Declare_Block,
      Cat_Return_Block,
      Cat_Simple_Block,
      Cat_Exception_Handler,  --  Sub-constructs
      Cat_Pragma,
      Cat_Aspect,
      Cat_Custom);            --  Custom construct
   --  The categories to consider for block highlighting

   type Category_Array_Access is access all Category_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Category_Array, Category_Array_Access);

   function Is_In
     (Cat : Language_Category; Categories : Category_Array) return Boolean;
   pragma Inline (Is_In);

   function Is_In_Category
     (Construct : Simple_Construct_Information; Categories : Category_Array)
      return Boolean;
   pragma Inline (Is_In_Category);
   --  Return true if the construct category matches one of the category listed
   --  in the array. If the array is empty, then will always return True (means
   --  that we don't care about the category).

   type Position_Type is (Start_Construct, Start_Name);

   type Text_Location (Absolute_Offset : Boolean) is record
      case Absolute_Offset is
         when True =>
            Offset      : String_Index_Type;
         when False =>
            Line        : Natural;
            Line_Offset : String_Index_Type;
      end case;
   end record;
   --  This type is used to make comparaisons to Source_Location when only a
   --  set "Line, Column" or "Offset" is available, but not both.

   function "=" (Left : Text_Location; Right : Source_Location) return Boolean;
   pragma Inline ("=");

   function "<" (Left : Text_Location; Right : Source_Location) return Boolean;
   pragma Inline ("<");

   function "<="
     (Left : Text_Location; Right : Source_Location) return Boolean;
   pragma Inline ("<=");

   function ">"
     (Left : Text_Location; Right : Source_Location) return Boolean;
   pragma Inline (">");

   function ">="
     (Left : Text_Location; Right : Source_Location) return Boolean;
   pragma Inline (">=");

   function To_Location (Offset : String_Index_Type) return Text_Location;
   --  Return a text location for the offset given in parameter.

   function To_Location
     (Line        : Natural;
      Line_Offset : String_Index_Type)
      return Text_Location;
   --  Return a text location for the line/column given in parameter.

   function To_Location (Loc : Source_Location) return Text_Location;
   --  Converts a source location to a text location

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
   pragma Inline (Next);
   --  Return the next element of the tree. If Scope_Policy is Jump_Over, then
   --  the next function will not enter in any nested block, and will jump
   --  directly to the next element in the current scope. If there is no next
   --  element, then this function will return Null_Construct_Tree_Iterator.

   function Prev
     (Tree         : Construct_Tree;
      Iter         : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation := Jump_Into)
      return Construct_Tree_Iterator;
   pragma Inline (Prev);
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
      Offset : String_Index_Type)
       return Boolean;
   --  Same as above, but doesn't need a "real" entity, only a location

   function Get_Full_Name
     (Tree : Construct_Tree; It : Construct_Tree_Iterator) return String;
   --  Return the name of the construct given in parameter, prefixed by all the
   --  relevant enclosing units.

   function Get_Annotation_Container
     (Tree : Construct_Tree; It : Construct_Tree_Iterator)
      return access Construct_Annotations_Pckg.Annotation_Container;
   pragma Inline (Get_Annotation_Container);
   --  Return the annotation container holding annotations for this iterator.
   --  ??? Is the tree really needed here?

   function Has_Same_Scope
     (Left, Right : Construct_Tree_Iterator) return Boolean;
   --  Return True if Left and Right are on the same scope.

   function Get_Index (It : Construct_Tree_Iterator) return Integer;
   --  Return the index of this iterator in the list of ordered constructs of
   --  the tree, 0 if null iterator.

   function Get_Parent_Index (It : Construct_Tree_Iterator) return Integer;
   pragma Inline (Get_Parent_Index);
   --  Return the index of the parent iterator in the list of ordered
   --  constructs of the tree, 0 if null iterator.

   function To_String (It : Construct_Tree_Iterator) return String;
   --  Return a string version of the iterator, for debugging purposes.

   --------------------------
   -- Composite_Identifier --
   --------------------------

   type Composite_Identifier (<>) is private;
   --  This type is used to store identifiers with multiple parts, e.g.
   --  A.B.

   Null_Composite_Identifier : constant Composite_Identifier;

   type Composite_Identifier_Access is access all Composite_Identifier;

   procedure Free (This : in out Composite_Identifier_Access);
   --  Free the data associated to a composite identifier

   function Length (Id : Composite_Identifier) return Natural;
   pragma Inline (Length);
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
     (Tree   : Construct_Tree;
      Offset : String_Index_Type) return Construct_Tree_Iterator_Array;
   --  Return an array containing all the parents around the offset given in

   -----------------
   -- Identifiers --
   -----------------

   type Normalized_Symbol is new GNATCOLL.Symbols.Symbol;
   No_Normalized_Symbol : constant Normalized_Symbol :=
     Normalized_Symbol (GNATCOLL.Symbols.No_Symbol);
   --  A symbol that has been converted to lower case when it applies to a
   --  case insensitive language. Showing this concept at the type level shows
   --  whether the entity can be printed as is (a Symbol) or not (a
   --  Normalized_Symbol).

   function Find_Normalized
     (Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class;
      Name    : String) return Normalized_Symbol;
   --  Normalizes Name.
   --  ??? For now, this is specific to Ada and always converts to lower-case.
   --  It will not work for C, and should really receive a Language parameter
   --  in addition.

   type Referenced_Identifiers_List is private;
   --  This is a list of referenced datas.

   Null_Referenced_Identifiers_List : constant Referenced_Identifiers_List;

   overriding function "="
     (Left, Right : Referenced_Identifiers_List) return Boolean;
   --  Return true if all the elements of the two lists are the same, and if
   --  the two lists contains the same number of elements, false otherwise.

   function "="
     (Left : Referenced_Identifiers_List; Right : Normalized_Symbol)
      return Boolean;
   --  Return true if Left contains only one element, equals to Right.

   function "="
     (Left : Normalized_Symbol; Right : Referenced_Identifiers_List)
      return Boolean;
   --  Return true if Right contains only one element, equals to Left.

   function Get_Identifier
     (It : Construct_Tree_Iterator) return Normalized_Symbol;
   --  Return the identifier stored at this iterator location.

   function Get_Referenced_Identifiers
     (It : Construct_Tree_Iterator) return Referenced_Identifiers_List;
   --  Return the referenced data list pointed by this iterator.

   function Get_Next_Referenced_Identifiers
     (Ref : Referenced_Identifiers_List) return Referenced_Identifiers_List;
   --  Return the list of referenced identifiers starting at the next element
   --  of the one given in parameter.

   function Get_Identifier
     (Ref : Referenced_Identifiers_List) return Normalized_Symbol;
   --  Return the first distinct identifier of the list.

   procedure Analyze_Constructs_Identifiers
     (Lang : access Language_Root'Class;
      Tree : Construct_Tree);
   --  Initialize the indentifier information of the contents of the tree.
   --  Get_Identifier needs to have this function called before.

   procedure Analyze_Referenced_Identifiers
     (Buffer  : String;
      Lang    : access Language_Root'Class;
      Tree    : Construct_Tree);
   --  Initialize the referenced indentifier information of the contents of the
   --  tree. Get_Referenced_Identifiers needs to have this function called
   --  before.

   function Match
     (Seeked_Name, Tested_Name : Normalized_Symbol;
      Seeked_Is_Partial : Boolean)
      return Boolean;
   --  If Seeked_Is_Partial is false, return true if Seeked_Name equals
   --  Tested_Name. Otherwise, return true if Seeked_Name is a prefix of
   --  Tested_Name

   ----------------------------
   -- Abstract_Tree_Language --
   ----------------------------

   type Abstract_Tree_Language is abstract tagged null record;
   --  This type contains basic capabilities to be implemented by a tree
   --  languages. More advanced capabilities are declared in the database
   --  package.

   function Get_Name_Index
     (Lang      : access Abstract_Tree_Language;
      Construct : Simple_Construct_Information)
      return GNATCOLL.Symbols.Symbol is abstract;
   --  Return the name that should be used to index the given construct. Takes
   --  care of e.g. case handling. Default implementation return the actual
   --  construct name.

   type Diff_Kind is (Removed, Added, Preserved);

   type Diff_Callback is access procedure
     (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

   procedure Diff
     (Lang               : access Abstract_Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback) is abstract;
   --  Calls the callback on each construct, showing if it's an added, modified
   --  or unmodified construct. The default implementation calls remove on all
   --  the contents of the old tree, and add to all the contents of the new
   --  tree. The implementer may use the referenced data stored in the
   --  constructs nodes.

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
      Element : Normalized_Symbol;
      Next    : Referenced_Identifiers_List;
   end record;

   type Construct_Tree_Node is record
      Construct              : aliased Simple_Construct_Information;
      Sub_Nodes_Length       : Natural := 0;
      Previous_Sibling_Index : Natural := 0;
      Parent_Index           : Natural := 0;
      Annotations            : aliased
        Construct_Annotations_Pckg.Annotation_Container;
      Id                     : Normalized_Symbol;
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
      Construct_Annotations_Pckg.Null_Annotation_Container,
      No_Normalized_Symbol,
      (Contents => null));

   type Construct_Tree_Iterator is record
      Node  : access Construct_Tree_Node := Null_Construct_Tree_Node'Access;
      Index : Natural := 0;
   end record;

   type Construct_Index is new Natural;

   Null_Construct_Tree_Iterator : aliased constant Construct_Tree_Iterator :=
     (Null_Construct_Tree_Node'Access, 0);

   Null_Construct_Tree : constant Construct_Tree := null;

   Null_Construct_Tree_Record : constant Construct_Tree_Record :=
     (0,
      Contents    => (others => Null_Construct_Tree_Node),
      Annotations => Tree_Annotations_Pckg.Null_Annotation_Container);

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

   Null_Composite_Identifier : constant Composite_Identifier :=
     (0, 0, "", (others => 0), (others => 0));

end Language.Tree;
