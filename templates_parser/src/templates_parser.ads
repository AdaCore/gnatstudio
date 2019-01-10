------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2019, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Strings.Unbounded;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Strings.Hash;

with Templates_Parser_Tasking;
pragma Elaborate_All (Templates_Parser_Tasking);
pragma Warnings (Off, Templates_Parser_Tasking);
--  This unit is not used in the spec but is placed here to force proper
--  finalization order.

package Templates_Parser is

   use Ada.Strings.Unbounded;

   Version : constant String := "11.7w";

   Template_Error : exception;

   Default_Begin_Tag : constant String := "@_";
   Default_End_Tag   : constant String := "_@";

   Default_Separator : constant String := ", ";

   procedure Set_Tag_Separators
     (Start_With : String := Default_Begin_Tag;
      Stop_With  : String := Default_End_Tag);
   --  Set the tag separators for the whole session. This should be changed as
   --  the very first API call and should not be changed after.

   function Tag_From_Name (Name : String) return String;
   --  Returns the tag given the Name, default is @_NAME_@

   -----------------
   -- Generic Tag --
   -----------------

   type Tag is private;
   --  A tag is using a by reference semantic

   function "+" (Value : String)           return Tag;
   function "+" (Value : Character)        return Tag;
   function "+" (Value : Boolean)          return Tag;
   function "+" (Value : Unbounded_String) return Tag;
   function "+" (Value : Integer)          return Tag;
   function "+" (Value : Tag)              return Tag;
   --  Tag constructors

   function "&" (T : Tag; Value : String)           return Tag;
   function "&" (T : Tag; Value : Character)        return Tag;
   function "&" (T : Tag; Value : Boolean)          return Tag;
   function "&" (T : Tag; Value : Unbounded_String) return Tag;
   function "&" (T : Tag; Value : Integer)          return Tag;
   function "&" (T : Tag; Value : Tag)              return Tag;
   --  Add Value at the end of the tag, note that "&" will modify its
   --  first parameter. It is intended to be used as [T := T & "val"],
   --  doing [T1 := T2 & "val"] will add val to T2 and set T1 as an
   --  alias. This is designed this way for efficiency.

   function "&" (Value : String;           T : Tag) return Tag;
   function "&" (Value : Character;        T : Tag) return Tag;
   function "&" (Value : Boolean;          T : Tag) return Tag;
   function "&" (Value : Unbounded_String; T : Tag) return Tag;
   function "&" (Value : Integer;          T : Tag) return Tag;
   --  Add Value at the front of the tag, see note above

   procedure Append (T : in out Tag; Value : String);
   procedure Append (T : in out Tag; Value : Character);
   procedure Append (T : in out Tag; Value : Boolean);
   procedure Append (T : in out Tag; Value : Unbounded_String);
   procedure Append (T : in out Tag; Value : Integer);
   procedure Append (T : in out Tag; Value : Tag);
   --  Add Value at the end of tag

   procedure Set_Separator (T : in out Tag; Separator : String);
   --  Set separator to be used when building a flat representation of
   --  a composite tag.

   procedure Clear (T : in out Tag);
   --  Removes all values in the tag. Current tag T is not released but
   --  the returned object is separated (not using the same reference) than
   --  the original one.

   function Size (T : Tag) return Natural;
   --  Returns the number of value into T

   function Item (T : Tag; N : Positive) return String;
   --  Returns the Nth Tag's item. Raises Constraint_Error if there is
   --  no such Item in T (i.e. T length < N).

   function Composite (T : Tag; N : Positive) return Tag;
   --  Returns the Nth Tag's item. Raises Constraint_Error if there is
   --  no such Item in T (i.e. T length < N).

   subtype Vector_Tag is Tag;
   subtype Matrix_Tag is Tag;

   ------------------
   -- Associations --
   ------------------

   type Association is private;

   Null_Association : constant Association;

   type Association_Kind is (Std, Composite);
   --  The kind of association which is either Std (a simple value), a vector
   --  tag or a Matrix tag.

   function Assoc
     (Variable : String;
      Value    : String) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Set. This is a standard association, value is a string.

   function Assoc
     (Variable : String;
      Value    : Unbounded_String) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Set. This is a standard association, value is an
   --  Unbounded_String.

   function Assoc
     (Variable : String;
      Value    : Integer) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Set. This is a standard association, value is an Integer.
   --  It will be displayed without leading space if positive.

   function Assoc
     (Variable : String;
      Value    : Boolean) return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Set. It set the variable to TRUE or FALSE depending on
   --  value.

   function Assoc
     (Variable  : String;
      Value     : Tag;
      Separator : String := Default_Separator) return Association;
   --  Build an Association (Variable = Value) to be added to Translate_Set.
   --  This is a tag association. Separator will be used when outputting the
   --  a flat representation of the Tag (outside a table statement).

   function Get (Assoc : Association) return Tag;
   --  Returns the Tag in Assoc, raise Constraint_Error if Assoc is not
   --  containing a Tag (Association_Kind is Std).
   --  See also the Templates_Parser.Query package for other functions to
   --  manipulate associations.

   function Get (Assoc : Association) return String;
   --  Returns the value in Assoc, raise Constraint_Error if Assoc is not
   --  containing a simple value (Association_Kind is Composite).
   --  See also the Templates_Parser.Query package for other functions to
   --  manipulate associations.

   ---------------------------
   -- Association table/set --
   ---------------------------

   type Translate_Table is array (Positive range <>) of Association;
   --  A table with a set of associations, note that it is better to use
   --  Translate_Set below as it is more efficient.

   No_Translation : constant Translate_Table;

   type Translate_Set is private;
   --  This is a set of association like Translate_Table but it is possible to
   --  insert item into this set more easily, furthermore there is no need to
   --  know the number of item before hand. This is the object used internally
   --  by the templates engine as it is far more efficient to retrieve a
   --  specific item from it.

   Null_Set : constant Translate_Set;

   procedure Insert (Set : in out Translate_Set; Item : Association);
   --  Add Item into the translate set. If an association for this variable
   --  already exists it just replaces it by the new item.

   procedure Insert (Set : in out Translate_Set; Items : Translate_Set);
   --  Add Items into the translate set. If an association for variables in
   --  Items already exists it just replaces it by the new one.

   function "&"
     (Left : Association; Right : Association) return Translate_Set;
   pragma Inline ("&");
   --  Returns new translate set created from 2 associations. If names of
   --  both associations are the same, the returned translate set will
   --  contain only Right.

   function "&"
     (Set : Translate_Set; Item : Association) return Translate_Set;
   pragma Inline ("&");
   --  Adds Item into Set. If an association with the same name already exists
   --  in Set it is replaced by the new one. Note that "&" will modify its
   --  first parameter. It is intended to be used as [T := T & Assoc],
   --  doing [T1 := T2 & Assoc] will add Assoc into T2 and set T1 as an
   --  alias. This is designed this way for efficiency.

   function "+" (Item : Association) return Translate_Set;
   pragma Inline ("+");
   --  Create translate set from one association

   procedure Remove (Set : in out Translate_Set; Name : String);
   --  Removes association named Name from the Set. Does nothing if there is
   --  not such association in the set.

   function Get (Set : Translate_Set; Name : String) return Association;
   --  Returns the association named Name in the Set. Returns Null_Association
   --  is no such association if found in Set.

   function Size (Set : Translate_Set) return Natural;
   --  Returns size of the translate set

   function Exists
     (Set : Translate_Set; Variable : String) return Boolean;
   --  Returns True if an association for Variable exists into the Set

   generic
      with procedure Action (Item : Association; Quit : in out Boolean);
   procedure For_Every_Association (Set : Translate_Set);
   --  Iterates through all associations in the set, call Action for each one.
   --  Set Quit to True to stop the iteration.

   function To_Set (Table : Translate_Table) return Translate_Set;
   --  Convert a Translate_Table into a Translate_Set

   -------------
   -- Dynamic --
   -------------

   package Dynamic is

      --------------
      -- Lazy_Tag --
      --------------

      type Lazy_Tag is abstract tagged private;
      type Lazy_Tag_Access is access all Lazy_Tag'Class;

      procedure Value
        (Lazy_Tag     : not null access Dynamic.Lazy_Tag;
         Var_Name     : String;
         Translations : in out Translate_Set) is abstract;
      --  Value is called by the Parse routines below if a tag variable was not
      --  found in the set of translations. This routine must then add the
      --  association for variable Name. It is possible to add other
      --  associations in the translation table but a check is done to see if
      --  the variable Name as been set or not. The default implementation does
      --  nothing.

      Null_Lazy_Tag : constant Lazy_Tag_Access;

      ----------------
      -- Cursor_Tag --
      ----------------

      type Cursor_Tag is abstract tagged private;
      type Cursor_Tag_Access is access all Cursor_Tag'Class;
      --  In some cases it is difficult and not efficient to have to map all
      --  Ada data into a template Tag. A Cursor_Tag object gives the ability
      --  to iterate through a data structure which is living on the Ada side
      --  only.

      function Dimension
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : String) return Natural is abstract;
      --  Must return the number of dimensions for the given variable name. For
      --  a matrix this routine should return 2 for example.

      type Path is array (Positive range <>) of Natural;
      --  A Path gives the full position of a given element in the cursor tag

      function Length
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : String;
         Path       : Dynamic.Path) return Natural is abstract;
      --  Must return the number of item for the given path. The first
      --  dimension is given by the Path (1), for the second column the Path is
      --  (1, 2). Note that each dimension can have a different length. For
      --  example a Matrix is not necessary square.

      function Value
        (Cursor_Tag : not null access Dynamic.Cursor_Tag;
         Var_Name   : String;
         Path       : Dynamic.Path) return String is abstract;
      --  Must return the value for the variable at the given Path. Note that
      --  this routine will be called only for valid items as given by the
      --  Dimension and Length above.

      Null_Cursor_Tag : constant Cursor_Tag_Access;

   private

      type Lazy_Tag is abstract tagged null record;

      Null_Lazy_Tag : constant Lazy_Tag_Access := null;

      type Cursor_Tag is abstract tagged null record;

      Null_Cursor_Tag : constant Cursor_Tag_Access := null;

   end Dynamic;

   package Dyn renames Dynamic;

   --------------------
   -- User's Filters --
   --------------------

   type Filter_Context is record
      Translations : Translate_Set;
      Lazy_Tag     : Dynamic.Lazy_Tag_Access;
   end record;

   type Callback is access function
     (Value      : String;
      Parameters : String;
      Context    : Filter_Context) return String;
   --  User's filter callback

   type Callback_No_Param is access function
     (Value   : String;
      Context : Filter_Context) return String;
   --  User's filter callback

   procedure Register_Filter
     (Name    : String;
      Handler : Callback);
   --  Register user's filter Name using the specified Handler

   procedure Register_Filter
     (Name    : String;
      Handler : Callback_No_Param);
   --  Register user's filter Name using the specified Handler

   type User_Filter is abstract tagged private;
   type User_Filter_Access is access all User_Filter'Class;
   function Execute
     (Filter     : not null access User_Filter;
      Value      : String;
      Parameters : String;
      Context    : Filter_Context) return String is abstract;
   --  User filters can also be implemented through a tagged type, which allows
   --  you to add your own user data and reuse a filter in several
   --  applications, perhaps with a slightly different behavior each time.
   --  It is possible for the callback to modify the data stored in Filter, but
   --  this needs to be done with care, since multiple concurrent calls to
   --  Callback might happen.

   procedure Register_Filter
     (Name   : String;
      Filter : not null access User_Filter'Class);
   --  Register a new filter. Filter must not be freed by the caller, since no
   --  copy is made.

   procedure Free_Filters;
   --  Free all user filters registered above. This is mostly intended when
   --  you are testing memory leaks in your application.

   -----------
   -- Macro --
   -----------

   type Parameter_Set is array (Natural range <>) of Unbounded_String;

   No_Parameter : constant Parameter_Set;

   type Macro_Callback is access
     function (Name : String; Parameters : Parameter_Set) return String;

   procedure Register_Macro_Handler (Callback : Macro_Callback);
   --  Use the given callbacks for every unknown macro in the template. The
   --  default implementation of this routine just displays the macro (name
   --  and parameters).

   -----------------------------
   -- Parsing and Translating --
   -----------------------------

   function Parse
     (Filename          : String;
      Translations      : Translate_Table       := No_Translation;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String;
   --  Parse the Template_File replacing variables' occurrences by the
   --  corresponding values. If Cached is set to True, Filename tree will be
   --  recorded into a cache for quick retrieval. If Keep_Unknown_Tags is set
   --  to True then tags that are not in the translate table are kept
   --  as-is if it is part of the template data. If this tags is part of a
   --  condition (in an IF statement tag), the condition will evaluate to
   --  False.

   function Parse
     (Filename          : String;
      Translations      : Translate_Table       := No_Translation;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String;
   --  Idem but returns an Unbounded_String

   function Parse
     (Filename          : String;
      Translations      : Translate_Set;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return String;
   --  Idem with a Translation_Set

   function Parse
     (Filename          : String;
      Translations      : Translate_Set;
      Cached            : Boolean               := False;
      Keep_Unknown_Tags : Boolean               := False;
      Lazy_Tag          : Dyn.Lazy_Tag_Access   := Dyn.Null_Lazy_Tag;
      Cursor_Tag        : Dyn.Cursor_Tag_Access := Dyn.Null_Cursor_Tag)
      return Unbounded_String;
   --  Idem with a Translation_Set

   function Translate
     (Template     : String;
      Translations : Translate_Table := No_Translation) return String;
   --  Just translate the discrete variables in the Template string using the
   --  Translations table. This function does not parse the command tag (TABLE,
   --  IF, INCLUDE). All composite tags are replaced by the empty string.

   function Translate
     (Template     : String;
      Translations : Translate_Set) return String;
   --  Idem with a Translation_Set

   procedure Release_Cache;
   --  Release the internal cache. This free the memory used for all currently
   --  loaded template trees.

private

   use Ada;
   type Integer_Access is access Integer;

   ------------------
   -- Generic Tags --
   ------------------

   type Node_Kind is (Value, Value_Set);

   type Tag_Access is access Tag;

   type Tag_Node (Kind : Node_Kind);
   type Tag_Node_Access is access Tag_Node;

   type Tag_Node (Kind : Node_Kind) is record
      Next : Tag_Node_Access;
      case Kind is
         when Value     => V  : Unbounded_String;
         when Value_Set => VS : Tag_Access;
      end case;
   end record;

   type Tag_Node_Arr is array (Positive range <>) of Tag_Node_Access;
   type Tag_Node_Arr_Access is access Tag_Node_Arr;

   package Tag_Values is new Containers.Indefinite_Hashed_Sets
     (String, Strings.Hash, "=");
   type Tag_Values_Access is access Tag_Values.Set;
   --  Map for all tag values to speed-up "in" operator

   type Tag_Data is record
      Count        : Natural;  -- Number of items
      Min, Max     : Natural;  -- Min/Max item's sizes, equal to 1 if leaf
      Nested_Level : Positive; -- Number of composite structures
      Separator    : Unbounded_String;
      Head         : Tag_Node_Access;
      Last         : Tag_Node_Access;
      Tag_Nodes    : Tag_Node_Arr_Access;
      --  This array will be setup during parsing to ensure fast iteration
      --  in reverse order.
      Values       : Tag_Values_Access;
   end record;

   type Tag_Data_Access is access Tag_Data;

   type Tag is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Data      : Tag_Data_Access;
   end record;

   overriding procedure Initialize (T : in out Tag);
   overriding procedure Finalize   (T : in out Tag);
   overriding procedure Adjust     (T : in out Tag);

   subtype Indices is Dynamic.Path;
   --  Set of indices that reference a specific item into a composite tag.
   --  Used by the parser.

   procedure Field
     (T      : Tag;
      N      : Positive;
      Result : out Tag;
      Found  : out Boolean);
   --  Returns the N'th item in Tag. Found is set to False is there is no
   --  such item.

   ------------------
   --  Association --
   ------------------

   type Association (Kind : Association_Kind := Std) is record
      Variable : Unbounded_String;

      case Kind is
         when Std =>
            Value : Unbounded_String;

         when Composite =>
            Comp_Value : Tag;
      end case;
   end record;

   Null_Association : constant Association :=
                        (Std, Null_Unbounded_String, Null_Unbounded_String);

   No_Translation : constant Translate_Table := (2 .. 1 => Null_Association);

   -----------
   -- Debug --
   -----------

   Expand_Macro : Boolean := False;
   --  If set to true the macro will be displayed inline instead of the named
   --  reference.

   procedure Print_Tree (Filename : String);
   --  Use for debugging purpose only, it will output the internal tree
   --  representation.

   procedure Print_Defined_Macros;
   --  Use for debugging purpose only, output all defined macros

   --------------------
   --  Translate_Set --
   --------------------

   package Association_Map is
     new Containers.Indefinite_Hashed_Maps
       (String, Association, Strings.Hash, "=", "=");

   type Map_Access is access Association_Map.Map;

   type Translate_Set is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Set       : Map_Access;
   end record;

   overriding procedure Initialize (Set : in out Translate_Set);
   overriding procedure Finalize   (Set : in out Translate_Set);
   overriding procedure Adjust     (Set : in out Translate_Set);

   Null_Set : constant Translate_Set :=
                (Ada.Finalization.Controlled with null, null);

   ------------------
   -- User filters --
   ------------------

   type User_Filter is abstract tagged null record;

   No_Parameter : constant Parameter_Set := (1 .. 0 => <>);

end Templates_Parser;
