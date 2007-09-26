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

--  This package gives a way of retreiving declarations from a location on a
--  file, based on the construct tree database

with Virtual_Lists;
with Virtual_Lists.Extensive;

with Language;                   use Language;
with Language.Tree;              use Language.Tree;
with Language.Tree.Database;     use Language.Tree.Database;

with Ada_Semantic_Tree.Expression_Parser;
use Ada_Semantic_Tree.Expression_Parser;
with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;

with Generic_Stack;

package Ada_Semantic_Tree.Declarations is

   --  Declaration Analysis  --

   type Visibility_Confidence is
     (Use_Visible,
      With_Visible,
      Project_Visible,
      Public_Library_Visible,
      Not_Visible);
   --  This type is used to describe the visibility level of an entity, from a
   --  location on the file.

   type Declaration_View is private;
   --  This type a view of a declaration, in a given context. It has
   --  information such as formal & actual parameters of the declaration.
   --  Instances of this type should be freed.

   function Get_Construct
     (Decl : Declaration_View) return access Simple_Construct_Information;
   --  Return the construct designated by this declaration view.

   function Get_File (Decl : Declaration_View) return Structured_File_Access;
   --  Return the file where the pointed declaration is located.

   function Is_All (Decl : Declaration_View) return Boolean;
   --  Return true if the declaration view given in parameter is viewed trough
   --  a "all" dereference.

   procedure Free (This : in out Declaration_View);
   --  Free the data created for this declaration view

   Null_Declaration_View : constant Declaration_View;

   type Declaration_List is private;
   --  A declaration list is an virtual list of declarations - contents
   --  partially calculated when the list is built, and partially when it's
   --  iterated over. This way, it's possible to cut the processing on too
   --  long results.

   Null_Declaration_List : constant Declaration_List;

   type Declaration_Iterator is private;
   --  This type is used to iterate over the contents of a declaration list.

   procedure Free (List : in out Declaration_List);
   --  Free the data associated to a list.

   procedure Free (It : in out Declaration_Iterator);
   --  Free the data associated to an iterator.

   function First (List : Declaration_List) return Declaration_Iterator;
   --  Return the first element found in the list.

   procedure Next (It : in out Declaration_Iterator);
   --  Moves the iterator over the next declaration of the list.

   function At_End (It : Declaration_Iterator) return Boolean;
   --  Return true if there is no more declaration to pick up - false
   --  otherwise.

   function Is_Valid (It : Declaration_Iterator) return Boolean;
   --  Return true if the iterator is in a regular state.

   function Get_View (It : Declaration_Iterator) return Declaration_View;
   --  Return the declaration view currently pointed by the iterator. The
   --  caller is responsible for freeing the result.

   function Get_Entity (It : Declaration_Iterator) return Entity_Access;
   --  Return the entity pointed by this declaration iterator.

   type Visibility_Filter is mod 2 ** 32;

   All_Visible_Packages : constant Visibility_Filter := 2#0000_0001#;
   --  Denotes only the packages that are already in the visible scope.
   All_Visible_Entities : constant Visibility_Filter :=
     2#0000_0010# or All_Visible_Packages;
   --  Denotes all the visible entities.
   All_Accessible_Units : constant Visibility_Filter := 2#0000_0100#;
   --  Denote only the units.
   All_Types            : constant Visibility_Filter :=
     2#0000_1000# or All_Accessible_Units;
   --  Denote any expression that can be interpreted as a type designation
   --  ??? This has to be used after a 'new' or ': [in|out|access]' or 'access'
   --  token (not yet used).
   Everything           : constant Visibility_Filter := 16#FFFFFF#
     and not All_Accessible_Units;
   --  Denotes everyting.

   type Visibility_Context is record
      File                      : Structured_File_Access;
      Offset                    : Natural;
      Filter                    : Visibility_Filter := Everything;
      Min_Visibility_Confidence : Visibility_Confidence;
   end record;
   --  This type gives a way to precise the file location from which a search
   --  has to be done, with the level of precision and the kind of entities
   --  needed.

   Null_Visibility_Context : constant Visibility_Context :=
     (null, 0, 0, Not_Visible);

   type Excluded_Stack_Type is private;
   --  This type holds a stack of entities excluded by an iteration process.
   --  Entities will get pushed and poped there in order to detect circular
   --  references and avoid infinite looping.

   Null_Excluded_Stack : constant Excluded_Stack_Type;

   procedure Pop_Entity (Stack : in out Excluded_Stack_Type);
   --  Removes the last entity from the stack.

   procedure Push_Entity
     (Stack : in out Excluded_Stack_Type; Entity : Entity_Access);
   --  Add a new entity at the top of the stack.

   function Is_Excluded
     (Stack : Excluded_Stack_Type; Entity : Entity_Access) return Boolean;
   --  Return true if this entity is excluded, false otherwise. Note that this
   --  function is aware that the same entity may be spread across several
   --  declarations - exclusion will be detected even if the stored and tested
   --  entities are the same declaration part.

   procedure Ref (Stack : in out Excluded_Stack_Type);
   --  Increments the reference counter of the stack - this has to be done if
   --  the stack is stored.

   procedure Unref (Stack : in out Excluded_Stack_Type);
   --  Decrement the reference counter of the stack.

   function Find_Declarations
     (File                      : Structured_File_Access;
      --  The file handle where the occurence is set.  This handle doesn't
      --  have to be up to date regarding the actuall buffer state, that's why
      --  the tree and the buffer is passed later on.

      Offset                    : Natural;
      --  The offset where the occurence is located, on the buffer

      From_Visibility           : Visibility_Context :=
        Null_Visibility_Context;
      --  The location from wich public / private / body visiblity has to be
      --  calculated. With / Use visiblity will be calculated from the
      --  File/Offset given in parameter. If no value is given, then
      --  File / Offset will be taken, and Library_Visible will be the
      --  required confidence.

      Expression                : Parsed_Expression := Null_Parsed_Expression;
      --  The expression of the occurence. If null, an expression will be
      --  analyzed from the offset given in parameter.

      Categories                : Category_Array := Null_Category_Array;
      --  A reduced set of categories lokked for. If there is any former
      --  knowledge, setting this variable might improve the search mechanism.
      --  In any case, the declarations mechanism will try to reduce the set
      --  of categories looked for.

      Is_Partial                : Boolean := False;
      --  If the expression is partial, then the last construct of the
      --  expression will be considered to be the prefix of the actual
      --  declaration looked for.

      Excluded_Entities         : Excluded_Stack_Type := Null_Excluded_Stack
      --  This holds a list of entities that can't be returned by the
      --  declaration procedure. It can be used by tools to break some
      --  circularities.
      --  The caller is responsible for freeing this. Once done, no more
      --  iteration nor accesses can be done to the list.
      ) return Declaration_List;
   --  Find the potential declarations for the occurence given in parameter.
   --  This procedure uses an ad hoc mechanism, and it can't be considered
   --  as 100% certain. On the other hand, it can work on non
   --  compiled/compilable files.
   --  The order of the elements put in the list is the following:
   --     - First all entities found in the local unit hierarchy, from the
   --       closest to the furthest
   --     - Then the ones found from the database, alphabetically ordered

   function Match_Declaration_With
     (Entity          : Entity_Access;
      File            : Structured_File_Access;
      Offset          : Natural;
      From_Visibility : Visibility_Context :=
        Null_Visibility_Context;
      Expression      : Parsed_Expression := Null_Parsed_Expression)
      return Visibility_Confidence;
   --  Check if the entity given in parameter is the one located at
   --  File / Offset, and return the level of confidence we can get if this is
   --  the case.

   function Get_Entity (It : Declaration_View) return Entity_Access;
   --  Return the entity pointed by this declaration view.

   function Get_Actual_Parameters
     (It : Declaration_View)
      return Actual_Parameter_Resolver_Access;
   --  If the instance of the declaration has been found with actual
   --  parameters, these parameters will be accessible trough this function.
   --  If not, then null will be returned.

   function Match
     (Seeked_Name, Tested_Name : String; Is_Partial : Boolean) return Boolean;
   --  Return true if Tested_Name matches Seeked_Name, possibly only partially
   --  (in which case Seeked_Name is the beginning of Tested_Name), false
   --  otherwise

   function To_Declaration (Entity : Entity_Access) return Declaration_View;
   --  Return the declaration view of the entity given in parameter.

private

   type Declaration_View is record
      Entity        : Entity_Access;
      Is_All        : Boolean := False;
      From_Prefixed : Boolean := False;
      Confidence    : Visibility_Confidence;
      Profile       : List_Profile_Access;
      Actuals       : Actual_Parameter_Resolver_Access := null;
   end record;

   package Declaration_List_Pckg is new Virtual_Lists (Declaration_View);

   package Declaration_List_Extensive_Pckg is new
     Declaration_List_Pckg.Extensive;

   use Declaration_List_Pckg;

   type Declaration_List is record
      Contents      : Declaration_List_Pckg.Virtual_List;
      Excluded_List : Excluded_Stack_Type;
   end record;

   type Declaration_Iterator is record
      It            : Declaration_List_Pckg.Virtual_List_Iterator;
      Excluded_List : Excluded_Stack_Type;
   end record;

   Null_Declaration_View : constant Declaration_View :=
     (Null_Entity_Access, False, False, Public_Library_Visible, null, null);

   package Excluded_Stack_Pckg is new Generic_Stack (Entity_Access);

   use Excluded_Stack_Pckg;

   type Excluded_Stack_Type_Record is record
      Entities : Excluded_Stack_Pckg.Simple_Stack;
      Refs     : Integer := 0;
   end record;

   type Excluded_Stack_Type is access all Excluded_Stack_Type_Record;

   Null_Excluded_Stack : constant Excluded_Stack_Type := null;

   Null_Declaration_List : constant Declaration_List :=
     (Declaration_List_Pckg.Null_Virtual_List, null);

end Ada_Semantic_Tree.Declarations;
