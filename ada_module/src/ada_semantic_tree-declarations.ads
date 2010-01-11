-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007-2010, AdaCore             --
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

with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;

package Ada_Semantic_Tree.Declarations is

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

   type Search_Context_Type is (From_Database, From_File);

   type Search_Context (Context_Type : Search_Context_Type) is record
      case Context_Type is
         when From_Database =>
            Db : Construct_Database_Access;
            --  The database where to perform the search

         when From_File =>
            File   : Structured_File_Access;
            --  The file handle where the occurence is set.

            Offset : Natural;
            --  The offset where the occurence is located, on the buffer passed
            --  in File.
      end case;
   end record;

   function Find_Declarations
     (Context                   : Search_Context;
      --  The context of the search, either database wide or from a file.

      From_Visibility           : Visibility_Context :=
        Null_Visibility_Context;
      --  The location from wich public / private / body visiblity has to be
      --  calculated. With / Use visiblity will be calculated from the
      --  File/Offset given in parameter from the context. If no value is
      --  given, then File / Offset will be taken, and Library_Visible will be
      --  the required confidence.

      Expression                : Parsed_Expression := Null_Parsed_Expression;
      --  The expression of the occurence. If null, an expression will be
      --  analyzed from the offset given in parameter by the context.

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
      ) return Entity_List;
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

   function Get_Actual_Parameters
     (It : Entity_View)
      return Actual_Parameter_Resolver_Access;
   --  If the instance of the declaration has been found with actual
   --  parameters, these parameters will be accessible trough this function.
   --  If not, then null will be returned.

   function Match
     (Seeked_Name, Tested_Name : String; Is_Partial : Boolean) return Boolean;
   --  Return true if Tested_Name matches Seeked_Name, possibly only partially
   --  (in which case Seeked_Name is the beginning of Tested_Name), false
   --  otherwise

   function To_Declaration (Entity : Entity_Access) return Entity_View;
   --  Return the declaration view of the entity given in parameter.

end Ada_Semantic_Tree.Declarations;
