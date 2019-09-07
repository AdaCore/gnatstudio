------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

--  This package gives a way of retreiving declarations from a location on a
--  file, based on the construct tree database

with Ada_Semantic_Tree.List_Resolver; use Ada_Semantic_Tree.List_Resolver;
with Ada_Semantic_Tree.Generics;      use Ada_Semantic_Tree.Generics;

package Ada_Semantic_Tree.Declarations is

   type Search_Context_Type is (From_Database, From_File);

   type Search_Context (Context_Type : Search_Context_Type) is record
      Generic_Context : Instance_Info :=
        Null_Instance_Info;

      case Context_Type is
         when From_Database =>
            Db : Construct_Database_Access;
            --  The database where to perform the search

         when From_File =>
            File   : Structured_File_Access;
            --  The file handle where the occurence is set.

            Offset : String_Index_Type;
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

      Analyzed_Expressions      : Expressions_List.List :=
                                    Expressions_List.Empty_List;
      --  The list of expressions already analyzed.

      Expression                : Parsed_Expression := Null_Parsed_Expression;
      --  The expression of the occurence. If null, an expression will be
      --  analyzed from the offset given in parameter by the context.

      Filter                    : Entity_Filter := Null_Filter;
      --  A filter to avoid returning certain entities. If there is any former
      --  knowledge, setting this variable might improve the search mechanism.
      --  In any case, the declarations mechanism will try to reduce the set
      --  of categories looked for. Note that this list may not be taken into
      --  account as-is in all cases, certain constructions imply certain
      --  category filters - e.g. use clauses will imply packages.

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
      Offset          : String_Index_Type;
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

   function To_Declaration
     (Entity : Entity_Access; Is_Accessible : Boolean := True)
      return Entity_View;
   --  Return the declaration view of the entity given in parameter.
   --  Is_Accessible is an optional parameter specifying if the view
   --  should be seen as accessible in the context of the search

   -----------------------------
   -- Declaration_View_Record --
   -----------------------------

   type Declaration_View_Record is new Entity_View_Record with record
      Profile         : List_Profile_Access;
      Actuals         : Actual_Parameter_Resolver_Access := null;
      Generic_Context : Instance_Info;
      Is_Accessible   : Boolean := True;
   end record;

   overriding function Get_Name
     (E : access Declaration_View_Record) return UTF8_String;

   overriding function Is_Accessible
     (E : access Declaration_View_Record) return Boolean;

   overriding procedure Fill_Children
     (E                   : access Declaration_View_Record;
      From_Visibility     : Visibility_Context;
      Name                : String;
      Is_Partial          : Boolean;
      Filter              : Entity_Filter;
      Ignored_Expressions : Expressions_List.List;
      Result              : in out Entity_List);

private

   overriding procedure Free (This : in out Declaration_View_Record);

   overriding procedure Deep_Copy (This : in out Declaration_View_Record);

   overriding procedure Configure_View
     (E : in out Declaration_View_Record; It : Entity_Iterator);

end Ada_Semantic_Tree.Declarations;
