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

--  This package provides containers & process used to analyze formal / actual
--  list of entities, in particular parameters.

with Language;                  use Language;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Ada_Semantic_Tree.Expression_Parser;
use Ada_Semantic_Tree.Expression_Parser;

package Ada_Semantic_Tree.List_Resolver is

   --  Formal parameters --

   type Formal_Parameter is private;
   --  This type is used to represent a formal parameter of a subprogram

   function Get_Construct_Tree_Iterator
     (Param : Formal_Parameter) return Construct_Tree_Iterator;
   --  Return the construct iterator corresponding to this formal parameter

   function Get_Construct
     (Param : Formal_Parameter) return access Simple_Construct_Information;
   --  Return the construct pointed by this formal paremeter

   function Get_Type (Param : Formal_Parameter) return Entity_Access;
   --  This computes the type of the parameter. The first time this function is
   --  called on a given formal, it involves an expensive declaration
   --  resolution. The result is then cached.

   type Formal_Parameter_Array is array (Integer range <>) of Formal_Parameter;

   type List_Profile (<>) is private;
   --  This type holds information about a formal list profile - for example
   --  the formal profile of a subprogram.

   type List_Profile_Access is access all List_Profile;

   procedure Free (This : in out List_Profile_Access);
   --  Free the data associated to a List_Profile_Access

   function Get_List_Profile (Entity : Entity_Access) return List_Profile;
   --  Create and return a list profile out of an entity. If no list profile
   --  can be found, then a special profile with no constraints will be
   --  returned. This is different from an empty profile (which doesn't allow
   --  any actual parameter).

   function Get_Entity (Profile : List_Profile) return Entity_Access;
   --  Return the entity holding this profile.

   function Get_Number_Of_Formals (Profile : List_Profile) return Integer;
   --  Return the total number of formals declared in this profile.

   --  Actual parameters --

   type Actual_Parameter is private;
   --  This type denotes an actual parameter. This is used to check that actual
   --  parameters actually match a formal profile.

   procedure Free (This : in out Actual_Parameter);
   --  Free the data associated to this actual parameter.

   Null_Actual_Parameter : constant Actual_Parameter;

   function Get_Actual_Parameter
     (Buffer      : UTF8_String_Access;
      Param_Start : Natural;
      Param_End   : Natural) return Actual_Parameter;
   --  Created an actual parameter out of a piece of code - param start and
   --  param end has to be set around the entier parameter, including the
   --  name preceded by the arrow if any.

   type Actual_Parameter_Resolver (<>) is private;
   --  This type is the master of the actual formal / actual parameter checks.

   type Actual_Parameter_Resolver_Access is
     access all Actual_Parameter_Resolver;

   procedure Free (This : in out Actual_Parameter_Resolver_Access);
   --  Free the data associated to This.

   function Get_Actual_Parameter_Resolver
     (Profile : List_Profile) return Actual_Parameter_Resolver;
   --  Create a parameter resolver matching the profile given in parameter.

   procedure Append_Actual
     (Params      : in out Actual_Parameter_Resolver;
      Actual      : Actual_Parameter;
      Do_Semantic : Boolean;
      Param_Added : out Boolean;
      Success     : out Boolean);
   --  Tries to add a parameter to the actual parameter resolver. If Success
   --  is true, then the target profile is still valid. Eventhough Success is
   --  true, the parameter might not be added (because we do not do any kind
   --  of parameter analysis for this kind for example). In this case, the
   --  parameter should be freed by the caller. Otherwise, it will get freed
   --  when the resolver is freed.

   function Is_Complete (Params : Actual_Parameter_Resolver) return Boolean;
   --  Return true if all the parameters without default values have been
   --  given a value by the actual list.

   function Get_Number_Of_Parameters
     (Params : Actual_Parameter_Resolver) return Integer;
   --  Return the number of actual parameters set in this resolver.

   function Get_Missing_Formals
     (Params : Actual_Parameter_Resolver)
      return Formal_Parameter_Array;
   --  Return all the formal parameters that are not set with an actual yet.

   function Any_Named_Formal_Missing
     (Params : Actual_Parameter_Resolver) return Boolean;
   --  Return true if there is at least one formal parameter waiting for its
   --  actual. This formal has to have a name.

private

   type Formal_Parameter is new Entity_Access;

   type List_Profile (Nb_Params : Integer) is record
      Params : Entity_Array (1 .. Nb_Params);

      Extra_Params_Allowed : Boolean := False;
      --  For certain kinds of entities (e.g. arrays) we're allowing more
      --  params than expected (since we currently don't analyze array
      --  dimensions). This is a workaround - the implementation will be
      --  more precise in the future.
   end record;

   type Actual_Parameter is record
      Expression : Parsed_Expression;
      Is_Named   : Boolean;
   end record;

   Null_Actual_Parameter : constant Actual_Parameter :=
     (Null_Parsed_Expression, False);

   type Actual_Params_Array is array (Integer range <>) of Actual_Parameter;

   type Actual_Parameter_Resolver (Length : Integer) is record
      Profile              : List_Profile (Length);
      Actual_Params        : Actual_Params_Array (1 .. Length) :=
        (others => Null_Actual_Parameter);
      Params_Set           : Integer := 0;
   end record;

end Ada_Semantic_Tree.List_Resolver;
