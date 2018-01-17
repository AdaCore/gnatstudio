------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

--  This package provides containers & process used to analyze formal / actual
--  list of entities, in particular parameters.

with GNAT.Strings;     use GNAT.Strings;

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

   function To_Entity_Access (Param : Formal_Parameter) return Entity_Access;
   --  Converts the formal parameter to the corresponding entity.

   type Formal_Parameter_Array is array (Integer range <>) of Formal_Parameter;

   type List_Profile (<>) is private;
   --  This type holds information about a formal list profile - for example
   --  the formal profile of a subprogram.

   type List_Profile_Access is access all List_Profile;

   procedure Free (This : in out List_Profile_Access);
   --  Free the data associated to a List_Profile_Access

   type Profile_Kind is (Regular_Profile, Generic_Profile);

   function Is_Entity_With_Profile
     (Entity       : Entity_Access;
      Visible_From : Visibility_Context) return Boolean;
   --  Return true if the entity is expected to have a "profile", (subprograms,
   --  arrays, records) false otherwise.

   function Get_List_Profile
     (Entity       : Entity_Access;
      Visible_From : Visibility_Context;
      Kind         : Profile_Kind := Regular_Profile) return List_Profile;
   --  Create and return a list profile out of an entity. If no list profile
   --  can be found, then a special profile with no constraints will be
   --  returned. This is different from an empty profile (which doesn't allow
   --  any actual parameter).
   --  Visibility may be required e.g. for types "profiles", typically used in
   --  aggregates.

   function Get_Formals (Profile : List_Profile) return Entity_Array;
   --  Return the list of formal parameters associated with this profile

   function Get_Entity (Profile : List_Profile) return Entity_Access;
   --  Return the entity holding this profile.

   function Get_Number_Of_Formals (Profile : List_Profile) return Integer;
   --  Return the total number of formals declared in this profile.

   function Get_Aggregate_Parent (Params : List_Profile) return Entity_Access;
   --  If the list represents a aggregate of a tagged type, this returns the
   --  last private parent that has to be given to the aggregate, if any.

   --  Actual parameters --

   type Actual_Parameter is private;
   --  This type denotes an actual parameter. This is used to check that actual
   --  parameters actually match a formal profile.

   procedure Free (This : in out Actual_Parameter);
   --  Free the data associated to this actual parameter.

   Null_Actual_Parameter : constant Actual_Parameter;

   function Get_Actual_Parameter
     (Buffer      : access UTF8_String;
      Param_Start : String_Index_Type;
      Param_End   : String_Index_Type) return Actual_Parameter;
   --  Created an actual parameter out of a piece of code - param start and
   --  param end has to be set around the entire parameter, including the
   --  name preceded by the arrow if any.

   type Actual_Parameter_Resolver (<>) is private;
   --  This type is the master of the actual formal / actual parameter checks.

   type Actual_Parameter_Resolver_Access is
     access all Actual_Parameter_Resolver;

   procedure Free (This : in out Actual_Parameter_Resolver_Access);
   --  Free the data associated to This.

   function Deep_Copy
     (This : Actual_Parameter_Resolver) return Actual_Parameter_Resolver;
   --  Perform a deep copy of the resolver. The caller is responsible of
   --  freeing the returned object.

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

   procedure Append_Actuals
     (Params     : in out Actual_Parameter_Resolver;
      Buffer     : String_Access;
      Start_Call : String_Index_Type;
      Success    : out Boolean);
   --  Starting at the location given in parameter, the resolver will look
   --  at the first open parenthesis, and then set the resolver according to
   --  the actual parameters found until the closing parenthesis.
   --  If some actuals couldn't be added, then success is false

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

   function Get_Expression_For_Formal
     (Params : Actual_Parameter_Resolver;
      Name   : String) return Parsed_Expression;
   --  Return the expression provided to the parameter of the name given.
   --  Null_Parsed_Expression if it can't be found.

   function Get_Profile
     (Params : Actual_Parameter_Resolver) return List_Profile;
   --  Return the profile associated to this resolver

private

   type Formal_Parameter is new Entity_Access;

   type List_Profile (Nb_Params : Integer) is record
      Params : Entity_Array (1 .. Nb_Params);

      Extra_Params_Allowed : Boolean := False;
      --  For certain kinds of entities (e.g. arrays) we're allowing more
      --  params than expected (since we currently don't analyze array
      --  dimensions). This is a workaround - the implementation will be
      --  more precise in the future.

      Aggregate_Parent : Entity_Access;
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
