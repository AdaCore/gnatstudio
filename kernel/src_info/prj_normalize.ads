-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  Project files can be written freely by the user (through any standard
--  editor). However, although we are able to import them whatever form they
--  have, these can'be easily manipulated, and a different form needs to be
--  used, called normalized project files.
--
--  Projects are normalized only the first time they are actually modified (ie
--  if they are open in the project browser but never modified, then we don't
--  need to modify what the user did, since Prj.Proc.Process can of course work
--  with any form of projects).
--
--  However, the normalized projects are needed, so that we know exactly where
--  to add new statements depending on the current scenario.
--
--  Normalized projects have the following invariant:
--      There is only one case statement per project or package.
--  This is in fact a nested case statement, where each environment variable is
--  referenced.
--
--  They also have the following invariant:
--      A project has exactly the same behavior in its normalized form as in
--      its original form.
--  Of course, this is only true until the next modification to any of the two
--  forms.
--
--  Thus, the projects have the following format:
--
--      Project_Header
--      [Variable_Declarations]
--      [Common_Section]
--      [Nested_Case]
--      [Package_Declaration
--         [Common_Section]
--         [Nested_Case]
--      ]*
--
--  Where:
--     Project_Header is the standard header, including importing other
--     projects, declaring the name of the current project, ...
--
--     Variable_Declarations is the list of scenario variables, including their
--     types. There can be no variable declaration outside of this section,
--     including in packages.
--     ??? Not two variables can reference the same external variables.
--
--     Common_Section is the list of statements that need to be executed in all
--     scenarios (like common source directories, common switches when inside a
--     package, ...). This section can not include any case statement.
--
--     Nested_Case is one big case statement, including other nested cases. Its
--     format is similar to:
--
--           case Var1 is
--              when Value1 =>
--                 case Var2 is
--                    when Value1_1 => stmt1;
--                    when Value1_2 => stmt2;
--                 end case;
--              when Value2 =>
--                 case Var2 is
--                    when Value2_1 => stmt3;
--                    when Value2_2 => stmt4;
--                 end case;
--           end case;
--
--     The "when others" section is not allowed in the nested cases, and are
--     replaced by the appropriate list of "when" statements.
--
--  </description>

with Prj.Tree;
with Types;

package Prj_Normalize is

   function Is_Normalized (Project : Prj.Tree.Project_Node_Id)
      return String;
   --  Return the empy string if Project is normalized, or an error message if
   --  otherwise.

   procedure Normalize (Project : Prj.Tree.Project_Node_Id);
   --  Normalize Project.
   --  For efficiency, no check is done to make sure Project is not already
   --  normalized. Some modifications might still happen.
   --  The exception Normalize_Error is raised if Project uses some features
   --  that cannot currently be normalized.

   Normalize_Error : exception;
   --  Raised when a project could not be normalized.

   type Matching_Item_Callback is access
     procedure (Item : Prj.Tree.Project_Node_Id);
   --  A callback function called for each case item that matches a specific
   --  set of values

   type External_Variable_Value is record
      Variable_Type  : Prj.Tree.Project_Node_Id;
      Variable_Name  : Types.String_Id;
      Variable_Value : Types.String_Id;
      Negated        : Boolean := False;
   end record;
   --  Description for one possible value of an external variable. Through an
   --  array of such values, it is possible to reference multiple case items in
   --  a case construction of a normalized project.
   --  If Negated is True, then Variable_Name must not be Variable_Value for
   --  the case item to match.
   --  See the example in the description of External_Variable_Value_Array.

   type External_Variable_Value_Array is array (Natural range <>) of
     External_Variable_Value;
   --  Description for a case item (or a set of them).
   --  The same variable name can appear multiple times in the array. In that
   --  case, the value of the variable must match any of the choises for the
   --  case item to match.
   --  If a variable name exists in the case construction but not in the array,
   --  then the variable can have any value.
   --
   --  As an example, given the following case construction:
   --      case V1 is
   --         when Val1 | Val2 =>
   --            case V2 is
   --                when Val2_1 => stmt1;
   --                when others => stmt2;
   --            end case;
   --         when others =>
   --            case V3 is
   --               when V3_1 => stmt3;
   --               when V3_2 => stmt4;
   --            end V3;
   --      end case;
   --
   --  Then stmt1 can be reach with an External_Variable_Value_Array equal to:
   --      ((V1, Val1), (V1, Val2), (V2, Val2_1))
   --  stmt2 can be reached with
   --      ((V1, Val1), (V1, Val2), (V2, Val2_1, False))
   --  stmt3 can be reached with
   --      ((V1, Val1, False), (V1, Val2, False), (V3, V3_1))
   --  Both stmt3 and stmt4 can be reached at the same time with
   --      ((V1, Val1, False), (V1, Val2, False))
   --
   --  If there was at least one non-negated element in the array, then at
   --  least one of the non-negated elements must be matched

   All_Case_Items : constant External_Variable_Value_Array;
   --  Matching all case items.

   procedure For_Each_Matching_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Values  : External_Variable_Value_Array;
      Action  : Matching_Item_Callback);
   --  Execute Action for all the case items in Project or Pkg that match
   --  Values.
   --  If no case item exists, Action is still called once on Project or Pkg
   --  itself.
   --  If Pkg is not the Empty_Node, then this subprogram only works on that
   --  package. However, the project must still be specified so that the
   --  declaration of the variables can be found.
   --  If a variable is referenced in Values, but doesn't have an associated
   --  case construction, a new case construction is added at the lowest level.
   --
   --  Important: Project must have been normalized first, and it is
   --  recommended to call Check_Case_Construction before

   procedure For_Each_Scenario_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Action  : Matching_Item_Callback);
   --  Same above, but it works directly for the current scenario (ie its gets
   --  the value of the variables directly from the environment).
   --  Important: Project must have been normalized first, and it is
   --  recommended to call Check_Case_Construction before

private

   No_Value : constant External_Variable_Value :=
     (Variable_Type  => Prj.Tree.Empty_Node,
      Variable_Name  => Types.No_String,
      Variable_Value => Types.No_String,
      Negated        => False);

   All_Case_Items : constant External_Variable_Value_Array (1 .. 0) :=
     (others => No_Value);
end Prj_Normalize;
