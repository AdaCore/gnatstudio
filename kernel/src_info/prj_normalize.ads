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
--  There are two aspects to normalized projects:
--  One shared project called <root>-internal.gpr (<root> is the name of the
--  root node of the project hierarchy), with the following format:
--
--      project <root>.Internal is
--          [Variable_Declarations]
--          [Nested_Case]  --  Set up of the Scenario attribute
--      end <root>.Internal;
--
--  In addition, all the projects from the user are modified to the following
--  format:
--
--      with <root>.Internal;
--      Project_Header
--      [Variable_Declarations]
--      [Common_Section]
--      [Case_On_Scenario]
--      [Package_Declaration
--         [Common_Section]
--         [Case_On_Scenario]
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
--                    when Value1_1 => for Scenario use "<NAME1>";
--                    when Value1_2 => for Scenario use "<NAME2>";
--                 end case;
--              when Value2 =>
--                 case Var2 is
--                    when Value2_1 => for Scenario use "<NAME3>";
--                    when Value2_2 => for Scenario use "<NAME4>";
--                 end case;
--           end case;
--
--     The "when others" section is not allowed in the nested cases, and are
--     replaced by the appropriate list of "when" statements.
--
--     Case_On_Scenario is a single case statement that only tests the Scenario
--     attribute.
--
--  </description>

with Prj.Tree;

package Prj_Normalize is

   type Matching_Item_Callback is access
     procedure (Item : Prj.Tree.Project_Node_Id);
   --  A callback function called for each case item that matches a specific
   --  set of values (see For_Each_Scenario_Case_Item).

   procedure Normalize_Project (Project : Prj.Tree.Project_Node_Id);
   --  Converts Project to a normalized form.

   procedure For_Each_Scenario_Case_Item
     (Project : Prj.Tree.Project_Node_Id;
      Pkg     : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Action  : Matching_Item_Callback);
   --  Execute Action for all the nested case items in Project or Pkg that
   --  match the current scenario.
   --  If there is currently no scenario variable, we simply call Action once
   --  on Project or Pkg itself.
   --  For instance, in the example above, and if the current value for Var1 is
   --  Value1, and the current value for Var2 is Value_1_1, this calls Action
   --  once on the case item for "when Value_1_1 => ".
   --
   --  Nested case items are created if needed to match the current scenario.
   --
   --  Important: Project must have been normalized first.

end Prj_Normalize;
