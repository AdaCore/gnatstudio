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
--  This package provides a manager for scenario. It can easily convert from a
--  set of specific values for environment variables to a scenario name, and
--  back.
--
--  The idea is that this is a higher-level representation of the nested case
--  statement inside the internal project for scenarios:
--     case Var1 is
--        when Val1 =>
--           case Var2 is
--              when Val2 =>  for Scenario use "NAME1";
--              when Val3 =>  for Scenario use "NAME2";
--           end case;
--        when Val4 =>
--           case Var2 is
--              when Val2 =>  for Scenario use "NAME3";
--              when Val3 =>  for Scenario use "NAME4";
--           end case;
--     end case;
--
--  The scenario manager can report the list of names that match a given set of
--  values.
--
--  </description>

with GNAT.Spitbol;
--  ??? consider using GNAT.HTable instead

with Prj_API;

with Types;

package Prj_Scenarios is

   type Scenario_Manager is private;
   --  See description above.

   type String_Id_Array is array (Positive range <>) of Types.String_Id;
   --  A list of values for the environment variables.
   --  The indexes in this table are the number of the variable in the
   --  scenario_manager.
   --  The values in the table are the current value for these variables. A
   --  value of No_String indicates that the variable can take any value. This
   --  is the equivalent of a "when others" in a case statements.

   procedure Initialize
     (Manager   : in out Scenario_Manager;
      Variables : Prj_API.Project_Node_Array);
   --  Create a new scenario manager for the list of Variables.
   --  The previous contents of Manager is discarded.

   procedure Set_Scenario_Name
     (Manager : in out Scenario_Manager;
      Values  : String_Id_Array;
      Name    : String);
   --  Set a new name for the scenario matching Values.
   --  The old name is overriden.
   --  No element in Values can be equal to No_String.

   function Get_Scenario_Names
     (Manager : Scenario_Manager; Values  : String_Id_Array)
      return String_Id_Array;
   --  Return the list scenario names that match a specific set of Values of
   --  the environment variables.

   function Variable_Index
     (Manager : Scenario_Manager; Env_Variable_Name : Types.String_Id)
      return Natural;
   --  Return the index that should be used in a String_Id_Array to represent
   --  the environment variable Env_Variable_Name.
   --  Note that Env_Variable_Name is not the internal name used in project
   --  file for the variable, but really the name of the environment variable.
   --
   --  0 is returned if the variable wasn't found.

private

   type Header_Num is range 0 .. 20;
   --  The integer indicating the number and types of hash headers.
   --  20 is probably a good approximation of the maximum number of scenario
   --  variables, and thus we shouldn't get too many conflicts in the htable by
   --  using 20 as Header_Num'Last above.

   --  function Hash is new Htables.Hash (Header_Num => Header_Num);

   package Name_Htables is new GNAT.Spitbol.Table
     (Value_Type => Types.String_Id,
      Null_Value => Types.No_String,
      Img        => Types.String_Id'Image,
      "="        => Types."=");

   type Scenario_Manager is record
      Variables : Prj_API.Project_Node_Array_Access;
      --  List of scenario variables know to the scenario manager.
      --  This is initially extracted from the project tree, and then cached
      --  for easy lookup.
      --  Note that two entries can not reference the same environment
      --  variable. These would be consider as aliases, and only one such entry
      --  is created.

      Names : Name_Htables.Table (100);
      --  The name of the scenario associated with a given value for all the
      --  scenario variables. The key should be a string similar to
      --    "1=11,2=22",
      --  where "1" and "2" are the indexes of the variables, and "11" and "22"
      --  their values (as string_id).
      --
      --  The size for Names is an estimated of the number of named scenarios
      --  in the GUI. It is not a hard limit.
   end record;

end Prj_Scenarios;
