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

with GNAT.Spitbol;
with Prj_API;       use Prj_API;

with Types;         use Types;
with Prj.Tree;      use Prj.Tree;
with Stringt;       use Stringt;

package body Prj_Scenarios is

   use Name_Htables;

   function Build_Key (Values : String_Id_Array) return String;
   --  Return the key to use in the htable for a given set of values.

   ---------------
   -- Build_Key --
   ---------------

   function Build_Key (Values : String_Id_Array) return String is
      S : String (1 .. 1024);
      Index : Natural := S'First;
   begin
      for V in Values'Range loop
         pragma Assert (Values (V) /= No_String);

         declare
            T : constant String := Positive'Image (V) & "="
              & String_Id'Image (Values (V) - No_String) & ",";
         begin
            S (Index .. Index + T'Length - 1) := T;
            Index := Index + T'Length - 1;
         end;
      end loop;
      return S (S'First .. Index - 1);
   end Build_Key;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Manager : in out Scenario_Manager;
      Variables : Prj_API.Project_Node_Array)
   is
   begin
      Free (Manager.Variables);
      Clear (Manager.Names);

      Manager.Variables := new Project_Node_Array' (Variables);
   end Initialize;

   ------------------------
   -- Get_Scenario_Names --
   ------------------------

   function Get_Scenario_Names
     (Manager : Scenario_Manager;
      Values  : String_Id_Array)
      return String_Id_Array
   is
      Num : Positive := 1;
   begin
      --  Evaluate the number of possible scenarios that match values
      for V in Values'Range loop
         if Values (V) = No_String then
            Num := Num * Typed_Values_Count (Manager.Variables (V));
         end if;
      end loop;

      if Num = 1 then
         return (1 => Get (Manager.Names, Build_Key (Values)));

      else
         declare
            Output : String_Id_Array (1 .. Num);
            Val    : String_Id_Array := Values;
            Nodes  : Project_Node_Array (Values'Range) :=
              (others => Empty_Node);
            Index  : Natural := Output'First;
            Var_Index : Integer := Nodes'First;
         begin
            Main_Loop :
            loop
               --  Reinitialize all the lists that need to be reinitialized

               while Var_Index <= Nodes'Last loop
                  if Values (Var_Index) = No_String
                    and then Nodes (Var_Index) = Empty_Node
                  then
                     Nodes (Var_Index) := First_Literal_String
                       (String_Type_Of (Manager.Variables (Var_Index)));
                     Val (Var_Index) := String_Value_Of (Nodes (Var_Index));
                  end if;

                  Var_Index := Var_Index + 1;
               end loop;

               --  Output the current status

               Output (Index) := Get (Manager.Names, Build_Key (Val));
               Index := Index + 1;
               Var_Index := Var_Index - 1;

               --  Move the the next value

               loop
                  if Values (Var_Index) = No_String then
                     Nodes (Var_Index) :=
                       Next_Literal_String (Nodes (Var_Index));
                     exit when Nodes (Var_Index) /= Empty_Node;
                  end if;
                  Var_Index := Var_Index - 1;
                  exit Main_Loop when Var_Index < Nodes'First;
               end loop;

               Val (Var_Index) := String_Value_Of (Nodes (Var_Index));
               Var_Index := Var_Index + 1;
            end loop Main_Loop;

            return Output;
         end;
      end if;
   end Get_Scenario_Names;

   -----------------------
   -- Set_Scenario_Name --
   -----------------------

   procedure Set_Scenario_Name
     (Manager : in out Scenario_Manager;
      Values  : String_Id_Array;
      Name    : String) is
   begin
      Start_String;
      Store_String_Chars (Name);
      Set (Manager.Names, Build_Key (Values), End_String);
   end Set_Scenario_Name;

   --------------------
   -- Variable_Index --
   --------------------

   function Variable_Index
     (Manager           : Scenario_Manager;
      Env_Variable_Name : Types.String_Id)
      return Natural is
   begin
      pragma Assert (Manager.Variables /= null);

      for V in Manager.Variables'Range loop
         if String_Equal
           (External_Reference_Of (Manager.Variables (V)), Env_Variable_Name)
         then
            return V;
         end if;
      end loop;
      return 0;
   end Variable_Index;

end Prj_Scenarios;

