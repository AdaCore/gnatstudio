
with Prj_Scenarios;  use Prj_Scenarios;
with Prj_API;        use Prj_API;
with Types;          use Types;
with Namet;          use Namet;
with Stringt;        use Stringt;
with Prj.Tree;       use Prj.Tree;

with Text_IO; use Text_IO;

procedure Test_Scenario is
   Mgr : Scenario_Manager;

   procedure Output (Values : String_Id_Array);
   --  Print the scenario names for (Value1, Value2)

   procedure Output (Values : String_Id_Array) is
   begin
      for V in Values'Range loop
         if Values (V) /= No_String then
            String_To_Name_Buffer (Values (V));
            Put_Line ("variable (" & V'Img & ") = "
                      & Name_Buffer (1 .. Name_Len));
         else
            Put_Line ("variable (" & V'Img & ") = <any>");
         end if;
      end loop;

      declare
         Result : constant String_Id_Array := Get_Scenario_Names (Mgr, Values);
      begin
         for R in Result'Range loop
            if Result (R) = No_String then
               Put_Line ("    Scenar=   <unknown> !!!!!!");
            else
               String_To_Name_Buffer (Result (R));
               Put_Line ("    Scenar=" & Name_Buffer (1 .. Name_Len));
            end if;
         end loop;
         New_Line;
      end;
   end Output;

   Val1, Val2, Val3, Val4, Val5, Val6 : String_Id;
   Project, Typ, Var1, Var2, Var3     : Project_Node_Id;

begin
   Project := Create_Project ("Foo", "");

   Typ := Get_Or_Create_Type (Project, "Typ1");
   Val1 := Add_Possible_Value (Typ, "Val1");
   Val4 := Add_Possible_Value (Typ, "Val4");
   Var1 := Get_Or_Create_Typed_Variable (Project, "Var1", Typ);

   Typ := Get_Or_Create_Type (Project, "Typ2");
   Val2 := Add_Possible_Value (Typ, "Val2");
   Val3 := Add_Possible_Value (Typ, "Val3");
   Var2 := Get_Or_Create_Typed_Variable (Project, "Var2", Typ);

   Typ := Get_Or_Create_Type (Project, "Typ3");
   Val5 := Add_Possible_Value (Typ, "Val5");
   Val6 := Add_Possible_Value (Typ, "Val6");
   Var3 := Get_Or_Create_Typed_Variable (Project, "Var3", Typ);

   Initialize (Mgr, (Var1, Var2, Var3));

   Set_Scenario_Name (Mgr, (Val1, Val2, Val5), "SCENAR_1_2_5");
   Set_Scenario_Name (Mgr, (Val1, Val3, Val5), "SCENAR_1_3_5");
   Set_Scenario_Name (Mgr, (Val4, Val2, Val5), "SCENAR_4_2_5");
   Set_Scenario_Name (Mgr, (Val4, Val3, Val5), "SCENAR_4_3_5");
   Set_Scenario_Name (Mgr, (Val1, Val2, Val6), "SCENAR_1_2_6");
   Set_Scenario_Name (Mgr, (Val1, Val3, Val6), "SCENAR_1_3_6");
   Set_Scenario_Name (Mgr, (Val4, Val2, Val6), "SCENAR_4_2_6");
   Set_Scenario_Name (Mgr, (Val4, Val3, Val6), "SCENAR_4_3_6");

   Output ((Val1, Val2, Val5));
   Output ((No_String, Val2, Val5));
   Output ((Val1, No_String, Val5));
   Output ((Val1, No_String, No_String));
end Test_Scenario;
