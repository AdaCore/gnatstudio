package body Test_Lib is

   procedure Corrections_Proposed
     (Message      : Error_Message;
      Id           : Error_Id;
      Solutions    : Solution_List;
      Current_Text : Text_Navigator_Abstr'Class;
      Corrector    : in out Correction_Manager) is

      Current_Solution : Extract_List.List_Node;
      Num_Sol          : Integer;

   begin
      Put_Line ("Message : ");
      Put_Line (Get_Message (Message));
      Current_Solution := First (Solutions);
      Put_Line ("Old text : ");
      Put_Line_Original
        (Data (Current_Solution),
         Current_Text);
      Num_Sol := 0;
      while Current_Solution /= Extract_List.Null_Node loop
         Num_Sol := Num_Sol + 1;
         Put_Line ("Proposition" & Integer'Image (Num_Sol) & " : ");
         Put_Line (Data (Current_Solution));
         Current_Solution := Next (Current_Solution);
      end loop;
      Put_Line ("What correction do you want ? (0 means none)");
      Num_Sol := Get_Number (0, Num_Sol);
      if Num_Sol /= 0 then
         Validate (Corrector, Id, Num_Sol, True);
      end if;
      New_Line;
   end Corrections_Proposed;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number (Min, Max : Integer) return Integer is
      Number : Integer;
   begin
      loop
         begin
            Get (Number);
            Skip_Line;
            exit when Number >= Min and then Number <= Max;
            Put_Line ("Number out of bounds, try again.");
         exception
            when Data_Error =>
               Skip_Line;
               Put_Line ("Wrong number format, try again.");
         end;
      end loop;
      return Number;
   end Get_Number;

   ---------------
   -- Ambiguity --
   ---------------

   procedure Ambiguity
     (Alternative_1, Alternative_2 : Extract;
      Delete_Choice                : out Alternative_Choice) is
   begin
      Put_Line ("Problem beetween");
      Put_Line (Alternative_1);
      Put_Line ("  and");
      Put_Line (Alternative_2);
      Delete_Choice := 0;
   end Ambiguity;


end Test_Lib;
