-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

package body Test_Lib is

   procedure Corrections_Proposed
     (Message      : Error_Message;
      Id           : Error_Id;
      Solutions    : Solution_List;
      Current_Text : Text_Navigator_Abstr'Class;
      Corrector    : in out Correction_Manager)
   is
      Current_Solution              : Extract_List.List_Node;
      Num_Sol                       : Integer;
      Extended_Extract              : Extract;
      Success_Update, Already_Fixed : Boolean;
      Display_Lines_Before          : constant Integer := 5;
      Display_Lines_After           : constant Integer := 5;
   begin

      Current_Solution := First (Solutions);

      if Visible then
         Put_Line ("--------------------------------------------------------");
         Put_Line ("Message: " & Get_Message (Message));
         Put_Line ("--------------- Old text: ---------------");
         Put_Line (Get_Old_Text
                     (Data (Current_Solution),
                      Current_Text,
                      Display_Lines_Before,
                      Display_Lines_After));
      end if;

      Num_Sol := 0;

      while Current_Solution /= Extract_List.Null_Node loop
         Extended_Extract := Clone (Extract (Data (Current_Solution)));

         Extend_Before
           (Extended_Extract,
            Current_Text,
            Display_Lines_Before);
         Extend_After
           (Extended_Extract,
            Current_Text,
            Display_Lines_After);

         Update_Changes
           (Corrector,
            Current_Text,
            Extended_Extract,
            Success_Update,
            Already_Fixed);

         if Already_Fixed or else not Success_Update then
            Free (Extended_Extract);
            return;
         end if;

         Reduce
           (Extended_Extract,
            Display_Lines_Before,
            Display_Lines_After);

         Num_Sol := Num_Sol + 1;
         if Visible then
            Put_Line
              ("--------------- Proposition" & Integer'Image (Num_Sol) &
               ": ---------------");
            Put_Line (Get_New_Text (Extended_Extract));
         end if;

         Reduce (Extended_Extract, 0, 0);
         Set_Data (Current_Solution, Extended_Extract);

         Current_Solution := Next (Current_Solution);
      end loop;

      if Visible then
         Put_Line ("What correction do you want ? (0 means none)");
      end if;
      Num_Sol := Get_Number (0, Num_Sol);

      Validate (Corrector, Id, Num_Sol);

      if Is_Open (Capture_File) then
         Put (Capture_File, Num_Sol);
         New_Line (Capture_File);
      end if;

      if Visible then
         Put_Line ("--------------------------------------------------------");
         New_Line;
      end if;
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
            Put_Line (Number'Img & " out of bounds (" & Min'Img & "," &
                        Max'Img & "), try again.");
         exception
            when Data_Error =>
               Skip_Line;
               Put_Line ("Wrong number format, try again.");
         end;
      end loop;
      return Number;
   end Get_Number;

end Test_Lib;
