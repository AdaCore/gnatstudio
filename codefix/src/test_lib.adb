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
      Current_Solution : Extract_List.List_Node;
      Num_Sol          : Integer;
   begin

      Current_Solution := First (Solutions);
      if Visible then
         Put_Line ("Message: ");
         Put_Line (Get_Message (Message));
         Put_Line ("Old text: ");
         Put_Line_Original
           (Data (Current_Solution),
            Current_Text);
      end if;

      Num_Sol := 0;

      while Current_Solution /= Extract_List.Null_Node loop
         Num_Sol := Num_Sol + 1;
         if Visible then
            Put_Line ("Proposition" & Integer'Image (Num_Sol) & " : ");
            Put_Line (Data (Current_Solution));
         end if;
         Current_Solution := Next (Current_Solution);
      end loop;

      if Visible then
         Put_Line ("What correction do you want ? (0 means none)");
      end if;
      Num_Sol := Get_Number (0, Num_Sol);

      Validate (Corrector, Id, Num_Sol, True);

      if Is_Open (Capture_File) then
         Put (Capture_File, Num_Sol);
         New_Line (Capture_File);
      end if;

      if Visible then
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
