------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with Dynamic_Arrays;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Arrays is
   type My_Data is new Integer;
   package Data_Arrays is new Dynamic_Arrays
     (Data                    => My_Data,
      Table_Multiplier        => 2,
      Table_Minimum_Increment => 10,
      Table_Initial_Size      => 5);
   use Data_Arrays;

   procedure Assert (Data1 : Index_Type; Data2 : Integer; Msg : String);

   procedure Assert (Data1 : Index_Type; Data2 : Integer; Msg : String) is
   begin
      if Integer (Data1) /= Data2 then
         Put_Line ("--- Failed: " & Msg);
         Put_Line ("Expected:" & Data2'Img);
         Put_Line ("Got:     " & Data1'Img);
      end if;
   end Assert;

   Arr : Instance;
begin
   for J in My_Data'(1) .. 10_000 loop
      Append (Arr, J);
   end loop;

   Assert (Length (Arr), 10_000, "Incorrect number of elements");

   Remove (Arr, Item => 5_000);
   Assert (Length (Arr), 9_999,  "Incorrect size after remove");

   Free (Arr);
   Assert (Length (Arr), 0, "Incorrect size after free");
end Test_Arrays;
