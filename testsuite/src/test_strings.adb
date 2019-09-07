------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with GNATCOLL.Utils; use GNATCOLL.Utils;

procedure Test_Strings is
   File_1 : constant String :=
     ASCII.HT & "ABC";
   File_2 : constant String :=
     " " & ASCII.HT & "ABC";
   File_3 : constant String :=
     ASCII.HT & ASCII.HT & "ABC";

   procedure Test_Skip_To_Column
     (File           : String;
      Column         : Integer;
      Start_Line     : Integer;
      Expected_Index : Integer);

   procedure Test_Skip_To_Column
     (File           : String;
      Column         : Integer;
      Start_Line     : Integer;
      Expected_Index : Integer)
   is
      Index : Integer := Start_Line;
   begin
      Skip_To_Column
        (Str       => File,
         Columns   => Column,
         Index     => Index);
      pragma Assert
        (Index = Expected_Index,
         "INDEX =" & Index'Img & " INSTEAD OF" & Expected_Index'Img);
   end Test_Skip_To_Column;

begin

   --  Tests on File_1

   Test_Skip_To_Column
     (File_1,
      Column         => 1,
      Start_Line     => 1,
      Expected_Index => 1);
   Test_Skip_To_Column
     (File_1,
      Column         => 9,
      Start_Line     => 1,
      Expected_Index => 2);

   --  Tests on File_2

   Test_Skip_To_Column
     (File_2,
      Column         => 1,
      Start_Line     => 1,
      Expected_Index => 1);
   Test_Skip_To_Column
     (File_2,
      Column         => 9,
      Start_Line     => 1,
      Expected_Index => 3);

   --  Tests on File_3

   Test_Skip_To_Column
     (File_3,
      Column         => 9,
      Start_Line     => 1,
      Expected_Index => 2);
   Test_Skip_To_Column
     (File_3,
      Column         => 17,
      Start_Line     => 1,
      Expected_Index => 3);

end Test_Strings;
