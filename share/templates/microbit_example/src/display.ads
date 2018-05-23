------------------------------------------------------------------------------
--                     Copyright (C) 2018, AdaCore                          --
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

package Display is

   procedure Scroll_Text (Str : String);
   --  Scroll a string from right to left across the LED matrix

private

   subtype LED_Row_Coord is Natural range 0 .. 4;
   --  Row coordinate in LED matrix

   subtype LED_Column_Coord is Natural range 0 .. 4;
   --  Column coordinate in LED matrix

   subtype GPIO_Pin_Index is Natural range 0 .. 31;
   --  Pin index of the nRF51 GPIO points

   ----------------------
   -- Pixel to IO Pins --
   ----------------------

   --  There is no one to one correspondence between the GPIO matrix and LED
   --  matrix. The GPIO matrix is 3x9 where the LED matrix is 5x5. The types
   --  and data below define the mapping from LED matrix coordinates to the
   --  GPIO points.

   type Row_Range is new Natural range 1 .. 3;
   --  Row coordinate in the GPIO matrix

   type Column_Range is new Natural range 1 .. 9;
   --  Column coordinate in the GPIO matrix

   type LED_Point is record
      Row_Id    : Row_Range;
      Column_Id : Column_Range;
   end record;
   --  Address of an LED in the GPIO matrix

   Row_Points : array (Row_Range) of GPIO_Pin_Index :=
     (13, 14, 15);
   --  Pins for the GPIO matrix rows

   Column_Points : array (Column_Range) of GPIO_Pin_Index :=
     (04, 05, 06, 07, 08, 09, 10, 11, 12);
   --  Pins for the GPIO matrix columns

   Map : constant array (LED_Column_Coord, LED_Row_Coord) of LED_Point :=
     (((1, 1), (3, 4), (2, 2), (1, 8), (3, 3)),
      ((2, 4), (3, 5), (1, 9), (1, 7), (2, 7)),
      ((1, 2), (3, 6), (2, 3), (1, 6), (3, 1)),
      ((2, 5), (3, 7), (3, 9), (1, 5), (2, 6)),
      ((1, 3), (3, 8), (2, 1), (1, 4), (3, 2))
     );
   --  Address of each LED in the GPIO matrix

end Display;
