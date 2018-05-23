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

with Ada.Real_Time;  use Ada.Real_Time;
with HAL;            use HAL;
with NRF51_SVD.GPIO; use NRF51_SVD.GPIO;
with Font5x5;        use Font5x5;
with Generic_Timers;

package body Display is

   procedure Display_Update;
   --  Callback for the LED matrix scan

   package Display_Update_Timer is new Generic_Timers
     (One_Shot   => False,
      Timer_Name => "Display update",
      Period     => Ada.Real_Time.Microseconds (900),
      Action     => Display_Update);
   --  This timing event will call the procedure Display_Update every
   --  900 microseconds.

   subtype Width is Natural range
     LED_Column_Coord'First ..
       LED_Column_Coord'First + LED_Column_Coord'Range_Length * 2;
   --  The bitmap width is 2 time the display size so we can instert hidden
   --  characters to the right of the screen and scroll them in with the
   --  Shift_Left procedure.

   Bitmap : array (Width, LED_Row_Coord) of Boolean :=
     (others => (others => False));

   Current_X : LED_Column_Coord := 0;
   Current_Y : LED_Row_Coord := 0;
   --  Coordinate of the current LED begin lit

   procedure Clear (Pin : GPIO_Pin_Index);
   --  Set the GPIO to a low state

   procedure Set (Pin : GPIO_Pin_Index);
   --  Set the GPIO to a High state

   procedure Initialize;
   --  Initialize GPIOs and timers for the LED matrix

   procedure Shift_Left;
   --  Shift all pixels of the bitmap buffer to the left

   procedure Put_Char (X_Org : Width;
                       C     : Character);
   --  Print a charater to the bitmap buffer

   procedure Scroll_Character (Char : Character);
   --  Print a character to the hidden part of the bitmap buffer and scroll it
   --  to the visible part.

   --------------------
   -- Display_Update --
   --------------------

   procedure Display_Update is
   begin
      --  Turn Off the current LED
      Clear (Row_Points (Map (Current_X, Current_Y).Row_Id));
      Set (Column_Points (Map (Current_X, Current_Y).Column_Id));

      --  Compute the coordinate of the next LED to be lit
      if Current_X = LED_Column_Coord'Last then
         Current_X :=  LED_Column_Coord'First;

         if Current_Y =  LED_Row_Coord'Last then
            Current_Y :=  LED_Row_Coord'First;
         else
            Current_Y := Current_Y + 1;
         end if;
      else
         Current_X := Current_X + 1;
      end if;

      --  Turn on the new LED?
      if Bitmap (Current_X, Current_Y) then
         --  Row source current
         Set (Row_Points (Map (Current_X, Current_Y).Row_Id));
         --  Column sink current
         Clear (Column_Points (Map (Current_X, Current_Y).Column_Id));
      end if;
   end Display_Update;

   -----------
   -- Clear --
   -----------

   procedure Clear (Pin : GPIO_Pin_Index) is
   begin
      GPIO_Periph.OUT_k.Arr (Pin) := Low;
   end Clear;

   ---------
   -- Set --
   ---------

   procedure Set (Pin : GPIO_Pin_Index) is
   begin
      GPIO_Periph.OUT_k.Arr (Pin) := High;
   end Set;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      procedure Configure_GPIO (Pin : GPIO_Pin_Index);

      --------------------
      -- Configure_GPIO --
      --------------------

      procedure Configure_GPIO (Pin : GPIO_Pin_Index) is
         CNF : PIN_CNF_Register renames GPIO_Periph.PIN_CNF (Pin);
      begin
         CNF.DIR   := Output;
         CNF.INPUT := Disconnect;
         CNF.PULL  := Pullup;
         CNF.DRIVE := S0S1;
         CNF.SENSE := Disabled;
      end Configure_GPIO;
   begin

      --  Initialize LED maxtrix GPIO

      for Pin of Row_Points loop
         Configure_GPIO (Pin);
         Clear (Pin);
      end loop;

      for Pin of Column_Points loop
         Configure_GPIO (Pin);
         Set (Pin);
      end loop;

      --  Start the LED scan timer
      Display_Update_Timer.Start;
   end Initialize;

   ----------------
   -- Shift_Left --
   ----------------

   procedure Shift_Left is
   begin
      --  Shift pixel columns to the left, erasing the left most one
      for X in Bitmap'First (1) .. Bitmap'Last (1) - 1 loop
         for Y in Bitmap'Range (2) loop
            Bitmap (X, Y) := Bitmap (X + 1, Y);
         end loop;
      end loop;

      --  Insert black pixels to the right most column
      for Y in Bitmap'Range (2) loop
         Bitmap (Bitmap'Last (1), Y) := False;
      end loop;
   end Shift_Left;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (X_Org : Width;
                       C     : Character)
   is
      C_Index : constant Integer := Character'Pos (C) - Character'Pos ('!');
   begin

      if C_Index not in Font'Range then
         --  C is not a printable character
         return;
      end if;

      --  Copy the glyph into the bitmap buffer
      for X in LED_Column_Coord loop
         for Y in LED_Row_Coord loop
            if X_Org + X in Width then
               if (Font (C_Index) (Y) and 2**X) /= 0 then
                  Bitmap (X_Org + X, Y) := True;
               end if;
            end if;
         end loop;
      end loop;
   end Put_Char;

   ----------------------
   -- Scroll_Character --
   ----------------------

   procedure Scroll_Character (Char : Character) is
   begin
      --  Insert glyph in the hidden part of the buffer
      Put_Char (5, Char);

      --  Shift the buffer 6 times with a 150 milliseconds delay between each
      --  shifts.
      for Shifts in 1 .. 6 loop
         Shift_Left;
         delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (150);
      end loop;
   end Scroll_Character;

   -----------------
   -- Scroll_Text --
   -----------------

   procedure Scroll_Text (Str : String) is
   begin

      --  Scroll each character of the string
      for Char of Str loop
         Scroll_Character (Char);
      end loop;
   end Scroll_Text;

begin
   Initialize;
end Display;
