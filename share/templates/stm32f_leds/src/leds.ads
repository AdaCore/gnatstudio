------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides declarations for the user LEDs on the STM32F4 Discovery
--  board from ST Microelectronics.

with STM32F4;  use STM32F4;

package LEDs is
   pragma Elaborate_Body;

   type User_LED is (Green, Orange, Red, Blue);

   for User_LED use
     (Green  => 16#1000#,
      Orange => 16#2000#,
      Red    => 16#4000#,
      Blue   => 16#8000#);

   --  As a result of the representation clause, avoid iterating directly over
   --  the type since that will require an implicit lookup in the generated
   --  code of the loop.  Such usage seems unlikely so this direct
   --  representation is reasonable, and efficient.

   for User_LED'Size use Word'Size;
   --  we convert the LED values to Word values in order to write them to
   --  the register, so the size must be the same

   LED3 : User_LED renames Orange;
   LED4 : User_LED renames Green;
   LED5 : User_LED renames Red;
   LED6 : User_LED renames Blue;

   procedure On  (This : User_LED) with Inline;
   procedure Off (This : User_LED) with Inline;

   procedure All_Off with Inline;
   procedure All_On  with Inline;

end LEDs;
