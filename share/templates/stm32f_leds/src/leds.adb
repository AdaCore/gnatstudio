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

with Ada.Unchecked_Conversion;

with Registers;     use Registers;
with STM32F4.GPIO;  use STM32F4.GPIO;

package body LEDs is

   function As_Word is new Ada.Unchecked_Conversion
     (Source => User_LED, Target => Word);

   procedure On (This : User_LED) is
   begin
      GPIOD.BSRR := As_Word (This);
   end On;

   procedure Off (This : User_LED) is
   begin
      GPIOD.BSRR := Shift_Left (As_Word (This), 16);
   end Off;

   All_LEDs_On  : constant Word := Green'Enum_Rep or Red'Enum_Rep or
                                   Blue'Enum_Rep  or Orange'Enum_Rep;

   pragma Compile_Time_Error
     (All_LEDs_On /= 16#F000#,
      "Invalid representation for All_LEDs_On");

   All_LEDs_Off : constant Word := Shift_Left (All_LEDs_On, 16);

   procedure All_Off is
   begin
      GPIOD.BSRR := All_LEDs_Off;
   end All_Off;

   procedure All_On is
   begin
      GPIOD.BSRR := All_LEDs_On;
   end All_On;

   procedure Initialize is
      RCC_AHB1ENR_GPIOD : constant Word := 16#08#;
   begin
      --  Enable clock for GPIO-D
      RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOD;

      --  Configure PD12-15
      GPIOD.MODER   (12 .. 15) := (others => Mode_OUT);
      GPIOD.OTYPER  (12 .. 15) := (others => Type_PP);
      GPIOD.OSPEEDR (12 .. 15) := (others => Speed_100MHz);
      GPIOD.PUPDR   (12 .. 15) := (others => No_Pull);
   end Initialize;

begin
   Initialize;
end LEDs;
