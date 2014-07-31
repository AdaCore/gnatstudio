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

--  This file provides register definitions for the STM32F4 (ARM Cortex M4F)
--  microcontrollers from ST Microelectronics.

pragma Restrictions (No_Elaboration_Code);

package STM32F4.GPIO is

   --  MODER constants
   Mode_IN      : constant Bits_2 := 0;
   Mode_OUT     : constant Bits_2 := 1;
   Mode_AF      : constant Bits_2 := 2;
   Mode_AN      : constant Bits_2 := 3;

   --  OTYPER constants
   Type_PP      : constant Bits_1 := 0; -- Push/pull
   Type_OD      : constant Bits_1 := 1; -- Open drain

   --  OSPEEDR constants
   Speed_2MHz   : constant Bits_2 := 0; -- Low speed
   Speed_25MHz  : constant Bits_2 := 1; -- Medium speed
   Speed_50MHz  : constant Bits_2 := 2; -- Fast speed
   Speed_100MHz : constant Bits_2 := 3; -- High speed on 30pF, 80MHz on 15

   --  PUPDR constants
   No_Pull      : constant Bits_2 := 0;
   Pull_Up      : constant Bits_2 := 1;
   Pull_Down    : constant Bits_2 := 2;

   --  AFL constants
   AF_USART1    : constant Bits_4 := 7;

   --  Reset constants
   GPIOA_Reset       : constant Word := 16#A800_0000#;
   GPIOB_Reset       : constant Word := 16#0000_0280#;
   GPIO_Others_Reset : constant Word := 16#0000_0000#;

   type GPIO_Register is record
      MODER   : Bits_16x2;  --  mode register
      OTYPER  : Bits_32x1;  --  output type register
      OSPEEDR : Bits_16x2;  --  output speed register
      PUPDR   : Bits_16x2;  --  pull-up/pull-down register
      IDR     : Word;       --  input data register
      ODR     : Word;       --  output data register
      BSRR    : Word;       --  bit set/reset register
      LCKR    : Word;       --  configuration lock register
      AFRL    : Bits_8x4;   --  alternate function low register
      AFRH    : Bits_8x4;   --  alternate function high register
   end record;

   for GPIO_Register use record
      MODER   at 0  range 0 .. 31;
      OTYPER  at 4  range 0 .. 31;
      OSPEEDR at 8  range 0 .. 31;
      PUPDR   at 12 range 0 .. 31;
      IDR     at 16 range 0 .. 31;
      ODR     at 20 range 0 .. 31;
      BSRR    at 24 range 0 .. 31;
      LCKR    at 28 range 0 .. 31;
      AFRL    at 32 range 0 .. 31;
      AFRH    at 36 range 0 .. 31;
   end record;

end STM32F4.GPIO;
