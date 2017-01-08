------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--          Copyright (C) 2014-2017, Free Software Foundation, Inc.         --
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

with Ada.Interrupts.Names;
with Ada.Real_Time; use Ada.Real_Time;
with Registers;     use Registers;
with STM32F4;       use STM32F4;
with STM32F4.GPIO;  use STM32F4.GPIO;

package body Button is

   protected Button is
      pragma Interrupt_Priority;

      function Current_Direction return Directions;

   private
      procedure Interrupt_Handler;
      pragma Attach_Handler
         (Interrupt_Handler,
          Ada.Interrupts.Names.EXTI0_Interrupt);

      Direction : Directions := Clockwise;  -- arbitrary
      Last_Time : Time := Clock;
   end Button;

   Debounce_Time : constant Time_Span := Milliseconds (500);

   protected body Button is

      function Current_Direction return Directions is
      begin
         return Direction;
      end Current_Direction;

      procedure Interrupt_Handler is
         Now : constant Time := Clock;
      begin
         --  Clear interrupt
         EXTI.PR (0) := 1;

         --  Debouncing
         if Now - Last_Time >= Debounce_Time then
            if Direction = Counterclockwise then
               Direction := Clockwise;
            else
               Direction := Counterclockwise;
            end if;

            Last_Time := Now;
         end if;
      end Interrupt_Handler;

   end Button;

   function Current_Direction return Directions is
   begin
      return Button.Current_Direction;
   end Current_Direction;

   procedure Initialize is
      RCC_AHB1ENR_GPIOA  : constant Word := 16#01#;
      RCC_APB2ENR_SYSCFG : constant Word := 2**14;
   begin
      --  Enable clock for GPIO-A
      RCC.AHB1ENR := RCC.AHB1ENR or RCC_AHB1ENR_GPIOA;

      --  Configure PA0
      GPIOA.MODER (0) := Mode_IN;
      GPIOA.PUPDR (0) := No_Pull;

      --  Enable clock for SYSCFG
      RCC.APB2ENR := RCC.APB2ENR or RCC_APB2ENR_SYSCFG;

      --  Select PA for EXTI0
      SYSCFG.EXTICR1 (0) := 0;

      --  Interrupt on rising edge
      EXTI.FTSR (0) := 0;
      EXTI.RTSR (0) := 1;
      EXTI.IMR (0) := 1;
   end Initialize;

begin
   Initialize;
end Button;
