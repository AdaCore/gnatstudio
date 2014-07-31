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

--  This file provides register declarations for those actually used by this
--  demonstration, for the STM32F4 (ARM Cortex M4F) microcontrollers from
--  ST Microelectronics.

with System;
with STM32F4;                     use STM32F4;
with STM32F4.GPIO;                use STM32F4.GPIO;
with STM32F4.Reset_Clock_Control; use STM32F4.Reset_Clock_Control;
with STM32F4.SYSCONFIG_Control;   use STM32F4.SYSCONFIG_Control;

package Registers is

   pragma Warnings (Off, "*may call Last_Chance_Handler");
   pragma Warnings (Off, "*may be incompatible with alignment of object");

   RCC : RCC_Register with
     Volatile,
     Address => System'To_Address (RCC_Base);

   GPIOA : GPIO_Register with
     Volatile,
     Address => System'To_Address (GPIOA_Base);
   pragma Import (Ada, GPIOA);

   GPIOD : GPIO_Register with
     Volatile,
     Address => System'To_Address (GPIOD_Base);
   pragma Import (Ada, GPIOD);

   EXTI : EXTI_Register with
     Volatile,
     Address => System'To_Address (EXTI_Base);
   pragma Import (Ada, EXTI);

   SYSCFG : SYSCFG_Register with
     Volatile,
     Address => System'To_Address (SYSCFG_Base);
   pragma Import (Ada, SYSCFG);

   pragma Warnings (On, "*may call Last_Chance_Handler");
   pragma Warnings (On, "*may be incompatible with alignment of object");

end Registers;
