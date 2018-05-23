--  Copyright (c) 2013, Nordic Semiconductor ASA
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice, this
--  list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--  this list of conditions and the following disclaimer in the documentation
--  and/or other materials provided with the distribution.
--
--  * Neither the name of Nordic Semiconductor ASA nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
--  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
--  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
--  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

--  This spec has been automatically generated from nrf51.svd

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2012;
pragma Style_Checks (Off);

with HAL;
with System;

package NRF51_SVD.GPIO is
   pragma Preelaborate;

   ---------------
   -- Registers --
   ---------------

   --  Pin 0.
   type OUT_PIN0_Field is
     (
      --  Pin driver is low.
      Low,
      --  Pin driver is high.
      High)
     with Size => 1;
   for OUT_PIN0_Field use
     (Low => 0,
      High => 1);

   --  OUT_PIN array
   type OUT_PIN_Field_Array is array (0 .. 31) of OUT_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Write GPIO port.
   type OUT_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUT_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for OUT_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0.
   type OUTSET_PIN0_Field is
     (
      --  Pin driver is low.
      Low,
      --  Pin driver is high.
      High)
     with Size => 1;
   for OUTSET_PIN0_Field use
     (Low => 0,
      High => 1);

   --  Pin 0.
   type OUTSET_PIN0_Field_1 is
     (
      --  Reset value for the field
      Outset_Pin0_Field_Reset,
      --  Set pin driver high.
      Set)
     with Size => 1;
   for OUTSET_PIN0_Field_1 use
     (Outset_Pin0_Field_Reset => 0,
      Set => 1);

   --  OUTSET_PIN array
   type OUTSET_PIN_Field_Array is array (0 .. 31) of OUTSET_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  Set individual bits in GPIO port.
   type OUTSET_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUTSET_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for OUTSET_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0.
   type OUTCLR_PIN0_Field is
     (
      --  Pin driver is low.
      Low,
      --  Pin driver is high.
      High)
     with Size => 1;
   for OUTCLR_PIN0_Field use
     (Low => 0,
      High => 1);

   --  Pin 0.
   type OUTCLR_PIN0_Field_1 is
     (
      --  Reset value for the field
      Outclr_Pin0_Field_Reset,
      --  Set pin driver low.
      Clear)
     with Size => 1;
   for OUTCLR_PIN0_Field_1 use
     (Outclr_Pin0_Field_Reset => 0,
      Clear => 1);

   --  OUTCLR_PIN array
   type OUTCLR_PIN_Field_Array is array (0 .. 31) of OUTCLR_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  Clear individual bits in GPIO port.
   type OUTCLR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : OUTCLR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for OUTCLR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0.
   type IN_PIN0_Field is
     (
      --  Pin input is low.
      Low,
      --  Pin input is high.
      High)
     with Size => 1;
   for IN_PIN0_Field use
     (Low => 0,
      High => 1);

   --  IN_PIN array
   type IN_PIN_Field_Array is array (0 .. 31) of IN_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Read GPIO port.
   type IN_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : IN_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for IN_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin 0.
   type DIR_PIN0_Field is
     (
      --  Pin set as input.
      Input,
      --  Pin set as output.
      Output)
     with Size => 1;
   for DIR_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  DIR_PIN array
   type DIR_PIN_Field_Array is array (0 .. 31) of DIR_PIN0_Field
     with Component_Size => 1, Size => 32;

   --  Direction of GPIO pins.
   type DIR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DIR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Set as output pin 0.
   type DIRSET_PIN0_Field is
     (
      --  Pin set as input.
      Input,
      --  Pin set as output.
      Output)
     with Size => 1;
   for DIRSET_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  Set as output pin 0.
   type DIRSET_PIN0_Field_1 is
     (
      --  Reset value for the field
      Dirset_Pin0_Field_Reset,
      --  Set pin as output.
      Set)
     with Size => 1;
   for DIRSET_PIN0_Field_1 use
     (Dirset_Pin0_Field_Reset => 0,
      Set => 1);

   --  DIRSET_PIN array
   type DIRSET_PIN_Field_Array is array (0 .. 31) of DIRSET_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  DIR set register.
   type DIRSET_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIRSET_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DIRSET_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Set as input pin 0.
   type DIRCLR_PIN0_Field is
     (
      --  Pin set as input.
      Input,
      --  Pin set as output.
      Output)
     with Size => 1;
   for DIRCLR_PIN0_Field use
     (Input => 0,
      Output => 1);

   --  Set as input pin 0.
   type DIRCLR_PIN0_Field_1 is
     (
      --  Reset value for the field
      Dirclr_Pin0_Field_Reset,
      --  Set pin as input.
      Clear)
     with Size => 1;
   for DIRCLR_PIN0_Field_1 use
     (Dirclr_Pin0_Field_Reset => 0,
      Clear => 1);

   --  DIRCLR_PIN array
   type DIRCLR_PIN_Field_Array is array (0 .. 31) of DIRCLR_PIN0_Field_1
     with Component_Size => 1, Size => 32;

   --  DIR clear register.
   type DIRCLR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PIN as a value
            Val : HAL.UInt32;
         when True =>
            --  PIN as an array
            Arr : DIRCLR_PIN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access,
          Bit_Order => System.Low_Order_First;

   for DIRCLR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  Pin direction.
   type PIN_CNF_DIR_Field is
     (
      --  Configure pin as an input pin.
      Input,
      --  Configure pin as an output pin.
      Output)
     with Size => 1;
   for PIN_CNF_DIR_Field use
     (Input => 0,
      Output => 1);

   --  Connect or disconnect input path.
   type PIN_CNF_INPUT_Field is
     (
      --  Connect input pin.
      Connect,
      --  Disconnect input pin.
      Disconnect)
     with Size => 1;
   for PIN_CNF_INPUT_Field use
     (Connect => 0,
      Disconnect => 1);

   --  Pull-up or -down configuration.
   type PIN_CNF_PULL_Field is
     (
      --  No pull.
      Disabled,
      --  Pulldown on pin.
      Pulldown,
      --  Pullup on pin.
      Pullup)
     with Size => 2;
   for PIN_CNF_PULL_Field use
     (Disabled => 0,
      Pulldown => 1,
      Pullup => 3);

   --  Drive configuration.
   type PIN_CNF_DRIVE_Field is
     (
      --  Standard '0', Standard '1'.
      S0S1,
      --  High '0', Standard '1'.
      H0S1,
      --  Standard '0', High '1'.
      S0H1,
      --  High '0', High '1'.
      H0H1,
      --  Disconnected '0', Standard '1'.
      D0S1,
      --  Disconnected '0', High '1'.
      D0H1,
      --  Standard '0', Disconnected '1'.
      S0D1,
      --  High '0', Disconnected '1'.
      H0D1)
     with Size => 3;
   for PIN_CNF_DRIVE_Field use
     (S0S1 => 0,
      H0S1 => 1,
      S0H1 => 2,
      H0H1 => 3,
      D0S1 => 4,
      D0H1 => 5,
      S0D1 => 6,
      H0D1 => 7);

   --  Pin sensing mechanism.
   type PIN_CNF_SENSE_Field is
     (
      --  Disabled.
      Disabled,
      --  Wakeup on high level.
      High,
      --  Wakeup on low level.
      Low)
     with Size => 2;
   for PIN_CNF_SENSE_Field use
     (Disabled => 0,
      High => 2,
      Low => 3);

   --  Configuration of GPIO pins.
   type PIN_CNF_Register is record
      --  Pin direction.
      DIR            : PIN_CNF_DIR_Field := NRF51_SVD.GPIO.Input;
      --  Connect or disconnect input path.
      INPUT          : PIN_CNF_INPUT_Field := NRF51_SVD.GPIO.Disconnect;
      --  Pull-up or -down configuration.
      PULL           : PIN_CNF_PULL_Field := NRF51_SVD.GPIO.Disabled;
      --  unspecified
      Reserved_4_7   : HAL.UInt4 := 16#0#;
      --  Drive configuration.
      DRIVE          : PIN_CNF_DRIVE_Field := NRF51_SVD.GPIO.S0S1;
      --  unspecified
      Reserved_11_15 : HAL.UInt5 := 16#0#;
      --  Pin sensing mechanism.
      SENSE          : PIN_CNF_SENSE_Field := NRF51_SVD.GPIO.Disabled;
      --  unspecified
      Reserved_18_31 : HAL.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIN_CNF_Register use record
      DIR            at 0 range 0 .. 0;
      INPUT          at 0 range 1 .. 1;
      PULL           at 0 range 2 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      DRIVE          at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      SENSE          at 0 range 16 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Configuration of GPIO pins.
   type PIN_CNF_Registers is array (0 .. 31) of PIN_CNF_Register
     with Volatile;

   -----------------
   -- Peripherals --
   -----------------

   --  General purpose input and output.
   type GPIO_Peripheral is record
      --  Write GPIO port.
      OUT_k   : aliased OUT_Register;
      --  Set individual bits in GPIO port.
      OUTSET  : aliased OUTSET_Register;
      --  Clear individual bits in GPIO port.
      OUTCLR  : aliased OUTCLR_Register;
      --  Read GPIO port.
      IN_k    : aliased IN_Register;
      --  Direction of GPIO pins.
      DIR     : aliased DIR_Register;
      --  DIR set register.
      DIRSET  : aliased DIRSET_Register;
      --  DIR clear register.
      DIRCLR  : aliased DIRCLR_Register;
      --  Configuration of GPIO pins.
      PIN_CNF : aliased PIN_CNF_Registers;
   end record
     with Volatile;

   for GPIO_Peripheral use record
      OUT_k   at 16#504# range 0 .. 31;
      OUTSET  at 16#508# range 0 .. 31;
      OUTCLR  at 16#50C# range 0 .. 31;
      IN_k    at 16#510# range 0 .. 31;
      DIR     at 16#514# range 0 .. 31;
      DIRSET  at 16#518# range 0 .. 31;
      DIRCLR  at 16#51C# range 0 .. 31;
      PIN_CNF at 16#700# range 0 .. 1023;
   end record;

   --  General purpose input and output.
   GPIO_Periph : aliased GPIO_Peripheral
     with Import, Address => System'To_Address (16#50000000#);

end NRF51_SVD.GPIO;
