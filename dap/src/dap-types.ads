------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

--  Common types that are used for DAP integration

with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded;

with GNATCOLL.VFS;

package DAP.Types is

   type Debugger_Status_Kind is
     (Initialization, Initialized, Ready, Stopped, Running, Terminating);

   type Breakpoint_Identifier is new Integer;
   No_Breakpoint : constant Breakpoint_Identifier := 0;
   --  How breakpoints are identified. Currently, the debuggers supported
   --  by gvd all associate numbers with breakpoints.

   package Breakpoint_Identifier_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Breakpoint_Identifier);
   --  This type is used when doing the same debugger action on a list of
   --  breakpoints (delete/enable/disable).

   package Numbers is new Ada.Containers.Vectors (Positive, Positive);

   type Command_Type is (Internal, Hidden, Visible, User);

   type Endian_Type is (Unknown_Endian, Little_Endian, Big_Endian);

   ---------------
   -- Addresses --
   ---------------

   subtype Address_Range is Integer range 0 .. 20;

   type Address_Type (Last : Address_Range := 0) is record
      Address_String : String (1 .. Last);
      --  The string representing the address

      Length         : Natural := 0;
      --  This is the length of the remaining string once the "0x" prefix as
      --  well as all the following zeros have been stripped.
      --  The meaningful part of Address_String is therefore the one in
      --  the Last - Length + 1 .. Last range.

      Offset         : Integer := 0;
      --  Offset used when the address is used to query the debugger.
   end record;

   function String_To_Address (Address_String : String) return Address_Type;
   --  Given  a string, return the corresponding Address_Type. If string
   --  does not represent a valid address, Invalid_Address is return.

   function Address_To_String (Address : Address_Type) return String;
   --  Return a string representation of Address.

   function Set_Offset
     (Address : Address_Type;
      Offset  : Integer) return Address_Type;

   function Add_Address
     (Address : Address_Type;
      Offset  : Integer) return Address_Type;

   function Address_To_Integer
     (Address : Address_Type) return Long_Long_Integer;

   overriding function "="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function ">"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function ">="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function "<"
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   function "<="
     (Address_1 : Address_Type;
      Address_2 : Address_Type)
      return Boolean;
   --  Arithmetic on addresses

   Invalid_Address : constant Address_Type :=
     (Address_String => "",
      Last           => 0,
      Length         => 0,
      Offset         => 0);

   type Disassemble_Element is record
      Address       : Address_Type := Invalid_Address;
      Method_Offset : Ada.Strings.Unbounded.Unbounded_String;
      Instr         : Ada.Strings.Unbounded.Unbounded_String;
      Opcodes       : Ada.Strings.Unbounded.Unbounded_String;
      File          : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Line          : Natural := 0;
   end record;

   package Disassemble_Element_Vectors is new Ada.Containers.Vectors
     (Positive, Disassemble_Element);

   subtype Disassemble_Elements is Disassemble_Element_Vectors.Vector;

   package Integer_Ordered_Set is new Ada.Containers.Ordered_Sets (Integer);

end DAP.Types;
