------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package GPS.Kernel.Properties.File_Writer is

   function Constructor
     (Kernel : access Kernel_Handle_Record'Class)
      return GPS.Properties.Writer;
   --  Return a writer suitable for saving to file.

private

   type Key_Name is record
      Key  : Unbounded_String;
      Name : Unbounded_String;
   end record;

   function Hash (Key : Key_Name) return Ada.Containers.Hash_Type;
   --  Hash function suitable for the container below

   package Key_Value is new Ada.Containers.Hashed_Maps
     (Key_Type        => Key_Name,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type File_Writer_Record
     (Kernel : access Kernel_Handle_Record'Class) is new Writer_Record
   with record
      Map : Key_Value.Map;
   end record;
   type File_Writer is access all File_Writer_Record'Class;

   overriding procedure Get_Value
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : out Property_Record'Class;
      Found    : out Boolean);
   --  See inherited documentation

   overriding procedure Get_Values
     (Self     : not null access File_Writer_Record;
      Name     : String;
      Property : in out Property_Record'Class;
      Callback : access procedure
        (Key : String; Property : in out Property_Record'Class));
   --  See inherited documentation

   overriding procedure Insert
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description);
   --  See inherited documentation

   overriding procedure Update
     (Self     : not null access File_Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description);
   --  See inherited documentation

   overriding procedure Remove
     (Self : not null access File_Writer_Record;
      Key  : String;
      Name : String);
   --  See inherited documentation

   overriding procedure Dump_Database
     (Self   : not null access File_Writer_Record);
   --  See inherited documentation

   overriding procedure Finalize (Self : in out File_Writer_Record);
   --  See inherited documentation

end GPS.Kernel.Properties.File_Writer;
