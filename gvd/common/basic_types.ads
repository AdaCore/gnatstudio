-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package Basic_Types is

   subtype Pixmap_Array is Interfaces.C.Strings.chars_ptr_array (0 .. 0);
   type Pixmap_Access is access all Pixmap_Array;

   type String_Access is access all String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type String_Array is array (Natural range <>) of String_Access;

   type String_Array_Access is access all String_Array;

   procedure Free (Ar : in out String_Array);
   --  Free all the strings in the array.

   procedure Free (Ar : in out String_Array_Access);
   --  Free all the strings in the array and the array itself.

   subtype Unchecked_String is String (Positive);

   type Unchecked_String_Access is access all Unchecked_String;
   --  For efficiency reasons, use this type compatible with C char*,
   --  so that C strings can be reused without making extra copies.

   function To_Unchecked_String is new Ada.Unchecked_Conversion
     (System.Address, Unchecked_String_Access);

   function To_Unchecked_String is new Ada.Unchecked_Conversion
     (Interfaces.C.Strings.chars_ptr, Unchecked_String_Access);

   type Position_Type is new Natural;
   --  Indicates the position in a file.
   --  Note that these positions are relative to the real contents of the
   --  editor, not necessarily the positions visible to the user (which
   --  might be different because of ASCII.HT handling)

   -----------------
   -- File caches --
   -----------------

   type Packed_Boolean_Array is array (Positive range <>) of Boolean;
   pragma Pack (Packed_Boolean_Array);
   type Packed_Boolean_Access is access Packed_Boolean_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Packed_Boolean_Array, Packed_Boolean_Access);

   type File_Cache;
   type File_Cache_List is access File_Cache;
   type File_Cache is record
      File_Name     : String_Access := null;
      --  The full name (including directory) for the file associated with
      --  this record.

      Line_Has_Code : Packed_Boolean_Access := null;
      Line_Parsed   : Packed_Boolean_Access := null;

      File_Contents : String_Access := null;
      --  The contents of the file. To save some memory, this is not allocated
      --  for files that can be found on the local disk. However, it is used
      --  for files that had to be downloaded from a remote machine.

      CR_Stripped : Boolean := False;
      --  True if the carriage return characters were stripped when the file
      --  was read.

      Next : File_Cache_List := null;
      --  Next file in the cache list
   end record;
   --  Data associated with each file, and that contain cached data for the
   --  file.
   --  Line_Parsed indicates whether the line at a given index has been parsed.
   --  This array is freed once the parsing has been finished (and in the
   --  case Current_Line points to the last line with a breakpoint.

   procedure Free is new
     Ada.Unchecked_Deallocation (File_Cache, File_Cache_List);

end Basic_Types;
