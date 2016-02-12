------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

with GNAT.OS_Lib;
with GNAT.Expect;
with GNAT.Regpat;
with GNAT.Strings;
with GNATCOLL.VFS;
with GNATCOLL.Xref;

package Basic_Types is

   subtype Pixmap_Array is Interfaces.C.Strings.chars_ptr_array (0 .. 0);
   type Pixmap_Access is access all Pixmap_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Strings.String_List, GNAT.Strings.String_List_Access);
   --  Free the array, but not the strings it contains.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (GNAT.Regpat.Pattern_Matcher, GNAT.Expect.Pattern_Matcher_Access);

   subtype Unchecked_String is String (Positive);
   pragma Suppress (All_Checks, Unchecked_String);
   --  Do not use this type directly, use Unchecked_String_Access instead.

   type Unchecked_String_Access is access all Unchecked_String;
   --  For efficiency reasons, use this type compatible with C char*,
   --  so that C strings can be reused without making extra copies.

   function To_Unchecked_String is new Ada.Unchecked_Conversion
     (System.Address, Unchecked_String_Access);

   function To_Unchecked_String is new Ada.Unchecked_Conversion
     (Interfaces.C.Strings.chars_ptr, Unchecked_String_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Unchecked_String, Unchecked_String_Access);

   subtype UTF8_String is String;
   subtype UTF8_Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function Is_Equal
     (List1, List2   : GNAT.OS_Lib.Argument_List;
      Case_Sensitive : Boolean := True;
      Ordered        : Boolean := False) return Boolean;
   --  Return True if List1 has the same contents of List2 (no matter the order
   --  of the strings in both arrays).
   --  If Ordered is true, then each item of List1 much match the
   --  corresponoding item of List2

   function Contains
     (List           : GNAT.OS_Lib.Argument_List;
      Str            : String;
      Case_Sensitive : Boolean := True) return Boolean;
   --  Return True if List contains Str

   type Date_Type is record
      Year  : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day   : Ada.Calendar.Day_Number;
   end record;

   Null_Date : constant Date_Type := Date_Type'
     (Year  => Ada.Calendar.Year_Number'First,
      Month => Ada.Calendar.Month_Number'First,
      Day   => Ada.Calendar.Day_Number'First);

   function "<" (Left, Right : Date_Type) return Boolean;
   --  Compares the two dates, return true if left is before right

   function "<=" (Left, Right : Date_Type) return Boolean;
   function ">" (Left, Right : Date_Type) return Boolean;
   function ">=" (Left, Right : Date_Type) return Boolean;

   --------------
   -- Entities --
   --------------

   type File_Error_Reporter_Record is abstract tagged null record;
   type File_Error_Reporter is access all File_Error_Reporter_Record'Class;
   procedure Error
     (Report : in out File_Error_Reporter_Record;
      File   : GNATCOLL.VFS.Virtual_File) is abstract;
   --  Used to report errors while parsing files

   ------------------
   -- Column types --
   ------------------

   subtype Visible_Column_Type is GNATCOLL.Xref.Visible_Column;
   --  Visible_Column_Type correspond to user perception of the columns, ie,
   --  after TAB expansion. The first character in the line has a value of 1.
   --  Columns are counted in terms of UT8 characters.

   type Character_Offset_Type is new Integer;
   --  Character_Offset_Type indicates the number of characters between the
   --  beginning of the line and the character. First character has offset 0.

   type String_Index_Type is new Natural;
   --  String_Index_Type indicates a index in a string, in bytes, starts at 1.

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
      File_Name     : GNAT.Strings.String_Access := null;
      --  The full name (including directory) for the file associated with
      --  this record.

      Line_Has_Code : Packed_Boolean_Access := null;
      Line_Parsed   : Packed_Boolean_Access := null;

      File_Contents : GNAT.Strings.String_Access := null;
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
