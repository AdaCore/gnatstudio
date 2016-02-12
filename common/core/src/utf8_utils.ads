------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

--  This package provides a set of high-level subprograms for handling UTF8
--  encoding

with Basic_Types;    use Basic_Types;
with GNAT.Strings;   use GNAT.Strings;
with GNATCOLL.Iconv; use GNATCOLL.Iconv;

package UTF8_Utils is

   function Unknown_To_UTF8
     (Input   : String;
      Success : access Boolean) return UTF8_String;
   --  Transform a string of unknown encoding to UTF-8.
   --  The heuristics used is the following:
   --    - if S already contains valid UTF-8, assume it is already encoded
   --      in UTF8 (the statistical chances for this are very high)
   --    - if S does not contain valid UTF-8, assume it is encoded using the
   --      locale, and attempt to convert it from the locale to UTF-8.
   --  Success is set to False if the conversion failed.

   function Unknown_To_UTF8
     (Input   : String) return UTF8_String;
   --  Same as above, but return "<could not convert to UTF8>" if the
   --  conversion could not be done.

   procedure Unknown_To_UTF8
     (Input   : String;
      Output  : out String_Access;
      Success : out Boolean);
   --  Same as above, but return Output as Unchecked_String_Access for
   --  efficiency. Output is still in UTF8 format, and the caller is
   --  responsible for freeing it.
   --  In addition, if Input is already a valid UTF8 string, then Output
   --  will be set to null: you should use Input in this case.
   --  If Success is set to False, Output will also be set to null.
   --  Warning: Never reference Output (Output'Range) or Output'Last,
   --  use Output (1 .. Len) and Len instead.

   function UTF8_To_Locale (Input : UTF8_String) return String;
   --  Convert Input to the GPS locale (ie, the contents of the environment
   --  variable CHARSET, defaulting to ISO-8859-1).
   --  If Input could not be converted, Input is returned as-is.

   function Locale_To_UTF8 (Input : String) return UTF8_String;
   --  Convert Input from the GPS locale (ie, the contents of the environment
   --  variable CHARSET, defaulting to ISO-8859-1).
   --  If Input could not be converted, Input is returned as-is.

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Positive) return Positive;
   function UTF8_Next_Char
     (Str : UTF8_Unbounded_String; Index : Positive) return Positive;
   --  Find the start of the next UTF8 character after the Index-th byte.
   --  Index has to be on the start of a character.
   --  Index is set to a value greater than Str'Last if there is no more
   --  character.

   function UTF8_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   --  Find the start of the previous UTF8 character before the Index-th byte.
   --  Index has to be on the start of a character.
   --  Index is set to 0 if there is no more character.

   function UTF8_Get_Char (Input : UTF8_String) return Wide_Wide_Character;
   --  Return first character of UTF8_String

   function Latin_1_To_UTF8 (Input : String) return UTF8_String;
   --  Convert Latin_1 string to UTF-8.

   function Column_To_Index
     (Buffer : UTF8_String; Column : Character_Offset_Type) return Natural;
   --  Return index of first byte of UTF8 character in given Column
   --  Return 0 in Column <= 0. Return Buffer'First when Column = 1

   function UTF8_Length (Item : UTF8_String) return Natural;
   --  Returns number of user visible characters in UTF8 encoded string.

   function Validate (Object : Iconv_T; Input : Byte_Sequence) return Boolean;
   --  Check if convertion of Text is possible using given Object

   function Validate_UTF_8 (Input : Byte_Sequence) return Boolean;
   --  Check whether Input is valid UTF-8

end UTF8_Utils;
