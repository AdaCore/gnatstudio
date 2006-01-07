-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2005-2006 AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--
--  This package provides functions for handling of unicode characters and
--  utf8 strings. See also Glib.Convert.
--
--  </description>
--  <c_version>2.2.1</c_version>

package Glib.Unicode is
   pragma Preelaborate;

   type G_Unicode_Type is
     (Unicode_Control,
      Unicode_Format,
      Unicode_Unassigned,
      Unicode_Private_Use,
      Unicode_Surrogate,
      Unicode_Lowercase_Letter,
      Unicode_Modifier_Letter,
      Unicode_Other_Letter,
      Unicode_Titlecase_Letter,
      Unicode_Uppercase_Letter,
      Unicode_Combining_Mark,
      Unicode_Enclosing_Mark,
      Unicode_Non_Spacing_Mark,
      Unicode_Decimal_Number,
      Unicode_Letter_Number,
      Unicode_Other_Number,
      Unicode_Connect_Punctuation,
      Unicode_Dash_Punctuation,
      Unicode_Close_Punctuation,
      Unicode_Final_Punctuation,
      Unicode_Initial_Punctuation,
      Unicode_Other_Punctuation,
      Unicode_Open_Punctuation,
      Unicode_Currency_Symbol,
      Unicode_Modifier_Symbol,
      Unicode_Math_Symbol,
      Unicode_Other_Symbol,
      Unicode_Line_Separator,
      Unicode_Paragraph_Separator,
      Unicode_Space_Separator);
   --  The possible character classifications.
   --  See http://www.unicode.org/Public/UNIDATA/UnicodeData.html

   -----------------------------
   -- Manipulating characters --
   -----------------------------

   function Is_Space (Char : Gunichar) return Boolean;
   --  True if Char is a space character

   function Is_Alnum (Char : Gunichar) return Boolean;
   --  True if Char is an alphabetical or numerical character

   function To_Lower (Char : Gunichar) return Gunichar;
   --  Convert Char to lower cases

   function To_Upper (Char : Gunichar) return Gunichar;
   --  Convert Char to upper cases

   function Unichar_Type (Char : Gunichar) return G_Unicode_Type;
   --  Return the unicode character type of a given character. This is a
   --  limited version that handle only Unicode_Lowercase_Letter,
   --  Unicode_Uppercase_Letter. It returns Unicode_Other_Symbol for all other
   --  characters.

   --------------------------
   -- Manipulating strings --
   --------------------------

   function UTF8_Next_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   pragma Inline (UTF8_Next_Char);
   --  Find the start of the next UTF8 character after the Index-th byte.
   --  Index has to be on the start of a character.
   --  Index is set to a value greater than Str'Last if there is no more
   --  character.

   function UTF8_Find_Prev_Char
     (Str : UTF8_String; Index : Natural) return Natural;
   --  Find the start of the previous UTF8 character after the Index-th byte.
   --  Index doesn't need to be on the start of a character.
   --  Index is set to a value smaller than Str'First if there is no
   --  previous character.

   function UTF8_Strdown (Str : UTF8_String) return UTF8_String;
   --  Convert Str to lower cases

   function UTF8_Strup (Str : UTF8_String) return UTF8_String;
   --  Convert Str to upper cases

   -----------------
   -- Conversions --
   -----------------

   function UTF8_Get_Char (Str : UTF8_String) return Gunichar;
   --  Converts a sequence of bytes encoded as UTF8 to a unicode character.
   --  If Str doesn't point to a valid UTF8 encoded character, the result is
   --  undefined.

   procedure Unichar_To_UTF8
     (Char : Gunichar;
      Str  : out String;
      Last : out Natural);
   --  Encode Char into Buffer. Buffer must have at least 6 bytes free.
   --  Return the index of the last byte written in Buffer.

end Glib.Unicode;
