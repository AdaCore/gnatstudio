------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

--  This package provides a set of subprograms for manipulating and parsing
--  strings.

with GNAT.Strings;
with Interfaces.C.Strings;
with Basic_Types; use Basic_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_Utils is

   function "+"
     (S : String) return Unbounded_String renames To_Unbounded_String;

   function "+"
     (S : Unbounded_String) return String renames To_String;
   --  Utility conversion operators from String to Unbounded_String

   function Hex_Value (Hex : String) return Natural;
   --  Return the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   procedure Skip_To_Blank
     (Type_Str : String;
      Index    : in out Natural);
   --  Skip to the next blank character

   procedure Skip_To_Index
     (Buffer        : String;
      Columns       : out Visible_Column_Type;
      Index_In_Line : String_Index_Type;
      Index         : in out String_Index_Type;
      Tab_Width     : Positive := 8);
   procedure Skip_To_Index
     (Buffer        : Unbounded_String;
      Columns       : out Visible_Column_Type;
      Index_In_Line : String_Index_Type;
      Index         : in out String_Index_Type;
      Tab_Width     : Positive := 8);
   --  Assuming Index points to the begining of a line, move the index by
   --  "Index_In_Line" characters, and give the new column value.

   procedure Skip_Hexa_Digit
     (Type_Str : String;
      Index    : in out Natural);
   --  Move Index to the first character that can not be part of an hexadecimal
   --  digit. Note that an hexadecimal digit can optionally start with '0x',
   --  which is the only case where x is recognized as part of the digit.

   procedure Skip_To_Char
     (Type_Str : String;
      Index    : in out Natural;
      Char     : Character;
      Step     : Integer := 1);
   procedure Skip_To_Char
     (Type_Str : Unbounded_String;
      Index    : in out Natural;
      Char     : Character;
      Step     : Integer := 1);
   --  Skip every character up to the first occurence of Char in the string.
   --  If no occurrence found, then Index is set over Type_Str'Last.

   procedure Skip_Word
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1);
   --  Skip the word starting at Index (at least one character, even if there
   --  is no word).
   --  Currently, a word is defined as any string made of alphanumeric
   --  character or underscore.

   procedure Skip_CPP_Token
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1);
   --  Skip the cpp token starting at Index (at least one character, even if
   --  there is no cpp token).
   --  Currently, a cpp token is defined as any string made of alphanumeric
   --  character, underscore or period.

   function Tab_Width return Positive;
   pragma Inline (Tab_Width);
   --  Default value of tab width in the text editor (current value is 8)

   function Lines_Count (Text : String) return Natural;
   --  Return the number of lines in Text

   function Blank_Slice
     (Count     : Integer;
      Use_Tabs  : Boolean := False;
      Tab_Width : Positive := 8) return String;
   --  Return a string representing count blanks.
   --  If Use_Tabs is True, use ASCII.HT characters as much as possible,
   --  otherwise use only spaces.
   --  Return a null string if Count is negative.

   function Is_Blank (C : Character) return Boolean;
   --  Return True if C is a blank character: CR, LF, HT or ' '

   procedure Next_Line
     (Buffer  : String;
      P       : Natural;
      Next    : out Natural;
      Success : out Boolean);
   --  Return the start of the next line in Next or Buffer'Last if the end of
   --  the buffer is reached.
   --  Success is set to True if a new line was found, false otherwise (end of
   --  buffer reached).

   procedure Parse_Num
     (Type_Str : String;
      Index    : in out Natural;
      Result   : out Long_Integer);
   --  Parse the integer found at position Index in Type_Str.
   --  Index is set to the position of the first character that does not
   --  belong to the integer.

   function Looking_At
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Boolean;
   --  Return True if the characters starting at Index in Type_Str are
   --  equivalent to Substring.

   procedure Parse_Cst_String
     (Type_Str          : String;
      Index             : in out Natural;
      Str               : out String;
      Str_Last          : out Natural;
      Backslash_Special : Boolean := True);
   --  Parse the string pointed to by Index, and copy the result in Str.
   --  Index must point to the opening " character, and will be set to
   --  point after the closing " character.
   --  Special characters, as output by gdb (["0a"]) are also interpreted
   --  and converted to the equivalent Character value.
   --  Str must be long enough to contain the string, not check is done. As a
   --  special case, if Str'Length = 0 then no attempt is done to fill up
   --  the string, and only Length is computed. Str_Last is set to the last
   --  meaningful character in Str.
   --
   --  Index is set to the number of characters parsed in the string.

   procedure Skip_Simple_Value
     (Type_Str             : String;
      Index                : in out Natural;
      Array_Item_Separator : Character := ',';
      End_Of_Array         : Character := ')';
      Repeat_Item_Start    : Character := '<');
   --  Skip the value of a simple value ("65 'A'" for instance).
   --  This stops at the first special character.
   --
   --  Array_Item_Separator is the separator in an array value (ie "5, 2, 3").
   --  End_Of_Array is the array that indicates the end of an array value, as
   --  in "((1, 2), (3, 4))".
   --  Repeat_Item_Start if the character that starts a repeat statements, as
   --  in "<repeats .. times>"

   function Reduce
     (S            : String;
      Max_Length   : Positive := Positive'Last;
      Continuation : String := "...") return String;
   --  Replace in string S all ASCII.LF and ASCII.HT characters with a space,
   --  and replace multiple spaces with a single one. Return the resulting
   --  string with at most Max_Length character including the continuation
   --  characters. S should be encoded in UTF-8.

   function Krunch
     (S                 : String;
      Max_String_Length : Positive := 20) return String;
   --  If String is less than Max_String_Length characters long, return it,
   --  otherwise return a krunched string no longer than Max_String_Length.

   procedure Strip_CR
     (Text     : in out String;
      Last     : out Integer;
      CR_Found : out Boolean);
   --  Same as above, but works on Text, and more efficient
   --  Text (Text'First .. Last) contains the new result.
   --  CR_Found is set to True if a CR was found in Text.

   procedure Strip_CR_And_NUL
     (Text      : in out String;
      Last      : out Integer;
      CR_Found  : out Boolean;
      NUL_Found : out Boolean;
      Trailing_Space_Found : out Boolean);
   --  Same as Strip_CR, and strip also ASCII.NUL characters
   --  Note that CR chars alone are not replaced by LF chars.
   --  Also check if Text has trailing spaces in a line.

   function Strip_Ending_Linebreaks (Text : String) return String;
   --  Return a version of Text after stripping all ending CR and LF
   --  characters.

   function Do_Tab_Expansion
     (Text : String; Tab_Size : Integer) return String;
   --  Return a version of Text after all tabs have been correctly expanded
   --  depending on the value of Tab_Size.
   --  This function works correctly with multiple-line strings.

   function Strip_Quotes (S : String) return String;
   --  Remove the quotes and the spaces at the beginning and end of S

   function Strip_Single_Underscores (S : String) return String;
   --  Return S stripped of single underscores, and with multiple underscores
   --  concatenated into one.
   --  This is used to process menu shortcuts, for example
   --    Strip_Single_Underscores ("/_Project/C_lean") returns "/Project/Clean"

   function Image (N : Integer) return String;
   --  Create a string image of the given Integer

   function Safe_Value (S : String; Default : Integer := 1) return Integer;
   --  Convert S to a Natural, making sure there is no exception raised.
   --  If S doesn't contain a valid number, Default is returned.

   function Number_Of_Digits (N : Integer) return Natural;
   --  Return the number of digits for the given Integer number;

   function Is_Entity_Letter (Char : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_Entity_Letter);
   --  Return True if the given letter is a valid letter for an entity name
   --  (ie if the letter is either alphanumeric or an '_').

   function Is_Operator_Letter (Char : Wide_Wide_Character) return Boolean;
   --  Return True if the given letter is a valid operator

   function Is_File_Letter (Char : Wide_Wide_Character) return Boolean;
   pragma Inline (Is_File_Letter);
   --  Return True if the given letter is a valid letter for a file name.

   procedure Replace
     (S     : in out GNAT.Strings.String_Access;
      Value : String);
   --  Set S to Value, free previous content if any

   procedure Replace
     (S     : in out GNAT.Strings.String_Access;
      Value : GNAT.Strings.String_Access);
   --  Idem, but does nothing if Value is null

   function Has_Include_Directive (Str : String) return Boolean;
   --  Return True is Str contains an #include directive

   -------------------
   -- Argument_List --
   -------------------

   function Clone
     (List : GNAT.Strings.String_List) return GNAT.Strings.String_List;
   --  Return a deep-copy of List. The returned value must be freed by the
   --  caller.

   procedure Append
     (List  : in out GNAT.Strings.String_List_Access;
      List2 : GNAT.Strings.String_List);
   procedure Append
     (List  : in out GNAT.Strings.String_List_Access;
      Item  : String);
   --  Append all the strings in List2 to the end of List.
   --  The strings in List2 are not duplicated.
   --  List might be null initially.

   ---------------------------
   -- C String manipulation --
   ---------------------------

   procedure Copy_String
     (Item : Interfaces.C.Strings.chars_ptr;
      Str  : out String;
      Len  : Natural);
   --  Copy Len characters from Item to Str

   ----------------------------
   -- Arguments manipulation --
   ----------------------------

   function Protect
     (S                   : String;
      Protect_Quotes      : Boolean := True;
      Protect_Spaces      : Boolean := False;
      Protect_Backslashes : Boolean := True) return String;
   --  Escape special characters in S.
   --  Quotes are only escaped when Protect_Quotes is true.
   --  Spaces are only escaped when Protect_Spaces is true.

   function Unprotect (S : String) return String;
   --  Unprotect an argument: remove the leading and ending '"',
   --  and un-escape the "\" when necessary.

   function Unquote (S : String) return String;
   --  Remove the leading and ending '"' if present

   function Revert (S : String; Separator : String := ".") return String;
   --  Given a string S composed of names separated with Separator
   --  (e.g. Put_Line.Text_IO.Ada), return the names reversed
   --  (e.g. Ada.Text_IO.Put_Line).

   ----------------------
   -- URL manipulation --
   ----------------------

   function URL_Decode (URL : String) return String;
   --  Decode URL into a regular string. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

   function Compare (A, B : String) return Integer;
   function Compare (A, B : Integer) return Integer;
   --  Return -1 if A<B, 1 if A>B and 0 otherwise. This routine is useful for
   --  model specific sorting. The second version does the same comparing
   --  integers. Even if not using string, it is better to keep this routine
   --  next to the compare based on strings.

   ----------
   -- Hash --
   ----------

   generic
      type Header_Num is range <>;
   function Hash (Key : String) return Header_Num;
   --  A generic hashing function working on String keys

   generic
      type Header_Num is range <>;
   function Case_Insensitive_Hash (Key : String) return Header_Num;
   --  A generic hashing function working on case insensitive String keys

private
   pragma Inline (Is_Blank);
   pragma Inline (Looking_At);
   pragma Inline (Skip_To_Char);
   pragma Inline (Copy_String);
   pragma Inline (Replace);
   pragma Inline (Compare);
end String_Utils;
