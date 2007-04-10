-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  This package provides a set of subprograms for manipulating and parsing
--  strings.

with GNAT.Strings;
with Interfaces.C.Strings;
with Glib;

package String_Utils is

   function Hex_Value (Hex : String) return Natural;
   --  Return the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   procedure Skip_Blanks
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1);
   --  Skip all the blank characters (' ', HT, LF, CR) starting from Index.
   --  Index is modified to point to the first non-blank character.
   --  Step should be either 1 or -1, depending on whether you want to search
   --  forward or backward.

   procedure Skip_To_Blank
     (Type_Str : String;
      Index    : in out Natural);
   --  Skip to the next blank character

   procedure Skip_Lines
     (Buffer        : String;
      Lines         : Integer;
      Index         : in out Natural;
      Lines_Skipped : out Natural);
   --  Skip Lines forward or backward. Index is set to the beginning of a line.
   --  Lines_Skipped is the number of lines that have actually been skipped.

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
   --  Skip every character up to the first occurence of Char in the string.
   --  If no occurrence found, then Index is set over Type_Str'Last.

   procedure Skip_To_String
     (Type_Str  : String;
      Index     : in out Natural;
      Substring : String);
   --  Skip every character until an occurence of Substring is found.
   --  Index is set to the first character of the occurence.

   procedure Skip_Word
     (Type_Str : String;
      Index    : in out Natural;
      Step     : Integer := 1);
   --  Skip the word starting at Index (at least one character, even if there
   --  is no word).
   --  Currently, a word is defined as any string made of alphanumeric
   --  character or underscore.

   function Lines_Count (Text : String) return Natural;
   --  Return the number of lines in Text

   function Blank_Slice
     (Count     : Integer;
      Use_Tabs  : Boolean := False;
      Tab_Width : Natural := 8) return String;
   --  Return a string representing count blanks.
   --  If Use_Tabs is True, use ASCII.HT characters as much as possible,
   --  otherwise use only spaces.
   --  Return a null string is Count is negative.

   function Is_Blank (C : Character) return Boolean;
   --  Return True if C is a blank character: CR, LF, HT or ' '

   function Is_Blank_Line (Buffer : String; Index : Natural) return Boolean;
   --  Return True if the line pointed by Index only contains blank characters
   --  (' ', HT, LF, CR).

   function Line_Start (Buffer : String; P : Natural) return Natural;
   --  Return the start of the line pointed by P.

   function Line_End (Buffer : String; P : Natural) return Natural;
   --  Return the end of the line pointed by P.

   function Next_Line (Buffer : String; P : Natural) return Natural;
   --  Return the start of the next line or Buffer'Last if the end of the
   --  buffer is reached.
   --  Consider using procedure Next_Line below instead.

   procedure Next_Line
     (Buffer  : String;
      P       : Natural;
      Next    : out Natural;
      Success : out Boolean);
   --  Return the start of the next line in Next or Buffer'Last if the end of
   --  the buffer is reached.
   --  Success is set to True if a new line was found, false otherwise (end of
   --  buffer reached).

   function Previous_Line (Buffer : String; P : Natural) return Natural;
   --  Return the start of the previous line or Buffer'First if P already
   --  points to the first line of Buffer.

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
     (Type_Str : String;
      Index    : in out Natural;
      Str      : out String;
      Str_Last : out Natural;
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

   function Reduce (S : String) return String;
   --  Replace in string S all ASCII.LF and ASCII.HT characters with a space,
   --  and replace multiple spaces with a single one.
   --  Return the resulting string.

   function Krunch
     (S                 : String;
      Max_String_Length : Positive := 20) return String;
   --  If String is less than Max_String_Length characters long, return it,
   --  otherwise return a krunched string no longer than Max_String_Length.

   function Strip_Character (Text : String; C : Character) return String;
   --  Return a version of Text after stripping all C's from the string.

   function Strip_CR (Text : String) return String;
   pragma Inline (Strip_CR);
   --  Return a version of Text after stripping all the CR from the string.
   --  This function is used on Windows or when the Strip_CR preference is
   --  enabled (for systems that share dos files).

   procedure Strip_CR
     (Text     : in out String;
      Last     : out Integer;
      CR_Found : out Boolean);
   --  Same as above, but works on Text, and more efficient
   --  Text (Text'First .. Last) contains the new result.
   --  CR_Found is set to True if a CR was found in Text.

   function Do_Tab_Expansion
     (Text : String; Tab_Size : Integer) return String;
   --  Return a version of Text after all tabs have been correctly expanded
   --  depending on the value of Tab_Size.
   --  This function works correctly with multiple-line strings.

   function Strip_Quotes (S : String) return String;
   --  Remove the quotes and the spaces at the beginning and end of S.

   function Image (N : Integer) return String;
   --  Create a string image of the given Integer.

   function Image (N : Integer; Length : Positive) return String;
   --  Create a string image of the given Integer.
   --  The returned string is padded with leading spaces to create a string of
   --  at least length characters.

   function Safe_Value (S : String; Default : Integer := 1) return Integer;
   --  Convert S to a Natural, making sure there is no exception raised.
   --  If S doesn't contain a valid number, Default is returned.

   function Number_Of_Digits (N : Integer) return Natural;
   --  Return the number of digits for the given Integer number;

   function Is_Entity_Letter (Char : Glib.Gunichar) return Boolean;
   pragma Inline (Is_Entity_Letter);
   --  Return True if the given letter is a valid letter for an entity name
   --  (ie if the letter is either alphanumeric or an '_').

   function Is_Operator_Letter (Char : Glib.Gunichar) return Boolean;
   --  Return True if the given letter is a valid operator.

   function Equal (S1, S2 : String; Case_Sensitive : Boolean) return Boolean;
   pragma Inline (Equal);
   --  Compare two strings

   -------------------
   -- Argument_List --
   -------------------

   function Argument_List_To_String
     (List           : GNAT.Strings.String_List;
      Protect_Quotes : Boolean := True) return String;
   --  Concatenate all the elements in List into a single string.
   --    Argument_String_To_List (Argument_List_To_String (X)) = X
   --  The returned string ends with a space.
   --  If Protect_Quotes is True, then all quotes (single and double) are
   --  preceded by a backslash.

   function Clone (List : GNAT.Strings.String_List)
      return GNAT.Strings.String_List;
   --  Return a deep-copy of List. The returned value must be freed by the
   --  caller.

   procedure Append
     (List  : in out GNAT.Strings.String_List_Access;
      List2 : GNAT.Strings.String_List);
   --  Append all the strings in List2 to the end of List.
   --  The strings in List2 are not duplicated.
   --  List might be null initially.

   ----------------------------
   -- Substring substitution --
   ----------------------------

   type Substitution_Value is record
      Name  : GNAT.Strings.String_Access;
      Value : GNAT.Strings.String_Access;
   end record;
   type Substitution_Array is array (Natural range <>) of Substitution_Value;
   --  ??? Should have a second version where Value is replaced with a function
   --  that returns the substitution:

   --  type Substitution_Function is access
   --     function (Substring : String) return String;
   --  Return the string that should replace substring.
   --  Raise No_Replacement if the substitution couldn't be done in this
   --  context.
   --  For each substring, the callback function is called to get the
   --  replacement. The caller should cache the result in most cases, but this
   --  interface allows the lazy compution of substrings, in case some of these
   --  are expansive to compute.

   procedure Free (Substrings : in out Substitution_Array);
   --  Free the memory occupied by the array

   function Substitute
     (Str               : String;
      Substitution_Char : Character;
      Substrings        : Substitution_Array;
      Recursive         : Boolean := False) return String;
   --  Replace all substrings in Str that start with Substition_Char, followed
   --  by one of the names described in Substrings.
   --  The first substitution that matches is used. For instance:
   --     Substitution_Char := '%'
   --     Substrings (1) := (Name => "a",  Value => "1")
   --     Substrings (2) := (Name => "ab", Value => "2")
   --
   --     Str := "d%abc" results in "d1bc", not "d2c"
   --
   --
   --  If Recursive is true, then this function will also substitute substrings
   --  in the values specified in Substrings (for instance:
   --      Substition_Char := %
   --      Substrings (1) := (Name => "a", Value => "c%b")
   --      Substrings (2) := (Name => "b", Value => "d")
   --
   --      Str := "%a"   results in   "cd"  if Recursive is True
   --                    results in   "c%b" otherwise

   type Substitute_Callback is access
     function (Param : String; Quoted : Boolean) return String;
   --  A callback for Substitute below. It is called once for each '%...'
   --  parameter found in the string. Param doesn't include the leading '%'
   --  sign.
   --  Quoted indicate whether the parameter was quoted, ie the '%...' was
   --  surrounded by quotes.
   --  Should raise Invalid_Substitution if Param cannot be substituted

   function Substitute
     (Str               : String;
      Substitution_Char : Character;
      Callback          : Substitute_Callback;
      Recursive         : Boolean := False) return String;
   --  Same as Substitute above, except with a callback.
   --  Callback will be passed the number or string following the substitution
   --  character, followed by any '-' or '*' symbol found. As a result, with
   --  the following input, callback will be called with:
   --      "a %1bd() "    =>  callback receives "1bd"
   --      "a %1--   "    =>  callback receives "1--"

   Invalid_Substitution : exception;

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

   function Argument_List_To_Quoted_String
     (Args            : GNAT.Strings.String_List;
      Quote           : Character := '"';
      Quote_Backslash : Boolean := True) return String;
   --  Return the arguments as a full string.
   --  Arguments that contain spaces but do not already contain quotes
   --  will be put into quotes.
   --  Backslashes are duplicated if Quote_Baskslash is True.
   --  The result of this subprogram on the string     A simple\ "string"
   --  is:     Quote_Backslash =>   "A simple\\ \"string\""
   --      not Quote_Backslash =>   "A simple\ \"string\""

   function Argument_String_To_List_With_Triple_Quotes
     (Arg_String : String) return GNAT.Strings.String_List_Access;
   --  This is similar to GNAT.OS_Lib.Argument_String_To_List, except that
   --  if part of the string is surrounded by triple quotes, any special
   --  character is ignored till the closing triple quotes. This is the same
   --  behavior as in Python, and is needed for easier quoting of string.
   --
   --  Here is the output in some cases:
   --     "foo"       -> "foo"       (quotes preserved)
   --     """foo"""   -> foo         (quotes removed when at beginning and end)
   --     ("""foo""") -> ("""foo""") (quotes preserved in middle)
   --     foo\"foo    -> foo\"foo    (backslash not removed from output)

   function Protect
     (S : String; Protect_Quotes : Boolean := True) return String;
   --  Escape special characters in S.
   --  Quotes are only escaped with Protect_Quotes is true

   function Unprotect (S : String) return String;
   --  Unprotect an argument: remove the leading and ending '"',
   --  and un-escape the "\" when necessary.

   function Unquote (S : String) return String;
   --  Remove the leading and ending '"' if present.

   ----------------------
   -- URL manipulation --
   ----------------------

   function URL_Decode (URL : String) return String;
   --  Decode URL into a regular string. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

private
   pragma Inline (Is_Blank);
   pragma Inline (Looking_At);
   pragma Inline (Skip_Blanks);
   pragma Inline (Skip_To_Char);
   pragma Inline (Copy_String);
end String_Utils;
