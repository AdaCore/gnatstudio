
--  This package provides a set of subprograms for manipulating and parsing
--  strings.

package Odd.Strings is

   procedure Skip_Blanks (Type_Str : String;
                          Index    : in out Natural);
   --  Skip all the blank characters starting from Index.
   --  Index is modified to point to the first non-blank character.

   procedure Skip_Hexa_Digit (Type_Str : String;
                              Index    : in out Natural);
   --  Move Index to the first character that can not be part of an hexadecimal
   --  digit. Note that an hexadecimal digit can optionally start with '0x',
   --  which is the only case where x is recognized as part of the digit.

   procedure Skip_To_Char (Type_Str : String;
                           Index    : in out Natural;
                           Char     : Character);
   --  Skip every character up to the first occurence of Char in the string.

   procedure Skip_To_String (Type_Str  : String;
                             Index     : in out Natural;
                             Substring : String);
   --  Skip every character until an occurence of Substring is found.
   --  Index is set to the first character of the occurence.

   procedure Parse_Num (Type_Str : String;
                        Index    : in out Natural;
                        Result   : out Long_Integer);
   --  Parse the integer found at position Index in Type_Str.
   --  Index is set to the position of the first character that does not
   --  belong to the integer.

   function Looking_At (Type_Str  : String;
                        Index     : Natural;
                        Substring : String)
                       return Boolean;
   --  Return True if the characters starting at Index in Type_Str are
   --  equivalent to Substring.

   procedure Parse_Cst_String (Type_Str : String;
                               Index    : in out Natural;
                               Str      : out String);
   --  Parse the string pointed to by Index, and copy the result in Str.
   --  Index must point to the opening " character, and will be set to
   --  point after the closing " character.
   --  Special characters, as output by gdb (["0a"]) are also interpreted
   --  and converted to the equivalent Character value.
   --  Str must be long enough to contain the string, not check is done.

private
   pragma Inline (Looking_At);
   pragma Inline (Skip_Blanks);
   pragma Inline (Skip_To_Char);
end Odd.Strings;
