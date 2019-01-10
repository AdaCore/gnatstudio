------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

--  This is the general C++ (non debugger specific) support package.
--  See language.ads for a complete spec.

with Language.C;

package Language.Cpp is

   type Cpp_Language is new Language.C.C_Language with private;

   Cpp_Lang : constant Language_Access;
   --  Class constant for the C++ language.

   ------------------
   -- Highlighting --
   ------------------

   overriding function Keywords
     (Lang : access Cpp_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Get_Language_Context
     (Lang : access Cpp_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access Cpp_Language) return Explorer_Categories;

   ----------------------
   -- Source Analyzing --
   ----------------------

   overriding
   procedure Parse_Entities
     (Lang     : access Cpp_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding
   procedure Parse_Tokens_Backwards
     (Lang              : access Cpp_Language;
      Buffer            : UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0;
      Callback          : access procedure (Token : Token_Record;
                                            Stop  : in out Boolean));

   Tok_Unknown          : constant Token_Type := 100;

   --  C++ operators

   Tok_Assign           : constant Token_Type := 101;   --  =
   Tok_Plus             : constant Token_Type := 102;   --  +
   Tok_Minus            : constant Token_Type := 103;   --  -
   Tok_Asterisk         : constant Token_Type := 104;   --  *
   Tok_Slash            : constant Token_Type := 105;   --  /
   Tok_Modulus          : constant Token_Type := 106;   --  %
   Tok_Increment        : constant Token_Type := 107;   --  ++
   Tok_Decrement        : constant Token_Type := 108;   --  --

   Tok_Equal            : constant Token_Type := 109;   --  ==
   Tok_Not_Equal        : constant Token_Type := 110;   --  !=
   Tok_Greater_Than     : constant Token_Type := 111;   --  >
   Tok_Less_Than        : constant Token_Type := 112;   --  <
   Tok_Greater_Or_Eq    : constant Token_Type := 113;   --  >=
   Tok_Less_Or_Eq       : constant Token_Type := 114;   --  <=

   Tok_Negation         : constant Token_Type := 115;   --  !
   Tok_And              : constant Token_Type := 116;   --  &&
   Tok_Or               : constant Token_Type := 117;   --  ||

   Tok_Bitwise_Not      : constant Token_Type := 118;   --  ~
   Tok_Bitwise_And      : constant Token_Type := 119;   --  &
   Tok_Bitwise_Or       : constant Token_Type := 120;   --  |
   Tok_Bitwise_Xor      : constant Token_Type := 121;   --  ^
   Tok_Bitwise_Left     : constant Token_Type := 122;   --  <<
   Tok_Bitwise_Right    : constant Token_Type := 123;   --  >>

   Tok_Add_Assign       : constant Token_Type := 124;   --  +=
   Tok_Sub_Assign       : constant Token_Type := 125;   --  -=
   Tok_Mul_Assign       : constant Token_Type := 126;   --  *=
   Tok_Div_Assign       : constant Token_Type := 127;   --  /=
   Tok_Mod_Assign       : constant Token_Type := 128;   --  %=

   Tok_Bit_And_Assign   : constant Token_Type := 129;   --  &=
   Tok_Bit_Or_Assign    : constant Token_Type := 130;   --  |=
   Tok_Bit_Xor_Assign   : constant Token_Type := 131;   --  ^=
   Tok_Bit_Less_Assign  : constant Token_Type := 132;   --  <<=
   Tok_Bit_Right_Assign : constant Token_Type := 133;   --  >>=

   Tok_Dereference      : constant Token_Type := 134;   --  ->
   Tok_Dot              : constant Token_Type := 135;   --  .
   Tok_Left_Sq_Bracket  : constant Token_Type := 136;   --  [
   Tok_Right_Sq_Bracket : constant Token_Type := 137;   --  ]
   Tok_Question         : constant Token_Type := 138;   --  ?
   Tok_Scope            : constant Token_Type := 139;   --  ::
   Tok_Colon            : constant Token_Type := 140;   --  :

   --  Separators

   Tok_Comma            : constant Token_Type := 141;   --  ,
   Tok_Semicolon        : constant Token_Type := 142;   --  ;
   Tok_Left_Paren       : constant Token_Type := 143;   --  (
   Tok_Right_Paren      : constant Token_Type := 144;   --  )
   Tok_Block_Begin      : constant Token_Type := 145;   --  {
   Tok_Block_End        : constant Token_Type := 146;   --  }

   --  Others

   Tok_Identifier       : constant Token_Type := 147;
   Tok_String_Literal   : constant Token_Type := 148;
   Tok_Literal_Number   : constant Token_Type := 149;
   Tok_Keyword          : constant Token_Type := 150;

private
   type Cpp_Language is new Language.C.C_Language with null record;

   overriding function Get_Name (Lang : access Cpp_Language) return String;

   overriding function Entities_Indexed (Self : Cpp_Language) return Boolean;
   --  Unconditionally return True. This enables storing all the C++ entities
   --  in the structure Entities_Search_Tries, and it is required to give
   --  support for entities completion (see Completion-C packages) and
   --  navigation in Ada sources through entities imported from C++.

   Cpp_Lang : constant Language_Access := new Cpp_Language;
end Language.Cpp;
