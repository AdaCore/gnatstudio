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

--  This is the general Ada (non debugger specific) support package.
--  See language.ads for a complete spec.

with Ada_Analyzer;

package Language.Ada is

   type Ada_Language is new Language_Root with private;

   Ada_Lang : constant Language_Access;
   --  Class constant for the Ada language

   -------------
   -- Parsing --
   -------------

   overriding function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Ada_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access Ada_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access Ada_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access Ada_Language) return Language_Context_Access;

   overriding function Is_Entity_Name
     (Lang : access Ada_Language;
      Name : String)
      return Boolean;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access Ada_Language) return Explorer_Categories;

   overriding function Is_System_File
     (Lang : access Ada_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   overriding function Dereference_Name
     (Lang : access Ada_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Field : String) return String;

   ----------------------
   -- Source Analyzing --
   ----------------------

   overriding
   procedure Parse_Constructs
     (Lang   : access Ada_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List);

   overriding
   procedure Format_Buffer
     (Lang                : access Ada_Language;
      Buffer              : String;
      Replace             : Replace_Text_Callback;
      From, To            : Natural := 0;
      Indent_Params       : Indent_Parameters := Default_Indent_Parameters;
      Case_Exceptions     : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception;
      Is_Optional_Keyword : access function (S : String)
                                             return Boolean := null);

   overriding
   procedure Parse_Entities
     (Lang     : access Ada_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding
   procedure Get_Referenced_Entity
     (Lang       : access Ada_Language;
      Buffer     : String;
      Construct  : Simple_Construct_Information;
      Sloc_Start : out Source_Location;
      Sloc_End   : out Source_Location;
      Success    : out Boolean;
      From_Index : Natural := 0);
   --  See inherited documentation

   Ada_Abstract_Attribute  : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Abstract_Attribute;
   Ada_Access_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Access_Attribute;
   Ada_Aliased_Attribute   : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Aliased_Attribute;
   Ada_Array_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Array_Attribute;
   Ada_Assign_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Assign_Attribute;
   Ada_Constant_Attribute  : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Constant_Attribute;
   Ada_Delta_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Delta_Attribute;
   Ada_Digits_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Digits_Attribute;
   Ada_In_Attribute        : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_In_Attribute;
   Ada_Interface_Attribute : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Interface_Attribute;
   Ada_Mod_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Mod_Attribute;
   Ada_New_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_New_Attribute;
   Ada_Not_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Not_Attribute;
   Ada_Null_Attribute      : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Null_Attribute;
   Ada_Out_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Out_Attribute;
   Ada_Range_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Range_Attribute;
   Ada_Record_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Record_Attribute;
   Ada_Tagged_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Tagged_Attribute;
   Ada_Class_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Class_Attribute;
   Ada_Renames_Attribute   : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Renames_Attribute;
   Ada_Generic_Attribute   : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Generic_Attribute;

   Ada_Library_Visibility_Attribute : constant Construct_Att_Key :=
                                        Language.Ada.Ada_Generic_Attribute + 1;
   --  This attribute is not set by the parser itself - but external
   --  process will need to compute & store this value

   overriding function Get_Name (Lang : access Ada_Language) return String;

   ---------------------
   -- Ada token types --
   ---------------------

   --  Reserved words
   Tok_Abort             : constant Token_Type := 1;
   Tok_Abs               : constant Token_Type := 2;
   Tok_Abstract          : constant Token_Type := 3;
   Tok_Accept            : constant Token_Type := 4;
   Tok_Access            : constant Token_Type := 5;
   Tok_Aliased           : constant Token_Type := 6;
   Tok_All               : constant Token_Type := 7;
   Tok_And               : constant Token_Type := 8;
   Tok_Array             : constant Token_Type := 9;
   Tok_At                : constant Token_Type := 10;
   Tok_Begin             : constant Token_Type := 11;
   Tok_Body              : constant Token_Type := 12;
   Tok_Case              : constant Token_Type := 13;
   Tok_Constant          : constant Token_Type := 14;
   Tok_Declare           : constant Token_Type := 15;
   Tok_Delay             : constant Token_Type := 16;
   Tok_Delta             : constant Token_Type := 17;
   Tok_Digits            : constant Token_Type := 18;
   Tok_Do                : constant Token_Type := 19;
   Tok_Else              : constant Token_Type := 20;
   Tok_Elsif             : constant Token_Type := 21;
   Tok_End               : constant Token_Type := 22;
   Tok_Entry             : constant Token_Type := 23;
   Tok_Exception         : constant Token_Type := 24;
   Tok_Exit              : constant Token_Type := 25;
   Tok_For               : constant Token_Type := 26;
   Tok_Function          : constant Token_Type := 27;
   Tok_Generic           : constant Token_Type := 28;
   Tok_Goto              : constant Token_Type := 29;
   Tok_If                : constant Token_Type := 30;
   Tok_In                : constant Token_Type := 31;
   Tok_Interface         : constant Token_Type := 32;
   Tok_Is                : constant Token_Type := 33;
   Tok_Limited           : constant Token_Type := 34;
   Tok_Loop              : constant Token_Type := 35;
   Tok_Mod               : constant Token_Type := 36;
   Tok_New               : constant Token_Type := 37;
   Tok_Not               : constant Token_Type := 38;
   Tok_Null              : constant Token_Type := 39;
   Tok_Others            : constant Token_Type := 40;
   Tok_Out               : constant Token_Type := 41;
   Tok_Of                : constant Token_Type := 42;
   Tok_Or                : constant Token_Type := 43;
   Tok_Overriding        : constant Token_Type := 44;
   Tok_Package           : constant Token_Type := 45;
   Tok_Pragma            : constant Token_Type := 46;
   Tok_Private           : constant Token_Type := 47;
   Tok_Procedure         : constant Token_Type := 48;
   Tok_Protected         : constant Token_Type := 49;
   Tok_Raise             : constant Token_Type := 50;
   Tok_Range             : constant Token_Type := 51;
   Tok_Record            : constant Token_Type := 52;
   Tok_Rem               : constant Token_Type := 53;
   Tok_Renames           : constant Token_Type := 54;
   Tok_Requeue           : constant Token_Type := 55;
   Tok_Return            : constant Token_Type := 56;
   Tok_Reverse           : constant Token_Type := 57;
   Tok_Select            : constant Token_Type := 58;
   Tok_Separate          : constant Token_Type := 59;
   Tok_Some              : constant Token_Type := 60;
   Tok_Subtype           : constant Token_Type := 61;
   Tok_Synchronized      : constant Token_Type := 62;
   Tok_Tagged            : constant Token_Type := 63;
   Tok_Task              : constant Token_Type := 64;
   Tok_Terminate         : constant Token_Type := 65;
   Tok_Then              : constant Token_Type := 66;
   Tok_Type              : constant Token_Type := 67;
   Tok_Until             : constant Token_Type := 68;
   Tok_Use               : constant Token_Type := 69;
   Tok_When              : constant Token_Type := 70;
   Tok_While             : constant Token_Type := 71;
   Tok_With              : constant Token_Type := 72;
   Tok_Aspect            : constant Token_Type := 73;
   Tok_Xor               : constant Token_Type := 74;

   --  Separators
   Tok_Dot               : constant Token_Type := 75;
   Tok_Open_Parenthesis  : constant Token_Type := 76;
   Tok_Close_Parenthesis : constant Token_Type := 77;
   Tok_Colon             : constant Token_Type := 78;
   Tok_Arrow             : constant Token_Type := 79;
   Tok_Operator          : constant Token_Type := 80;
   Tok_Comma             : constant Token_Type := 81;
   Tok_Semicolon         : constant Token_Type := 82;
   Tok_Blank             : constant Token_Type := 83;
   Tok_Tick              : constant Token_Type := 84;
   Tok_Dot_Dot           : constant Token_Type := 85;

   --  Words
   Tok_Identifier        : constant Token_Type := 86;
   Tok_String            : constant Token_Type := 87;

   --  Unparsed parts
   Tok_Expression        : constant Token_Type := 88;

   subtype Ada_Token          is Token_Type range No_Token .. Tok_Expression;
   subtype Ada_Reserved_Token is Token_Type range Tok_Abort .. Tok_Xor;

   function Image (Token : Ada_Token) return String;

   overriding procedure Parse_Tokens_Backwards
     (Lang              : access Ada_Language;
      Buffer            : UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0;
      Callback          :
      access procedure (Token : Token_Record;
                        Stop : in out Boolean));

   overriding function Parse_Reference_Backwards
     (Lang              : access Ada_Language;
      Buffer            : UTF8_String;
      Start_Offset      : String_Index_Type;
      End_Offset        : String_Index_Type := 0) return String;

private
   type Ada_Language is new Language_Root with null record;
   overriding function Comment_Line
     (Lang    : access Ada_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;

   Ada_Lang : constant Language_Access := new Ada_Language;

end Language.Ada;
