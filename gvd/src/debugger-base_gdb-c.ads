------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

--  This package defines the object Gdb_C_Language that provides support
--  for the language C in Gdb.
--
--  See language.ads and language-debugger.ads for a complete spec.

with Language;          use Language;
with Language.Debugger;
with GNAT.Expect;

package Debugger.Base_Gdb.C is

   type Gdb_C_Language is new
     Language.Debugger.Language_Debugger with private;

   --------------------
   -- Simple Parsing --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Gdb_C_Language; Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Gdb_C_Language)
      return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access Gdb_C_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access Gdb_C_Language) return Language.Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access Gdb_C_Language) return Language.Explorer_Categories;

   overriding function Is_System_File
     (Lang : access Gdb_C_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   overriding function Dereference_Name
     (Lang : access Gdb_C_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Field : String) return String;

   -------------
   -- Parsing --
   -------------

   overriding procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder);

   overriding procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive);

   overriding procedure Parse_Array_Type
     (Lang         : access Gdb_C_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder);

   overriding procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String);
   --  End_On is ignored in the C implementation.

   overriding procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out GVD.Variables.Types.GVD_Type_Holder);

   overriding function Set_Variable
     (Lang     : access Gdb_C_Language;
      Var_Name : String;
      Value    : String) return String;

   overriding function Get_Language_Debugger_Context
     (Lang : access Gdb_C_Language)
      return Language.Debugger.Language_Debugger_Context;

   procedure C_Field_Name
     (Lang       : access Language.Debugger.Language_Debugger'Class;
      Entity     : String;
      Type_Str   : String;
      Index      : Natural;
      Name_Start : out Natural;
      Name_End   : out Natural;
      Field_End  : out Natural;
      Result     : out GVD.Variables.Types.GVD_Type_Holder);
   --  Parse the field of a struct or union in C.
   --  Index should point at the first word of the field.
   --  The name of the field is at indexes Name_Start .. Name_End.
   --  Field_End is set to point to the closing semicolon (';').
   --  This also parses the field itself and return it in Result.

   procedure C_Parse_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder);
   --  Generic function to parse the type information for C-related
   --  languages.

   procedure C_Detect_Composite_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder);
   --  Return a non null value in Result if the type definition pointed to
   --  by Index is in fact an array or access type (this is tricky in C since
   --  we have to skip the whole struct definition to check this, or even to
   --  handle pointers to functions). In that case, Index is left after the
   --  type definition.
   --  null is returned if we have neither an access type nor an array. In that
   --  case, Index is left at the beginning of the type definition.

   procedure C_Parse_Array_Type
     (Lang         : access Language.Debugger.Language_Debugger'Class;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder);
   --  Generic function to parse arrays for C-related languages.

   procedure C_Parse_Record_Type
     (Lang      : access Language.Debugger.Language_Debugger'Class;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String);
   --  Generic function to parse records for C-related languages.

private

   type Gdb_C_Language is new
     Language.Debugger.Language_Debugger with null record;

   overriding function Get_Name (Lang : access Gdb_C_Language) return String;
   --  See inherited documentation

end Debugger.Base_Gdb.C;
