------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

--  This package defines the object Lldb_Cpp_Language that provides support
--  for the language C++ in lldb.
--
--  See language.ads and language-debugger.ads for a complete spec.

with GNAT.Expect;
with Language;           use Language;
with Language.Debugger;

package Language.Debugger.Lldb.Cpp is

   type LLDB_Cpp_Language is new
     Language.Debugger.Language_Debugger with private;

   --------------------
   -- Simple Parsing --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access LLDB_Cpp_Language; Str : String) return Boolean;

   overriding function Keywords
     (Lang : access LLDB_Cpp_Language)
      return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Keywords
     (Lang : access LLDB_Cpp_Language) return GNAT.Strings.String_List;

   overriding function Get_Language_Context
     (Lang : access LLDB_Cpp_Language) return Language.Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   overriding function Explorer_Regexps
     (Lang : access LLDB_Cpp_Language) return Language.Explorer_Categories;

   overriding function Is_System_File
     (Lang : access LLDB_Cpp_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   overriding function Dereference_Name
     (Lang : access LLDB_Cpp_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access LLDB_Cpp_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access LLDB_Cpp_Language;
      Name  : String;
      Field : String) return String;

   -------------
   -- Parsing --
   -------------

   overriding procedure Parse_Type
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out GVD.Variables.Types.GVD_Type_Holder);

   overriding procedure Parse_Value
     (Lang       : access LLDB_Cpp_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out GVD.Variables.Types.GVD_Type_Holder;
      Repeat_Num : out Positive);

   overriding procedure Parse_Array_Type
     (Lang         : access LLDB_Cpp_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out GVD.Variables.Types.GVD_Type_Holder);

   overriding procedure Parse_Record_Type
     (Lang      : access LLDB_Cpp_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out GVD.Variables.Types.GVD_Type_Holder;
      End_On    : String);
   --  End_On is ignored in the C implementation.

   overriding procedure Parse_Array_Value
     (Lang     : access LLDB_Cpp_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out GVD.Variables.Types.GVD_Type_Holder);

   overriding function Set_Variable
     (Lang     : access LLDB_Cpp_Language;
      Var_Name : String;
      Value    : String) return String;

   overriding function Get_Language_Debugger_Context
     (Lang : access LLDB_Cpp_Language)
      return Language.Debugger.Language_Debugger_Context;

private

   type LLDB_Cpp_Language is new
     Language.Debugger.Language_Debugger with null record;

   overriding function Get_Name
     (Lang : access LLDB_Cpp_Language) return String;
   --  See inherited documentation

end Language.Debugger.Lldb.Cpp;
