-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  This package defines the object Gdb_C_Language that provides support
--  for the language C in Gdb.
--
--  See language.ads and language-debugger.ads for a complete spec.

with Language.Debugger;
with Items.Arrays;

package Debugger.Gdb.C is

   type Gdb_C_Language is new
     Language.Debugger.Language_Debugger with private;

   --------------------
   -- Simple Parsing --
   --------------------

   function Is_Simple_Type
     (Lang : access Gdb_C_Language; Str : String) return Boolean;

   function Keywords
     (Lang : access Gdb_C_Language) return GNAT.Regpat.Pattern_Matcher;

   function Get_Language_Context
     (Lang : access Gdb_C_Language) return Language.Language_Context;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Gdb_C_Language) return Language.Explorer_Categories;

   function Is_System_File
     (Lang : access Gdb_C_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Gdb_C_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Gdb_C_Language;
      Name  : String;
      Field : String) return String;

   -------------
   -- Parsing --
   -------------

   procedure Parse_Type
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access);

   procedure Parse_Value
     (Lang       : access Gdb_C_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive);

   function Break_Exception
     (Debugger  : access Gdb_C_Language;
      Name      : String  := "";
      Unhandled : Boolean := False) return String;

   procedure Parse_Array_Type
     (Lang         : access Gdb_C_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : in Natural;
      Result       : out Items.Generic_Type_Access);

   procedure Parse_Record_Type
     (Lang      : access Gdb_C_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String);
   --  End_On is ignored in the C implementation.

   procedure Parse_Array_Value
     (Lang     : access Gdb_C_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access);

   function Set_Variable
     (Lang     : access Gdb_C_Language;
      Var_Name : String;
      Value    : String) return String;

   function Start (Debugger : access Gdb_C_Language) return String;

   function Get_Language_Debugger_Context
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
      Result     : out Items.Generic_Type_Access);
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
      Result   : out Items.Generic_Type_Access);
   --  Generic function to parse the type information for C-related
   --  languages.

   procedure C_Detect_Composite_Type
     (Lang     : access Language.Debugger.Language_Debugger'Class;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access);
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
      Start_Of_Dim : in Natural;
      Result       : out Items.Generic_Type_Access);
   --  Generic function to parse arrays for C-related languages.

   procedure C_Parse_Record_Type
     (Lang      : access Language.Debugger.Language_Debugger'Class;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String);
   --  Generic function to parse records for C-related languages.

private

   type Gdb_C_Language is new
     Language.Debugger.Language_Debugger with null record;

end Debugger.Gdb.C;
