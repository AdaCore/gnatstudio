-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package defines the object Jdb_Java_Language that provides support
--  for the language Java in Jdb.
--
--  See language.ads for a complete spec.

with Language.Debugger;
with Items.Arrays;

package Debugger.Jdb.Java is

   use Language;

   type Jdb_Java_Language is new
     Language.Debugger.Language_Debugger with private;
   type Jdb_Java_Language_Access is access all Jdb_Java_Language'Class;

   function Is_Case_Sensitive (Lang : access Jdb_Java_Language) return Boolean;

   --------------------
   -- Simple Parsing --
   --------------------

   function Is_Simple_Type
     (Lang : access Jdb_Java_Language; Str : String) return Boolean;

   function Keywords (Lang : access Jdb_Java_Language)
     return GNAT.Regpat.Pattern_Matcher;

   function Get_Language_Context
     (Lang : access Jdb_Java_Language) return Language.Language_Context;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Jdb_Java_Language) return Language.Explorer_Categories;

   function Is_System_File
     (Lang : access Jdb_Java_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Jdb_Java_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Jdb_Java_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Jdb_Java_Language;
      Name  : String;
      Field : String) return String;

   -------------
   -- Parsing --
   -------------

   procedure Parse_Type
     (Lang     : access Jdb_Java_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access);

   procedure Parse_Value
     (Lang       : access Jdb_Java_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive);

   procedure Parse_Array_Type
     (Lang      : access Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Items.Generic_Type_Access);

   procedure Parse_Record_Type
     (Lang      : access Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Items.Generic_Type_Access;
      End_On    : String);

   procedure Parse_Array_Value
     (Lang     : access Jdb_Java_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access);

   function Set_Variable
     (Lang     : access Jdb_Java_Language;
      Var_Name : String;
      Value    : String) return String;

   function Start (Debugger : access Jdb_Java_Language) return String;

   function Get_Language_Debugger_Context
     (Lang : access Jdb_Java_Language)
      return Language.Debugger.Language_Debugger_Context;

private

   type Jdb_Java_Language is new
     Language.Debugger.Language_Debugger with null record;

end Debugger.Jdb.Java;
