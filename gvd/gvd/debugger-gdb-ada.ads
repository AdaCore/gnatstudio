-----------------------------------------------------------------------
--                             G P S                                 --
--                                                                   --
--                   Copyright (C) 2000-2008, AdaCore                --
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

--  This package defines the object Gdb_Ada_Language that provides support
--  for the language Ada in Gdb.
--
--  See language.ads and language-debugger.ads for a complete spec.

with Items.Arrays;
with Language; use Language;
with Language.Debugger;

package Debugger.Gdb.Ada is

   type Gdb_Ada_Language is new
     Language.Debugger.Language_Debugger with private;

   --------------------
   -- Simple Parsing --
   --------------------

   function Is_Simple_Type
     (Lang : access Gdb_Ada_Language; Str : String) return Boolean;

   function Keywords
     (Lang : access Gdb_Ada_Language) return Pattern_Matcher_Access;

   function Keywords
     (Lang : access Gdb_Ada_Language) return GNAT.Strings.String_List;

   function Get_Language_Context
     (Lang : access Gdb_Ada_Language) return Language.Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Gdb_Ada_Language) return Language.Explorer_Categories;

   function Is_System_File
     (Lang : access Gdb_Ada_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Gdb_Ada_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Gdb_Ada_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Gdb_Ada_Language;
      Name  : String;
      Field : String) return String;

   ---------------------
   -- Project support --
   ---------------------

   function Get_Project_Fields
     (Lang : access Gdb_Ada_Language) return Project_Field_Array;

   -------------
   -- Parsing --
   -------------

   procedure Parse_Type
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access);

   procedure Parse_Value
     (Lang       : access Gdb_Ada_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive);

   function Break_Exception
     (Debugger  : access Gdb_Ada_Language;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String;

   procedure Parse_Array_Type
     (Lang         : access Gdb_Ada_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : in Natural;
      Result       : out Items.Generic_Type_Access);

   procedure Parse_Record_Type
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Is_Union : Boolean;
      Result   : out Items.Generic_Type_Access;
      End_On   : String);

   procedure Parse_Array_Value
     (Lang     : access Gdb_Ada_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Items.Arrays.Array_Type_Access);

   function Set_Variable
     (Lang     : access Gdb_Ada_Language;
      Var_Name : String;
      Value    : String) return String;

   function Start (Debugger : access Gdb_Ada_Language) return String;

   function Get_Language_Debugger_Context
     (Lang : access Gdb_Ada_Language)
      return Language.Debugger.Language_Debugger_Context;

   function Can_Tooltip_On_Entity
     (Lang   : access Gdb_Ada_Language;
      Entity : String) return Boolean;

private

   type Gdb_Ada_Language is new
     Language.Debugger.Language_Debugger with null record;

   function Get_Name (Lang : access Gdb_Ada_Language) return String;
   --  See inherited documentation

end Debugger.Gdb.Ada;
