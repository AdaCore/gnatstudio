-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
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

with Language.Java; use Language.Java;
with Items;         use Items;
with Items.Arrays;  use Items.Arrays;

package body Debugger.Jdb.Java is

   use Language;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Jdb_Java_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Simple_Type (Java_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Jdb_Java_Language) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Java_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Jdb_Java_Language) return Language.Language_Context
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Language_Context (Java_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Jdb_Java_Language) return Language.Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Explorer_Regexps (Java_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang : access Jdb_Java_Language; File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_System_File (Java_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Jdb_Java_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Dereference_Name (Java_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Jdb_Java_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Array_Item_Name (Java_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Jdb_Java_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Record_Field_Name (Java_Lang, Name, Field);
   end Record_Field_Name;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   function Get_Project_Fields
     (Lang : access Jdb_Java_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Project_Fields (Java_Lang);
   end Get_Project_Fields;

   ------------------
   -- Set_Variable --
   ------------------

   function Set_Variable
     (Lang     : access Jdb_Java_Language;
      Var_Name : String;
      Value    : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "set " & Var_Name & "=" & Value;
   end Set_Variable;

   -----------
   -- Start --
   -----------

   function Start (Debugger : access Jdb_Java_Language) return String is
      pragma Unreferenced (Debugger);
   begin
      return "run";
   end Start;

   ----------------
   -- Parse_Type --
   ----------------

   procedure Parse_Type
     (Lang     : access Jdb_Java_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Generic_Type_Access)
   is
      pragma Unreferenced (Lang);
   begin
      raise Unexpected_Type;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Lang       : access Jdb_Java_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Generic_Type_Access;
      Repeat_Num : out Positive)
   is
      pragma Unreferenced (Lang);
   begin
      raise Program_Error;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   procedure Parse_Array_Type
     (Lang      : access Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Start_Of_Dim : in Natural;
      Result    : out Generic_Type_Access)
   is
      pragma Unreferenced (Lang);
   begin
      raise Program_Error;
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   procedure Parse_Record_Type
     (Lang      : access Jdb_Java_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access;
      End_On    : String)
   is
      pragma Unreferenced (Lang);
   begin
      raise Program_Error;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   procedure Parse_Array_Value
     (Lang     : access Jdb_Java_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
      pragma Unreferenced (Lang);
   begin
      raise Program_Error;
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   function Get_Language_Debugger_Context
     (Lang : access Jdb_Java_Language)
      return Language.Debugger.Language_Debugger_Context
   is
      pragma Unreferenced (Lang);
   begin
      return (Record_Field_Length  => 1,
              Record_Start         => '{',
              Record_End           => '}',
              Array_Start          => '{',
              Array_End            => '}',
              Record_Field         => "=");
   end Get_Language_Debugger_Context;

end Debugger.Jdb.Java;
