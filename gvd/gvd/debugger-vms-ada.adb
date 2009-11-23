-----------------------------------------------------------------------
--                                  GPS                              --
--                                                                   --
--                      Copyright (C) 2000-2008, AdaCore             --
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

with Items.Arrays;          use Items.Arrays;
with Items;                 use Items;
with Language.Ada;          use Language.Ada;
with Language.Debugger;     use Language.Debugger;

package body Debugger.VMS.Ada is

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Lang : access VMS_Ada_Language) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "ada";
   end Get_Name;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access VMS_Ada_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_Simple_Type (Ada_Lang, Str);
   end Is_Simple_Type;

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access VMS_Ada_Language)
      return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Ada_Lang);
   end Keywords;

   overriding function Keywords
     (Lang : access VMS_Ada_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords (Ada_Lang);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   overriding function Get_Language_Context
     (Lang : access VMS_Ada_Language) return Language.Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Language_Context (Ada_Lang);
   end Get_Language_Context;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   overriding function Explorer_Regexps
     (Lang : access VMS_Ada_Language) return Language.Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Explorer_Regexps (Ada_Lang);
   end Explorer_Regexps;

   --------------------
   -- Is_System_File --
   --------------------

   overriding function Is_System_File
     (Lang : access VMS_Ada_Language; File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Is_System_File (Ada_Lang, File_Name);
   end Is_System_File;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
     (Lang : access VMS_Ada_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Dereference_Name (Ada_Lang, Name);
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   overriding function Array_Item_Name
     (Lang  : access VMS_Ada_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Array_Item_Name (Ada_Lang, Name, Index);
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   overriding function Record_Field_Name
     (Lang  : access VMS_Ada_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Record_Field_Name (Ada_Lang, Name, Field);
   end Record_Field_Name;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   overriding function Get_Project_Fields
     (Lang : access VMS_Ada_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return Get_Project_Fields (Ada_Lang);
   end Get_Project_Fields;

   ---------------------
   -- Break Exception --
   ---------------------

   overriding function Break_Exception
     (Debugger  : access VMS_Ada_Language;
      Name      : String  := "";
      Temporary : Boolean := False;
      Unhandled : Boolean := False) return String
   is
      pragma Unreferenced (Debugger, Name, Unhandled);
   begin
      if Temporary then
         return "set break/exception/temp";
      else
         return "set break/exception";
      end if;
   end Break_Exception;

   ----------------
   -- Parse_Type --
   ----------------

   overriding procedure Parse_Type
     (Lang     : access VMS_Ada_Language;
      Type_Str : String;
      Entity   : String;
      Index    : in out Natural;
      Result   : out Items.Generic_Type_Access)
   is
      pragma Unreferenced (Lang, Type_Str, Entity);
   begin
      --  ???
      Index := 0;
      Result := null;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   overriding procedure Parse_Value
     (Lang       : access VMS_Ada_Language;
      Type_Str   : String;
      Index      : in out Natural;
      Result     : in out Items.Generic_Type_Access;
      Repeat_Num : out Positive) is
      pragma Unreferenced (Lang, Type_Str, Index, Repeat_Num);
   begin
      --  ???
      Result := null;
   end Parse_Value;

   ----------------------
   -- Parse_Array_Type --
   ----------------------

   overriding procedure Parse_Array_Type
     (Lang         : access VMS_Ada_Language;
      Type_Str     : String;
      Entity       : String;
      Index        : in out Natural;
      Start_Of_Dim : Natural;
      Result       : out Generic_Type_Access)
   is
   begin
      --  ???
      null;
   end Parse_Array_Type;

   -----------------------
   -- Parse_Record_Type --
   -----------------------

   overriding procedure Parse_Record_Type
     (Lang      : access VMS_Ada_Language;
      Type_Str  : String;
      Entity    : String;
      Index     : in out Natural;
      Is_Union  : Boolean;
      Result    : out Generic_Type_Access;
      End_On    : String)
   is
   begin
      --  ???
      null;
   end Parse_Record_Type;

   -----------------------
   -- Parse_Array_Value --
   -----------------------

   overriding procedure Parse_Array_Value
     (Lang     : access VMS_Ada_Language;
      Type_Str : String;
      Index    : in out Natural;
      Result   : in out Array_Type_Access)
   is
   begin
      --  ???
      null;
   end Parse_Array_Value;

   -----------------------------------
   -- Get_Language_Debugger_Context --
   -----------------------------------

   overriding function Get_Language_Debugger_Context
     (Lang : access VMS_Ada_Language) return Language_Debugger_Context
   is
      pragma Unreferenced (Lang);
   begin
      return (Record_Field_Length  => 2,
              Record_Start         => '(',
              Record_End           => ')',
              Array_Start          => '(',
              Array_End            => ')',
              Record_Field         => "=>");
   end Get_Language_Debugger_Context;

   ------------------
   -- Set_Variable --
   ------------------

   overriding function Set_Variable
     (Lang     : access VMS_Ada_Language;
      Var_Name : String;
      Value    : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "deposit " & Var_Name & " = " & Value;
   end Set_Variable;

   -----------
   -- Start --
   -----------

   overriding function Start
     (Debugger : access VMS_Ada_Language) return String
   is
      pragma Unreferenced (Debugger);
   begin
      return "go";
   end Start;

   ---------------------------
   -- Can_Tooltip_On_Entity --
   ---------------------------

   overriding
   function Can_Tooltip_On_Entity
     (Lang   : access VMS_Ada_Language;
      Entity : String) return Boolean
   is
      pragma Unreferenced (Lang, Entity);
   begin
      return True;
   end Can_Tooltip_On_Entity;

end Debugger.VMS.Ada;
