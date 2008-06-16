-----------------------------------------------------------------------
--                              G P S                                --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Regpat; use GNAT.Regpat;

package body Language.Unknown is

   Keywords_Regexp : aliased String := "";

   Keywords_List   : aliased Pattern_Matcher := GNAT.Regpat.Never_Match;

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Unknown_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return True;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Unknown_Language;
      Name : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
   begin
      return "";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
      pragma Unreferenced (Index);
   begin
      return "";
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Unknown_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
      pragma Unreferenced (Name);
      pragma Unreferenced (Field);
   begin
      return "";
   end Record_Field_Name;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Unknown_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   function Keywords
     (Lang : access Unknown_Language) return Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   function Keywords
     (Lang : access Unknown_Language) return GNAT.Strings.String_List
   is
      pragma Unreferenced (Lang);
   begin
      return GNAT.Strings.String_List'(1 .. 0 => null);
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Comment_Start_Pattern : aliased Pattern_Matcher := Never_Match;

   Unknown_Context : aliased Language_Context :=
     (Comment_Start_Length          => 0,
      Comment_End_Length            => 0,
      Comment_Start                 => "",
      Comment_End                   => "",
      New_Line_Comment_Start        => null,
      New_Line_Comment_Start_Regexp => Comment_Start_Pattern'Access,
      String_Delimiter              => ASCII.NUL,
      Quote_Character               => ASCII.NUL,
      Constant_Character            => ASCII.NUL,
      Can_Indent                    => False,
      Syntax_Highlighting           => False,
      Case_Sensitive                => True);

   function Get_Language_Context
     (Lang : access Unknown_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Unknown_Context'Access;
   end Get_Language_Context;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   function Get_Project_Fields
     (Lang : access Unknown_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return (1 .. 0 => No_Project_Field);
   end Get_Project_Fields;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang   : access Unknown_Language;
      Buffer : String;
      Result : out Construct_List)
   is
      pragma Unreferenced (Lang, Buffer);
   begin
      Result := (null, null, null, 0);
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Unknown_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      pragma Unreferenced (Lang, Buffer, Callback);
   begin
      null;
   end Parse_Entities;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Lang : access Unknown_Language) return String is
      pragma Unreferenced (Lang);
   begin
      return "Unknown";
   end Get_Name;

end Language.Unknown;
