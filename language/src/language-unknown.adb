------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with GNAT.Regpat; use GNAT.Regpat;

package body Language.Unknown is

   Keywords_Regexp : aliased String := "";

   Keywords_List   : aliased Pattern_Matcher := GNAT.Regpat.Never_Match;

   --------------------
   -- Is_Simple_Type --
   --------------------

   overriding function Is_Simple_Type
     (Lang : access Unknown_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang, Str);
   begin
      return True;
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   overriding function Dereference_Name
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

   overriding function Array_Item_Name
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

   overriding function Record_Field_Name
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

   overriding function Keywords
     (Lang : access Unknown_Language) return Strings.String_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_Regexp'Access;
   end Keywords;

   overriding function Keywords
     (Lang : access Unknown_Language) return GNAT.Expect.Pattern_Matcher_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List'Access;
   end Keywords;

   overriding function Keywords
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
     (Syntax =>
        (Comment_Start                 => null,
         Comment_End                   => null,
         New_Line_Comment_Start        => null,
         New_Line_Comment_Start_Regexp => Comment_Start_Pattern'Access),
      String_Delimiter              => ASCII.NUL,
      Quote_Character               => ASCII.NUL,
      Constant_Character            => ASCII.NUL,
      Can_Indent                    => False,
      Syntax_Highlighting           => False,
      Case_Sensitive                => True,
      Accurate_Xref                 => False,
      Use_Semicolon                 => False);

   overriding function Get_Language_Context
     (Lang : access Unknown_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Unknown_Context'Access;
   end Get_Language_Context;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   overriding procedure Parse_Constructs
     (Lang   : access Unknown_Language;
      File   : GNATCOLL.VFS.Virtual_File;
      Buffer : UTF8_String;
      Result : out Construct_List)
   is
      pragma Unreferenced (Lang, Buffer, File);
   begin
      Result := (null, null, null, 0);
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   overriding procedure Parse_Entities
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

   overriding function Get_Name
     (Lang : access Unknown_Language) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "Unknown";
   end Get_Name;

end Language.Unknown;
