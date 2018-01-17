------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  <description>
--  This package provides some data types and some services to help
--  in the source highlighting processing (syntax highlighting, but
--  also line or word highlighting, etc).
--  </description>

with Gdk.RGBA;
with Gtk.Text_Tag;
with Pango.Font;

with Language;

package Src_Highlighting is

   type Highlighting_Tags is array (Language.Standout_Language_Entity)
     of Gtk.Text_Tag.Gtk_Text_Tag;
   --  This array contains the tags associated to each Language Entity
   --  Kind that needs to be highlighted in the source editor.

   Type_Color_Tag_Name              : constant String := "type";
   Number_Color_Tag_Name            : constant String := "number";
   Block_Color_Tag_Name             : constant String := "block";
   Keyword_Color_Tag_Name           : constant String := "keyword";
   Comment_Color_Tag_Name           : constant String := "comment";
   Annotated_Keyword_Color_Tag_Name : constant String := "annotated-keyword";
   Annotated_Comment_Color_Tag_Name : constant String := "annotated-comment";
   Aspect_Keyword_Color_Tag_Name    : constant String := "aspect-keyword";
   Aspect_Comment_Color_Tag_Name    : constant String := "aspect-comment";
   Aspect_Color_Tag_Name            : constant String := "aspect";
   String_Color_Tag_Name            : constant String := "string";
   Character_Color_Tag_Name         : constant String := "character";
   --  Respectively, the name of the tags use to highlight keywords, comments,
   --  and strings. Declared as public so that one can retrieve them from
   --  the source buffer using their names.

   Highlight_Line_Tag_Name : constant String := "hl_line";
   --  The name of the tag used to highlight a line in the buffer.
   --  Declared as public so that one can retrieve them from the
   --  Source Buffer using its name.

   Highlight_Region_Tag_Name : constant String := "hl_region";
   --  The name of the tag used to highlight regions in the buffer.
   --  Declared as public so that one can retrieve them from the
   --  Source Buffer using its name.

   procedure Unref (Tags : in out Highlighting_Tags);
   --  Free memory

   procedure New_Tag
     (Tag        : in out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name   : String;
      Fore_Color : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Back_Color : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      Font_Desc  : Pango.Font.Pango_Font_Description := null);
   --  Create a new Gtk_Text_Tag with the given name.
   --  If the tag already exists, its properties are changed accordingly.

   procedure Create_Highlight_Line_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk.RGBA.Gdk_RGBA);
   --  Create a tag and set the Background_Gdk property using the given Color.
   --  The priority of this Tag is guarantied to exceed the priority of the
   --  syntax highlighting tags to ensure that highlighting a part of the
   --  buffer using this tag will always override their colors and font
   --  attributes.

end Src_Highlighting;
