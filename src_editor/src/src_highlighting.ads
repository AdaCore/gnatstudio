-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This package provides some data types and some services to help
--  in the source highlighting processing (syntax highlighting, but
--  also line or word highlighting, etc).
--  </description>

with Gtk.Text_Tag;
with Pango.Enums;

with Language;

package Src_Highlighting is

   type Highlighting_Tags is array (Language.Standout_Language_Entity)
     of Gtk.Text_Tag.Gtk_Text_Tag;
   --  This array contains the tags associated to each Language Entity
   --  Kind that needs to be highlighted in the source editor.

   type Font_Attributes is record
      Style  : Pango.Enums.Style;
      Weight : Pango.Enums.Weight;
   end record;
   --  A structure describing the font attributes for a given Highlighting Tag.
   --  Note that the color name is not included, since we can not have Strings
   --  inside such record, and using Strings access types or chars_ptr types is
   --  not very simple...

   Keyword_Color_Tag_Name : constant String := "keyword_tag";
   Comment_Color_Tag_Name : constant String := "comment_tag";
   String_Color_Tag_Name  : constant String := "string_tag";
   --  Respectively, the name of the tags use to highlight keywords, comments,
   --  and strings. Declared as public so that one can retrieve them from
   --  the source buffer using their names.

   Highlight_Line_Tag_Name : constant String := "hl_line_tag";
   --  The name of the tag used to highlight a line in the buffer.
   --  Declared as public so that one can retrieve them from the
   --  Source Buffer using its name.

   Highlight_Region_Tag_Name : constant String := "hl_region_tag";
   --  The name of the tag used to highlight regions in the buffer.
   --  Declared as public so that one can retrieve them from the
   --  Source Buffer using its name.

   function To_Font_Attributes
     (Style  : Pango.Enums.Style  := Pango.Enums.Pango_Style_Normal;
      Weight : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal)
     return Font_Attributes;
   --  Convenience function to create a Font_Attributes structure.

   function Create_Syntax_Tags
     (Keyword_Color     : String;
      Keyword_Font_Attr : Font_Attributes := To_Font_Attributes;
      Comment_Color     : String;
      Comment_Font_Attr : Font_Attributes := To_Font_Attributes;
      String_Color      : String;
      String_Font_Attr  : Font_Attributes := To_Font_Attributes)
     return Highlighting_Tags;
   --  Create a Highlighting_Tags object using the given color names.
   --  If some colors name can not be parsed, then no special color will
   --  be used to highlight the associated source parts.

   procedure Create_Highlight_Line_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color_Name : String);
   --  Create a tag and set the Background_Gdk property using the given Color.
   --  The priority of this Tag is guarantied to exceed the priority of the
   --  syntax highlighting tags to ensure that highlighting a part of the
   --  buffer using this tag will always override their colors and font
   --  attributes.

   procedure Create_Highlight_Region_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color_Name : String);
   --  Create a tag and set the Background_Gdk property using the given Color.
   --  The priority of this Tag is guarantied to exceed the priority of the
   --  line highlighting tag.

end Src_Highlighting;
