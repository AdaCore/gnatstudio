-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package provides some data types and some services to help
--  in the source highlighting processing (syntax highlighting, but
--  also line or word highlighting, etc).
--  </description>

with Gdk.Color;
with Gtk.Text_Tag;
with Pango.Font;

with Language;

package Src_Highlighting is

   type Highlighting_Tags is array (Language.Standout_Language_Entity)
     of Gtk.Text_Tag.Gtk_Text_Tag;
   --  This array contains the tags associated to each Language Entity
   --  Kind that needs to be highlighted in the source editor.

   Keyword_Color_Tag_Name   : constant String := "keyword_tag";
   Comment_Color_Tag_Name   : constant String := "comment_tag";
   String_Color_Tag_Name    : constant String := "string_tag";
   Character_Color_Tag_Name : constant String := "character_tag";
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

   procedure Create_Syntax_Tags
     (Result              : in out Highlighting_Tags;
      Keyword_Color       : Gdk.Color.Gdk_Color;
      Keyword_Color_Bg    : Gdk.Color.Gdk_Color;
      Keyword_Font_Desc   : Pango.Font.Pango_Font_Description := null;
      Comment_Color       : Gdk.Color.Gdk_Color;
      Comment_Color_Bg    : Gdk.Color.Gdk_Color;
      Comment_Font_Desc   : Pango.Font.Pango_Font_Description := null;
      Character_Color     : Gdk.Color.Gdk_Color;
      Character_Color_Bg  : Gdk.Color.Gdk_Color;
      Character_Font_Desc : Pango.Font.Pango_Font_Description := null;
      String_Color        : Gdk.Color.Gdk_Color;
      String_Color_Bg     : Gdk.Color.Gdk_Color;
      String_Font_Desc    : Pango.Font.Pango_Font_Description := null);
   --  Create or update a Highlighting_Tags object using the given color names.
   --  If some colors name can not be parsed, then no special color will be
   --  used to highlight the associated source parts.

   procedure Create_Highlight_Line_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk.Color.Gdk_Color);
   --  Create a tag and set the Background_Gdk property using the given Color.
   --  The priority of this Tag is guarantied to exceed the priority of the
   --  syntax highlighting tags to ensure that highlighting a part of the
   --  buffer using this tag will always override their colors and font
   --  attributes.

   procedure Create_Highlight_Region_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk.Color.Gdk_Color);
   --  Create a tag and set the Background_Gdk property using the given Color.
   --  The priority of this Tag is guarantied to exceed the priority of the
   --  line highlighting tag.

end Src_Highlighting;
