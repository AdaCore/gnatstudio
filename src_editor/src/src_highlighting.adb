-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Gdk.Color;    use Gdk.Color;
with Gtk;          use Gtk;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Pango.Enums;  use Pango.Enums;

with Language;     use Language;

package body Src_Highlighting is

   --------------------------
   -- Forward_Declarations --
   --------------------------

   function New_Tag
     (Tag_Name  : String;
      Color     : Gdk_Color;
      Font_Attr : Font_Attributes) return Gtk_Text_Tag;
   --  Create a new Gtk_Text_Tag with the given name. If Color is null,
   --  then the Foreground_Gdk_Property is set with the given color.

   procedure New_Tag
     (Tag      : out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name : String;
      Color    : Gdk_Color);
   --  Create a new Gtk_Text_Tag with the given Tag_Name. Color is used
   --  to set the Background_Gdk_Property.

   -------------
   -- New_Tag --
   -------------

   function New_Tag
     (Tag_Name  : String;
      Color     : Gdk_Color;
      Font_Attr : Font_Attributes) return Gtk_Text_Tag
   is
      Result : Gtk_Text_Tag;
   begin
      Gtk_New (Result, Tag_Name);

      if Font_Attr.Style /= Pango_Style_Normal then
         Set_Property (Result, Text_Tag.Style_Property, Font_Attr.Style);
      end if;

      if Font_Attr.Weight /= Pango_Weight_Normal then
         Set_Property (Result, Text_Tag.Weight_Property, Font_Attr.Weight);
      end if;

      if Color /= Null_Color then
         Set_Property (Result, Foreground_Gdk_Property, Color);
      end if;

      return Result;
   end New_Tag;

   procedure New_Tag
     (Tag      : out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name : String;
      Color    : Gdk_Color) is
   begin
      Gtk_New (Tag, Tag_Name);

      if Color /= Null_Color then
         Set_Property (Tag, Background_Gdk_Property, Color);
      end if;
   end New_Tag;

   ------------------------
   -- To_Font_Attributes --
   ------------------------

   function To_Font_Attributes
     (Style  : Pango.Enums.Style  := Pango.Enums.Pango_Style_Normal;
      Weight : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal)
      return Font_Attributes is
   begin
      return (Style => Style, Weight => Weight);
   end To_Font_Attributes;

   -----------------
   -- Create_Tags --
   -----------------

   function Create_Syntax_Tags
     (Keyword_Color       : Gdk_Color;
      Keyword_Font_Attr   : Font_Attributes := To_Font_Attributes;
      Comment_Color       : Gdk_Color;
      Comment_Font_Attr   : Font_Attributes := To_Font_Attributes;
      Character_Color     : Gdk_Color;
      Character_Font_Attr : Font_Attributes := To_Font_Attributes;
      String_Color        : Gdk_Color;
      String_Font_Attr    : Font_Attributes := To_Font_Attributes)
      return Highlighting_Tags
   is
      Result : Highlighting_Tags;
   begin
      Result (Keyword_Text) := New_Tag
        (Keyword_Color_Tag_Name, Keyword_Color, Keyword_Font_Attr);
      Result (Comment_Text) := New_Tag
        (Comment_Color_Tag_Name, Comment_Color, Comment_Font_Attr);
      Result (String_Text) := New_Tag
        (String_Color_Tag_Name, String_Color, String_Font_Attr);
      Result (Character_Text) := New_Tag
        (Character_Color_Tag_Name, Character_Color, Character_Font_Attr);
      --  ??? Set the tags priority...
      return Result;
   end Create_Syntax_Tags;

   -------------------------------
   -- Create_Highlight_Line_Tag --
   -------------------------------

   procedure Create_Highlight_Line_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk_Color) is
   begin
      New_Tag (Tag, Highlight_Line_Tag_Name, Color);
      --  ??? Set the tag priority...
   end Create_Highlight_Line_Tag;

   ---------------------------------
   -- Create_Highlight_Region_Tag --
   ---------------------------------

   procedure Create_Highlight_Region_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk_Color) is
   begin
      New_Tag (Tag, Highlight_Region_Tag_Name, Color);
      --  ??? Add the priority
   end Create_Highlight_Region_Tag;

end Src_Highlighting;
