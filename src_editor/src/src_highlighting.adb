-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gdk.Color;    use Gdk.Color;
with Gtk;          use Gtk;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Widget;   use Gtk.Widget;
with Pango.Enums;  use Pango.Enums;

with Language;     use Language;

package body Src_Highlighting is

   --------------------------
   -- Forward_Declarations --
   --------------------------

   procedure Create_Color
     (Color      : out Gdk_Color;
      Color_Name : String;
      Success    : out Boolean);
   --  Try to create a Gdk_Color by parsing the given Color_Name. The
   --  returned color is also already allocated. Sucess is set to False if
   --  the Color_Name could not be parsed or the new color could not be
   --  allocated.

   procedure Set_Foreground_Color
     (Tag        : access Gtk_Text_Tag_Record'Class;
      Color_Name : String);
   --  Set the Foreground_Gdk property of the given tag with the parsed
   --  Color_Name (the color is also allocated).
   --
   --  Error_Handling:
   --  If Color_Name can not be parsed or if the resulting color could not be
   --  allocated, the foreground property is not set. The default font color
   --  (probably black) will then be in effect.

   function New_Tag
     (Tag_Name   : String;
      Color_Name : String;
      Font_Attr  : Font_Attributes) return Gtk_Text_Tag;
   --  Create a new Gtk_Text_Tag with the given name. If Color_Name is not the
   --  empty string and can be parsed, then the Foreground_Gdk_Property is set
   --  with the given color.

   procedure New_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name   : String;
      Color_Name : String);
   --  Create a new Gtk_Text_Tag with the given Tag_Name. Color_Name is used
   --  to set the Background_Gdk_Property.

   ------------------
   -- Create_Color --
   ------------------

   procedure Create_Color
     (Color      : out Gdk_Color;
      Color_Name : String;
      Success    : out Boolean)
   is
   begin
      Color := Parse (Color_Name);
      Alloc_Color (Get_Default_Colormap, Color, Success => Success);

   exception
      when Wrong_Color =>
         --  The given color could not be parsed. Return a Failure. Also
         --  return a Null_Color to avoid returning an un-initialized value.
         Color := Null_Color;
         Success := False;
   end Create_Color;

   --------------------------
   -- Set_Foreground_Color --
   --------------------------

   procedure Set_Foreground_Color
     (Tag        : access Gtk_Text_Tag_Record'Class;
      Color_Name : String)
   is
      New_Color : Gdk_Color;
      Success   : Boolean;
   begin
      Create_Color (New_Color, Color_Name, Success);
      if Success then
         Set_Property (Tag, Foreground_Gdk_Property, New_Color);
      end if;
   end Set_Foreground_Color;


   -------------
   -- New_Tag --
   -------------

   function New_Tag
     (Tag_Name   : String;
      Color_Name : String;
      Font_Attr  : Font_Attributes) return Gtk_Text_Tag
   is
      Result    : Gtk_Text_Tag;
   begin
      Gtk_New (Result, Tag_Name);

      if Font_Attr.Style /= Pango_Style_Normal then
         Set_Property (Result, Text_Tag.Style_Property, Font_Attr.Style);
      end if;

      if Font_Attr.Weight /= Pango_Weight_Normal then
         Set_Property (Result, Text_Tag.Weight_Property, Font_Attr.Weight);
      end if;

      if Color_Name /= "" then
         Set_Foreground_Color (Result, Color_Name);
      end if;

      return Result;
   end New_Tag;

   procedure New_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name   : String;
      Color_Name : String)
   is
      New_Color : Gdk_Color;
      Success : Boolean;
   begin
      Gtk_New (Tag, Tag_Name);
      Create_Color (New_Color, Color_Name, Success);
      if Success then
         Set_Property (Tag, Background_Gdk_Property, New_Color);
      end if;
   end New_Tag;

   ------------------------
   -- To_Font_Attributes --
   ------------------------

   function To_Font_Attributes
     (Style  : Pango.Enums.Style  := Pango.Enums.Pango_Style_Normal;
      Weight : Pango.Enums.Weight := Pango.Enums.Pango_Weight_Normal)
     return Font_Attributes
   is
   begin
      return (Style => Style, Weight => Weight);
   end To_Font_Attributes;

   -----------------
   -- Create_Tags --
   -----------------

   function Create_Syntax_Tags
     (Keyword_Color     : String;
      Keyword_Font_Attr : Font_Attributes := To_Font_Attributes;
      Comment_Color     : String;
      Comment_Font_Attr : Font_Attributes := To_Font_Attributes;
      String_Color      : String;
      String_Font_Attr  : Font_Attributes := To_Font_Attributes)
     return Highlighting_Tags
   is
      Result : Highlighting_Tags;
   begin
      Result (Keyword_Text) :=
        New_Tag (Keyword_Color_Tag_Name, Keyword_Color, Keyword_Font_Attr);
      Result (Comment_Text) :=
        New_Tag (Comment_Color_Tag_Name, Comment_Color, Comment_Font_Attr);
      Result (String_Text) :=
        New_Tag (String_Color_Tag_Name, String_Color, String_Font_Attr);
      --  ??? Set the tags priority...
      return Result;
   end Create_Syntax_Tags;

   -------------------------------
   -- Create_Highlight_Line_Tag --
   -------------------------------

   procedure Create_Highlight_Line_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color_Name : String) is
   begin
      New_Tag (Tag, Highlight_Line_Tag_Name, Color_Name);
      --  ??? Set the tag priority...
   end Create_Highlight_Line_Tag;

   ---------------------------------
   -- Create_Highlight_Region_Tag --
   ---------------------------------

   procedure Create_Highlight_Region_Tag
     (Tag        : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color_Name : String) is
   begin
      New_Tag (Tag, Highlight_Region_Tag_Name, Color_Name);
      --  ??? Add the priority
   end Create_Highlight_Region_Tag;

end Src_Highlighting;
