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
      New_Color := Parse (Color_Name);
      Alloc_Color (Get_Default_Colormap, New_Color, Success => Success);

      if Success then
         Set_Property (Tag, Foreground_Gdk_Property, New_Color);
      end if;
      --  ??? Report a warning when failed to allocate the color?

   exception
      when Wrong_Color =>
         --  The given color could not be parsed. We don't set the
         --  foreground_gdk_property, so there is nothing more to do.
         null;
         --  ??? Maybe report a warning?
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

   function Create_Tags
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
      return Result;
   end Create_Tags;

end Src_Highlighting;
