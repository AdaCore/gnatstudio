------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Gdk.RGBA;     use Gdk.RGBA;
with Gtk;          use Gtk;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Pango.Font;   use Pango.Font;

with Language;     use Language;

package body Src_Highlighting is

   -----------
   -- Unref --
   -----------

   procedure Unref (Tags : in out Highlighting_Tags) is
   begin
      for T in Tags'Range loop
         if Tags (T) /= null then
            Unref (Tags (T));
            Tags (T) := null;
         end if;
      end loop;
   end Unref;

   -------------
   -- New_Tag --
   -------------

   procedure New_Tag
     (Tag        : in out Gtk.Text_Tag.Gtk_Text_Tag;
      Tag_Name   : String;
      Fore_Color : Gdk_RGBA := Null_RGBA;
      Back_Color : Gdk_RGBA := Null_RGBA;
      Font_Desc  : Pango_Font_Description := null) is
   begin
      if Tag = null then
         Gtk_New (Tag, Tag_Name);
      end if;

      if Font_Desc /= null then
         Set_Property (Tag, Text_Tag.Font_Desc_Property, Font_Desc);
      end if;

      if Fore_Color /= White_RGBA then
         Set_Property (Tag, Foreground_Rgba_Property, Fore_Color);
      else
         Set_Property (Tag, Foreground_Rgba_Property, Null_RGBA);
      end if;

      if Back_Color /= White_RGBA then
         Set_Property (Tag, Background_Rgba_Property, Back_Color);
      else
         Set_Property (Tag, Background_Rgba_Property, Null_RGBA);
      end if;
   end New_Tag;

   -------------------------------
   -- Create_Highlight_Line_Tag --
   -------------------------------

   procedure Create_Highlight_Line_Tag
     (Tag   : out Gtk.Text_Tag.Gtk_Text_Tag;
      Color : Gdk_RGBA) is
   begin
      Tag := null;
      New_Tag (Tag, Highlight_Line_Tag_Name, Back_Color => Color);
      --  ??? Set the tag priority...
   end Create_Highlight_Line_Tag;

end Src_Highlighting;
