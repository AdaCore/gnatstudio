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
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Widget;   use Gtk.Widget;

with Language;     use Language;

package body Src_Highlighting is

   --------------------------
   -- Forward_Declarations --
   --------------------------

   function New_Tag
     (Tag_Name   : String;
      Color_Name : String) return Gtk_Text_Tag;
   --  Create a new Gtk_Text_Tag with the given name. If Color_Name can
   --  be parsed, then the Foreground_Gdk_Property is set with the given
   --  color.

   -------------
   -- New_Tag --
   -------------

   function New_Tag
     (Tag_Name   : String;
      Color_Name : String) return Gtk_Text_Tag
   is
      Result    : Gtk_Text_Tag;
      New_Color : Gdk_Color;
      Success   : Boolean;

   begin
      Gtk_New (Result, Tag_Name);

      New_Color := Parse (Color_Name);
      Alloc_Color (Get_Default_Colormap, New_Color, Success => Success);

      if Success then
         Set_Property (Result, Foreground_Gdk_Property, New_Color);
      end if;

      return Result;

   exception
      when Wrong_Color =>
         return Result;
   end New_Tag;

   -----------------
   -- Create_Tags --
   -----------------

   function Create_Tags
     (Keyword_Color : String;
      Comment_Color : String;
      String_Color  : String) return Highlighting_Tags
   is
      Result : Highlighting_Tags;
   begin
      Result (Keyword_Text) := New_Tag (Keyword_Color_Tag_Name, Keyword_Color);
      Result (Comment_Text) := New_Tag (Comment_Color_Tag_Name, Comment_Color);
      Result (String_Text)  := New_Tag (String_Color_Tag_Name, String_Color);

      return Result;
   end Create_Tags;

end Src_Highlighting;
