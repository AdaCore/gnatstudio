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
--  in the syntax highlighting processing.
--  </description>

with Gtk.Text_Tag;

with Language;

package Src_Highlighting is

   type Highlighting_Tags is array (Language.Standout_Language_Entity)
     of Gtk.Text_Tag.Gtk_Text_Tag;
   --  This array contains the tags associated to each Language Entity
   --  Kind that needs to be highlighted in the source editor.

   Keyword_Color_Tag_Name : constant String := "keyword_tag";
   Comment_Color_Tag_Name : constant String := "comment_tag";
   String_Color_Tag_Name  : constant String := "string_tag";
   --  Respectively, the name of the tags use to highlight keywords, comments,
   --  and strings. Declared as public so that one can retrieve them from
   --  the source buffer using their names.

   function Create_Tags
     (Keyword_Color : String;
      Comment_Color : String;
      String_Color  : String) return Highlighting_Tags;
   --  Create a Highlighting_Tags object using the given color names.
   --  If some colors name can not be parsed, then no special color will
   --  be used to highlight the associated source parts.

end Src_Highlighting;
