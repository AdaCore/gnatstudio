-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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

--  This is a very dirty package used to test the Gtkada.File_Selector.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtk.Widget; use Gtk.Widget;

package Test_File_Selector is

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   type Filter_Show_Ada is new File_Filter_Record (new String'("Ada Files"))
     with record
        Spec_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Body_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Spec_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
        Body_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
     end record;

   type Filter_Show_Ada_Access is access all Filter_Show_Ada'Class;

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Ada;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access);

end Test_File_Selector;
