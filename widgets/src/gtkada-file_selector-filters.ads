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

--  <description>
--  This package provides a collection of default filters for the
--  file selector. Each of them can be registered when a new file selector is
--  created.
--  </description>

with Gdk.Bitmap;
with Gdk.Pixmap;
with GNAT.OS_Lib;

package Gtkada.File_Selector.Filters is

   procedure Create_Filters;
   --  This creates all the default filters. This function will be called
   --  automatically the first time you access one of these filters, but you
   --  might also want to explicitly call it in your program

   -------------------------
   -- Project file filter --
   -------------------------

   type Project_File_Filter_Record (<>) is new File_Filter_Record with private;
   type Project_File_Filter is access all Project_File_Filter_Record'Class;

   function Prj_File_Filter return Project_File_Filter;
   --  Return a new filter that only shows the Glide project files.

private

   procedure Use_File_Filter
     (Filter    : access Project_File_Filter_Record;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : String;
      File      : String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access);

   type Project_File_Filter_Record is new File_Filter_Record with null record;

end Gtkada.File_Selector.Filters;
