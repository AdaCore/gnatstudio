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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gdk.Pixmap;
with Gdk.Bitmap;

with Gtkada.File_Selector; use Gtkada.File_Selector;

package Aunit_Filters is

   --  This package provides file selector filters to use with Aunit dialogs.

   procedure Get_Suite_Name
     (File_Name : in String;
      Package_Name : out GNAT.OS_Lib.String_Access;
      Suite_Name   : out GNAT.OS_Lib.String_Access);
   --  Open File_Name, do a basic parsing on it to look for aunit tests or
   --  tests suite. If found, then Suite_Name is initialized to this test/suite
   --  name, and Package_Name to the name of the main unit in the file.
   --  The parsing is done by looking for the first occurence of " is " in
   --  non-commented lines, and

   type Filter_Show_Ada is new File_Filter_Record (new String'("Ada Files"))
     with record
        Spec_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Body_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Spec_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
        Body_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
     end record;
   type Filter_Show_Ada_Access is access all Filter_Show_Ada'Class;
   --  This filter shows all files, leaving only Ada files selectable.

   type Filter_Show_Tests is
     new File_Filter_Record (new String'("Suite and Test files"))
     with record
        Suite_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Suite_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
     end record;
   type Filter_Show_Tests_Access is access all Filter_Show_Tests'Class;
   --  This filter shows only files containing tests.

   type Filter_Show_Suites is
     new File_Filter_Record (new String'("Suite files"))
     with record
        Suite_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
        Suite_Bitmap : Gdk.Bitmap.Gdk_Bitmap;
     end record;
   type Filter_Show_Suites_Access is access all Filter_Show_Suites'Class;
   --  This filter shows only files containing tests or test suites.

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Ada;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Ada.
   --  Text is set to the name of the main unit in the file.

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Suites;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Suites.
   --  Text is set to the name of the suite found in the file.

   procedure Use_File_Filter
     (Filter    : access Filter_Show_Tests;
      Win       : access File_Selector_Window_Record'Class;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Tests.
   --  Text is set to the name of the suite/test case found in the file.

end Aunit_Filters;
