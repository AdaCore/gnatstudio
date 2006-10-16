-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gdk.Pixbuf;  use Gdk.Pixbuf;

with Gtkada.File_Selector; use Gtkada.File_Selector;

with VFS; use VFS;

package Aunit_Filters is

      type Test_Type is (Unknown, Test_Case, Test_Suite);

   --  This package provides file selector filters to use with Aunit dialogs.

   procedure Get_Suite_Name
     (File_Name    : String;
      Package_Name : out GNAT.OS_Lib.String_Access;
      Suite_Name   : out GNAT.OS_Lib.String_Access;
      F_Type       : out Test_Type);
   --  Open File_Name, do a basic parsing on it to look for aunit tests or
   --  tests suite. If found, then Suite_Name is initialized to this test/suite
   --  name, and Package_Name to the name of the main unit in the file.
   --  The parsing is done by looking for the first occurence of " is " in
   --  non-commented lines.
   --  Return values must be freed by the user.

   type Filter_Show_Ada is new File_Filter_Record with record
      Spec_Pixbuf : Gdk_Pixbuf;
      Body_Pixbuf : Gdk_Pixbuf;
   end record;

   type Filter_Show_Ada_Access is access all Filter_Show_Ada'Class;
   --  This filter shows all files, leaving only Ada files selectable.
   --  ??? Should eventually be replaced by a filter that only shows the files
   --  belonging to a project. This is easier to implement and more general

   type Filter_Show_Tests is new File_Filter_Record with record
      Pixbuf : Gdk_Pixbuf;
   end record;
   type Filter_Show_Tests_Access is access all Filter_Show_Tests'Class;
   --  This filter shows only files containing tests.

   type Filter_Show_Suites is new File_Filter_Record with record
      Pixbuf : Gdk_Pixbuf;
   end record;
   type Filter_Show_Suites_Access is access all Filter_Show_Suites'Class;
   --  This filter shows only files containing tests or test suites.

   procedure Use_File_Filter
     (Filter : access Filter_Show_Ada;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Ada.
   --  Text is set to the name of the main unit in the file.

   procedure Use_File_Filter
     (Filter : access Filter_Show_Suites;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Suites.
   --  Text is set to the name of the suite found in the file.

   procedure Use_File_Filter
     (Filter : access Filter_Show_Tests;
      Win    : access File_Selector_Window_Record'Class;
      File   : VFS.Virtual_File;
      State  : out File_State;
      Pixbuf : out Gdk_Pixbuf;
      Text   : out GNAT.OS_Lib.String_Access);
   --  Use_File_Filter procedure for Filter_Show_Tests.
   --  Text is set to the name of the suite/test case found in the file.

end Aunit_Filters;
