-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Ctree;
with Gtk.Widget;
with Gtk.Menu;
with Gtk.Style;
with Gdk.Pixmap;
with Gdk.Bitmap;
with GVD.Types;

--  This package implements a file explorer and browser.
--  It shows all the files that belong to the current application, and makes
--  it possible to browse the list of entities defined in each of these files.
--  The files are organized into several categories:
--     - File extension
--     - System and user files

package GVD.Explorer is

   type Explorer_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Explorer_Access is access all Explorer_Record'Class;

   procedure Gtk_New
     (Explorer    : out Explorer_Access;
      Code_Editor : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new explorer

   procedure Add_File_Node
     (Tree      : access Explorer_Record;
      File_Name : String);
   --  Insert a node for a new file.

   procedure Add_List_Of_Files
     (Tree : access Explorer_Record;
      List : GVD.Types.String_Array);
   --  Add several files in the explorer.

   procedure Set_Current_File
     (Tree : access Explorer_Record;
      File_Name : String);
   --  Set a new current file.
   --  The entry in the tree for this file is made visible, and highlighted.

   procedure Set_Current_Line
     (Tree : access Explorer_Record;
      Line : Natural);
   --  Set the line in the current file, ie the line on which the debugger
   --  is stopped.

   function Get_Current_Line (Tree : access Explorer_Record) return Natural;
   --  Return the current line number.
   --  This is the line on which the debugger is stopped.

   function Get_Current_File (Tree : access Explorer_Record) return String;
   --  Return the name of the file on which the debugger is stopped,
   --  or "" if the current file is not known.

   procedure On_Executable_Changed
     (Explorer : access Explorer_Record'Class);
   --  Called when the executable associated with the explorer has changed.

   procedure Preferences_Changed
     (Explorer : access Explorer_Record'Class);
   --  Called when the preferences have changed, and the explorer should be
   --  redisplayed with the new setup.

private
   type Explorer_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Explorer_Root      : Gtk.Ctree.Gtk_Ctree_Node;
      Code_Editor        : Gtk.Widget.Gtk_Widget;
      Current_File_Style : Gtk.Style.Gtk_Style;
      Current_File_Node  : Gtk.Ctree.Gtk_Ctree_Node;

      Current_Line       : Natural := 1;
      --  Line to use when displaying the current file.

      CR_Stripped        : Boolean := False;
      --  State of the Should_Strip_CR preference when the tree was last
      --  computed.

      Folder_Pixmap      : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Mask        : Gdk.Bitmap.Gdk_Bitmap;
      Folder_Open_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Open_Mask   : Gdk.Bitmap.Gdk_Bitmap;
      Contextual_Menu    : Gtk.Menu.Gtk_Menu;
      TTY_Mode           : Boolean := False;
   end record;
end GVD.Explorer;
