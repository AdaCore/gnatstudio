-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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
with Language;
with Gtk.Style;
with GVD.Types;
with Gdk.Pixmap;
with Gdk.Bitmap;

--  This package implements a file explorer and browser.
--  It shows all the files that belong to the current application, and makes
--  it possible to browse the list of entities defined in each of these files.
--  The files are organized into several categories:
--     - File extension
--     - System and user files

package GVD.Explorer is

   type Explorer_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Explorer_Access is access all Explorer_Record'Class;

   type Position_Type is new Natural;
   --  Indicates the position in a file.
   --  Note that these positions are relative to the real contents of the
   --  editor, not necessarily the positions visible to the user (which
   --  might be different because of ASCII.HT handling)

   procedure Gtk_New
     (Explorer    : out Explorer_Access;
      Code_Editor : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new explorer

   type Explorer_Handler is access
     procedure
       (Widget   : access Explorer_Record'Class;
        Position : Position_Type);
   --  Handler called when an item is selected in the tree.
   --  Index is the position in the buffer where the selected entity
   --  starts.
   --  Widget is the Window parameter given to Explore below.

   procedure Explore
     (Tree      : access Explorer_Record;
      Root      : Gtk.Ctree.Gtk_Ctree_Node;
      Window    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Buffer    : String;
      Lang      : Language.Language_Access;
      File_Name : String);
   --  Parse the entities present in buffer.
   --  The items for the explorer are added to Tree, as children of the
   --  Root Node
   --  See Explorer_Handler above for a description of Handler.

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


private
   type Explorer_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Explorer_Root      : Gtk.Ctree.Gtk_Ctree_Node;
      Code_Editor        : Gtk.Widget.Gtk_Widget;
      Current_File_Style : Gtk.Style.Gtk_Style;
      File_Name_Style    : Gtk.Style.Gtk_Style;
      Current_File_Node  : Gtk.Ctree.Gtk_Ctree_Node;

      Current_Line       : Natural := 1;
      --  Line to use when displaying the current file.

      Folder_Pixmap      : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Mask        : Gdk.Bitmap.Gdk_Bitmap;
      Folder_Open_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Open_Mask   : Gdk.Bitmap.Gdk_Bitmap;
   end record;
end GVD.Explorer;
