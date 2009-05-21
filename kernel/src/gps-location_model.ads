-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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

with Glib;
with Gdk.Color;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gtk.Tree_Model;    use Gtk.Tree_Model;
with Gtk.Tree_Store;    use Gtk.Tree_Store;

with GNATCOLL.VFS;

with Basic_Types;       use Basic_Types;
with GPS.Editors;       use GPS.Editors;
with GPS.Kernel.Styles; use GPS.Kernel.Styles;

package GPS.Location_Model is

   Icon_Column               : constant := 0;
   Base_Name_Column          : constant := 1;
   Absolute_Name_Column      : constant := 2;   --  Get_File
   Mark_Column               : constant := 3;
   Line_Column               : constant := 4;
   Column_Column             : constant := 5;
   Length_Column             : constant := 6;
   Color_Column              : constant := 7;
   Highlight_Column          : constant := 10;
   Highlight_Category_Column : constant := 11;  --  Get_Highlighting_Style
   Number_Of_Items_Column    : constant := 12;
   Category_Line_Column      : constant := 13;

   Messages_Padding : constant Integer := 10;

   function Get_File
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File;
   --  Return the file stored at Iter

   function Get_Highlighting_Style
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      return GPS.Kernel.Styles.Style_Access;
   --  Return the highlighting style stored at Iter

   procedure Fill_Iter
     (Model              : Gtk_Tree_Store;
      Iter               : Gtk_Tree_Iter;
      Base_Name          : String;
      Absolute_Name      : GNATCOLL.VFS.Virtual_File;
      Message            : Glib.UTF8_String;
      Mark               : Editor_Mark'Class := Nil_Editor_Mark;
      Line               : Integer;
      Column             : Visible_Column_Type;
      Length             : Integer;
      Highlighting       : Boolean;
      Highlight_Category : Style_Access;
      Pixbuf             : Gdk.Pixbuf.Gdk_Pixbuf := Null_Pixbuf;
      Color              : access Gdk.Color.Gdk_Color := null);
   --  Fill information in Iter.
   --  Base_Name can be left to the empty string, it will then be computed
   --  automatically from Absolute_Name.
   --  If Line is 0, consider the item as a non-leaf item.

end GPS.Location_Model;
