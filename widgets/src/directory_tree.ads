-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
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

--  This package implements a special version of a Gtk_Ctree that allows
--  browsing a complete directory hierarchy.
--  Initially, no node is expanded, so that the creation of the tree is as
--  fast as possible.
--  The directories on the disk are read only when a directory is expanded.
--
--  All directories passed as parameter to this file should use either '/'
--  or GNAT.Os_Lib.Directory_Separator as a separator.
--  They must end with a directory separator.

with Gtk.Ctree;
with Gdk.Pixmap;
with Gdk.Bitmap;

package Directory_Tree is

   type Dir_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Dir_Tree is access all Dir_Tree_Record'Class;

   procedure Gtk_New (Tree : out Dir_Tree; Root : String);
   --  Create a new tree, whose root node points to the directory Root.

   procedure Initialize (Tree : access Dir_Tree_Record'Class; Root : String);
   --  Internal function used to create the tree.

   procedure Show_Directory (Tree : access Dir_Tree_Record; Dir : String);
   --  Expand the tree so that the directory Dir is visible.
   --  Dir must end with a directory separator.

   procedure Add_Directory (Tree : access Dir_Tree_Record; Dir : String);
   --  Add the absolute directory Dir to the tree.
   --  A new node is created if the directory is not already in the tree.
   --  Dir must end with a directory separator.

   function Get_Selection (Tree : access Dir_Tree_Record) return String;
   --  Return the absolute directory for the selected node.
   --  An empty string "" is returned if there is no selection currently.

private
   type Dir_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Folder_Pix, Ofolder_Pix : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Mask, Ofolder_Mask : Gdk.Bitmap.Gdk_Bitmap;
   end record;
end Directory_Tree;
