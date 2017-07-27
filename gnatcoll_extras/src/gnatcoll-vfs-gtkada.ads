------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Glib;
with Glib.Values;
with Gtk.List_Store;
with Gtk.Tree_Store;
with Gtk.Tree_Model;

--  This package provides utilities to encapsulate a virtual file
--  in a GValue.

package GNATCOLL.VFS.GtkAda is

   -------------
   -- Gvalues --
   -------------

   procedure Set_File (Value : in out Glib.Values.GValue; File : Virtual_File);
   --  Store File into Value
   --  Value must have been initialized (See Glib.Values.Init) with type
   --  given by Get_Virtual_File_Type, below.

   function Get_File (Value : Glib.Values.GValue) return Virtual_File;
   --  Retrieve the file stored in Value

   function Get_Virtual_File_Type return Glib.GType;
   --  Return the gtype to use for virtual files

   procedure Set_File
     (Tree_Store : access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      File       : Virtual_File);
   procedure Set_File
     (List_Store : access Gtk.List_Store.Gtk_List_Store_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint;
      File       : Virtual_File);
   --  Set a file into a tree store. The column should have been initialized
   --  as GTYpe_Pointer

   function Get_File
     (Tree_Model : access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint) return Virtual_File;
   function Get_File
     (Tree_Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint) return Virtual_File;
   function Get_File
     (Store      : access Gtk.List_Store.Gtk_List_Store_Record'Class;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column     : Glib.Gint) return Virtual_File;
   --  Get a file from a tree model. The column should have been initialized
   --  as GType_Pointer

end GNATCOLL.VFS.GtkAda;
