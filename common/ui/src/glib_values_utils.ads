------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2018, AdaCore                   --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package contains a series of subprograms that can be used
--  for GValue_Array hadnling.

with Glib.Object;
with Glib.Values;

with Gdk.RGBA;

with Gtk.List_Store;
with Gtk.Tree_Store;
with Gtk.Tree_Model;

with GNATCOLL.VFS;

with System;

package Glib_Values_Utils is

   subtype Column_Index is Glib.Gint range 0 .. Glib.Gint'Last;

   type Columns_Array is array (Column_Index range <>) of Glib.Gint;

   procedure Init
     (Types   : Glib.GType_Array;
      Columns : Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array);
   --  Initializing each value in the array by GType from Types.
   --  Columns used for represent relationship between Values and Types.

   ------------------------------------------------------------------
   --  Set of functions which return initialized GValue with value --
   ------------------------------------------------------------------

   function As_Boolean (Value : Boolean)        return Glib.Values.GValue;
   function As_String  (Value : String)         return Glib.Values.GValue;
   function As_Int     (Value : Glib.Gint)      return Glib.Values.GValue;
   function As_Pointer (Value : System.Address) return Glib.Values.GValue;
   function As_File
     (Value : GNATCOLL.VFS.Virtual_File) return Glib.Values.GValue;
   function As_Object (Value : Glib.Object.GObject) return Glib.Values.GValue;
   function As_List_Store
     (Value : Gtk.List_Store.Gtk_List_Store) return Glib.Values.GValue;
   function As_Proxy  (Value : Glib.C_Proxy) return Glib.Values.GValue;
   function As_RGBA  (Value : Gdk.RGBA.Gdk_RGBA) return Glib.Values.GValue;

   procedure Unset (Values : in out Glib.Values.GValue_Array);
   --  Unsetting each value in the array

   procedure Set_And_Clear
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values : Glib.Values.GValue_Array);
   --  Setting values to the model and unset the array.
   --  Used the array range for index of columns

   procedure Set_And_Clear
     (Model   : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Columns : Columns_Array;
      Values  : Glib.Values.GValue_Array);
   --  Setting values to the model and unset the array.
   --  Columns is used to map entries in Values with
   --  actual columns in the tree model.

   procedure Set_All_And_Clear
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values : Glib.Values.GValue_Array);
   --  Setting values of all columns to the model and unset the array.
   --  First value set to model's column 0, next set to column 1 and so on.

end Glib_Values_Utils;
