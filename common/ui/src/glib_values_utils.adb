------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2013-2019, AdaCore                   --
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

with GNATCOLL.VFS.GtkAda;

package body Glib_Values_Utils is

   ----------------
   -- As_Boolean --
   ----------------

   function As_Boolean (Value : Boolean) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init_Set_Boolean (Result, Value);
      end return;
   end As_Boolean;

   -------------
   -- As_File --
   -------------

   function As_File
     (Value : GNATCOLL.VFS.Virtual_File) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init (Result, GNATCOLL.VFS.GtkAda.Get_Virtual_File_Type);
         GNATCOLL.VFS.GtkAda.Set_File (Result, Value);
      end return;
   end As_File;

   ------------
   -- As_Int --
   ------------

   function As_Int (Value : Glib.Gint) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init_Set_Int (Result, Value);
      end return;
   end As_Int;

   -------------------
   -- As_List_Store --
   -------------------

   function As_List_Store
     (Value : Gtk.List_Store.Gtk_List_Store) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init (Result, Gtk.List_Store.Get_Type);
         Glib.Values.Set_Object (Result, Value);
      end return;
   end As_List_Store;

   ---------------
   -- As_Object --
   ---------------

   function As_Object
     (Value : Glib.Object.GObject) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init       (Result, Glib.GType_Object);
         Glib.Values.Set_Object (Result, Value);
      end return;
   end As_Object;

   ----------------
   -- As_Pointer --
   ----------------

   function As_Pointer (Value : System.Address) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init (Result, Glib.GType_Pointer);
         Glib.Values.Set_Address (Result, Value);
      end return;
   end As_Pointer;

   --------------
   -- As_Proxy --
   --------------

   function As_Proxy (Value : Glib.C_Proxy) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init (Result, Glib.GType_Pointer);
         Glib.Values.Set_Proxy (Result, Value);
      end return;
   end As_Proxy;

   ---------------
   -- As_String --
   ---------------

   function As_String (Value : String) return Glib.Values.GValue is
   begin
      return Result : Glib.Values.GValue do
         Glib.Values.Init_Set_String (Result, Value);
      end return;
   end As_String;

   ----------
   -- Init --
   ----------

   procedure Init
     (Types   : Glib.GType_Array;
      Columns : Glib.Gint_Array;
      Values  : in out Glib.Values.GValue_Array) is
   begin
      for Index in Values'Range loop
         Glib.Values.Init
           (Values (Index),
            Types (Glib.Guint (Columns (Natural (Index)))));
      end loop;
   end Init;

   -----------------------
   -- Set_All_And_Clear --
   -----------------------

   procedure Set_All_And_Clear
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values : Glib.Values.GValue_Array)
   is
      Local : Glib.Values.GValue_Array := Values;
   begin
      Model.Set (Iter, Local);
      Unset (Local);
   end Set_All_And_Clear;

   -------------------
   -- Set_And_Clear --
   -------------------

   procedure Set_And_Clear
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Values : Glib.Values.GValue_Array)
   is
      Local   : Glib.Values.GValue_Array := Values;
      Columns : Columns_Array (Local'Range);
   begin
      for Index in Local'Range loop
         Columns (Index) := Index;
      end loop;

      Model.Set (Iter, Glib.Gint_Array (Columns), Local);
      Unset (Local);
   end Set_And_Clear;

   -------------------
   -- Set_And_Clear --
   -------------------

   procedure Set_And_Clear
     (Model   : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Columns : Columns_Array;
      Values  : Glib.Values.GValue_Array)
   is
      Local : Glib.Values.GValue_Array := Values;
   begin
      Model.Set (Iter, Glib.Gint_Array (Columns), Local);
      Unset (Local);
   end Set_And_Clear;

   -----------
   -- Unset --
   -----------

   procedure Unset (Values : in out Glib.Values.GValue_Array) is
   begin
      for Index in Values'Range loop
         Glib.Values.Unset (Values (Index));
      end loop;
   end Unset;

end Glib_Values_Utils;
