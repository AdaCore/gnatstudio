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

with Ada.Unchecked_Conversion;
with System;

with Glib.Values;         use Glib.Values;
with GNATCOLL.VFS.GtkAda; use GNATCOLL.VFS.GtkAda;

package body GPS.Location_Model is

   function To_Style is new Ada.Unchecked_Conversion
     (System.Address, GPS.Kernel.Styles.Style_Access);
--     function To_Address is new Ada.Unchecked_Conversion
--       (Style_Access, System.Address);

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return GNATCOLL.VFS.Virtual_File
   is
      Result : GNATCOLL.VFS.Virtual_File;
      Value  : GValue;

   begin
      Model.Get_Value (Iter, Absolute_Name_Column, Value);
      Result := Get_File (Value);
      Unset (Value);

      return Result;
   end Get_File;

   ----------------------------
   -- Get_Highlighting_Style --
   ----------------------------

   function Get_Highlighting_Style
     (Model : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      return GPS.Kernel.Styles.Style_Access
   is
      Result : GPS.Kernel.Styles.Style_Access;
      Value  : GValue;

   begin
      Model.Get_Value (Iter, Highlight_Category_Column, Value);
      Result := To_Style (Get_Address (Value));
      Unset (Value);

      return Result;
   end Get_Highlighting_Style;

end GPS.Location_Model;
