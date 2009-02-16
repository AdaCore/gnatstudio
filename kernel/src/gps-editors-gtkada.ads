-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

--  This package provides interfaces on top of GPS.Editors that are
--  specialized for GtkAda

with Glib.Values;      use Glib.Values;
with Gtk.Tree_Model;   use Gtk.Tree_Model;

package GPS.Editors.GtkAda is

   -----------
   -- Marks --
   -----------

   function Get_Editor_Mark_Type return Glib.GType;
   --  Return the GType to use for editor marks

   procedure Set_Mark (Value : in out Glib.Values.GValue;
                       Mark  : Editor_Mark'Class);
   --  Store the mark in Value. Value must have been initialized through
   --  Glib.Values.Init with the type given by Get_Editor_Mark_Type.

   function Get_Mark (Value : Glib.Values.GValue) return Editor_Mark'Class;
   --  Retrieve the mark stored in the value

   function Get_Mark
     (Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return Editor_Mark'Class;
   --  Return the mark stored in the tree at the specified location

end GPS.Editors.GtkAda;
