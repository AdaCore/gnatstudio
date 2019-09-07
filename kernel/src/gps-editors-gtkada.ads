------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package provides interfaces on top of GPS.Editors that are
--  specialized for GtkAda

with Gtkada.MDI;

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
     (Model  : not null
          access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint) return Editor_Mark'Class;
   --  Return the mark stored in the tree at the specified location

   ---------
   -- MDI --
   ---------

   function Get_MDI_Child
     (This : Editor_View'Class) return Standard.Gtkada.MDI.MDI_Child;
   --  Return the MDI child created for this view

end GPS.Editors.GtkAda;
