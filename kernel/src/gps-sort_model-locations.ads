-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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
--  This is concrete implementation of generic sorting proxy model to be used
--  in locations view.

package GPS.Sort_Model.Locations is

   type Locations_Proxy_Model_Record is
     new GPS_Sort_Model_Record with private;

   type Locations_Proxy_Model is
     access all Locations_Proxy_Model_Record'Class;

   procedure Gtk_New
     (Model  : in out Locations_Proxy_Model;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Initialize
     (Self   : not null access Locations_Proxy_Model_Record'Class;
      Source : not null Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Set_Locations_Order
     (Self : not null access Locations_Proxy_Model_Record'Class);
   --  Set sorting by locations mode

   procedure Set_Weight_Order
     (Self : not null access Locations_Proxy_Model_Record'Class);
   --  Set sorting by weight (subcategory) mode

private

   type Compare_Function is
     access function
       (Self : not null access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
        A    : Gtk.Tree_Model.Gtk_Tree_Iter;
        B    : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;

   type Locations_Proxy_Model_Record is
     new GPS_Sort_Model_Record with record
      Compare : Compare_Function;
   end record;

   overriding function Less_Than
     (Self  : not null access Locations_Proxy_Model_Record;
      Left  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Right : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

end GPS.Sort_Model.Locations;
