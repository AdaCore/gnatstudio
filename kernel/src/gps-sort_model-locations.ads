------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
