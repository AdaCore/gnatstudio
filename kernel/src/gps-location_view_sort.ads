------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

--  A model that provides sorting for the Locations view.

with Gtk.Tree_Model_Sort; use Gtk.Tree_Model_Sort;
with Gtk.Tree_Model;      use Gtk.Tree_Model;

package GPS.Location_View_Sort is

   type Messages_Sort_Order is (By_Location, By_Weight);

   type File_Sort_Order is
     (Category_Default_Sort,
      Alphabetical);
   --  The sort order of files. Each category defines its own default order
   --  (which is set in the model itself), but users can override it if needed.

   type Locations_Proxy_Model_Record is
     new Gtk_Tree_Model_Sort_Record with
      record
         Messages_Order : Messages_Sort_Order := By_Location;
         --  Sort order for locations within a file.

         File_Order : File_Sort_Order := Category_Default_Sort;
         --  Sort order for files within a category
      end record;

   type Locations_Proxy_Model is
     access all Locations_Proxy_Model_Record'Class;

   procedure Gtk_New
     (Model  : in out Locations_Proxy_Model;
      Source : not null
         access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class);
   --  Wraps a locations model so that we can provide sorting without changing
   --  the model itself.

   procedure Set_Order
     (Self       : not null access Locations_Proxy_Model_Record'Class;
      File_Order : File_Sort_Order;
      Msg_Order  : Messages_Sort_Order);
   --  Set the sort order for Self.

end GPS.Location_View_Sort;
