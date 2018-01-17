------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2012-2018, AdaCore                   --
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

with GPS.Search;
with Gtk.Tree_Model_Filter;
with Gtk.Tree_Model;

package GPS.Location_View_Filter is

   type Location_View_Filter_Model_Record is
     new Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record with private;
   type Location_View_Filter_Model is
     access all Location_View_Filter_Model_Record'Class;

   procedure Gtk_New
     (Model       : out Location_View_Filter_Model;
      Child_Model : Gtk.Tree_Model.Gtk_Tree_Model);
   --  Create a new model

   procedure Set_Pattern
     (Self         : not null access Location_View_Filter_Model_Record;
      Pattern      : GPS.Search.Search_Pattern_Access);
   --  Sets pattern to be used for filtering.

private

   type Location_View_Filter_Model_Record is
     new Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Record
   with record
      Pattern : GPS.Search.Search_Pattern_Access;
   end record;

end GPS.Location_View_Filter;
