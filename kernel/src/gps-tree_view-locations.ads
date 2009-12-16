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
--  This package provides specialized Gtk_Tree_View widget to be used
--  in the locations view. In addition to GPS_Tree_View it:
--
--   - make primary message node expanded by default to allow secondary
--     messages to be visible. Primary message node detected by its depth
--     in the tree (level 3 for now);
--
--   - view is scrolled automatically to make first child visible when node
--     is expanded.

private with Glib.Main;

package GPS.Tree_View.Locations is

   type GPS_Locations_Tree_View_Record is
     new GPS_Tree_View_Record with private;

   type GPS_Locations_Tree_View is
     access all GPS_Locations_Tree_View_Record'Class;

   procedure Gtk_New (Object : in out GPS_Locations_Tree_View);
   procedure Gtk_New
     (Object : in out GPS_Locations_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

   procedure Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class);
   procedure Initialize
     (Self  : not null access GPS_Locations_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class);

private

   type GPS_Locations_Tree_View_Record is
     new GPS_Tree_View_Record with record
      On_Row_Expanded_Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      On_Row_Expanded_Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      On_Row_Expanded_Handler : Glib.Main.G_Source_Id :=
        Glib.Main.No_Source_Id;
   end record;

   overriding procedure On_Lowerst_Model_Row_Inserted
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access);
   --  Force expanded state for all nodes of level 3 (which means primary
   --  message).

   overriding procedure On_Row_Expanded
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access);
   --  Registers idle callback to scroll view to make visible the first child
   --  node.

end GPS.Tree_View.Locations;
