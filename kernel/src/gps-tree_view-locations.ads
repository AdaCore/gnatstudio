-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2009-2010, AdaCore                  --
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
--  in the locations view. It uses some knownledge of underling model
--  layout, thus can be used only with model from locations window. In
--  addition to GPS_Tree_View it:
--
--   - creates columns and renderers at initialization time;
--
--   - handle "query-tooltip" request and display tooltip when visible
--     area of the view is insufficient to display all text of the message;
--
--   - make primary message node expanded by default to allow secondary
--     messages to be visible. Primary message node detected by its depth
--     in the tree (level 3 for now);
--
--   - view is scrolled automatically to make first child visible when node
--     is expanded;
--
--   - emit "action_clicked" signal on click on action's column;
--
--   - emit "location_clicked" signal on click on location's column.

with Glib;
private with Glib.Main;
private with Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model_Filter;
with Gtk.Tree_View_Column;

package GPS.Tree_View.Locations is

   type GPS_Locations_Tree_View_Record is
     new GPS_Tree_View_Record with private;

   type GPS_Locations_Tree_View is
     access all GPS_Locations_Tree_View_Record'Class;

   procedure Gtk_New
     (Object : in out GPS_Locations_Tree_View;
      Filter : out Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Model  : not null Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Initialize
     (Self   : not null access GPS_Locations_Tree_View_Record'Class;
      Filter : out Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter;
      Model  : not null Gtk.Tree_Model.Gtk_Tree_Model);

   Signal_Action_Clicked   : constant Glib.Signal_Name;
   --  Emitted on click in action column.
   --  procedure Handler
   --    (Self : not null access GPS_Locations_Tree_View_Record'Class;
   --     Path : Gtk_Tree_Path;
   --     Iter : Gtk_Tree_Iter);

   Signal_Location_Clicked : constant Glib.Signal_Name;
   --  Emitted on click in locations column.
   --  procedure Handler
   --    (Self : not null access GPS_Locations_Tree_View_Record'Class;
   --     Path : Gtk_Tree_Path;
   --     Iter : Gtk_Tree_Iter);

   function Sorting_Column
     (Self : not null access GPS_Locations_Tree_View_Record'Class)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   --  This one for use by old implementation only

private

   Signal_Action_Clicked   : constant Glib.Signal_Name := "action_clicked";
   Signal_Location_Clicked : constant Glib.Signal_Name := "location_clicked";

   type GPS_Locations_Tree_View_Record is
     new GPS_Tree_View_Record with record
      Action_Column           : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  Column for action icon
      Location_Column         : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      --  Column for location and text of the message; it is used for the name
      --  of category and file also.
      Text_Renderer           : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      --  Renderer for location and its text

      On_Row_Expanded_Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      On_Row_Expanded_Handler : Glib.Main.G_Source_Id :=
        Glib.Main.No_Source_Id;
      --  Context for scrolling after node expansion. Path points to the top
      --  expanded node. Handler is a Gtk+ idle handler.
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

   overriding function To_Lowerst_Model_Iter
     (Self : not null access GPS_Locations_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts iterator from the view's source model to lowerst model.

end GPS.Tree_View.Locations;
