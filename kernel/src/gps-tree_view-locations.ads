------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  This package provides specialized Gtk_Tree_View widget to be used
--  in the locations view. It uses some knownledge of underlying model
--  layout, thus can be used only with models provided by message
--  container. In addition to GPS_Tree_View it:
--
--   - creates columns and renderers at initialization time;
--
--   - creates intermediate filter and sort models which is connected between
--     lowest model and view;
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
--
--   - substitute number of children messages in categories and files nodes.

with Glib;
private with Glib.Main;
private with Gtk.Cell_Renderer_Text;
private with Gtk.Tree_View_Column;
with GPS.Location_View.Listener;
with GPS.Location_View_Filter;

package GPS.Tree_View.Locations is

   type GPS_Locations_Tree_View_Record is
     new GPS_Tree_View_Record with private;

   type GPS_Locations_Tree_View is
     access all GPS_Locations_Tree_View_Record'Class;

   function Get_Filter_Model
     (Self : not null access GPS_Locations_Tree_View_Record)
      return GPS.Location_View_Filter.Location_View_Filter_Model;
   --  Returns filter model.

   procedure Gtk_New
     (Object : out GPS_Locations_Tree_View;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Initialize
     (Self  : not null access GPS_Locations_Tree_View_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model);

   procedure Set_Order
     (Self       : not null access GPS_Locations_Tree_View_Record'Class;
      File_Order : GPS.Location_View.Listener.File_Sort_Order;
      Msg_Order  : GPS.Location_View.Listener.Messages_Sort_Order);
   --  Sets sorting order

   procedure Location_Clicked
     (Self : not null access GPS_Locations_Tree_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Emits "location-clicked" signal.

   function Get_Multiple_Action
     (Self : not null access GPS_Locations_Tree_View_Record'Class)
      return Boolean with Inline;
   --  Returns True if a last click was on message's action
   --  and more than one message is selected.

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

      Filter                  :
        GPS.Location_View_Filter.Location_View_Filter_Model;
      --  Intermediate model to support filtering of items in the view

      On_Row_Expanded_Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      On_Row_Expanded_Handler : Glib.Main.G_Source_Id :=
        Glib.Main.No_Source_Id;
      --  Context for scrolling after node expansion. Path points to the top
      --  expanded node. Handler is a Gtk+ idle handler.

      Multiple_Action         : Boolean := False;
      --  Set True when clicked on message's action
      --  and more than one message is selected
   end record;

   overriding procedure On_Lowest_Model_Row_Inserted
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

   overriding function To_Lowest_Model_Iter
     (Self : not null access GPS_Locations_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Converts iterator from the view's source model to lowerst model.

end GPS.Tree_View.Locations;
