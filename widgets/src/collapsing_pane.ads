------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

--  This package implements a pane that can be folded / unfolded by clicking on
--  the head pixmap & label. The user can add two widgets, the first one will
--  be displayed on the collapsed state and the other one on the expanded
--  state.

with Glib;          use Glib;

with Gtk.Arrow;
with Gtk.Box;
with Gtk.Container;
with Gtk.Event_Box;
with Gtk.Label;
with Gtk.Widget;

package Collapsing_Pane is

   type Collapsing_Pane_Record is new Gtk.Container.Gtk_Container_Record with
     private;

   type Collapsing_Pane is access all Collapsing_Pane_Record'Class;

   procedure Gtk_New (Pane : out Collapsing_Pane; Label : UTF8_String);
   --  Creates a new foldable box with the given label.

   procedure Initialize (Pane : Collapsing_Pane; Label : UTF8_String);
   --  Internal initialization function.

   procedure Set_Expanded_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the widget displayed in the expanded state.
   --  Removes the previous one if any.

   procedure Set_Collapsed_Widget
     (Pane   : access Collapsing_Pane_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the widget displayed in the collapsed state.
   --  Removes the previous one if any.

   type Foldable_Box_State is (Collapsed, Expanded);

   procedure Set_State
     (Pane : access Collapsing_Pane_Record'Class; State : Foldable_Box_State);
   --  Changes programmatically the state of the collapsing pane. The widget
   --  will be automatically refreshed if needed.

   function Get_State
     (Pane : access Collapsing_Pane_Record'Class) return Foldable_Box_State;
   --  Get the state of Pane.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "toggled"
   --    procedure Handler (Widget : access Gtk_Widget_Record'Class);

   Signal_Toggled : constant Signal_Name := "toggled";

private

   type Collapsing_Pane_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with
      record
         Main_Box         : Gtk.Box.Gtk_Box;
         State            : Foldable_Box_State := Collapsed;
         Label            : Gtk.Label.Gtk_Label;
         Label_Box        : Gtk.Event_Box.Gtk_Event_Box;
         Expanded_Box     : Gtk.Widget.Gtk_Widget := null;
         Collapsed_Box    : Gtk.Widget.Gtk_Widget := null;
         Arrow            : Gtk.Arrow.Gtk_Arrow;
      end record;

end Collapsing_Pane;
