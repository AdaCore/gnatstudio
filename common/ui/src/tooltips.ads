------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

--  This package provides tooltips-like functionality.
--  It is not based on GtkTooltip, because the latter has several drawbacks
--  as of gtk 3.4:
--      * it doesn't seem possible to define an area in which the tooltip stays
--        constant, and the window should stay visible while the pointer is in
--        that area.  Set_Tip_Area doesn't seem to do that at least for
--        GtTextView.
--      * the contents of the tooltip is computed every time the mouse moves,
--        not at the end of the timeout. This results in a lot of extra
--        computation for the contents of the tooltip.

with Glib;           use Glib;
with Gdk.RGBA;       use Gdk.RGBA;
with Gtk.Widget;     use Gtk.Widget;
with Gdk.Rectangle;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Label;      use Gtk.Label;

package Tooltips is

   procedure Initialize_Tooltips
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      X, Y : Gint;
      Area : out Gdk.Rectangle.Gdk_Rectangle;
      Iter : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Find out the position of the mouse over the tree, and compute the area
   --  that triggered the tooltip to appear (see Create_Contents below).
   --  Iter is the iterator for which we should generate a tooltip.
   --  Null_Iter is returned if no tooltip should be displayed.
   --
   --  See Gtk.Tree_View.Get_Tooltip_Context instead,
   --  and Gtk.Tree_View.Set_Tooltip_Cell

   function Tooltips_Foreground_Color return Gdk.RGBA.Gdk_RGBA;
   --  Return the default foreground color used for the text in the tooltip.

   --------------
   -- Tooltips --
   --------------

   type Tooltips is abstract tagged private;
   type Tooltips_Access is access all Tooltips'Class;
   --  This type represents a tooltip creator: it can be attached to one or
   --  more widgets, and will create a tooltip (ie a graphical window to
   --  display information) automatically for them when the mouse is left for
   --  a while over the window.
   --  This general form can embed any gtk widget in its window

   procedure Destroy (Tooltip : access Tooltips) is null;
   --  Destroy the memory occupied by the fields in Tooltip, not Tooltip
   --  itself.

   function Create_Contents
     (Tooltip  : not null access Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the widget to be displayed in the tooltip. This widget will be
   --  automatically destroyed when the tooltip is hidden.
   --  This function should return null if the tooltip shouldn't be
   --  displayed.
   --  This function should call Tooltip.Set_Tip_Area to indicate which area
   --  of widget the tooltip applies to (the tooltip will remain visible while
   --  the mouse is in this area).

   procedure Set_Tip_Area
     (Tooltip : not null access Tooltips;
      Area    : Gdk.Rectangle.Gdk_Rectangle);
   --  Set the active area for the tooltip. While the cursor remains in this
   --  area, the tooltip is kept on screen with the same contents.
   --  Coordinates are relative to the widget.

   procedure Set_Tooltip
     (Tooltip   : access Tooltips'Class;
      On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Bind Tooltip to the widget, so that when the mouse is left over Widget,
   --  the tooltip is displayed.
   --  You can attach a given tooltip to a single widget for the time being.
   --  A Program_Error will be raised if you do not respect that.
   --  Tooltip is automatically destroyed and freed when the widget is
   --  destroyed.

   ---------------
   -- Shortcuts --
   ---------------

   procedure Set_Static_Tooltip
     (Widget     : not null access Gtk_Widget_Record'Class;
      Text       : String;
      Use_Markup : Boolean := True);
   --  Set static text for a tooltip.
   --  This is similar to Gtk.Widget.Set_Tooltip_Text, but the placement of
   --  tooltips is different.

   -----------
   -- Utils --
   -----------

   procedure Create_Tooltip_Label
     (Label      : out Gtk_Label;
      Text       : String;
      Use_Markup : Boolean := True);
   --  Create a label suitable to be displayed in tooltips.
   --  In particular, it ensures that the created label will wrap if it's
   --  needed width becomes too wide to be displayed in a tooltip.

private
   type Tooltips is abstract tagged null record;

end Tooltips;
