-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2008, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides tooltips-like functionality. It differs from
--  the original Gtk.Tooltips package in that the drawing function is
--  left to the end user.
--  The drawing function is called after the timeout period, which means
--  that dynamic tooltips can be implemented with this package (ie the contents
--  of the tooltip changes at each call, possibly depending on the position of
--  the pointer at the time the tooltip is displayed)

with Glib;
with Gtk.Widget; use Gtk.Widget;
with Gdk.Pixmap;
with Gdk.Rectangle;
with Gtk.Main;
with Gtk.Tree_Model;
with Gtk.Tree_View;
with Gtk.Window;

package Tooltips is

   Default_Timeout : constant Glib.Guint32 := 600;
   --  The delay before a tooltip is displayed, in milliseconds)

   procedure Initialize_Tooltips
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Area : out Gdk.Rectangle.Gdk_Rectangle;
      Iter : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Find out the position of the mouse over the tree, and compute the area
   --  that triggered the tooltip to appear (see Create_Contents below).
   --  Iter is the iterator for which we should generate a tooltip.
   --  Null_Iter is returned if no tooltip should be displayed.

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

   procedure Destroy (Tooltip : access Tooltips);
   --  Destroy the memory occupied by the fields in Tooltip, not Tooltip
   --  itself.
   --  It does nothing by default

   procedure Create_Contents
     (Tooltip  : access Tooltips;
      Contents : out Gtk.Widget.Gtk_Widget;
      Area     : out Gdk.Rectangle.Gdk_Rectangle) is abstract;
   --  Return the widget to be displayed in the tooltip. This widget will be
   --  automatically destroyed when the tooltip is hidden.
   --  This function should set Contents to null if the tooltip shouldn't be
   --  displayed.
   --  While the mouse button remains in Area, the tooltip remains visible.

   procedure Set_Tooltip
     (Tooltip   : access Tooltips;
      On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Timeout   : Glib.Guint32 := Default_Timeout);
   --  Bind Tooltip to the widget, so that when the mouse is left over Widget,
   --  the tooltip is displayed.
   --  You can attach a given tooltip to a single widget for the time being.
   --  A Program_Error will be raised if you do not respect that.
   --  Tooltip is automatically destroyed when the widget is destroyed.

   ---------------------
   -- Pixmap_Tooltips --
   ---------------------

   type Pixmap_Tooltips is abstract new Tooltips with private;
   --  This special kind of tooltips has its contents given in the form of a
   --  pixmap.

   procedure Draw
     (Tooltip : access Pixmap_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle) is abstract;
   --  Create the contents of the tooltip.
   --  The tooltip is hidden when the mouse leaves the area defined by Area,
   --  which is relative to Get_Window for the widget on which the tooltip
   --  applies (On_Widget parameter to Set_Tooltip)

   overriding procedure Create_Contents
     (Tooltip  : access Pixmap_Tooltips;
      Contents : out Gtk.Widget.Gtk_Widget;
      Area     : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentation

private
   type Tooltips is abstract tagged record
      Timeout : Glib.Guint32 := Default_Timeout;
      --  The delay before draw function is called

      Active : Boolean := False;
      --  States whether tooltips should be displayed when drawing
      --  is complete.

      Display_Window : Gtk.Window.Gtk_Window := null;
      --  The window in which the tooltip is displayed

      Handler_Id : Gtk.Main.Timeout_Handler_Id := 0;
      --  Reference in case handler should be blocked

      X, Y : Glib.Gint := 0;
      --  The mouse coordinates associated with the last call to
      --  Draw_Tooltip.

      Area : Gdk.Rectangle.Gdk_Rectangle := (0, 0, 0, 0);
      --  The area of efficiency for the tooltip.
      --  (See Draw_Tooltip specification for details).

      Widget : Gtk.Widget.Gtk_Widget;
      --  The widget to which the tooltip is attached

      Width, Height : Glib.Gint := 0;
      --  Size of the tooltip, if set it will be used to avoid the tooltip to
      --  go outside the screen.
   end record;

   type Pixmap_Tooltips is abstract new Tooltips with null record;
   type Pixmap_Tooltips_Access is access all Pixmap_Tooltips'Class;

end Tooltips;
