------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gdk;                  use Gdk;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Window;           use Gdk.Window;
with Glib.Object;          use Glib.Object;
with Glib.Values;          use Glib.Values;
with GNATCOLL.Traces;      use GNATCOLL.Traces;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtk.Arguments;        use Gtk.Arguments;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;

package body Tooltips is
   Me : constant Trace_Handle := Create ("TOOLTIPS");

   procedure Destroy_Cb (Data : Tooltips_Access);
   --  Called when the tooltip is being destroyed

   function On_Query_Tooltip
     (Widget        : access Gtk_Widget_Record'Class;
      Args          : Glib.Values.GValues) return Boolean;
   --  Computes the contents of the tooltip.

   package Tooltip_User_Data is new Glib.Object.User_Data (Tooltips_Access);

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   function On_Query_Tooltip
     (Widget        : access Gtk_Widget_Record'Class;
      Args          : Glib.Values.GValues)
     return Boolean
   is
      --  X, Y are relative to Widget's left side
      X : constant Gint := To_Gint (Args, 1);
      Y : constant Gint := To_Gint (Args, 2);

      --  True if tooltip triggered from keyboard
      --  Keyboard_Mode : constant Boolean := To_Boolean (Args, 3);

      --  A newly created tooltip object
      Tooltip : constant Gtk_Tooltip := Gtk_Tooltip (To_Object (Args, 4));

      Tip : constant Tooltips_Access :=
        Tooltip_User_Data.Get (Widget, "gps-tooltip");

      W : Gtk_Widget;
   begin
      W := Tip.Create_Contents (Tooltip, Widget, X, Y);
      if W /= null then
         W.Show_All;
         Tooltip.Set_Custom (W);
      end if;
      return W /= null;
   end On_Query_Tooltip;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Tooltip   : access Tooltips'Class;
      On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      On_Widget.Set_Has_Tooltip (True);
      Tooltip_User_Data.Set
        (On_Widget, Tooltips_Access (Tooltip),
         "gps-tooltip", Destroy_Cb'Access);
      Return_Callback.Connect
        (On_Widget, Signal_Query_Tooltip, On_Query_Tooltip'Access);
   end Set_Tooltip;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Data : Tooltips_Access) is
   begin
      Trace (Me, "Destroying a tooltip");
      Destroy (Data);
   end Destroy_Cb;

   -------------------------
   -- Initialize_Tooltips --
   -------------------------

   procedure Initialize_Tooltips
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      X, Y : Gint;
      Area : out Gdk.Rectangle.Gdk_Rectangle;
      Iter : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Cell_X, Cell_Y : Gint;
      Path           : Gtk_Tree_Path;
      Found          :  Boolean;
      Column         : Gtk_Tree_View_Column;
   begin
      Area := (0, 0, 0, 0);
      Iter := Null_Iter;

      Get_Path_At_Pos (Tree, X, Y, Path, Column, Cell_X, Cell_Y, Found);
      if not Found then
         return;
      end if;

      Get_Cell_Area (Tree, Path, Column, Area);
      Iter := Get_Iter (Get_Model (Tree), Path);
      Path_Free (Path);
   end Initialize_Tooltips;

end Tooltips;
