-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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

with Gdk.Event;       use Gdk.Event;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.Drawable;    use Gdk.Drawable;
with Gdk.Rectangle;   use Gdk.Rectangle;
with Gdk.Types;       use Gdk.Types;
with Gdk.Window;      use Gdk.Window;
with Glib;            use Glib;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk.Main;        use Gtk.Main;
with Gtk.Pixmap;      use Gtk.Pixmap;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;
with Ada.Unchecked_Deallocation;

package body GVD.Tooltips is
   type Tooltips_Access is access all Tooltips'Class;

   package Tooltip_Handler is new Gtk.Handlers.User_Return_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      Return_Type => Boolean,
      User_Type   => Tooltips_Access);
   package Destroy_Handler is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Tooltips_Access);
   --  These need to be at library level, not in a generic, which is why we
   --  had to have a package Generic_Tooltips.

   package GVD_Tooltips_Timeout is new Timeout (Tooltips_Access);

   function Tooltip_Event_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Tooltip : Tooltips_Access) return Boolean;
   --  Callback for all events that will disable the tooltip
   --  e.g: focus_in/focus_out/motion_notify/button_clicked/key_press

   procedure Show_Tooltip
     (Tooltip   : access Tooltips'Class);
   --  Make the tooltip visible, applied to a specific widget

   procedure Hide_Tooltip
     (Tooltip   : access Tooltips'Class);
   --  Hide the tooltip if it is currently visible

   function Display_Tooltip (Tooltip : Tooltips_Access) return Boolean;
   --  Call the drawing function, then create a window which contains
   --  the pixmap, and display it.

   procedure Destroy_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip : Tooltips_Access);
   --  Called when Widget is destroyed

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Tooltip : access Tooltips) is
      pragma Unreferenced (Tooltip);
   begin
      null;
   end Destroy;

   ---------------------
   -- Display_Tooltip --
   ---------------------

   function Display_Tooltip (Tooltip : Tooltips_Access) return Boolean is
      Mask          : Gdk_Modifier_Type;
      X, Y          : Gint;
      Window        : Gdk_Window;
      W             : Gtk_Widget;
   begin
      if not Tooltip.Active then
         return False;
      end if;

      --  To avoid overlapping windows when the user moves the mouse while
      --  a tooltip is being prepared and is about to be displayed.

      if Tooltip.Display_Window /= null then
         Destroy (Tooltip.Display_Window);
         Tooltip.Display_Window := null;
      end if;

      if not Has_Focus_Is_Set (Tooltip.Widget) then
         return False;
      end if;

      W := Create_Contents (Tooltip);
      if W /= null then
         Gtk_New       (Tooltip.Display_Window, Window_Popup);
         Add           (Tooltip.Display_Window, W);
         Get_Pointer   (null, X, Y, Mask, Window);
         Set_UPosition (Tooltip.Display_Window, X + 10, Y + 10);
         Show_All      (Tooltip.Display_Window);
      end if;

      return False;
   end Display_Tooltip;

   ---------------------
   -- Create_Contents --
   ---------------------

   function Create_Contents
     (Tooltip : access Pixmap_Tooltips) return Gtk.Widget.Gtk_Widget
   is
      use type Gdk_Window;
      Pixmap        : Gdk_Pixmap;
      Pix           : Gtk_Pixmap;
      Width, Height : Gint;
   begin
      Draw (Pixmap_Tooltips_Access (Tooltip), Pixmap, Tooltip.Area);

      if Pixmap /= null then
         Gdk.Drawable.Get_Size (Pixmap, Width, Height);
         Gtk_New (Pix, Pixmap, null);
         Set_Size_Request (Pix, Width, Height);
         Gdk.Pixmap.Unref (Pixmap);
         return Gtk_Widget (Pix);
      else
         return null;
      end if;
   end Create_Contents;

   ------------------
   -- Show_Tooltip --
   ------------------

   procedure Show_Tooltip
     (Tooltip   : access Tooltips'Class)
   is
      Mask   : Gdk_Modifier_Type;
      Window : Gdk_Window;
      X, Y   : Gint;
   begin
      Get_Pointer (Get_Window (Tooltip.Widget), X, Y, Mask, Window);

      if X <= Tooltip.X + Tooltip.Area.X +
        GRectangle_Coord (Tooltip.Area.Width)
        and then Y <= Tooltip.Y + Tooltip.Area.Y +
          GRectangle_Coord (Tooltip.Area.Height)
        and then X >= Tooltip.X + Tooltip.Area.X
        and then Y >= Tooltip.Y + Tooltip.Area.Y
      then
         return;
      end if;

      if Tooltip.Active then
         Hide_Tooltip (Tooltip);
      end if;

      Tooltip.X := X;
      Tooltip.Y := Y;
      Tooltip.Area.Width := 0;
      Tooltip.Area.Height := 0;
      Tooltip.Active := True;
      Tooltip.Handler_Id :=
        GVD_Tooltips_Timeout.Add
          (Tooltip.Timeout, Display_Tooltip'Access, Tooltips_Access (Tooltip));
   end Show_Tooltip;

   ------------------
   -- Hide_Tooltip --
   ------------------

   procedure Hide_Tooltip
     (Tooltip   : access Tooltips'Class)
   is
      use type Gdk_Window;
   begin
      if Tooltip.Active then
         Timeout_Remove (Tooltip.Handler_Id);

         if Tooltip.Display_Window /= null then
            Destroy (Tooltip.Display_Window);
            Tooltip.Display_Window := null;
         end if;

         Tooltip.Active := False;
      end if;
   end Hide_Tooltip;

   ----------------------
   -- Tooltip_Event_Cb --
   ----------------------

   function Tooltip_Event_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Tooltip : Tooltips_Access) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      if Get_Event_Type (Event) = Motion_Notify then
         Show_Tooltip (Tooltip);
      elsif Tooltip.Active then
         Hide_Tooltip (Tooltip);
      end if;

      return False;
   end Tooltip_Event_Cb;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Tooltip   : access Tooltips;
      On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Timeout   : Glib.Guint32 := Default_Timeout)
   is
      use type Gdk.Window.Gdk_Window;
   begin
      if Tooltip.Widget /= null then
         --  Tooltips can be bound to only one widget for the time being
         raise Program_Error;
      end if;

      Tooltip.Timeout := Timeout;
      Tooltip.Widget  := Gtk_Widget (On_Widget);
      Add_Events
        (On_Widget,
         Pointer_Motion_Mask or Enter_Notify_Mask or Focus_Change_Mask);
      Tooltip_Handler.Connect
        (On_Widget, "button_press_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, "key_press_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, "motion_notify_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, "scroll_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, "focus_in_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, "focus_out_event",
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Destroy_Handler.Connect
        (On_Widget, "destroy",
         Destroy_Handler.To_Marshaller (Destroy_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
   end Set_Tooltip;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip : Tooltips_Access)
   is
      pragma Unreferenced (Widget);
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Tooltips'Class, Tooltips_Access);
      --  Free memory used by tooltip.
      T : Tooltips_Access := Tooltip;
   begin
      if Tooltip /= null then
         Hide_Tooltip (Tooltip);
         Destroy (Tooltip);

         --  The following call is in fact safe, since this callback is only
         --  called once per widget, and thus per tooltip
         Unchecked_Free (T);
      end if;
   end Destroy_Cb;

end GVD.Tooltips;
