-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2007                       --
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

with Ada.Unchecked_Deallocation;

with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Types;            use Gdk.Types;
with Gdk.Window;           use Gdk.Window;

with Glib.Properties;      use Glib.Properties;
with Glib;                 use Glib;

with Gtk.Enums;            use Gtk.Enums;
with Gtk.Handlers;         use Gtk.Handlers;
with Gtk.Main;             use Gtk.Main;
with Gtk.Image;            use Gtk.Image;
with Gtk.Object;           use Gtk.Object;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Window;           use Gtk.Window;

with Traces;               use Traces;

package body Tooltips is

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

   function Leave_Notify_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip : Tooltips_Access) return Boolean;
   --  Called when the mouse leaves the tooltip window

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
      Mask                  : Gdk_Modifier_Type;
      X, Y                  : Gint;
      Window                : Gdk_Window;
      W                     : Gtk_Widget;
      Focus                 : Gtk_Widget;
      Win_X, Win_Y          : Gint;
      Win_Width, Win_Height : Gint;
      Win_Depth             : Gint;
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

      Create_Contents (Tooltip, W, Tooltip.Area);

      if W /= null then
         Gtk_New       (Tooltip.Display_Window, Window_Popup);
         Add           (Tooltip.Display_Window, W);
         Get_Pointer   (null, X, Y, Mask, Window);

         --  Get screen size

         Get_Geometry
           (Null_Window, Win_X, Win_Y, Win_Width, Win_Height, Win_Depth);

         --  Adjust Win_Width and Win_Height to correspond to the proper screen
         --  in case of multiple screen display.
         --  ??? Note that this code assume that all the screens have the same
         --  size, it could be the case that some tooltips are partly of the
         --  screen if GPS is not on the main screen.

         if X > Win_Width then
            Win_Width := Win_Width * (1 + X / Win_Width);
         end if;

         if Y > Win_Height then
            Win_Height := Win_Height * (1 + Y / Win_Height);
         end if;

         --  Now adjust the screen position for the full tooltip window to
         --  always be on the screen.

         if X + Tooltip.Width + 12 > Win_Width then
            X := Win_Width - Tooltip.Width - 12;
         end if;

         if Y + Tooltip.Height + 12 > Win_Height then
            Y := Win_Height - Tooltip.Height - 12;
         end if;

         Set_UPosition (Tooltip.Display_Window, X + 10, Y + 10);

         Show_All      (Tooltip.Display_Window);

         Add_Events (Tooltip.Display_Window, Leave_Notify_Mask);
         Tooltip_Handler.Connect
           (Tooltip.Display_Window, Signal_Leave_Notify_Event,
            Leave_Notify_Cb'Access, User_Data => Tooltip);

         Focus := Get_Focus (Gtk_Window (Get_Toplevel (Tooltip.Widget)));

         if Focus /= null then
            Add_Watch
              (Tooltip_Handler.Connect
                 (Focus,
                  Signal_Focus_Out_Event,
                  Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
                  User_Data => Tooltip),
               Object => Tooltip.Display_Window);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Display_Tooltip;

   ---------------------
   -- Leave_Notify_Cb --
   ---------------------

   function Leave_Notify_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip : Tooltips_Access) return Boolean
   is
      pragma Unreferenced (Widget);
   begin
      Hide_Tooltip (Tooltip);
      return False;
   end Leave_Notify_Cb;

   ---------------------
   -- Create_Contents --
   ---------------------

   procedure Create_Contents
     (Tooltip  : access Pixmap_Tooltips;
      Contents : out Gtk.Widget.Gtk_Widget;
      Area     : out Gdk.Rectangle.Gdk_Rectangle)
   is
      use type Gdk_Window;
      Pixmap : Gdk_Pixmap;
      Pix    : Gtk_Image;
   begin
      Draw (Pixmap_Tooltips_Access (Tooltip), Pixmap, Tooltip.Area);

      if Pixmap /= null then
         Gdk.Drawable.Get_Size (Pixmap, Tooltip.Width, Tooltip.Height);
         Gtk_New (Pix, Pixmap, null);
         Set_Size_Request (Pix, Tooltip.Width, Tooltip.Height);
         Gdk.Pixmap.Unref (Pixmap);
         Contents := Gtk_Widget (Pix);
         Area     := Tooltip.Area;
      else
         Contents := null;
      end if;
   end Create_Contents;

   ------------------
   -- Show_Tooltip --
   ------------------

   procedure Show_Tooltip
     (Tooltip : access Tooltips'Class)
   is
      Mask   : Gdk_Modifier_Type;
      Window : Gdk_Window;
      X, Y   : Gint;
   begin
      Get_Pointer (Get_Window (Tooltip.Widget), X, Y, Mask, Window);

      if X <= Tooltip.Area.X + GRectangle_Coord (Tooltip.Area.Width)
        and then Y <= Tooltip.Area.Y + GRectangle_Coord (Tooltip.Area.Height)
        and then X >= Tooltip.Area.X
        and then Y >= Tooltip.Area.Y
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
      if Get_Event_Type (Event) = Motion_Notify
        and then Get_Property
          (Gtk_Window (Get_Toplevel (Tooltip.Widget)),
           Has_Toplevel_Focus_Property)
      then
         Show_Tooltip (Tooltip);
      elsif Tooltip.Active then
         Hide_Tooltip (Tooltip);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         Pointer_Motion_Mask or Enter_Notify_Mask or Focus_Change_Mask
         or Leave_Notify_Mask);
      Tooltip_Handler.Connect
        (On_Widget, Signal_Button_Press_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Key_Press_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Key_Release_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Motion_Notify_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Leave_Notify_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Scroll_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Focus_In_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Tooltip_Handler.Connect
        (On_Widget, Signal_Focus_Out_Event,
         Tooltip_Handler.To_Marshaller (Tooltip_Event_Cb'Access),
         User_Data => Tooltips_Access (Tooltip));
      Destroy_Handler.Connect
        (On_Widget, Signal_Destroy,
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

   exception
      when E : others => Trace (Exception_Handle, E);
   end Destroy_Cb;

   -------------------------
   -- Initialize_Tooltips --
   -------------------------

   procedure Initialize_Tooltips
     (Tree : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Area : out Gdk.Rectangle.Gdk_Rectangle;
      Iter : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Window     : Gdk_Window;
      New_Window : Gdk_Window;
      X, Y       : Gint;
      Mask       : Gdk_Modifier_Type;
      Cell_X, Cell_Y : Gint;
      Path       : Gtk_Tree_Path;
      Found      :  Boolean;
      Column     : Gtk_Tree_View_Column;
   begin
      Area   := (0, 0, 0, 0);
      Iter   := Null_Iter;

      Window := Get_Bin_Window (Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);
      Get_Path_At_Pos (Tree, X, Y, Path, Column, Cell_X, Cell_Y, Found);
      if not Found then
         return;
      end if;

      Get_Cell_Area (Tree, Path, Column, Area);
      Iter := Get_Iter (Get_Model (Tree), Path);
      Path_Free (Path);
   end Initialize_Tooltips;

end Tooltips;
