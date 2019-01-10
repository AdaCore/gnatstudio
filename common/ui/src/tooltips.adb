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

with Ada.Unchecked_Deallocation;
with Gdk;                  use Gdk;
with Gdk.Event;            use Gdk.Event;
with Gdk.Screen;           use Gdk.Screen;
with Gdk.Types;            use Gdk.Types;
with Gdk.Window;           use Gdk.Window;
with Glib.Main;            use Glib.Main;
with Glib.Object;
with Glib.Properties;      use Glib.Properties;
with GNAT.Strings;         use GNAT.Strings;
with GNATCOLL.Traces;      use GNATCOLL.Traces;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtk.Accel_Group;      use Gtk.Accel_Group;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Label;            use Gtk.Label;
with Gtk.Main;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Style_Context;    use Gtk.Style_Context;
with Gtk.Tree_Model;       use Gtk.Tree_Model;
with Gtk.Tree_View;        use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Window;           use Gtk.Window;

package body Tooltips is
   Me : constant Trace_Handle := Create ("GPS.COMMON.TOOLTIPS");

   Hover_Timeout : constant Guint := 500;
   --  Timeout before the initial display of the tooltip.
   --  Matches the hard-coded value in gtktooltip.c

   Browse_Timeout : constant Guint := 60;
   --  Timeout for displaying the tooltip when one was already displayed less
   --  than Browse_Disable_Timeout ago

   Browse_Disable_Timeout : constant Guint := 500;
   --  Timeout before we reset the timeout to Hover_Timeout

   procedure Destroy_Cb (Data : Tooltips_Access);
   --  Called when the tooltip is being destroyed

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Tooltips'Class, Tooltips_Access);

   function On_Tooltip_Delay return Boolean;
   --  Called when the mouse has been motionless for a while

   function On_Disable_Browse_Mode return Boolean;
   --  Called to disable browse mode

   procedure Hide_Tooltip;
   procedure Show_Tooltip (Widget  : not null access Gtk_Widget_Record'Class);
   --  Hide or show the tooltip

   function Is_In_Area (Widget  : Gtk_Widget; X, Y : Gint) return Boolean;
   --  Return True if the global tooltip is present, mapped, and the pointer
   --  location given by X,Y is within the area.

   function Tooltip_Event_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for all events that will disable the tooltip
   --  e.g: focus_in/focus_out/motion_notify/button_clicked/key_press

   package Tooltip_User_Data is new Glib.Object.User_Data (Tooltips_Access);

   function Can_Have_Tooltip
     (Widget : not null access Gtk_Widget_Record'Class)
      return Boolean
     is (Widget.Get_Has_Window
         or else Widget.all in Gtk_Menu_Item_Record'Class);
   --  Whether this widget is compatible with our tooltips.
   --  ??? Our tooltips do not work on widgets that do not have their own
   --  Gdk_Window (except for menu items). For static tooltips, users should
   --  fallback on using gtk+ tooltips.

   type Tooltip_Object_Record is new Gtk.Window.Gtk_Window_Record with record
      Timeout_Id             : G_Source_Id := 0;
      Browse_Mode_Timeout_Id : G_Source_Id := 0;

      Browse_Mode_Enabled : Boolean := False;

      On_Widget : Gtk_Widget;

      X, Y      : Glib.Gint;

      Area_Is_Set : Boolean := False;
      Area        : Gdk.Rectangle.Gdk_Rectangle := (0, 0, 0, 0);
      --  While the mouse remains on the same widget and same area, we keep
      --  displaying the same contents.
   end record;
   type Tooltip_Object is access all Tooltip_Object_Record'Class;
   --  There is one such object in the application.
   --  gtk+ creates one per display.

   Global_Tooltip : Tooltip_Object;

   procedure On_On_Widget_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Called on "destroy" on Global_Tooltip.On_Widget

   --------------------------
   -- On_On_Widget_Destroy --
   --------------------------

   procedure On_On_Widget_Destroy (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Global_Tooltip.On_Widget := null;
   end On_On_Widget_Destroy;

   ---------------------
   -- Static tooltips --
   ---------------------

   type Static_Tooltips_Record is new Tooltips with record
      Text       : GNAT.Strings.String_Access;
      Use_Markup : Boolean;
   end record;
   overriding procedure Destroy (Self : access Static_Tooltips_Record);
   overriding function Create_Contents
     (Self   : not null access Static_Tooltips_Record;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y   : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   -------------------------------
   -- Tooltips_Foreground_Color --
   -------------------------------

   function Tooltips_Foreground_Color return Gdk.RGBA.Gdk_RGBA is
      Color : Gdk_RGBA;
   begin
      Get_Style_Context (Global_Tooltip).Get_Color
        (Gtk_State_Flag_Normal, Color);
      return Color;
   end Tooltips_Foreground_Color;

   ------------------
   -- Set_Tip_Area --
   ------------------

   procedure Set_Tip_Area
     (Tooltip : not null access Tooltips;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Tooltip);
   begin
      if Global_Tooltip /= null then
         Global_Tooltip.Area_Is_Set := True;
         Global_Tooltip.Area := Area;
         Trace (Me, "Set_Tip_Area"
                & Area.X'Img & Area.Y'Img & Area.Width'Img & Area.Height'Img);
      end if;
   end Set_Tip_Area;

   ----------------------------
   -- On_Disable_Browse_Mode --
   ----------------------------

   function On_Disable_Browse_Mode return Boolean is
   begin
      Global_Tooltip.Browse_Mode_Timeout_Id := 0;
      Global_Tooltip.Browse_Mode_Enabled := False;
      return False;
   end On_Disable_Browse_Mode;

   ----------------------
   -- On_Tooltip_Delay --
   ----------------------

   function On_Tooltip_Delay return Boolean is
      Widget                : Gtk_Widget;
      Win_Width, Win_Height : Gint;
      X, Y, W, H            : Gint;
      Geom                  : Gdk.Rectangle.Gdk_Rectangle;
      Toplevel              : Gtk_Widget;
      Tip                   : Tooltips_Access;
   begin
      Global_Tooltip.Timeout_Id := 0;

      --  Right before displaying the tooltip, check that the toplevel window
      --  is still active. This prevents tooltips from appearing in the
      --  following cases:
      --    - when the user has switched using the window manager key shortcuts
      --      (for instance alt-tab, or the key to hide or minimize)
      --    - when a contextual menu has been created
      --  Both cases are difficult to detect by listening to events purely
      --  on the On_Widget

      if Global_Tooltip.On_Widget = null
        or else Global_Tooltip.On_Widget.In_Destruction
      then
         return False;
      end if;

      if Global_Tooltip.On_Widget.all in Gtk_Menu_Item_Record'Class then
         Toplevel := Global_Tooltip.On_Widget;
      else
         Toplevel := Global_Tooltip.On_Widget.Get_Toplevel;

         if Toplevel /= null
           and then Toplevel.all in Gtk_Window_Record'Class
           and then not Gtk_Window (Toplevel).Is_Active
         then
            return False;
         end if;
      end if;

      Tip := Tooltip_User_Data.Get (Global_Tooltip.On_Widget, "gps-tooltip");

      Widget := Tip.Create_Contents
        (Global_Tooltip.On_Widget,
         Global_Tooltip.X,
         Global_Tooltip.Y);

      if Widget /= null then
         Global_Tooltip.Add (Widget);

         --  These X, Y coordinates are global to all physical monitors

         Gdk.Window.Get_Root_Coords
           (Global_Tooltip.On_Widget.Get_Window,
            Global_Tooltip.X, Global_Tooltip.Y,
            X, Y);

         --  Get current active window width and height

         declare
            Screen  : constant Gdk.Screen.Gdk_Screen := Toplevel.Get_Screen;
            Monitor : constant Gint := Screen.Get_Monitor_At_Point (X, Y);
         begin
            Screen.Get_Monitor_Geometry (Monitor, Geom);

            Win_Width  := Geom.Width;
            Win_Height := Geom.Height;

            --  Set X, Y into the physical monitor area, this is needed to
            --  check if the tooltip actually fit into the current monitor
            --  or not.

            X := X - Geom.X;
            Y := Y - Geom.Y;
         end;

         --  Small offset on the right/bottom of the current mouse position.
         --  This is especially important for menus, since if the cursor goes
         --  on top of the tooltip, the focus_in/focus_out event in the menus
         --  are lost and the menus are not clickable anymore.

         X := X + 15;
         Y := Y + 15;

         --  Compute the future size of the tooltip, before we show it on
         --  screen.

         Widget.Show_All;  --  show contents to get right size
         Global_Tooltip.Get_Size (W, H);

         --  If it goes outside the right of the screen, move it back

         if X + W > Win_Width then
            X := Win_Width - W - 10;
         end if;

         --  If it goes outside the bottom of the screen, display it above
         --  the mouse pointer.

         if Y + H > Win_Height then
            Y := Y - H - 20;
         end if;

         --  Move the tooltip at the right place taking into account the
         --  physical monitor offset.

         Global_Tooltip.Move (X + Geom.X, Y + Geom.Y);
         Global_Tooltip.Show_All;

         --  Tooltip is visible again on screen, so reset timeout
         Global_Tooltip.Browse_Mode_Enabled := True;
         if Global_Tooltip.Browse_Mode_Timeout_Id /= 0 then
            Glib.Main.Remove (Global_Tooltip.Browse_Mode_Timeout_Id);
            Global_Tooltip.Browse_Mode_Timeout_Id := 0;
         end if;

      else
         Trace (Me, "No tooltip to display at this location");
         Hide_Tooltip;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Tooltip_Delay;

   ----------------
   -- Is_In_Area --
   ----------------

   function Is_In_Area (Widget : Gtk_Widget; X, Y : Gint) return Boolean is
   begin
      return Global_Tooltip.Get_Mapped
        and then Global_Tooltip.Area_Is_Set
        and then Global_Tooltip.On_Widget = Widget
        and then not
          (X < Global_Tooltip.Area.X
           or else X > Global_Tooltip.Area.X + Global_Tooltip.Area.Width
           or else Y < Global_Tooltip.Area.Y
           or else Y > Global_Tooltip.Area.Y + Global_Tooltip.Area.Height);
   end Is_In_Area;

   ------------------
   -- Show_Tooltip --
   ------------------

   procedure Show_Tooltip
     (Widget  : not null access Gtk_Widget_Record'Class)
   is
      X, Y            : Gint;
      Window, Ignored : Gdk_Window;
      Mask            : Gdk.Types.Gdk_Modifier_Type;
   begin
      if Global_Tooltip = null then
         Global_Tooltip := new Tooltip_Object_Record;
         Gtk.Window.Initialize (Global_Tooltip, Window_Popup);

         Global_Tooltip.Set_Decorated (False);
         Global_Tooltip.Set_Resizable (False);
         Global_Tooltip.Set_Type_Hint (Window_Type_Hint_Tooltip);
         Global_Tooltip.Set_Name ("gtk-tooltip");
         Get_Style_Context (Global_Tooltip).Add_Class ("tooltip");
      end if;

      Window := Widget.Get_Window;

      Gdk.Window.Get_Device_Position
        (Self   => Window,
         Device => Gtk.Main.Get_Current_Event_Device,
         X      => X,
         Y      => Y,
         Mask   => Mask,
         Window => Ignored);

      --  If still within the current area

      if Is_In_Area (Gtk_Widget (Widget), X, Y) then
         --  Leave the tooltip as is
         return;
      end if;

      Hide_Tooltip;

      Global_Tooltip.On_Widget := Gtk_Widget (Widget);
      Widget.On_Destroy (On_On_Widget_Destroy'Access);

      Global_Tooltip.X := X;
      Global_Tooltip.Y := Y;
      Global_Tooltip.Area_Is_Set := False;

      Global_Tooltip.Timeout_Id := Glib.Main.Timeout_Add
        ((if Global_Tooltip.Browse_Mode_Enabled
          then Browse_Timeout
          else Hover_Timeout),
         On_Tooltip_Delay'Access);
   end Show_Tooltip;

   ------------------
   -- Hide_Tooltip --
   ------------------

   procedure Hide_Tooltip is
      Child : Gtk_Widget;
   begin
      if Global_Tooltip /= null then
         if Global_Tooltip.Timeout_Id /= 0 then
            --  The tooltip is not displayed since we are still waiting
            Glib.Main.Remove (Global_Tooltip.Timeout_Id);
            Global_Tooltip.Timeout_Id := 0;
            return;
         end if;

         Global_Tooltip.On_Widget := null;
         Global_Tooltip.Area_Is_Set := False;
         Global_Tooltip.Hide;

         Child := Global_Tooltip.Get_Child;
         if Child /= null then
            Global_Tooltip.Remove (Child);
         end if;

         if Global_Tooltip.Browse_Mode_Timeout_Id = 0 then
            Global_Tooltip.Browse_Mode_Timeout_Id :=
              Glib.Main.Timeout_Add
                (Browse_Disable_Timeout, On_Disable_Browse_Mode'Access);
         end if;
      end if;
   end Hide_Tooltip;

   ----------------------
   -- Tooltip_Event_Cb --
   ----------------------

   function Tooltip_Event_Cb
     (Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Boolean
   is
      T : constant Gdk_Event_Type := Get_Event_Type (Event);
   begin
      if T = Motion_Notify

        --  Only if the user doesn't have the mouse button pressed, or a key
        --  press (since otherwise we might be in a drag-and-drop operation)
        and then (Event.Motion.State and Get_Default_Mod_Mask) = 0

        and then
          (Widget.all in Gtk_Menu_Item_Record'Class
           or else Get_Property
             (Gtk_Window (Widget.Get_Toplevel), Has_Toplevel_Focus_Property))
      then
         Show_Tooltip (Widget);
      else
         Hide_Tooltip;
      end if;

      return False;
   end Tooltip_Event_Cb;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Tooltip   : access Tooltips'Class;
      On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Assert
        (Me, Can_Have_Tooltip (On_Widget),
         "Widgets must have their own Gdk_Window to use tooltips.adb",
         Raise_Exception => False);

      --  from gtk_widget_real_set_has_tooltip
      if On_Widget.Get_Realized
        and then not On_Widget.Get_Has_Window
      then
         Gdk.Window.Set_Events
           (On_Widget.Get_Window,
            Gdk.Window.Get_Events (On_Widget.Get_Window)
            or Leave_Notify_Mask
            or Pointer_Motion_Mask);
      else
         On_Widget.Add_Events
           (Pointer_Motion_Mask
            or Focus_Change_Mask
            or Button_Press_Mask
            or Leave_Notify_Mask);
      end if;

      --  Do not connect to "destroy", since this will also result in a
      --  leave_notify event, which we are already monitoring

      Return_Callback.Connect
        (On_Widget, Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));
      Return_Callback.Connect
        (On_Widget, Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));
      Return_Callback.Connect
        (On_Widget, Signal_Motion_Notify_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));
      Return_Callback.Connect
        (On_Widget, Signal_Leave_Notify_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));
      Return_Callback.Connect
        (On_Widget, Signal_Scroll_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));
      Return_Callback.Connect
        (On_Widget, Signal_Focus_Out_Event,
         Return_Callback.To_Marshaller (Tooltip_Event_Cb'Access));

      Tooltip_User_Data.Set
        (On_Widget, Tooltips_Access (Tooltip),
         "gps-tooltip", Destroy_Cb'Access);
   end Set_Tooltip;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Data : Tooltips_Access) is
      D : Tooltips_Access := Data;
   begin
      Destroy (D);
      Unchecked_Free (D);
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
      X2, Y2         : Gint;
   begin
      Area := (0, 0, 0, 0);
      Iter := Null_Iter;

      Tree.Convert_Widget_To_Bin_Window_Coords (X, Y, X2, Y2);
      Get_Path_At_Pos (Tree, X2, Y2, Path, Column, Cell_X, Cell_Y, Found);
      if not Found then
         return;
      end if;

      Get_Cell_Area (Tree, Path, Column, Area);
      Iter := Get_Iter (Get_Model (Tree), Path);
      Path_Free (Path);
   end Initialize_Tooltips;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : access Static_Tooltips_Record) is
   begin
      Free (Self.Text);
   end Destroy;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Self     : not null access Static_Tooltips_Record;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (X, Y);
      Label : Gtk_Label;
      Alloc : Gtk_Allocation;
   begin
      Gtk_New (Label, Self.Text.all);
      Label.Set_Line_Wrap (True);
      Label.Set_Max_Width_Chars (70);  --  Match standard gtk+ tooltips
      Label.Set_Use_Markup (Self.Use_Markup);
      Widget.Get_Allocation (Alloc);
      Self.Set_Tip_Area ((X      => Alloc.X,
                          Y      => Alloc.Y,
                          Width  => Alloc.Width,
                          Height => Alloc.Height));
      return Gtk_Widget (Label);
   end Create_Contents;

   ------------------------
   -- Set_Static_Tooltip --
   ------------------------

   procedure Set_Static_Tooltip
     (Widget     : not null access Gtk_Widget_Record'Class;
      Text       : String;
      Use_Markup : Boolean := True)
   is
      Tip : Tooltips_Access;
   begin
      if not Can_Have_Tooltip (Widget) then
         if Use_Markup then
            Widget.Set_Tooltip_Markup (Text);
         else
            Widget.Set_Tooltip_Text (Text);
         end if;
         return;
      end if;

      Tip := new Static_Tooltips_Record'
        (Tooltips with
         Text       => new String'(Text),
         Use_Markup => Use_Markup);
      Tip.Set_Tooltip (Widget);
   end Set_Static_Tooltip;

end Tooltips;
