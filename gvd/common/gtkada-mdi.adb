-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;             use Glib;
with Glib.Object;      use Glib.Object;
with Glib.Values;      use Glib.Values;
with Gdk;              use Gdk;
with Gdk.Color;        use Gdk.Color;
with Gdk.Cursor;       use Gdk.Cursor;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Gdk.GC;           use Gdk.GC;
with Gdk.Main;         use Gdk.Main;
with Gdk.Pixmap;
with Gdk.Rectangle;    use Gdk.Rectangle;
with Gdk.Types;        use Gdk.Types;
with Gdk.Window;       use Gdk.Window;
with Gdk.Window_Attr;  use Gdk.Window_Attr;
with Gtk.Accel_Label;  use Gtk.Accel_Label;
with Gtk.Arguments;    use Gtk.Arguments;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Gtk.Event_Box;    use Gtk.Event_Box;
with Gtk.Fixed;        use Gtk.Fixed;
with Gtk.Handlers;
with Gtk.Label;        use Gtk.Label;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Notebook;     use Gtk.Notebook;
with Gtk.Object;
with Gtk.Pixmap;       use Gtk.Pixmap;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Style;        use Gtk.Style;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with GNAT.OS_Lib;      use GNAT.OS_Lib;
with System;           use System;

package body Gtkada.MDI is

   Title_Bar_Focus_Color : constant String := "#000088";
   --  <preference> Color to use for the title bar of the child that has
   --  the focus

   Title_Bar_Color : constant String := "#AAAAAA";
   --  <preference> Color to use for the title bar of children that do not
   --  have the focus.

   Title_Bar_Height : constant Gint := 15;
   --  <preference> Height of the title bar for the children

   MDI_Background_Color : constant String := "#666666";
   --  <preference> Background color to use for the MDI window

   Border_Thickness : constant Gint := 4;
   --  <preference> Thickness of the windows in the MDI

   Title_Font_Name : constant String := "Helvetica";
   --  <preference> Name of the font to use in the title bar

   Title_Font_Height : constant Gint := 10;
   --  <preference> Height of the font to use in the title bar

   Icons_Width : constant Gint := 100;
   --  <preference> Width to use for icons

   Opaque_Resize : constant Boolean := False;
   --  <preference> True if the contents of windows should be displayed while
   --  resizing widgets.

   Opaque_Move : constant Boolean := False;
   --  <preference> True if the contents of windows should be displayed while
   --  they are moved.

   Handle_Size : constant Gint := 8;
   --  <preference> The default width or height of the handles.

   Do_Grabs : constant Boolean := True;
   --  Should be set to False when debugging, so that pointer grabs are not
   --  done.

   Min_Width : constant Gint := 40;
   Min_Height : constant Gint := 2 * Border_Thickness + Title_Bar_Height;
   --  Minimal size for all windows

   Corner_Size : constant Gint := Border_Thickness * 2;
   --  Extra tolerance when the user selects a corner for resizing (if the
   --  pointer is within Corner_Size in both coordinates, then we are clicking
   --  on the corner)

   MDI_Class_Record        : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;
   MDI_Layout_Class_Record : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   Close_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 3 1"),
      New_String ("      c None"),
      New_String (".     c #C0C0C0"),
      New_String ("+     c #000000"),
      New_String ("............."),
      New_String ("............."),
      New_String ("...++....++.."),
      New_String ("....++..++..."),
      New_String (".....++++...."),
      New_String ("......++....."),
      New_String (".....++++...."),
      New_String ("....++..++..."),
      New_String ("...++....++.."),
      New_String ("............."),
      New_String ("............."));

   Iconify_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 2 1"),
      New_String (". c #000000"),
      New_String ("# c #c0c0c0"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("#############"),
      New_String ("##.......####"),
      New_String ("##.......####"),
      New_String ("#############"),
      New_String ("#############"));

   Maximize_Xpm : constant Interfaces.C.Strings.chars_ptr_array :=
     (New_String ("13 11 3 1"),
      New_String ("       c None"),
      New_String (".      c #C0C0C0"),
      New_String ("+      c #000000"),
      New_String ("............."),
      New_String ("....+++++++.."),
      New_String ("....+++++++.."),
      New_String ("....+.....+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+++++++.+.."),
      New_String ("..+.....+++.."),
      New_String ("..+.....+...."),
      New_String ("..+.....+...."),
      New_String ("..+++++++...."),
      New_String ("............."));

   use Widget_List;

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse button in the canvas.
   --  Test whether an item was selected.

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has released the mouse button.
   --  If an item was selected, refresh the canvas.

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user moves the mouse while a button is pressed.
   --  If an item was selected, the item is moved.

   function Button_Pressed_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Release_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Motion_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when the user has pressed the mouse while in the MDI, in
   --  particular in one of the handles

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  The pointer has left the mouse.

   procedure Reposition_Handles (M : access MDI_Window_Record'Class);
   --  Recompute the position of the four handles on each side of the MDI.

   procedure Create_Notebook
     (MDI : access MDI_Window_Record'Class; Side : Dock_Side);
   --  Create the notebook that goes to one of the sides of MDI, or to the
   --  middle. If this notebook already exists, its tabs are shown, since new
   --  children will be added to it.

   function Side
     (Child : access MDI_Child_Record'Class; X, Y : Gint)
      return Gdk_Cursor_Type;
   --  Return the cursor to use depending on the coordinates (X, Y) inside
   --  child.

   --  procedure Layout_Child
   --    (Child  : access MDI_Child_Record'Class;
   --     Region : Gdk.Region.Gdk_Region := null);
   --  Compute the coordinates for Child.
   --  If Region is null, loop through the list of all children, and try
   --  to position the child in an area where it doesn't overlap any child.
   --  If Region is not null, we use this to check that the child doesn't
   --  overlap any widget.

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Forward a delete_event from the toplevel window to the child.

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class);
   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class);
   --  Called when either the child itself, or the widget we initially put
   --  in it, are destroyed. Remove the child from the MDI properly.

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class);
   --  Called when the MDI is destroyed.

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class);
   --  Called when the Menu_Item associated with a Child is destroyed.

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class);
   --  Called when the Menu associated with a MDI is destroyed.

   procedure Size_Allocate_MDI_Layout
     (Layout : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_MDI_Layout);
   --  Handles size allocations for the layout contained in the MDI.

   procedure Size_Allocate_MDI
     (MDI : System.Address; MDI_Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_MDI);
   --  MDI was resized, need to resize the docks as well.

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class);
   --  Iconify a child (this act as toggles, for the title bar of all
   --  children).

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class;
      Args : GValues);
   --  Called when the current page in Docked_Child has changed.
   --  This is used to refresh the notebook so that is reflects the selected
   --  widget.

   procedure Close_Child (Child : access Gtk_Widget_Record'Class);
   --  A child should be destroyed.
   --  As opposed to a direct call to Destroy, this also checks whether the
   --  child can be closed at that time, through a call to delete_event.

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle);
   procedure Draw_Child
     (Child : access Gtk_Widget_Record'Class; Params : Gtk_Args);
   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean;
   --  Draw the child (and the title bar)

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class);
   procedure Realize_MDI_Layout (MDI : access Gtk_Widget_Record'Class);
   --  Called when the child is realized.

   function Expose_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : GValues) return Boolean;
   --  Called when the child needs to be redrawn.

   procedure Activate_Child (Child : access MDI_Child_Record'Class);
   --  Make Child the active widget, and raise it at the top.

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class);
   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class);
   --  Update the state of the "Float" menu item associated with child.

   procedure Put_In_Notebook
     (MDI : access MDI_Window_Record'Class;
      Side : Dock_Side;
      Child : access MDI_Child_Record'Class);
   --  Remove Child from MDI, and put it under control of a dock box, on the
   --  specific Side.

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side);
   --  Remove Child from the notebook it belongs to.
   --  Child will be destroyed, unless you Ref' it first.
   --  The notebook is destroyed if Child was the last item.

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class);
   --  Add an entry to the MDI menu that provides easy activation of Child

   procedure Cascade_Cb    (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_H_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Tile_V_Cb     (MDI   : access Gtk_Widget_Record'Class);
   procedure Dock_Cb       (MDI   : access Gtk_Widget_Record'Class);
   procedure Float_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Close_Cb      (MDI   : access Gtk_Widget_Record'Class);
   procedure Focus_Cb      (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Child_Cb  (Child : access Gtk_Widget_Record'Class);
   procedure Maximize_Cb   (MDI   : access Gtk_Widget_Record'Class);
   procedure Unmaximize_Cb (MDI   : access Gtk_Widget_Record'Class);
   --  Callbacks for the menu

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (MDI : out MDI_Window) is
   begin
      MDI := new MDI_Window_Record;
      Gtkada.MDI.Initialize (MDI);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (MDI : access MDI_Window_Record'Class) is
      No_Signals : chars_ptr_array (1 .. 0) := (others => Null_Ptr);
   begin
      Gtk.Fixed.Initialize (MDI);
      Gtk.Object.Initialize_Class_Record
        (MDI,
         Signals      => No_Signals,
         Class_Record => MDI_Class_Record,
         Type_Name    => "GtkAdaMDI");

      Gtk_New (MDI.Layout);
      Gtk.Object.Initialize_Class_Record
        (MDI.Layout,
         Signals      => No_Signals,
         Class_Record => MDI_Layout_Class_Record,
         Type_Name    => "GtkAdaMDI_Layout");

      Put (MDI, MDI.Layout, 0, 0);

      Widget_Callback.Connect
        (MDI, "realize", Widget_Callback.To_Marshaller (Realize_MDI'Access));
      Widget_Callback.Object_Connect
        (MDI.Layout, "realize",
         Widget_Callback.To_Marshaller (Realize_MDI_Layout'Access), MDI);
      Widget_Callback.Connect
        (MDI, "destroy", Widget_Callback.To_Marshaller (Destroy_MDI'Access));

      Set_Default_Size_Allocate_Handler
        (MDI_Class_Record, Size_Allocate_MDI'Access);
      Set_Default_Size_Allocate_Handler
        (MDI_Layout_Class_Record, Size_Allocate_MDI_Layout'Access);
      --  Widget_Callback.Connect
      --    (MDI, "size_allocate",
      --     Widget_Callback.To_Marshaller (Size_Allocate_MDI'Access));
      Return_Callback.Connect
        (MDI, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed_MDI'Access));
      Return_Callback.Connect
        (MDI, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release_MDI'Access));
      Return_Callback.Connect
        (MDI, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion_MDI'Access));
      Return_Callback.Connect (MDI, "expose_event", Expose_MDI'Access);
   end Initialize;

   ------------------------
   -- Realize_MDI_Layout --
   ------------------------

   procedure Realize_MDI_Layout (MDI : access Gtk_Widget_Record'Class) is
      Color : Gdk_Color := Parse (MDI_Background_Color);
   begin
      Alloc (Get_Default_Colormap, Color);
      Gdk.Window.Set_Background (Get_Window (MDI_Window (MDI).Layout), Color);
   end Realize_MDI_Layout;

   -----------------
   -- Realize_MDI --
   -----------------

   procedure Realize_MDI (MDI : access Gtk_Widget_Record'Class) is
      Window_Attr : Gdk.Gdk_Window_Attr;
      M         : MDI_Window := MDI_Window (MDI);
      Color     : Gdk_Color;
      Cursor    : Gdk_Cursor;

   begin
      Color := Parse (MDI_Background_Color);
      Alloc (Get_Default_Colormap, Color);
      Gdk.Window.Set_Background (Get_Window (M), Color);

      Gdk_New (M.Non_Focus_GC, Get_Window (MDI));
      Color := Parse (Title_Bar_Color);
      Alloc (Get_Default_Colormap, Color);
      Set_Foreground (M.Non_Focus_GC, Color);
      Set_Exposures (M.Non_Focus_GC, False);

      Gdk_New (M.Focus_GC, Get_Window (MDI));
      Color := Parse (Title_Bar_Focus_Color);
      Alloc (Get_Default_Colormap, Color);
      Set_Foreground (M.Focus_GC, Color);
      Set_Exposures (M.Focus_GC, False);

      Gdk_New (M.Xor_GC, Get_Window (MDI));
      Set_Function (M.Xor_GC, Invert);
      Set_Exposures (M.Xor_GC, False);
      Set_Subwindow (M.Xor_GC, Include_Inferiors);

      Gdk_New (Cursor, Cross);
      Gdk_New (Window_Attr,
               Window_Type => Window_Child,
               Wclass      => Input_Output,
               Cursor      => Cursor,
               Visual      => Get_Visual (MDI),
               Colormap    => Get_Colormap (MDI),
               Event_Mask  => Get_Events (MDI)
               or Exposure_Mask
               or Button_Press_Mask
               or Button_Release_Mask
               or Button_Motion_Mask);

      for J in Left .. Bottom loop
         Gdk_New (M.Handles (J),
                  Parent          => Get_Window (MDI),
                  Attributes      => Window_Attr,
                  Attributes_Mask => Wa_Cursor or Wa_Colormap or Wa_Visual);
         Set_User_Data (M.Handles (J), MDI);

         if M.Docks (J) /= null then
            Gdk.Window.Show (M.Handles (J));
         end if;
      end loop;

      --  Destroy the window attribute and the cursor
      Destroy (Cursor);
      Destroy (Window_Attr);

      --  Reposition all handles correctly (since this wasn't done when
      --  Size_Allocate was called prior to realizing the MDI).

      Reposition_Handles (M);

      --  Initialize the size of all children.
      --  This couldn't be done earlier since the child would have requested
      --  a size of 0x0..
      --  ??? Should put children at 0x0 while MDI is not realized, and
      --  ??? provide a Compute_Layout subprogram, since Realize can be called
      --  ??? several times.

      --  Gdk_New (Region);

      --  while Tmp /= Null_List loop
      --     Child := MDI_Child (Get_Data (Tmp));

      --     if Child.State /= Docked then
      --        Size_Request (Child, Child_Req);
      --        Child.Uniconified_Width := Child_Req.Width;
      --        Child.Uniconified_Height := Child_Req.Height;
      --        Layout_Child (Child, Region);
      --        Union_With_Rect
      --          (Region, Region,
      --           (Child.X, Child.Y,
      --            GRectangle_Length (Child.Uniconified_Width),
      --            GRectangle_Length (Child.Uniconified_Height)));
      --     end if;

      --     Tmp := Next (Tmp);
      --  end loop;

      --  Free (List);
      --  Destroy (Region);
   end Realize_MDI;

   ----------------
   -- Expose_MDI --
   ----------------

   function Expose_MDI
     (MDI : access Gtk_Widget_Record'Class; Args : GValues)
     return Boolean
   is
      M : MDI_Window := MDI_Window (MDI);
      Event : Gdk_Event := To_Event (Args, 1);
      Area : Gdk_Rectangle := Get_Area (Event);
      Orientation : Gtk_Orientation;
      First, Last : Gint;
      X, Y, W, H, Depth : Gint;

   begin
      if Visible_Is_Set (M) and then Mapped_Is_Set (M) then
         for J in M.Handles'Range loop
            if J = Left or else J = Right then
               Orientation := Orientation_Vertical;
            else
               Orientation := Orientation_Horizontal;
            end if;

            Paint_Handle
              (Get_Style (M),
               M.Handles (J),
               State_Normal,
               Shadow_None,
               Area,
               M,
               X => 0,
               Y => 0,
               Width => -1,
               Height => -1,
               Orientation => Orientation);
         end loop;
      end if;

      --  Draw the relief lines. Note that this is slightly complex, since
      --  we might have to draw on several windows if there are several
      --  handles in the layout.
      --  ??? Relief could be achieved by putting all the children in frames.

      if M.Docks (Left) /= null then
         Get_Geometry (M.Handles (Left), X, Y, W, H, Depth);
         First := 0;
         Last := H;

         Draw_Line
           (M.Handles (Left), Get_White_GC (Get_Style (M)), 0, 0, 0, H);

         if M.Docks (Top) /= null then
            First := Handle_Size - 1;
            Draw_Line
              (M.Handles (Left), Get_White_GC (Get_Style (M)),
               0, 0, Handle_Size - 1, 0);
         end if;

         if M.Docks (Bottom) /= null then
            Last := Last - Handle_Size + 1;
            Draw_Line
              (M.Handles (Left), Get_Black_GC (Get_Style (M)),
               0, H - 1, Handle_Size - 1, H - 1);
         end if;

         Draw_Line
           (M.Handles (Left), Get_Black_GC (Get_Style (M)),
            Handle_Size - 1, First, Handle_Size - 1, Last);
      end if;

      if M.Docks (Bottom) /= null then
         Get_Geometry (M.Handles (Bottom), X, Y, W, H, Depth);
         First := 0;
         Last := W;

         Draw_Line
           (M.Handles (Bottom), Get_Black_GC (Get_Style (M)),
            0, Handle_Size - 1, W, Handle_Size - 1);

         if M.Docks (Right) /= null then
            Last := Last - Handle_Size;
            Draw_Line
              (M.Handles (Bottom), Get_Black_GC (Get_Style (M)),
               W - 1, 0, W - 1, Handle_Size - 1);
         end if;

         if M.Docks (Left) /= null then
            First := Handle_Size - 1;
            Draw_Line
              (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
               0, 0, 0, Handle_Size - 2);
         end if;

         Draw_Line
           (M.Handles (Bottom), Get_White_GC (Get_Style (M)),
            First, 0, Last, 0);
      end if;

      if M.Docks (Right) /= null then
         Get_Geometry (M.Handles (Right), X, Y, W, H, Depth);
         First := 0;
         Last := H;

         Draw_Line
           (M.Handles (Right), Get_Black_GC (Get_Style (M)),
            Handle_Size - 1, 0, Handle_Size - 1, H);

         if M.Docks (Top) /= null then
            First := Handle_Size - 1;
            Draw_Line
              (M.Handles (Right), Get_White_GC (Get_Style (M)),
               0, 0, Handle_Size - 1, 0);
         end if;

         if M.Docks (Bottom) /= null then
            Last := Last - Handle_Size + 1;
            Draw_Line
              (M.Handles (Right), Get_Black_GC (Get_Style (M)),
               0, H - 1, Handle_Size - 1, H - 1);
         end if;

         Draw_Line
           (M.Handles (Right), Get_White_GC (Get_Style (M)),
            0, First, 0, Last);
      end if;

      if M.Docks (Top) /= null then
         Get_Geometry (M.Handles (Top), X, Y, W, H, Depth);
         First := 0;
         Last := W;

         Draw_Line
           (M.Handles (Top), Get_White_GC (Get_Style (M)),
            0, 0, W, 0);

         if M.Docks (Left) /= null then
            First := Handle_Size - 1;
            Draw_Line
              (M.Handles (Top), Get_White_GC (Get_Style (M)),
               0, 0, 0, Handle_Size);
         end if;

         if M.Docks (Right) /= null then
            Last := Last - Handle_Size;
            Draw_Line
              (M.Handles (Top), Get_Black_GC (Get_Style (M)),
               W - 1, 0, W - 1, Handle_Size);
         end if;

         Draw_Line
           (M.Handles (Top), Get_Black_GC (Get_Style (M)),
            First, Handle_Size - 1, Last, Handle_Size - 1);
      end if;
      return True;
   end Expose_MDI;

   ------------------------
   -- Reposition_Handles --
   ------------------------

   procedure Reposition_Handles (M : access MDI_Window_Record'Class) is
      Alloc : Gtk_Allocation;
      MDI_Width  : constant Gint := Gint (Get_Allocation_Width (M));
      MDI_Height : constant Gint := Gint (Get_Allocation_Height (M));

   begin
      --  If none of the handles has been created yet.
      if not Realized_Is_Set (M) then
         return;
      end if;

      if M.Docks_Size (Left) /= 0 then
         Alloc := (M.Docks_Size (Left),
                   M.Docks_Size (Top),
                   Handle_Size,
                   MDI_Height - M.Docks_Size (Top) - M.Docks_Size (Bottom));
         Show (M.Handles (Left));
         Gdk.Window.Move_Resize
           (M.Handles (Left), Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
      else
         Hide (M.Handles (Left));
      end if;

      if M.Docks_Size (Right) /= 0 then
         Alloc := (MDI_Width - M.Docks_Size (Right) - Handle_Size,
                   M.Docks_Size (Top),
                   Handle_Size,
                   MDI_Height - M.Docks_Size (Top) - M.Docks_Size (Bottom));
         Show (M.Handles (Right));
         Gdk.Window.Move_Resize
           (M.Handles (Right), Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
      else
         Hide (M.Handles (Right));
      end if;

      if M.Docks_Size (Top) /= 0 then
         Alloc := (M.Docks_Size (Left),
                   M.Docks_Size (Top),
                   MDI_Width - M.Docks_Size (Left) - M.Docks_Size (Right),
                   Handle_Size);
         Show (M.Handles (Top));
         Gdk.Window.Move_Resize
           (M.Handles (Top), Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
      else
         Hide (M.Handles (Top));
      end if;

      if M.Docks_Size (Bottom) /= 0 then
         Alloc := (M.Docks_Size (Left),
                   MDI_Height - M.Docks_Size (Bottom) - Handle_Size,
                   MDI_Width - M.Docks_Size (Left) - M.Docks_Size (Right),
                   Handle_Size);
         Show (M.Handles (Bottom));
         Gdk.Window.Move_Resize
           (M.Handles (Bottom), Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
      else
         Hide (M.Handles (Bottom));
      end if;
   end Reposition_Handles;

   ------------------------------
   -- Size_Allocate_MDI_Layout --
   ------------------------------

   procedure Size_Allocate_MDI_Layout
     (Layout : System.Address; Alloc : Gtk_Allocation)
   is
      Stub : Gtk_Widget_Record;
      L : Gtk_Widget := Gtk_Widget (Get_User_Data (Layout, Stub));
   begin
      --  First, register the new size of the MDI itself
      Set_Allocation (L, Alloc);
      if Realized_Is_Set (L) then
         Move_Resize
           (Get_Window (L), Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
      end if;
   end Size_Allocate_MDI_Layout;

   -----------------------
   -- Size_Allocate_MDI --
   -----------------------

   procedure Size_Allocate_MDI
     (MDI : System.Address; MDI_Alloc : Gtk_Allocation)
   is
      use type Widget_List.Glist;
      Stub : MDI_Window_Record;
      M : MDI_Window := MDI_Window (Get_User_Data (MDI, Stub));
      Alloc : Gtk_Allocation;
      Req   : Gtk_Requisition;
      List : Widget_List.Glist;
      C : MDI_Child;

   begin
      --  First, register the new size of the MDI itself
      Set_Allocation (M, MDI_Alloc);
      if Realized_Is_Set (M) then
         Move_Resize
           (Get_Window (M),
            MDI_Alloc.X, MDI_Alloc.Y, MDI_Alloc.Width, MDI_Alloc.Height);
      end if;

      --  Resize the children that haven't been initialized yet.

      List := First (M.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.Uniconified_Width = -1 or else C.Uniconified_Height = -1 then
            Size_Request (C, Req);
            C.Uniconified_Width := Req.Width;
            C.Uniconified_Height := Req.Height;
            Alloc := (C.X, C.Y, C.Uniconified_Width, C.Uniconified_Height);
            Size_Allocate (C, Alloc);
         end if;

         List := Widget_List.Next (List);
      end loop;

      --  Then resize all the handles and notebooks on the sides.

      for J in Left .. Bottom loop
         if M.Docks_Size (J) = -1
           and then M.Docks (J) /= null
         then
            Size_Request (M.Docks (J), Req);

            case J is
               when Left | Right =>
                  M.Docks_Size (J) := Req.Width;

               when Top | Bottom =>
                  M.Docks_Size (J) := Req.Height;
            end case;
         end if;
      end loop;

      --  Left dock

      if M.Docks (Left) /= null then
         Alloc.X := 0;
         Alloc.Width := M.Docks_Size (Left);

         if M.Priorities (Top) < M.Priorities (Left) then
            Alloc.Y := M.Docks_Size (Top);
         else
            Alloc.Y := 0;
         end if;

         Alloc.Height := MDI_Alloc.Height - Alloc.Y;
         if M.Priorities (Bottom) < M.Priorities (Left) then
            Alloc.Height := Alloc.Height - M.Docks_Size (Bottom);
         end if;

         Size_Allocate (M.Docks (Left), Alloc);
      end if;

      --  Right dock

      if M.Docks (Right) /= null then
         Alloc.Width := M.Docks_Size (Right);
         Alloc.X := MDI_Alloc.Width - Alloc.Width;

         if M.Priorities (Top) < M.Priorities (Right) then
            Alloc.Y := M.Docks_Size (Top);
         else
            Alloc.Y := 0;
         end if;

         Alloc.Height := MDI_Alloc.Height - Alloc.Y;
         if M.Priorities (Bottom) < M.Priorities (Right) then
            Alloc.Height := Alloc.Height - M.Docks_Size (Bottom);
         end if;

         Size_Allocate (M.Docks (Right), Alloc);
      end if;

      --  Top dock

      if M.Docks (Top) /= null then
         Alloc.Y := 0;
         Alloc.Height := M.Docks_Size (Top);

         if M.Priorities (Left) < M.Priorities (Top) then
            Alloc.X := M.Docks_Size (Left);
         else
            Alloc.X := 0;
         end if;

         Alloc.Width := MDI_Alloc.Width - Alloc.X;
         if M.Priorities (Right) < M.Priorities (Top) then
            Alloc.Width := Alloc.Width - M.Docks_Size (Right);
         end if;

         Size_Allocate (M.Docks (Top), Alloc);
      end if;

      --  Bottom dock

      if M.Docks (Bottom) /= null then
         Alloc.Height := M.Docks_Size (Bottom);
         Alloc.Y := MDI_Alloc.Height - Alloc.Height;

         if M.Priorities (Left) < M.Priorities (Bottom) then
            Alloc.X := M.Docks_Size (Left);
         else
            Alloc.X := 0;
         end if;

         Alloc.Width := MDI_Alloc.Width - Alloc.X;
         if M.Priorities (Right) < M.Priorities (Bottom) then
            Alloc.Width := Alloc.Width - M.Docks_Size (Right);
         end if;

         Size_Allocate (M.Docks (Bottom), Alloc);
      end if;

      --  Middle container
      if M.Docks (Left) /= null then
         Alloc.X := M.Docks_Size (Left) + Handle_Size;
      else
         Alloc.X := 0;
      end if;

      if M.Docks (Top) /= null then
         Alloc.Y := M.Docks_Size (Top) + Handle_Size;
      else
         Alloc.Y := 0;
      end if;

      Alloc.Width := MDI_Alloc.Width - Alloc.X;
      if M.Docks (Right) /= null then
         Alloc.Width := Alloc.Width - Handle_Size - M.Docks_Size (Right);
      end if;

      Alloc.Height := MDI_Alloc.Height - Alloc.Y;
      if M.Docks (Bottom) /= null then
         Alloc.Height := Alloc.Height - Handle_Size - M.Docks_Size (Bottom);
      end if;

      if M.Docks (None) /= null then
         Size_Allocate (M.Docks (None), Alloc);
      else
         Size_Allocate (M.Layout, Alloc);
      end if;

      Reposition_Handles (M);
   end Size_Allocate_MDI;

   -----------------
   -- Destroy_MDI --
   -----------------

   procedure Destroy_MDI (MDI : access Gtk_Widget_Record'Class) is
      use Widget_List;
      Tmp : Widget_List.Glist := First (MDI_Window (MDI).Items);
      N   : Widget_List.Glist;
   begin
      while Tmp /= Null_List loop
         --  Get the next field first, since Destroy will actually destroy Tmp
         N := Next (Tmp);
         Destroy (Get_Data (Tmp));
         Tmp := N;
      end loop;

      Free (MDI_Window (MDI).Items);

      if MDI_Window (MDI).Menu /= null then
         Destroy (MDI_Window (MDI).Menu);
      end if;
   end Destroy_MDI;

   -------------------
   -- Iconify_Child --
   -------------------

   procedure Iconify_Child (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      Minimize_Child (C, not (C.State = Iconified));
   end Iconify_Child;

   -----------------
   -- Close_Child --
   -----------------

   procedure Close_Child (Child : access Gtk_Widget_Record'Class) is
      C          : MDI_Child := MDI_Child (Child);
      Event      : Gdk_Event;
      Old_Parent : Gtk_Widget;
      Result     : Boolean;

   begin
      Allocate (Event, Delete, Get_Window (C.MDI));

      --  For a top-level window, we must rebuild the initial widget
      --  temporarily, so that the application can do all the test it wants.
      --  However, we need to restore the initial state before calling
      --  Dock_Child and Float_Child below

      if C.Initial.all in Gtk_Window_Record'Class then
         Old_Parent := Get_Parent (C.Initial_Child);
         Reparent (C.Initial_Child, Gtk_Window (C.Initial));
         Result := Return_Callback.Emit_By_Name
           (C.Initial, "delete_event", Event);
         Reparent (C.Initial_Child, Old_Parent);

      else
         Result := Return_Callback.Emit_By_Name
           (C.Initial, "delete_event", Event);
      end if;

      if not Result then
         Destroy (C);
      end if;

      Free (Event);
   end Close_Child;

   -------------------
   -- Destroy_Child --
   -------------------

   procedure Destroy_Child (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      --  We know at that stage that Child has already been unparent-ed
      pragma Assert (Get_Parent (Child) = null);

      if C.Menu_Item /= null then
         Destroy (C.Menu_Item);
      end if;

      if C = C.MDI.Focus_Child then
         C.MDI.Focus_Child := null;
      end if;

      Widget_List.Remove (C.MDI.Items, Gtk_Widget (C));

      if C.State = Floating or else C.State = Docked then
         if C.State = Floating then
            --  Initial_Child could be null if we are destroying a floating
            --  child explicitly (by closing its X11 window)

            if C.Initial_Child /= null
              and then Get_Parent (C.Initial_Child) /= null
            then
               Destroy (Get_Parent (C.Initial_Child));
            end if;
         else
            Ref (C.Initial_Child);
            Remove_From_Notebook (C, C.Dock);
            Unref (C.Initial_Child);
         end if;
      end if;

      --  Destroy the toplevel Child is associated with
      if C.Initial /= C.Initial_Child then
         Destroy (C.Initial);
      end if;

      C.Initial := null;
      Free (C.Title);
   end Destroy_Child;

   ---------------------------
   -- Destroy_Initial_Child --
   ---------------------------

   procedure Destroy_Initial_Child (Child : access Gtk_Widget_Record'Class) is
   begin
      --  The initial child has already been unparented
      pragma Assert (Get_Parent (MDI_Child (Child).Initial_Child) = null);

      if MDI_Child (Child).Initial = MDI_Child (Child).Initial_Child then
         MDI_Child (Child).Initial := null;
      end if;

      MDI_Child (Child).Initial_Child := null;

      --  If the initial_child wasn't explicitly destroyed in Destroy_Child

      if not Gtk.Object.Destroyed_Is_Set (Child) then
         Destroy (Child);
      end if;
   end Destroy_Initial_Child;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access MDI_Child_Record'Class; Area : Gdk_Rectangle)
   is
      F  : Gdk.Font.Gdk_Font :=
        Get_Gdkfont (Title_Font_Name, Title_Font_Height);
      GC : Gdk.Gdk_GC := Child.MDI.Non_Focus_GC;
   begin
      --  Call this function so that for a dock item is highlighted if the
      --  current page is linked to the focus child.

      if Child.MDI.Focus_Child = MDI_Child (Child) then
         GC := Child.MDI.Focus_GC;
      end if;

      Draw_Rectangle
        (Get_Window (Child),
         GC,
         True,
         Border_Thickness,
         Border_Thickness,
         Gint (Get_Allocation_Width (Child)) - 2 * Border_Thickness,
         Title_Bar_Height);

      Draw_Text
        (Get_Window (Child),
         F,
         Get_White_GC (Get_Style (Child.MDI)),
         Border_Thickness + 3,
         Border_Thickness +
         (Title_Bar_Height + Get_Ascent (F) - Get_Descent (F)) / 2,
         Child.Title.all);

      Draw_Shadow
        (Get_Style (Child),
         Get_Window (Child),
         State_Normal,
         Shadow_Out,
         1, 1,
         Gint (Get_Allocation_Width (Child)) - 1,
         Gint (Get_Allocation_Height (Child)) - 1);
   end Draw_Child;

   ----------------
   -- Draw_Child --
   ----------------

   procedure Draw_Child
     (Child : access Gtk_Widget_Record'Class; Params : Gtk_Args) is
   begin
      Draw_Child (MDI_Child (Child), Full_Area);
      Gtk.Handlers.Emit_Stop_By_Name (Child, "draw");
   end Draw_Child;

   ----------------
   -- Draw_Child --
   ----------------

   function Draw_Child
     (Child : access Gtk_Widget_Record'Class; Event : Gdk_Event)
      return Boolean is
   begin
      Draw_Child (MDI_Child (Child), Get_Area (Event));
      Gtk.Handlers.Emit_Stop_By_Name (Child, "expose_event");
      return True;
   end Draw_Child;

   ------------------------
   -- Button_Pressed_MDI --
   ------------------------

   function Button_Pressed_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M : MDI_Window := MDI_Window (MDI);
      Cursor : Gdk_Cursor;
      Tmp    : Boolean;
      Win_X, Win_Y : Gint;
   begin
      if Get_Button (Event) /= 1 then
         return False;
      end if;

      M.Selected := None;

      for J in Left .. Bottom loop
         if Get_Window (Event) = M.Handles (J) then
            M.Selected := J;
         end if;
      end loop;

      if M.Selected = None then
         return False;
      end if;

      Gdk_New (Cursor, Cross);
      Tmp := Pointer_Grab
        (M.Handles (M.Selected),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time   => 0);
      Destroy (Cursor);

      Get_Position (M.Handles (M.Selected), Win_X, Win_Y);

      case M.Selected is
         when Left | Right =>
            M.Current_X := Gint (Get_X (Event)) + Win_X;
            M.Current_W := M.Current_X;
            M.Current_Y := 0;
            M.Current_H := Gint (Get_Allocation_Height (M));

         when Top | Bottom =>
            M.Current_X := 0;
            M.Current_W := Gint (Get_Allocation_Width (M));
            M.Current_Y := Gint (Get_Y (Event)) + Win_Y;
            M.Current_H := M.Current_Y;

         when None =>
            null;
      end case;

      Draw_Line
        (Get_Window (MDI),
         M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);

      return False;
   end Button_Pressed_MDI;

   ------------------------
   -- Button_Release_MDI --
   ------------------------

   function Button_Release_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M : MDI_Window := MDI_Window (MDI);
   begin
      if M.Selected = None then
         return False;
      end if;

      Draw_Line
        (Get_Window (MDI),
         M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);

      case M.Selected is
         when Left =>
            M.Docks_Size (Left) := M.Current_X;

         when Right =>
            M.Docks_Size (Right) :=
              Gint (Get_Allocation_Width (M)) - M.Current_X;

         when Top =>
            M.Docks_Size (Top) := M.Current_Y;

         when Bottom =>
            M.Docks_Size (Bottom) :=
              Gint (Get_Allocation_Height (M)) - M.Current_Y;

         when None =>
            null;
      end case;

      --  Make sure the size is at least one, or the handle will completely
      --  disappear and the user will not have access to the window any more
      M.Docks_Size (M.Selected) := Gint'Max (1, M.Docks_Size (M.Selected));

      Queue_Resize (M);

      Pointer_Ungrab (Time => 0);
      M.Selected := None;
      return False;
   end Button_Release_MDI;

   -----------------------
   -- Button_Motion_MDI --
   -----------------------

   function Button_Motion_MDI
     (MDI   : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      M : MDI_Window := MDI_Window (MDI);
      X, Y, Win_X, Win_Y : Gint;
   begin
      if M.Selected = None then
         return False;
      end if;

      Draw_Line
        (Get_Window (MDI),
         M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);

      if Get_Window (Event) /= Get_Window (M) then
         Get_Pointer (M, X, Y);
      else
         Get_Position (M.Handles (M.Selected), Win_X, Win_Y);
         X := Gint (Get_X (Event)) + Win_X;
         Y := Gint (Get_Y (Event)) + Win_Y;
      end if;

      case M.Selected is
         when Left | Right =>
            M.Current_X := X;
            M.Current_W := X;

         when Top | Bottom =>
            M.Current_Y := Y;
            M.Current_H := Y;

         when None =>
            null;
      end case;

      Draw_Line
        (Get_Window (MDI),
         M.Xor_GC, M.Current_X, M.Current_Y, M.Current_W, M.Current_H);
      return False;
   end Button_Motion_MDI;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C      : MDI_Child := MDI_Child (Child);
      MDI    : MDI_Window := C.MDI;
      Cursor : Gdk.Cursor.Gdk_Cursor;
      Tmp    : Boolean;
      Curs   : Gdk_Cursor_Type;

   begin
      --  It sometimes happens that widgets let events pass through (for
      --  instance scrollbars do that), and thus wouldn't be useable anymore
      --  if we do a grab.

      if Get_Window (Child) /= Get_Window (Event)
        or else Get_Event_Type (Event) /= Button_Press
      then
         return False;
      end if;

      Activate_Child (C);

      --  Can't move items inside a notebook
      if C.State = Docked
        or else (C.State = Normal and then MDI.Docks (None) /= null)
      then
         return False;
      end if;

      MDI.Selected_Child := C;

      MDI.X_Root := Gint (Get_X_Root (Event));
      MDI.Y_Root := Gint (Get_Y_Root (Event));
      MDI.Initial_Width := Gint (Get_Allocation_Width (Child));
      MDI.Initial_Height := Gint (Get_Allocation_Height (Child));
      MDI.Current_W := MDI.Initial_Width;
      MDI.Current_H := MDI.Initial_Height;
      MDI.Current_X := C.X;
      MDI.Current_Y := C.Y;

      Curs := Side (C, Gint (Get_X (Event)), Gint (Get_Y (Event)));
      MDI.Current_Cursor := Curs;

      Gdk_New (Cursor, Curs);
      Tmp := Pointer_Grab
        (Get_Window (C),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time => 0);
      Destroy (Cursor);

      if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
        or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
      then
         Draw_Rectangle
           (Get_Window (MDI.Layout),
            MDI.Xor_GC,
            Filled => False,
            X => MDI.Current_X,
            Y => MDI.Current_Y,
            Width => MDI.Current_W,
            Height => MDI.Current_H);
      end if;
      return True;
   end Button_Pressed;

   --------------------
   -- Button_Release --
   --------------------

   function Button_Release
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      MDI : MDI_Window := MDI_Child (Child).MDI;
      Alloc : Gtk_Allocation;
   begin
      if Get_Window (Child) /= Get_Window (Event)
        or else MDI.Selected_Child = null
      then
         return False;
      end if;

      Pointer_Ungrab (Time => 0);

      Alloc := (MDI.Current_X, MDI.Current_Y, MDI.Current_W, MDI.Current_H);

      if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
        or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
      then
         Draw_Rectangle
           (Get_Window (MDI.Layout),
            MDI.Xor_GC,
            Filled => False,
            X => Alloc.X,
            Y => Alloc.Y,
            Width => Alloc.Width,
            Height => Alloc.Height);
         Size_Allocate (Child, Alloc);

         --  Move (MDI.Layout, Child, Gint16 (Alloc.X), Gint16 (Alloc.Y));
         --  Set_USize (Child, Alloc.Width, Alloc.Height);
      end if;

      MDI_Child (Child).X := Alloc.X;
      MDI_Child (Child).Y := Alloc.Y;

      if MDI.Current_Cursor /= Left_Ptr then
         MDI_Child (Child).Uniconified_Width  := Alloc.Width;
         MDI_Child (Child).Uniconified_Height := Alloc.Height;
      end if;
      return True;
   end Button_Release;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Child : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      C       : MDI_Child := MDI_Child (Child);
      MDI     : MDI_Window := C.MDI;
      Delta_X : Gint;
      Delta_Y : Gint;
      Cursor  : Gdk_Cursor;
      Curs    : Gdk_Cursor_Type;
      W, H    : Gint;
      Alloc   : Gtk_Allocation;

   begin
      if Get_Window (Child) /= Get_Window (Event) then
         return False;
      end if;

      --  A button_motion event ?

      if (Get_State (Event) and Button1_Mask) /= 0
        and then MDI.Selected_Child /= null
      then

         if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
           or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
         then
            Draw_Rectangle
              (Get_Window (MDI.Layout),
               MDI.Xor_GC,
               Filled => False,
               X => MDI.Current_X,
               Y => MDI.Current_Y,
               Width => MDI.Current_W,
               Height => MDI.Current_H);
         end if;

         Delta_X := Gint (Get_X_Root (Event)) - MDI.X_Root;
         Delta_Y := Gint (Get_Y_Root (Event)) - MDI.Y_Root;
         W := MDI.Initial_Width;
         H := MDI.Initial_Height;

         MDI.Current_X := C.X;
         MDI.Current_Y := C.Y;

         case MDI.Current_Cursor is
            when Left_Ptr =>
               MDI.Current_X := Delta_X + C.X;
               MDI.Current_Y := Delta_Y + C.Y;

            when Left_Side =>
               W := Gint'Max (Min_Width, W - Delta_X);
               MDI.Current_X := MDI.Current_X + MDI.Initial_Width - W;

            when Right_Side =>
               W := Gint'Max (Min_Width, W + Delta_X);

            when Top_Side =>
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_Y := C.Y + MDI.Initial_Height - H;

            when Bottom_Side =>
               H := Gint'Max (Min_Height, H + Delta_Y);

            when Top_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_X := C.X + MDI.Initial_Width - W;
               MDI.Current_Y := C.Y + MDI.Initial_Height - H;

            when Top_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H - Delta_Y);
               MDI.Current_Y := C.Y + MDI.Initial_Height - H;

            when Bottom_Left_Corner =>
               W := Gint'Max (Min_Width, W - Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);
               MDI.Current_X := C.X + MDI.Initial_Width - W;

            when Bottom_Right_Corner =>
               W := Gint'Max (Min_Width, W + Delta_X);
               H := Gint'Max (Min_Height, H + Delta_Y);

            when others => null;
         end case;

         if MDI.Current_Cursor = Left_Ptr and then Opaque_Move then
            Alloc :=
              (MDI.Current_X, MDI.Current_Y, MDI.Current_W, MDI.Current_H);
--            Move (MDI.Layout, Child, Gint16 (Alloc.X), Gint16 (Alloc.Y));
            Size_Allocate (Child, Alloc);

         elsif MDI.Current_Cursor /= Left_Ptr
           and then Opaque_Resize
           and then (W /= Gint (Get_Allocation_Width (C))
                     or else H /= Gint (Get_Allocation_Height (C)))
         then
            Alloc := (C.X, C.Y, W, H);
            Size_Allocate (Child, Alloc);
         end if;

         if (not Opaque_Resize and then MDI.Current_Cursor /= Left_Ptr)
           or else (not Opaque_Move and then MDI.Current_Cursor = Left_Ptr)
         then
            MDI.Current_W := W;
            MDI.Current_H := H;
            Draw_Rectangle
              (Get_Window (MDI.Layout),
               MDI.Xor_GC,
               Filled => False,
               X      => MDI.Current_X,
               Y      => MDI.Current_Y,
               Width  => MDI.Current_W,
               Height => MDI.Current_H);
         end if;

      --  A motion_event ? change the cursor if needed

      elsif C.State = Normal then
         Delta_X := Gint (Get_X (Event));
         Delta_Y := Gint (Get_Y (Event));
         Curs := Side (C, Delta_X, Delta_Y);

         if Curs /= MDI.Current_Cursor then
            MDI.Current_Cursor := Curs;
            Gdk_New (Cursor, MDI.Current_Cursor);
            Gdk.Window.Set_Cursor (Get_Window (Child), Cursor);
            Destroy (Cursor);
         end if;
      end if;
      return True;
   end Button_Motion;

   -----------------
   -- Leave_Child --
   -----------------

   function Leave_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      MDI    : MDI_Window := MDI_Child (Child).MDI;
      Cursor : Gdk_Cursor;
   begin
      if Get_State (Event) = 0
        and then MDI.Current_Cursor /= Left_Ptr
      then
         MDI.Current_Cursor := Left_Ptr;
         Gdk_New (Cursor, MDI.Current_Cursor);
         Gdk.Window.Set_Cursor (Get_Window (Child), Cursor);
         Destroy (Cursor);
      end if;

      return False;
   end Leave_Child;

   ----------
   -- Side --
   ----------

   function Side
     (Child : access MDI_Child_Record'Class; X, Y  : Gint)
      return Gdk_Cursor_Type
   is
      X_Side, Y_Side : Gint;
   begin
      if X <= Border_Thickness then
         X_Side := -2;
      elsif X <= Corner_Size then
         X_Side := -1;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Border_Thickness then
         X_Side := 2;
      elsif X >= Gint (Get_Allocation_Width (Child)) - Corner_Size then
         X_Side := 1;
      else
         X_Side := 0;
      end if;

      if Y <= Border_Thickness then
         Y_Side := -2;
      elsif Y <= Corner_Size then
         Y_Side := -1;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Border_Thickness then
         Y_Side := 2;
      elsif Y >= Gint (Get_Allocation_Height (Child)) - Corner_Size then
         Y_Side := 1;
      else
         Y_Side := 0;
      end if;

      if X_Side <= -1 and then Y_Side <= -1 then
         return Top_Left_Corner;

      elsif X_Side <= -1 and then Y_Side >= 1 then
         return Bottom_Left_Corner;

      elsif X_Side >= 1 and then Y_Side <= -1 then
         return Top_Right_Corner;

      elsif X_Side >= 1 and then Y_Side >= 1 then
         return Bottom_Right_Corner;

      elsif X_Side = -2 and then Y_Side in -1 .. 1 then
         return Left_Side;

      elsif X_Side = 2 and then Y_Side in -1 .. 1 then
         return Right_Side;

      elsif Y_Side = -2 and then X_Side in -1 .. 1 then
         return Top_Side;

      elsif Y_Side = 2 and then X_Side in -1 .. 1 then
         return Bottom_Side;
      end if;

      return Left_Ptr;
   end Side;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Child   : out MDI_Child;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Child := new MDI_Child_Record;
      Initialize (Child, Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Child   : access MDI_Child_Record;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Button    : Gtk_Button;
      Box, Box2 : Gtk_Box;
      Pix       : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      Pixmap    : Gtk_Pixmap;

   begin
      Gtk.Event_Box.Initialize (Child);
      Child.Initial := Gtk_Widget (Widget);
      Child.Uniconified_Width := -1;

      Child.State := Normal;

      Add_Events
        (Child, Button_Press_Mask
           or Button_Motion_Mask
           or Button_Release_Mask
           or Pointer_Motion_Mask);
      Return_Callback.Connect
        (Child, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Child, "button_release_event",
         Return_Callback.To_Marshaller (Button_Release'Access));
      Return_Callback.Connect
        (Child, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      --  Widget_Callback.Connect (Child, "draw", Draw_Child'Access);
      Widget_Callback.Connect
        (Child, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Child'Access));
      Return_Callback.Connect
        (Child, "leave_notify_event",
         Return_Callback.To_Marshaller (Leave_Child'Access));
      Return_Callback.Connect
        (Child, "expose_event",
         Return_Callback.To_Marshaller (Draw_Child'Access));

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Child, Box);

      --  Buttons in the title bar

      Gtk_New_Hbox (Box2, Homogeneous => False);
      Pack_Start (Box, Box2, Expand => False, Fill => False);

      Set_Border_Width (Box, Border_Thickness);
      Set_USize (Box2, -2, Title_Bar_Height);

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Close_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Button);
      Add (Button, Pixmap);
      Pack_End (Box2, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Close_Child'Access), Child);

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Maximize_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Child.Maximize_Button);
      Add (Child.Maximize_Button, Pixmap);
      Pack_End
        (Box2, Child.Maximize_Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Child.Maximize_Button, "clicked",
         Widget_Callback.To_Marshaller (Maximize_Child_Cb'Access), Child);

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Iconify_Xpm);
      Gtk_New (Pixmap, Pix, Mask);
      Gtk_New (Child.Minimize_Button);
      Add (Child.Minimize_Button, Pixmap);
      Pack_End (Box2, Child.Minimize_Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Child.Minimize_Button, "clicked",
         Widget_Callback.To_Marshaller (Iconify_Child'Access), Child);

      --  The child widget

      if Widget.all in Gtk_Window_Record'Class then
         pragma Assert (Get_Child (Gtk_Window (Widget)) /= null);
         Child.Initial_Child := Get_Child (Gtk_Window (Widget));
         Reparent (Child.Initial_Child, Box);

      else
         Child.Initial_Child := Gtk_Widget (Widget);
         Pack_Start
           (Box, Widget, Expand => True, Fill => True, Padding => 0);
      end if;

      Widget_Callback.Object_Connect
        (Child.Initial_Child, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Initial_Child'Access), Child);
   end Initialize;

   ---------
   -- Put --
   ---------

   function Put
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
      return MDI_Child
   is
      C : MDI_Child;
      Alloc : Gtk_Allocation;
   begin
      if Child.all in MDI_Child_Record'Class then
         C := MDI_Child (Child);
      else
         Gtk_New (C, Child);
      end if;

      C.MDI := MDI_Window (MDI);
      C.X := 10;
      C.Y := 10;

      if Child.all in Gtk_Window_Record'Class then
         C.Title := new String' (Get_Title (Gtk_Window (Child)));
      else
         C.Title := new String' (" ");
      end if;

      --  We need to show the widget before inserting it in a notebook,
      --  otherwise the notebook page will not be made visible.
      Show_All (C);

      Widget_List.Prepend (MDI.Items, Gtk_Widget (C));

      --  If all items are maximized, add Child to the notebook
      if MDI.Docks (None) /= null then
         Put_In_Notebook (MDI, None, C);
      else
         Put (MDI.Layout, C, 0, 0);
         Alloc := (C.X, C.Y, 10, 10);
         Size_Allocate (C, Alloc);

         if Realized_Is_Set (MDI) then
            Queue_Resize (MDI);
         end if;
      end if;

      if MDI.Menu /= null then
         Create_Menu_Entry (C);
      end if;

      --  If MDI is not realized, then we don't need to do anything now,
      --  this will be done automatically in Realize_MDI

      if Realized_Is_Set (MDI) then
         Realize (C);
         Activate_Child (C);
      end if;

      return C;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      C : MDI_Child;
   begin
      C := Put (MDI, Child);
   end Put;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Child : access MDI_Child_Record) return String is
   begin
      return Child.Title.all;
   end Get_Title;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Child : access MDI_Child_Record; Title : String) is
      Label : Gtk_Accel_Label;
   begin
      Free (Child.Title);
      Child.Title := new String' (Title);

      if Child.Initial.all in Gtk_Window_Record'Class then
         Set_Title (Gtk_Window (Child.Initial), Title);
      end if;

      if Child.State = Docked then
         Set_Tab_Label_Text (Child.MDI.Docks (Child.Dock), Child, Title);

      elsif Child.State = Normal
        and then Child.MDI.Docks (None) /= null
      then
         Set_Tab_Label_Text (Child.MDI.Docks (None), Child, Title);
      end if;

      --  Update the menu, if it exists

      if Child.Menu_Item /= null then
         --  Since we don't want to use Gtk.Type_Conversion in this package,
         --  the simplest is to destroy and recreate the label associated
         --  with the menu_item.
         Gtk_New (Label, Title);
         Set_Alignment (Label, 0.0, 0.5);
         Set_Accel_Widget (Label, Child.Menu_Item);
         Remove (Child.Menu_Item, Get_Child (Child.Menu_Item));
         Add (Child.Menu_Item, Label);
         Show (Label);
      end if;
   end Set_Title;

   --------------------
   -- Find_MDI_Child --
   --------------------

   function Find_MDI_Child
     (MDI  : access MDI_Window_Record;
      Name : String) return MDI_Child
   is
      use Widget_List;
      Tmp : Widget_List.Glist := First (MDI.Items);
   begin
      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).Title.all = Name then
            return MDI_Child (Get_Data (Tmp));
         end if;

         Tmp := Next (Tmp);
      end loop;
      return null;
   end Find_MDI_Child;

   --------------------
   -- Find_MDI_Child --
   --------------------

   function Find_MDI_Child
     (MDI    : access MDI_Window_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return MDI_Child
   is
      use Widget_List;
      Tmp : Widget_List.Glist;
   begin
      Tmp := First (MDI.Items);

      while Tmp /= Null_List loop
         if MDI_Child (Get_Data (Tmp)).Initial_Child = Gtk_Widget (Widget)
           or else MDI_Child (Get_Data (Tmp)).Initial = Gtk_Widget (Widget)
         then
            return MDI_Child (Get_Data (Tmp));
         end if;

         Tmp := Next (Tmp);
      end loop;

      return null;
   end Find_MDI_Child;

   -----------------
   -- Raise_Child --
   -----------------

   procedure Raise_Child (Child : access MDI_Child_Record'Class) is
   begin
      --  For an docked item, we in fact want to raise its parent dock,
      --  and make sure the current page in that dock is the correct one.

      if Child.State = Docked then
         Set_Page
           (Child.MDI.Docks (Child.Dock),
            Page_Num (Child.MDI.Docks (Child.Dock), Child));

      elsif Child.State = Normal
        and then Child.MDI.Docks (None) /= null
      then
         Set_Page
           (Child.MDI.Docks (None), Page_Num (Child.MDI.Docks (None), Child));

      elsif Realized_Is_Set (Child) then
         Gdk.Window.Gdk_Raise (Get_Window (Child));

         if Child.State = Floating then
            Gdk.Window.Gdk_Raise
              (Get_Window (Gtk_Window (Get_Parent (Child.Initial_Child))));
         end if;
      end if;
   end Raise_Child;

   ----------------------
   -- Update_Dock_Menu --
   ----------------------

   procedure Update_Dock_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Dock_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
         Set_Active (Child.MDI.Dock_Menu_Item, Child.State = Docked);
         Set_Sensitive (Child.MDI.Dock_Menu_Item, Child.Dock /= None);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Dock_Menu_Item, Child.MDI.Dock_Menu_Item_Id);
      end if;
   end Update_Dock_Menu;

   -----------------------
   -- Update_Float_Menu --
   -----------------------

   procedure Update_Float_Menu (Child : access MDI_Child_Record'Class) is
   begin
      if Child.MDI.Float_Menu_Item /= null then
         Gtk.Handlers.Handler_Block
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
         Set_Active (Child.MDI.Float_Menu_Item, Child.State = Floating);
         Gtk.Handlers.Handler_Unblock
           (Child.MDI.Float_Menu_Item, Child.MDI.Float_Menu_Item_Id);
      end if;
   end Update_Float_Menu;

   --------------------
   -- Activate_Child --
   --------------------

   procedure Activate_Child (Child : access MDI_Child_Record'Class) is
      Old  : MDI_Child := Child.MDI.Focus_Child;
      C    : MDI_Child := MDI_Child (Child);

   begin
      --  Be lazy. And avoid infinite loop when updating the MDI menu...

      if C = Old then
         return;
      end if;

      Child.MDI.Focus_Child := C;

      if Old /= null
        and then Realized_Is_Set (Old)
      then
         Draw_Child (Old, Full_Area);
      end if;

      --  Make sure the page containing Child in a notebook is put on top.
      Raise_Child (Child);

      if Realized_Is_Set (C) then
         Draw_Child (C, Full_Area);
      end if;

      Update_Dock_Menu (C);
      Update_Float_Menu (C);

      if C.Menu_Item /= null
        and then not Get_Active (C.Menu_Item)
      then
         Set_Active (C.Menu_Item, True);
      end if;
   end Activate_Child;

   ----------------------
   -- Cascade_Children --
   ----------------------

   procedure Cascade_Children (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;
      Level : Gint := 0;
      W, H : Gint;
      List : Widget_List.Glist := First (MDI.Items);
      C : MDI_Child;
      Num_Children : Gint := 0;
      Alloc : Gtk_Allocation;

   begin
      Maximize_Children (MDI, False);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal or else C.State = Iconified then
            Num_Children := Num_Children + 1;
         end if;
         List := Widget_List.Next (List);
      end loop;

      W := Gint (Get_Allocation_Width (MDI.Layout))
        - (Num_Children - 1) * Title_Bar_Height;
      H := Gint (Get_Allocation_Height (MDI.Layout))
        - (Num_Children - 1) * Title_Bar_Height;

      List := First (MDI.Items);

      --  Resize all children, except the one that has the focus (since
      --  we want it to be on top)
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if (C.State = Normal or else C.State = Iconified)
           and then C /= MDI.Focus_Child
         then
            C.X := Level;
            C.Y := Level;
            C.Uniconified_Width := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y, C.Uniconified_Width, H);
            Size_Allocate (C, Alloc);
            --  Move (MDI.Layout, C, Gint16 (C.X), Gint16 (C.Y));
            --  Set_USize (C, C.Uniconified_Width, H);
            Raise_Child (C);
            Level := Level + Title_Bar_Height;
         end if;
         List := Widget_List.Next (List);
      end loop;

      if MDI.Focus_Child /= null
        and then (MDI.Focus_Child.State = Normal
                  or else MDI.Focus_Child.State = Iconified)
      then
         MDI.Focus_Child.X := Level;
         MDI.Focus_Child.Y := Level;
         MDI.Focus_Child.Uniconified_Width := W;
         MDI.Focus_Child.Uniconified_Height := H;
         Alloc := (Level, Level, W, H);
         Size_Allocate (MDI.Focus_Child, Alloc);
         --  Move (MDI.Layout, MDI.Focus_Child, Gint16 (Level), Gint16 (Level));
         --  Set_USize (MDI.Focus_Child, W, H);
         Raise_Child (MDI.Focus_Child);
      end if;
   end Cascade_Children;

   -----------------------
   -- Tile_Horizontally --
   -----------------------

   procedure Tile_Horizontally (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;
      Level : Gint := 0;
      W, H : Gint;
      List : Widget_List.Glist := First (MDI.Items);
      C : MDI_Child;
      Num_Children : Gint := 0;
      Alloc : Gtk_Allocation;

   begin
      Maximize_Children (MDI, False);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal or else C.State = Iconified then
            Num_Children := Num_Children + 1;
         end if;
         List := Widget_List.Next (List);
      end loop;

      W := Gint (Get_Allocation_Width (MDI.Layout))
        / Num_Children;
      H := Gint (Get_Allocation_Height (MDI.Layout));

      List := First (MDI.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal or else C.State = Iconified then
            C.X := Level;
            C.Y := 0;
            C.Uniconified_Width := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y, C.Uniconified_Width, C.Uniconified_Height);
            Size_Allocate (C, Alloc);
            --  Move (MDI.Layout, C, Gint16 (C.X), Gint16 (C.Y));
            --  Set_USize (C, C.Uniconified_Width, C.Uniconified_Height);
            Level := Level + W;
         end if;
         List := Widget_List.Next (List);
      end loop;
   end Tile_Horizontally;

   ---------------------
   -- Tile_Vertically --
   ---------------------

   procedure Tile_Vertically (MDI : access MDI_Window_Record) is
      use type Widget_List.Glist;
      Level : Gint := 0;
      W, H : Gint;
      List : Widget_List.Glist := First (MDI.Items);
      C : MDI_Child;
      Num_Children : Gint := 0;
      Alloc : Gtk_Allocation;

   begin
      Maximize_Children (MDI, False);

      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal or else C.State = Iconified then
            Num_Children := Num_Children + 1;
         end if;
         List := Widget_List.Next (List);
      end loop;

      W := Gint (Get_Allocation_Width (MDI.Layout));
      H := Gint (Get_Allocation_Height (MDI.Layout)) / Num_Children;

      List := First (MDI.Items);
      while List /= Null_List loop
         C := MDI_Child (Get_Data (List));
         if C.State = Normal or else C.State = Iconified then
            C.X := 0;
            C.Y := Level;
            C.Uniconified_Width := W;
            C.Uniconified_Height := H;
            Alloc := (C.X, C.Y, C.Uniconified_Width, C.Uniconified_Height);
            Size_Allocate (C, Alloc);
            --  Move (MDI.Layout, C, Gint16 (C.X), Gint16 (C.Y));
            --  Set_USize (C, C.Uniconified_Width, C.Uniconified_Height);
            Level := Level + H;
         end if;
         List := Widget_List.Next (List);
      end loop;
   end Tile_Vertically;

   ------------------
   -- Delete_Child --
   ------------------

   function Delete_Child
     (Child : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      --  Gtk_Window children are handled differently (see Float_Child)
      pragma Assert
        (not (MDI_Child (Child).Initial.all in Gtk_Window_Record'Class));
      return Return_Callback.Emit_By_Name
        (MDI_Child (Child).Initial, "delete_event", Event);
   end Delete_Child;

   -----------------
   -- Float_Child --
   -----------------

   procedure Float_Child
     (Child : access MDI_Child_Record'Class;
      Float : Boolean)
   is
      Win : Gtk_Window;
      Alloc : Gtk_Allocation;
   begin
      if Child.State /= Floating and then Float then
         Minimize_Child (Child, False);
         Dock_Child (Child, False);

         Ref (Child);
         if Child.MDI.Docks (None) /= null then
            Remove_From_Notebook (Child, None);
         else
            Remove (Child.MDI.Layout, Child);
         end if;

         if Child.Initial.all in Gtk_Window_Record'Class then
            Win := Gtk_Window (Child.Initial);
            --  Delete_Event should be forwarded to the child, not to the
            --  toplevel window
            Return_Callback.Object_Connect
              (Win, "delete_event",
               Return_Callback.To_Marshaller (Delete_Child'Access), Child);
         else
            Gtk_New (Win, Window_Toplevel);
            Set_Title (Win, Child.Title.all);
         end if;

         Reparent (Child.Initial_Child, Win);
         Show_All (Win);
         Set_Default_Size
           (Win, Child.Uniconified_Width, Child.Uniconified_Height);

         Child.State := Floating;

      elsif Child.State = Floating and then not Float then
         --  Reassign the widget to Child instead of the notebook
         Ref (Child.Initial_Child);
         Win := Gtk_Window (Get_Parent (Child.Initial_Child));
         Reparent (Child.Initial_Child, Gtk_Box (Get_Child (Child)));
         Unref (Child.Initial_Child);
         Child.State := Normal;

         if Gtk_Widget (Child.Initial) = Gtk_Widget (Win) then
            Hide (Child.Initial);
         else
            Destroy (Win);
         end if;

         if Child.MDI.Docks (None) /= null then
            Put_In_Notebook (Child.MDI, None, Child);
         else
            Alloc := (Child.X, Child.Y, Child.Uniconified_Width,
                      Child.Uniconified_Height);
            Put (Child.MDI.Layout, Child, 0, 0);
            Size_Allocate (Child, Alloc);
         end if;
      end if;

      Activate_Child (Child);
      Update_Float_Menu (Child);
   end Float_Child;

   -----------------
   -- Is_Floating --
   -----------------

   function Is_Floating
     (Child : access MDI_Child_Record'Class) return Boolean is
   begin
      return Child.State = Floating;
   end Is_Floating;

   ------------------------
   -- Docked_Switch_Page --
   ------------------------

   procedure Docked_Switch_Page
     (Docked_Child : access Gtk_Widget_Record'Class; Args : GValues)
   is
      Page_Num : Guint := To_Guint (Args, 2);
   begin
      if Page_Num /= -1 then
         Activate_Child
           (MDI_Child (Get_Nth_Page
                       (Gtk_Notebook (Docked_Child), Gint (Page_Num))));
      end if;
   end Docked_Switch_Page;

   ---------------------
   -- Create_Notebook --
   ---------------------

   procedure Create_Notebook
     (MDI : access MDI_Window_Record'Class; Side : Dock_Side) is
   begin
      if MDI.Docks (Side) = null then
         Gtk_New (MDI.Docks (Side));
         Set_Tab_Pos (MDI.Docks (Side), Pos_Bottom);
         Set_Show_Tabs (MDI.Docks (Side), False);
         Set_Show_Border (MDI.Docks (Side), False);
         Set_Border_Width (MDI.Docks (Side), 0);
         --  Coordinates don't matter, they are set in Size_Allocate_MDI.
         Put (MDI, MDI.Docks (Side), 0, 0);
         Widget_Callback.Connect
           (MDI.Docks (Side), "switch_page",
            Docked_Switch_Page'Access, After => True);

         --   Size to be computed
         if Side /= None then
            MDI.Docks_Size (Side) := -1;
         end if;
         Show_All (MDI.Docks (Side));

      else
         --  Putting the following creates an infinite loop with the
         --  size_allocate signal in GVD (in the source window, when it is
         --  maximized).
         Set_Show_Tabs (MDI.Docks (Side), True);
      end if;
   end Create_Notebook;

   ---------------------
   -- Put_In_Notebook --
   ---------------------

   procedure Put_In_Notebook
     (MDI   : access MDI_Window_Record'Class;
      Side  : Dock_Side;
      Child : access MDI_Child_Record'Class)
   is
      Label : Gtk_Label;
   begin
      --  Embed the contents of the child into the notebook, and mark
      --  Child as docked, so that we can't manipulate it afterwards.

      Ref (Child);

      if Get_Parent (Child) /= null then
         case Child.State is
            when Docked =>
               Remove_From_Notebook (Child, Child.Dock);

            when Normal =>
               if MDI.Docks (None) = null
                 or else Page_Num (MDI.Docks (None), Child) = -1
               then
                  Remove (MDI.Layout, Child);
               else
                  Remove_From_Notebook (Child, None);
               end if;

            when Iconified =>
               null;

            when Floating =>
               null;
         end case;
      end if;

      Create_Notebook (MDI, Side);
      Gtk_New (Label, Child.Title.all);
      Append_Page (MDI.Docks (Side), Child, Label);
      Unref (Child);

      Set_Show_Tabs
        (MDI.Docks (Side), Get_Nth_Page (MDI.Docks (Side), 1) /= null);

      Set_Page (MDI.Docks (Side), -1);

      if Side = None then
         Child.State := Normal;
      else
         Child.State := Docked;
      end if;
      Queue_Resize (MDI);
      Set_Sensitive (Child.Minimize_Button, False);
   end Put_In_Notebook;

   --------------------------
   -- Remove_From_Notebook --
   --------------------------

   procedure Remove_From_Notebook
     (Child : access MDI_Child_Record'Class; Side : Dock_Side)
   is
      Note : Gtk_Notebook := Child.MDI.Docks (Side);
   begin
      Remove_Page (Note, Page_Num (Note, Child));
      Unparent (Child);

      if Get_Nth_Page (Note, 0) = null then
         Destroy (Child.MDI.Docks (Side));
         Child.MDI.Docks (Side) := null;

         if Side /= None then
            Child.MDI.Docks_Size (Side) := 0;
         end if;
      else
         Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
      end if;

      Child.State := Normal;
      Set_Sensitive (Child.Minimize_Button, True);
   end Remove_From_Notebook;

   ----------------
   -- Dock_Child --
   ----------------

   procedure Dock_Child
     (Child : access MDI_Child_Record'Class;
      Dock  : Boolean := True)
   is
      MDI : MDI_Window := Child.MDI;
      Alloc : Gtk_Allocation;
   begin
      Show_All (Child);
      if Dock and then Child.Dock /= None then
         Float_Child (Child, False);
         Minimize_Child (Child, False);
         Put_In_Notebook (MDI, Child.Dock, Child);
         Set_Sensitive (Child.Maximize_Button, False);

      elsif not Dock and then Child.State = Docked then
         if MDI.Docks (None) /= null then
            Put_In_Notebook (MDI, None, Child);

         else
            Ref (Child);
            Remove_From_Notebook (Child, Child.Dock);
            Alloc := (Child.X, Child.Y, Child.Uniconified_Width,
                      Child.Uniconified_Height);
            Put (MDI.Layout, Child, 0, 0);
            Size_Allocate (Child, Alloc);
            Unref (Child);
         end if;
         Child.State := Normal;

         Set_Sensitive (Child.Maximize_Button, True);
      end if;

      Activate_Child (Child);
      Update_Dock_Menu (Child);
   end Dock_Child;

   -------------------
   -- Set_Dock_Side --
   -------------------

   procedure Set_Dock_Side
     (Child : access MDI_Child_Record'Class; Side : Dock_Side)
   is
   begin
      if Child.State = Docked then
         Put_In_Notebook (Child.MDI, Side, Child);
      end if;

      Child.Dock := Side;
      Update_Dock_Menu (Child);
   end Set_Dock_Side;

   --------------------
   -- Minimize_Child --
   --------------------

   procedure Minimize_Child
     (Child : access MDI_Child_Record'Class; Minimize : Boolean)
   is
      use type Widget_List.Glist;
      MDI         : MDI_Window := Child.MDI;
      Icon_Height : constant Gint := Title_Bar_Height + 2 * Border_Thickness;
      List        : Widget_List.Glist;
      C2          : MDI_Child;
      Alloc       : Gtk_Allocation;

   begin
      --  Items can't be iconified if they are maximized

      if Child.State /= Iconified and then Minimize then
         Float_Child (Child, False);
         Dock_Child (Child, False);
         Child.Uniconified_X := Child.X;
         Child.Uniconified_Y := Child.Y;
         Child.State := Iconified;

         List := First (MDI.Items);
         Child.X := 0;
         Child.Y := (Gint (Get_Allocation_Height (MDI.Layout)) / Icon_Height)
           * Icon_Height;

         --  Find the best placement for the icon
         while List /= Null_List loop
            C2 := MDI_Child (Get_Data (List));

            if C2 /= MDI_Child (Child) and then C2.State = Iconified then
               if C2.Y mod Icon_Height = 0
                 and then C2.Y <= Child.Y
               then
                  if C2.X + Icons_Width >=
                    Gint (Get_Allocation_Width (MDI.Layout))
                  then
                     Child.X := 0;
                     Child.Y := C2.Y - Icon_Height;
                  elsif C2.X + Icons_Width > Child.X then
                     Child.X := C2.X + Icons_Width;
                     Child.Y := C2.Y;
                  end if;
               end if;
            end if;

            List := Next (List);
         end loop;

         Alloc := (Child.X, Child.Y, Icons_Width, Icon_Height);
         Size_Allocate (Child, Alloc);
         Set_Sensitive (Child.Maximize_Button, False);

      elsif Child.State = Iconified and then not Minimize then
         Child.X := Child.Uniconified_X;
         Child.Y := Child.Uniconified_Y;
         Alloc := (Child.Uniconified_X, Child.Uniconified_Y,
                   Child.Uniconified_Width, Child.Uniconified_Height);
         Size_Allocate (Child, Alloc);
         Child.State := Normal;
         Set_Sensitive (Child.Maximize_Button, True);
      end if;

      Activate_Child (Child);
   end Minimize_Child;

   -----------------------
   -- Maximize_Children --
   -----------------------

   procedure Maximize_Children
     (MDI : access MDI_Window_Record; Maximize : Boolean := True)
   is
      use Widget_List;
      List : Widget_List.Glist := First (MDI.Items);
      C    : MDI_Child;
      Alloc : Gtk_Allocation;

   begin
      if Maximize and then MDI.Docks (None) = null then
         while List /= Null_List loop
            C := MDI_Child (Get_Data (List));
            List := Next (List);

            if C.State = Normal or else C.State = Iconified then
               Put_In_Notebook (MDI, None, C);
            end if;
         end loop;

         Hide (MDI.Layout);

         if MDI.Focus_Child /= null then
            Raise_Child (MDI.Focus_Child);
         end if;

      elsif MDI.Docks (None) /= null then
         while List /= Null_List loop
            C := MDI_Child (Get_Data (List));
            List := Next (List);

            if C.State = Normal then
               --  Remove from middle notebook and put in layout
               Ref (C);
               Remove_From_Notebook (C, None);
               Put (MDI.Layout, C, 0, 0);
               Alloc := (C.X, C.Y, C.Uniconified_Width,
                         C.Uniconified_Height);
               Size_Allocate (C, Alloc);
               Unref (C);
            end if;
         end loop;

         --  The middle notebook was already destroyed by the last call to
         --  Remove_From_Notebook in the above loop
         Show (MDI.Layout);
      end if;
      Queue_Resize (MDI);
   end Maximize_Children;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Child : access MDI_Child_Record) return Gtk_Widget is
   begin
      return Gtk_Widget (Child.Initial_Child);
   end Get_Widget;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window (Child : access MDI_Child_Record) return Gtk_Window is
   begin
      if Child.Initial.all in Gtk_Window_Record'Class then
         return Gtk_Window (Child.Initial);
      else
         return null;
      end if;
   end Get_Window;

   ---------------------
   -- Get_Focus_Child --
   ---------------------

   function Get_Focus_Child
     (MDI : access MDI_Window_Record) return MDI_Child is
   begin
      return MDI.Focus_Child;
   end Get_Focus_Child;

   ----------------
   -- Cascade_Cb --
   ----------------

   procedure Cascade_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Cascade_Children (MDI_Window (MDI));
   end Cascade_Cb;

   ---------------
   -- Tile_H_Cb --
   ---------------

   procedure Tile_H_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Horizontally (MDI_Window (MDI));
   end Tile_H_Cb;

   ---------------
   -- Tile_V_Cb --
   ---------------

   procedure Tile_V_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Tile_Vertically (MDI_Window (MDI));
   end Tile_V_Cb;

   -----------------
   -- Maximize_Cb --
   -----------------

   procedure Maximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), True);
   end Maximize_Cb;

   -----------------------
   -- Maximize_Child_Cb --
   -----------------------

   procedure Maximize_Child_Cb (Child : access Gtk_Widget_Record'Class) is
      M : MDI_Window := MDI_Child (Child).MDI;
   begin
      Maximize_Children (M, M.Docks (None) = null);
   end Maximize_Child_Cb;

   -------------------
   -- Unmaximize_Cb --
   -------------------

   procedure Unmaximize_Cb (MDI : access Gtk_Widget_Record'Class) is
   begin
      Maximize_Children (MDI_Window (MDI), False);
   end Unmaximize_Cb;

   -------------
   -- Dock_Cb --
   -------------

   procedure Dock_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Dock_Child (C, C.State /= Docked);
      end if;
   end Dock_Cb;

   --------------
   -- Float_Cb --
   --------------

   procedure Float_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := Get_Focus_Child (MDI_Window (MDI));
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Float_Child (C, C.State /= Floating);
      end if;
   end Float_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Close_Cb (MDI : access Gtk_Widget_Record'Class) is
      C : MDI_Child;
   begin
      if MDI.all in MDI_Window_Record'Class then
         C := MDI_Window (MDI).Focus_Child;
         --  Close automatically gets the contents of docks, instead of the
         --  dock itself
      else
         C := MDI_Child (MDI);
      end if;

      if C /= null then
         Close_Child (C);
      end if;
   end Close_Cb;

   --------------
   -- Close_Cb --
   --------------

   procedure Focus_Cb (Child : access Gtk_Widget_Record'Class) is
      C : MDI_Child := MDI_Child (Child);
   begin
      if Get_Active (C.Menu_Item) then
         Activate_Child (C);
      end if;
   end Focus_Cb;

   --------------------------
   -- Menu_Entry_Destroyed --
   --------------------------

   procedure Menu_Entry_Destroyed (Child : access Gtk_Widget_Record'Class) is
   begin
      MDI_Child (Child).Menu_Item := null;
   end Menu_Entry_Destroyed;

   -----------------------
   -- Create_Menu_Entry --
   -----------------------

   procedure Create_Menu_Entry (Child : access MDI_Child_Record'Class) is
      G : Widget_SList.GSlist;
   begin
      if Child.Menu_Item = null then
         G := Child.MDI.Menu_Item_Group;
         Gtk_New (Child.Menu_Item, G, Child.Title.all);
         Append (Child.MDI.Menu, Child.Menu_Item);
         Set_Active
           (Child.Menu_Item, MDI_Child (Child) = Child.MDI.Focus_Child);
         Show_All (Child.Menu_Item);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "activate",
            Widget_Callback.To_Marshaller (Focus_Cb'Access), Child,
            After => True);
         Widget_Callback.Object_Connect
           (Child.Menu_Item, "destroy",
            Widget_Callback.To_Marshaller (Menu_Entry_Destroyed'Access),
            Child);
         Child.MDI.Menu_Item_Group := Group (Child.Menu_Item);
      end if;
   end Create_Menu_Entry;

   --------------------
   -- Menu_Destroyed --
   --------------------

   procedure Menu_Destroyed (MDI : access Gtk_Widget_Record'Class) is
   begin
      MDI_Window (MDI).Menu := null;
      MDI_Window (MDI).Dock_Menu_Item := null;
      MDI_Window (MDI).Float_Menu_Item := null;
      MDI_Window (MDI).Menu_Item_Group := Widget_SList.Null_List;
   end Menu_Destroyed;

   -----------------
   -- Create_Menu --
   -----------------

   function Create_Menu
     (MDI : access MDI_Window_Record) return Gtk.Menu.Gtk_Menu
   is
      Item  : Gtk_Menu_Item;
      Child : MDI_Child;

   begin
      if MDI.Menu = null then
         Gtk_New (MDI.Menu);

         Gtk_New (Item, "Cascade");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Cascade_Cb'Access), MDI);

         Gtk_New (Item, "Tile Horizontally");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_H_Cb'Access), MDI);

         Gtk_New (Item, "Tile Vertically");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Tile_V_Cb'Access), MDI);

         Gtk_New (Item, "Maximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Maximize_Cb'Access), MDI);

         Gtk_New (Item, "Unmaximize All");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Unmaximize_Cb'Access), MDI);

         Gtk_New (Item, "Arrange Icons");
         Append (MDI.Menu, Item);
         Set_Sensitive (Item, False);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (MDI.Dock_Menu_Item, "Docked");
         Append (MDI.Menu, MDI.Dock_Menu_Item);
         Set_Active (MDI.Dock_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Docked);
         MDI.Dock_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Dock_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Dock_Cb'Access), MDI);

         Gtk_New (MDI.Float_Menu_Item, "Floating");
         Append (MDI.Menu, MDI.Float_Menu_Item);
         Set_Active (MDI.Float_Menu_Item,
                     MDI.Focus_Child /= null
                     and then MDI.Focus_Child.State = Floating);
         MDI.Float_Menu_Item_Id := Widget_Callback.Object_Connect
           (MDI.Float_Menu_Item, "toggled",
            Widget_Callback.To_Marshaller (Float_Cb'Access), MDI);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         Gtk_New (Item, "Close");
         Append (MDI.Menu, Item);
         Widget_Callback.Object_Connect
           (Item, "activate",
            Widget_Callback.To_Marshaller (Close_Cb'Access), MDI);

         Gtk_New (Item);
         Append (MDI.Menu, Item);

         declare
            Tmp : Widget_List.Glist := First (MDI.Items);
         begin
            while Tmp /= Null_List loop
               Child := MDI_Child (Get_Data (Tmp));
               Create_Menu_Entry (Child);
               Tmp := Next (Tmp);
            end loop;
         end;

         Widget_Callback.Object_Connect
           (MDI.Menu, "destroy",
            Widget_Callback.To_Marshaller (Menu_Destroyed'Access), MDI);
      end if;

      Show_All (MDI.Menu);
      return MDI.Menu;
   end Create_Menu;

   -----------------------
   -- Create_Child_Menu --
   -----------------------

   function Create_Child_Menu
     (Child : access MDI_Child_Record'Class) return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Item  : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;

   begin
      Gtk_New (Menu);

      Gtk_New (Item, "Cascade");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Horizontally");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Tile Vertically");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Maximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Unmaximize All");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item, "Arrange Icons");
      Append (Menu, Item);
      Set_Sensitive (Item, False);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Check, "Docked");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Docked);
      Set_Sensitive (Check, Child.Dock /= None);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Dock_Cb'Access), Child);

      Gtk_New (Check, "Floating");
      Append (Menu, Check);
      Set_Active (Check, Child.State = Floating);
      Widget_Callback.Object_Connect
        (Check, "toggled",
         Widget_Callback.To_Marshaller (Float_Cb'Access), Child);

      Gtk_New (Item);
      Append (Menu, Item);

      Gtk_New (Item, "Close");
      Append (Menu, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Close_Cb'Access), Child);

      Show_All (Menu);
      return Menu;
   end Create_Child_Menu;

   --------------------
   -- Set_Priorities --
   --------------------

   procedure Set_Priorities
     (MDI : access MDI_Window_Record; Prio : Priorities_Array) is
   begin
      MDI.Priorities := Prio;
   end Set_Priorities;

end Gtkada.MDI;
