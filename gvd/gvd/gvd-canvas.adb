-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;             use Glib;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtk.Handlers;     use Gtk.Handlers;
with GVD.Preferences;  use GVD.Preferences;
with Gdk;              use Gdk;
with Gdk.Window;       use Gdk.Window;
with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.GC;           use Gdk.GC;
with Gdk.Bitmap;       use Gdk.Bitmap;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Widget;       use Gtk.Widget;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Gtk.Accel_Group;  use Gtk.Accel_Group;
with Items;            use Items;
with Pixmaps_IDE;      use Pixmaps_IDE;
with GVD.Dialogs;      use GVD.Dialogs;
with GVD.Process;      use GVD.Process;
with GVD.Memory_View;  use GVD.Memory_View;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Display_Items;    use Display_Items;
with Odd_Intl;         use Odd_Intl;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtkada.Handlers;  use Gtkada.Handlers;
with Debugger;         use Debugger;

with GVD.Main_Window;  use GVD.Main_Window;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;

package body GVD.Canvas is

   -----------------
   -- Local Types --
   -----------------

   type Item_Record (Name_Length : Natural) is record
      Canvas         : GVD_Canvas;
      Item           : Display_Item;
      Component      : Items.Generic_Type_Access;
      Component_Name : String (1 .. Name_Length);
      Mode           : Display_Mode;

      Zoom           : Guint;
   end record;

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (15, 25, 50, 75, 100, 150, 200, 300, 400);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Steps : constant := 7;
   --  Number of steps while zooming in or out.

   --------------------
   -- Local Packages --
   --------------------

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, GVD_Canvas);

   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "align on grid" contextual menu item.

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "detect aliases" contextual menu item.

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the mode of a specific item to indicate whether the value of the
   --  item should be displayed

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Clone the item or its selected component.

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class);
   --  Popup a dialog to display any expression in the canvas

   procedure Zoom_In (Canvas : access Gtk_Widget_Record'Class);
   --  Zoom in to the previous zoom level, if any

   procedure Zoom_Out (Canvas : access Gtk_Widget_Record'Class);
   --  Zoom out to the next zoom level, if any

   procedure Zoom_Level
     (Canvas : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Zoom directly to a specific level (Item.Zoom)

   procedure Zoomed (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the Canvas has been zoomed. This redraws all the items

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item.

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Set the value for a specific component

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Show all the subcomponents of the selected item.

   procedure View_Into_Memory
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Bring up the memory view if needed, and view the memory at the address
   --  corresponding to Item.

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item.

   procedure Undisplay_Item
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item.

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Toggle between "auto_refresh" and "frozen" modes.

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class);
   --  Reallocate all the fonts, with the appropriate size given the current
   --  zoom

   function Refresh_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean;
   --  Recompute the size and redisplay the item. This function is meant to
   --  be used in a For_Each_Item loop

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas) is
   begin
      Align_On_Grid (Canvas, Get_Active (Item));
   end Change_Align_On_Grid;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas) is
   begin
      Set_Detect_Aliases (Canvas, not Get_Detect_Aliases (Canvas));

      --  Recompute all the aliases
      Recompute_All_Aliases (Canvas);

      Refresh_Canvas (Canvas);
   end Change_Detect_Aliases;

   ------------------------
   -- Display_Expression --
   ------------------------

   procedure Display_Expression (Canvas : access Gtk_Widget_Record'Class) is
      Process : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Get_Process (GVD_Canvas (Canvas)));
   begin
      On_Display_Expression1_Activate (Process.Window);
   end Display_Expression;

   ------------------------
   -- Get_Detect_Aliases --
   ------------------------

   function Get_Detect_Aliases
     (Canvas : access GVD_Canvas_Record'Class) return Boolean is
   begin
      return Canvas.Detect_Aliases;
   end Get_Detect_Aliases;

   ------------------------
   -- Set_Detect_Aliases --
   ------------------------

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean) is
   begin
      --  ??? We should modify the items displayed so as to remove currently
      --  detected aliases. This is part of the whole aliases detection
      --  implementation.
      Canvas.Detect_Aliases := Activate;
   end Set_Detect_Aliases;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Canvas : out GVD_Canvas) is
      Menu : Gtk_Menu;
   begin
      Canvas := new GVD_Canvas_Record;
      Canvas.Detect_Aliases := Get_Pref (Default_Detect_Aliases);
      Initialize (Canvas);

      Widget_Callback.Connect
        (Canvas, "zoomed", Widget_Callback.To_Marshaller (Zoomed'Access));

      --  Create the  background contextual menu now, so that the key shortcuts
      --  are activated
      Menu := Contextual_Background_Menu (Canvas);
   end Gtk_New;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics (Canvas : access GVD_Canvas_Record'Class) is
      Win : constant Gdk.Window.Gdk_Window := Get_Window (Canvas);
   begin
      pragma Assert (Win /= null);
      Create_From_Xpm_D
        (Canvas.Box_Context.Close_Pixmap, Win,
         Canvas.Box_Context.Close_Mask, Null_Color, cancel_xpm);
      Create_From_Xpm_D
        (Canvas.Box_Context.Locked_Pixmap, Win,
         Canvas.Box_Context.Locked_Mask, Null_Color, lock_xpm);
      Create_From_Xpm_D
        (Canvas.Box_Context.Auto_Display_Pixmap, Win,
         Canvas.Box_Context.Auto_Display_Mask, Null_Color, display_small_xpm);
      Create_From_Xpm_D
        (Canvas.Item_Context.Hidden_Pixmap, Win,
         Canvas.Item_Context.Hidden_Mask, Null_Color, box_xpm);
      Create_From_Xpm_D
        (Canvas.Item_Context.Unknown_Pixmap, Win,
         Canvas.Item_Context.Unknown_Mask, Null_Color, trash_xpm);

      GVD.Canvas.Preferences_Changed (Canvas);
   end Init_Graphics;

   -----------------------
   -- Get_Next_Item_Num --
   -----------------------

   function Get_Next_Item_Num
     (Canvas : access GVD_Canvas_Record'Class) return Integer is
   begin
      Canvas.Item_Num := Canvas.Item_Num + 1;
      return Canvas.Item_Num;
   end Get_Next_Item_Num;

   -----------------
   -- Set_Process --
   -----------------

   procedure Set_Process
     (Canvas  : access GVD_Canvas_Record;
      Process : access Gtk.Window.Gtk_Window_Record'Class) is
   begin
      Canvas.Process := Gtk.Window.Gtk_Window (Process);
   end Set_Process;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process (Canvas : access GVD_Canvas_Record)
      return Gtk.Window.Gtk_Window is
   begin
      return Canvas.Process;
   end Get_Process;

   ------------------
   -- Refresh_Item --
   ------------------

   function Refresh_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean
   is
      pragma Warnings (Off, Canvas);
   begin
      Update_Resize_Display
        (Display_Item (Item), True, Get_Pref (Hide_Big_Items),
         Redisplay_Canvas => False);
      return True;
   end Refresh_Item;

   --------------------
   -- Allocate_Fonts --
   --------------------

   procedure Allocate_Fonts (Canvas : access GVD_Canvas_Record'Class) is
      Size : Gint;
   begin
      ------------------
      -- Item_Context --
      ------------------

      if Canvas.Item_Context.Font /= null then
         Unref (Canvas.Item_Context.Font);
      end if;

      Size := To_Canvas_Coordinates (Canvas, Get_Pref (Value_Font_Size));
      Canvas.Item_Context.Font := Get_Gdkfont (Get_Pref (Value_Font), Size);

      if Canvas.Item_Context.Type_Font /= null then
         Unref (Canvas.Item_Context.Type_Font);
      end if;

      Size := To_Canvas_Coordinates (Canvas, Get_Pref (Type_Font_Size));
      Canvas.Item_Context.Type_Font :=
        Get_Gdkfont (Get_Pref (Type_Font), Size);

      if Canvas.Item_Context.Command_Font /= null then
         Unref (Canvas.Item_Context.Command_Font);
      end if;

      Size := To_Canvas_Coordinates (Canvas, Get_Pref (Value_Font_Size));
      Canvas.Item_Context.Command_Font :=
        Get_Gdkfont (Get_Pref (Command_Font), Size);

      ---------------------
      -- Tooltip_Context --
      ---------------------

      if Canvas.Tooltip_Context.Font /= null then
         Unref (Canvas.Tooltip_Context.Font);
      end if;
      Canvas.Tooltip_Context.Font :=
        Get_Gdkfont (Get_Pref (Value_Font), Get_Pref (Value_Font_Size));

      if Canvas.Tooltip_Context.Type_Font /= null then
         Unref (Canvas.Tooltip_Context.Type_Font);
      end if;
      Canvas.Tooltip_Context.Type_Font :=
        Get_Gdkfont (Get_Pref (Type_Font), Get_Pref (Type_Font_Size));

      if Canvas.Tooltip_Context.Command_Font /= null then
         Unref (Canvas.Tooltip_Context.Command_Font);
      end if;
      Canvas.Tooltip_Context.Command_Font := Get_Gdkfont
        (Get_Pref (Command_Font), Get_Pref (Value_Font_Size));

      -----------------
      -- Box_Context --
      -----------------

      if Canvas.Box_Context.Title_Font /= null then
         Unref (Canvas.Box_Context.Title_Font);
      end if;

      Size := To_Canvas_Coordinates (Canvas, Get_Pref (Title_Font_Size));
      Canvas.Box_Context.Title_Font :=
        Get_Gdkfont (Get_Pref (Title_Font), Size);

      For_Each_Item (Canvas, Refresh_Item'Unrestricted_Access);
   end Allocate_Fonts;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Canvas : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      C   : GVD_Canvas := GVD_Canvas (Canvas);
      Win : Gdk.Window.Gdk_Window;

   begin
      Realize (C);
      Win := Get_Window (C);
      Align_On_Grid (C, Get_Pref (Align_Items_On_Grid));
      Set_Detect_Aliases (C, Get_Pref (Default_Detect_Aliases));
      Recompute_All_Aliases (C);

      if Get_Pref (Display_Grid) then
         Configure (C, Grid_Size => Default_Grid_Size);
      else
         Configure (C, Grid_Size => 0);
      end if;

      --  The drawing context for the items

      if C.Item_Context.GC /= null then
         Destroy (C.Item_Context.GC);
      end if;

      Gdk_New (C.Item_Context.GC, Win);
      Set_Foreground (C.Item_Context.GC, Black (Get_Default_Colormap));
      C.Tooltip_Context.GC := C.Item_Context.GC;

      if C.Item_Context.Xref_GC /= null then
         Destroy (C.Item_Context.Xref_GC);
      end if;

      Gdk_New (C.Item_Context.Xref_GC, Win);
      Set_Foreground (C.Item_Context.Xref_GC, Get_Pref (Xref_Color));
      C.Tooltip_Context.Xref_GC := C.Item_Context.Xref_GC;

      if C.Item_Context.Modified_GC /= null then
         Destroy (C.Item_Context.Modified_GC);
      end if;

      Gdk_New (C.Item_Context.Modified_GC, Win);
      Set_Foreground (C.Item_Context.Modified_GC, Get_Pref (Change_Color));
      C.Tooltip_Context.Modified_GC := C.Item_Context.Modified_GC;

      if C.Item_Context.Selection_GC /= null then
         Destroy (C.Item_Context.Selection_GC);
      end if;

      Gdk_New (C.Item_Context.Selection_GC, Win);
      Set_Foreground (C.Item_Context.Selection_GC, Get_Pref (Selection_Color));
      C.Tooltip_Context.Selection_GC := C.Item_Context.Selection_GC;

      --  The drawing context for the boxes

      if C.Box_Context.Grey_GC /= null then
         Destroy (C.Box_Context.Grey_GC);
      end if;

      Gdk_New (C.Box_Context.Grey_GC, Win);
      Set_Foreground (C.Box_Context.Grey_GC, Get_Pref (Title_Color));

      if C.Box_Context.Black_GC /= null then
         Destroy (C.Box_Context.Black_GC);
      end if;

      Gdk_New (C.Box_Context.Black_GC, Win);
      Set_Foreground (C.Box_Context.Black_GC, Black (Get_Default_Colormap));

      if C.Box_Context.Refresh_Button_GC /= null then
         Destroy (C.Box_Context.Refresh_Button_GC);
      end if;

      Gdk_New (C.Box_Context.Refresh_Button_GC, Win);

      if C.Box_Context.Thaw_Bg_GC /= null then
         Destroy (C.Box_Context.Thaw_Bg_GC);
      end if;

      Gdk_New (C.Box_Context.Thaw_Bg_GC, Win);
      Set_Foreground (C.Box_Context.Thaw_Bg_GC, Get_Pref (Thaw_Bg_Color));

      if C.Box_Context.Freeze_Bg_GC /= null then
         Destroy (C.Box_Context.Freeze_Bg_GC);
      end if;

      Gdk_New (C.Box_Context.Freeze_Bg_GC, Win);
      Set_Foreground (C.Box_Context.Freeze_Bg_GC, Get_Pref (Freeze_Bg_Color));

      Allocate_Fonts (C);

      For_Each_Item (C, Refresh_Item'Unrestricted_Access);

      Refresh_Canvas (C);
   end Preferences_Changed;

   -------------------------
   -- Change_Display_Mode --
   -------------------------

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      if Get_Active (Gtk_Radio_Menu_Item (Widget))
        and then Get_Display_Mode (Item.Item) /= Item.Mode
      then
         Set_Display_Mode (Item.Item, Item.Mode);
      end if;
   end Change_Display_Mode;

   ---------------------
   -- Clone_Component --
   ---------------------

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      if Is_A_Variable (Item.Item) then
         Process_User_Command
           (Get_Debugger (Item.Item),
            "graph display " & Item.Component_Name,
            Output_Command => True);
      else
         Process_User_Command
           (Get_Debugger (Item.Item),
            "graph display `" & Get_Name (Item.Item) & "`",
            Output_Command => True);
      end if;
   end Clone_Component;

   --------------------------------
   -- Contextual_Background_Menu --
   --------------------------------

   function Contextual_Background_Menu
     (Canvas : access GVD_Canvas_Record) return Gtk_Menu
   is
      Check : Gtk_Check_Menu_Item;
      Mitem : Gtk_Menu_Item;
      Zooms_Menu : Gtk_Menu;

   begin
      if Canvas.Contextual_Background_Menu /= null then
         return Canvas.Contextual_Background_Menu;
      end if;

      Unlock (Gtk.Accel_Group.Get_Default);

      Gtk_New (Canvas.Contextual_Background_Menu);

      Gtk_New (Mitem, Label => -"Display Expression...");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Display_Expression'Access), Canvas);

      Gtk_New (Mitem);
      Append (Canvas.Contextual_Background_Menu, Mitem);

      Gtk_New (Check, Label => -"Align On Grid");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Align_On_Grid (Canvas));
      Append (Canvas.Contextual_Background_Menu, Check);
      Check_Canvas_Handler.Connect
        (Check, "activate",
         Check_Canvas_Handler.To_Marshaller (Change_Align_On_Grid'Access),
         GVD_Canvas (Canvas));

      Gtk_New (Check, Label => -"Detect Aliases");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Detect_Aliases (Canvas));
      Append (Canvas.Contextual_Background_Menu, Check);
      Check_Canvas_Handler.Connect
        (Check, "activate",
         Check_Canvas_Handler.To_Marshaller (Change_Detect_Aliases'Access),
         GVD_Canvas (Canvas));

      Gtk_New (Mitem);
      Append (Canvas.Contextual_Background_Menu, Mitem);

      Gtk_New (Mitem, Label => -"Zoom in");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_In'Access), Canvas);
      Add_Accelerator
        (Mitem, "activate",
         Gtk.Accel_Group.Get_Default, GDK_equal, 0, Accel_Visible);

      Gtk_New (Mitem, Label => -"Zoom out");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_Out'Access), Canvas);
      Add_Accelerator
        (Mitem, "activate",
         Gtk.Accel_Group.Get_Default, GDK_minus, 0, Accel_Visible);

      Gtk_New (Zooms_Menu);

      for J in Zoom_Levels'Range loop
         Gtk_New (Mitem, Label => Guint'Image (Zoom_Levels (J)) & '%');
         Append (Zooms_Menu, Mitem);
         Item_Handler.Connect
           (Mitem, "activate",
            Item_Handler.To_Marshaller (Zoom_Level'Access),
            (Name_Length    => 0,
             Canvas         => GVD_Canvas (Canvas),
             Item           => null,
             Component      => null,
             Component_Name => "",
             Mode           => Value,
             Zoom           => Zoom_Levels (J)));
      end loop;

      Gtk_New (Mitem, Label => -"Zoom");
      Append (Canvas.Contextual_Background_Menu, Mitem);
      Set_Submenu (Mitem, Zooms_Menu);

      Show_All (Canvas.Contextual_Background_Menu);

      Lock (Gtk.Accel_Group.Get_Default);
      return Canvas.Contextual_Background_Menu;
   end Contextual_Background_Menu;

   ----------------------
   -- Get_Item_Context --
   ----------------------

   function Get_Item_Context
     (Canvas : access GVD_Canvas_Record'Class) return Items.Drawing_Context is
   begin
      return Canvas.Item_Context;
   end Get_Item_Context;

   -------------------------
   -- Get_Tooltip_Context --
   -------------------------

   function Get_Tooltip_Context
     (Canvas : access GVD_Canvas_Record'Class) return Items.Drawing_Context is
   begin
      return Canvas.Tooltip_Context;
   end Get_Tooltip_Context;

   ---------------------
   -- Get_Box_Context --
   ---------------------

   function Get_Box_Context
     (Canvas : access GVD_Canvas_Record'Class) return Box_Drawing_Context is
   begin
      return Canvas.Box_Context;
   end Get_Box_Context;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      Set_Visibility (Item.Component, False, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Hide_All;

   --------------------------
   -- Item_Contextual_Menu --
   --------------------------

   function Item_Contextual_Menu
     (Canvas         : access GVD_Canvas_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String) return Gtk.Menu.Gtk_Menu
   is
      Mitem : Gtk_Menu_Item;
      Radio : Gtk_Radio_Menu_Item;
      Check : Gtk_Check_Menu_Item;

   begin
      if Canvas.Item_Contextual_Menu /= null then
         Destroy (Canvas.Item_Contextual_Menu);
      end if;

      Gtk_New (Canvas.Item_Contextual_Menu);

      --  Display "Close" option.

      Gtk_New (Mitem, Label => -"Close" & " " & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Undisplay_Item'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      --  Display a separator

      Gtk_New (Mitem);
      Append (Canvas.Item_Contextual_Menu, Mitem);

      Gtk_New (Mitem, Label => -"Hide all " & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Hide_All'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show all " & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Show_All'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      --  Display a separator

      Gtk_New (Mitem);
      Append (Canvas.Item_Contextual_Menu, Mitem);

      if Is_A_Variable (Item) then
         Gtk_New (Mitem, Label => -"Clone" & " " & Component_Name);
      else
         Gtk_New (Mitem, Label => -"Clone");
      end if;
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Clone_Component'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      Gtk_New (Mitem, Label => -"View memory at &" & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (View_Into_Memory'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      if Is_A_Variable (Item) then
         Gtk_New (Mitem, Label => -"Set Value of " & Component_Name);
      else
         Gtk_New (Mitem, Label => -"Set Value");
      end if;

      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Set_Value'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);
      Set_Sensitive (Mitem, Is_A_Variable (Item));

      Gtk_New (Mitem, Label => -"Update Value");
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Update_Variable'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Mitem);

      --  Display a separator
      Gtk_New (Mitem);
      Append (Canvas.Item_Contextual_Menu, Mitem);

      Gtk_New (Radio, Widget_SList.Null_List, -"Show Value");
      Set_Active (Radio, Get_Display_Mode (Item) = Value);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Type");
      Set_Active (Radio, Get_Display_Mode (Item) = Type_Only);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Type_Only,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Value + Type");
      Set_Active (Radio, Get_Display_Mode (Item) = Type_Value);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Type_Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      --  Display a separator

      Gtk_New (Mitem);
      Append (Canvas.Item_Contextual_Menu, Mitem);

      --  Display "Toggle auto-refresh" option.

      Gtk_New (Check, "Auto refresh");
      Set_Active (Check, Get_Auto_Refresh (Display_Item (Item)));
      Set_Show_Toggle (Check, True);

      Item_Handler.Connect
        (Check, "activate",
         Item_Handler.To_Marshaller (Toggle_Refresh_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value,
                      Zoom           => 100));
      Append (Canvas.Item_Contextual_Menu, Check);

      Show_All (Canvas.Item_Contextual_Menu);
      return Canvas.Item_Contextual_Menu;
   end Item_Contextual_Menu;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      pragma Warnings (Off, Widget);
      S : constant String :=
        Simple_Entry_Dialog
        (Parent   => Get_Debugger (Item.Item).Window,
         Title    => -"Setting value of " & Item.Component_Name,
         Message  => -"Setting value of " & Item.Component_Name & ':',
         Position => Win_Pos_Mouse,
         Key      => "gvd_set_value_dialog");

   begin
      if S /= "" and then S (S'First) /= ASCII.NUL then
         Set_Variable
           (Get_Debugger (Item.Item).Debugger, Item.Component_Name, S);
         Update_Variable (Widget, Item);
      end if;
   end Set_Value;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      Set_Visibility (Item.Component, True, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Show_All;

   ----------------------
   -- View_Into_Memory --
   ----------------------

   procedure View_Into_Memory
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
      Top  : constant GVD_Main_Window :=
        GVD_Main_Window (Debugger_Process_Tab (Item.Canvas.Process).Window);
      View : constant GVD_Memory_View := Top.Memory_View;

   begin
      if not Visible_Is_Set (View) then
         Show_All (View);
      end if;

      Display_Memory (View, Item.Component_Name);
      Gdk_Raise (Get_Window (View));
   end View_Into_Memory;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      Display_Items.Update
        (Item.Canvas, Item.Item, Redisplay_Canvas => True);
   end Update_Variable;

   --------------------
   -- Undisplay_Item --
   --------------------

   procedure Undisplay_Item
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      Process_User_Command
        (Get_Debugger (Item.Item),
         "graph undisplay" & Integer'Image (Get_Num (Item.Item)),
         Output_Command => True);
   end Undisplay_Item;

   -------------------------
   -- Toggle_Refresh_Mode --
   -------------------------

   procedure Toggle_Refresh_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      Set_Auto_Refresh
        (Item.Item,
         Get_Window (Item.Canvas),
         not Get_Auto_Refresh (Item.Item),
         True);
   end Toggle_Refresh_Mode;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In (Canvas : access Gtk_Widget_Record'Class) is
      Z : constant Guint := Get_Zoom (GVD_Canvas (Canvas));
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
               Zoom (GVD_Canvas (Canvas), Zoom_Levels (J + 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out (Canvas : access Gtk_Widget_Record'Class) is
      Z : constant Guint := Get_Zoom (GVD_Canvas (Canvas));
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
               Zoom (GVD_Canvas (Canvas), Zoom_Levels (J - 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_Out;

   ----------------
   -- Zoom_Level --
   ----------------

   procedure Zoom_Level
     (Canvas : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      Zoom (Item.Canvas, Item.Zoom, 1);
   end Zoom_Level;

   ------------
   -- Zoomed --
   ------------

   procedure Zoomed (Canvas : access Gtk_Widget_Record'Class) is
   begin
      Allocate_Fonts (GVD_Canvas (Canvas));
   end Zoomed;

end GVD.Canvas;
