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

with Gtkada.Canvas;   use Gtkada.Canvas;
with Gtk.Handlers;    use Gtk.Handlers;
with GVD.Preferences; use GVD.Preferences;
with Gdk.Window;      use Gdk.Window;
with Gdk.Color;       use Gdk.Color;
with Gdk.Font;        use Gdk.Font;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.GC;          use Gdk.GC;
with Gdk.Bitmap;      use Gdk.Bitmap;
with Gtk.Widget;      use Gtk.Widget;
with Items;           use Items;
with GVD.Pixmaps;      use GVD.Pixmaps;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;
with Display_Items;   use Display_Items;
with Gdk;             use Gdk;

package body GVD.Canvas is

   package Canvas_Handler is new Gtk.Handlers.Callback
     (GVD_Canvas_Record);

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
   begin
      Canvas := new GVD_Canvas_Record;
      Canvas.Detect_Aliases := Get_Pref (Default_Detect_Aliases);
      Initialize (Canvas);
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

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Canvas : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      function Refresh_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Recompute the size of the item and redisplay it on the canvas

      ------------------
      -- Refresh_Item --
      ------------------

      function Refresh_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         Update_Resize_Display
           (Display_Item (Item), True, Get_Pref (Hide_Big_Items),
            Redisplay_Canvas => False);
         return True;
      end Refresh_Item;

      C   : GVD_Canvas := GVD_Canvas (Canvas);
      Win : Gdk.Window.Gdk_Window := Get_Window (C);

   begin
      Set_Detect_Aliases (C, Get_Pref (Default_Detect_Aliases));
      Align_On_Grid (C, Get_Pref (Align_Items_On_Grid));

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

      if C.Item_Context.Xref_GC /= null then
         Destroy (C.Item_Context.Xref_GC);
      end if;

      Gdk_New (C.Item_Context.Xref_GC, Win);
      Set_Foreground (C.Item_Context.Xref_GC, Get_Pref (Xref_Color));

      if C.Item_Context.Modified_GC /= null then
         Destroy (C.Item_Context.Modified_GC);
      end if;

      Gdk_New (C.Item_Context.Modified_GC, Win);
      Set_Foreground (C.Item_Context.Modified_GC, Get_Pref (Change_Color));

      if C.Item_Context.Font /= null then
         Unref (C.Item_Context.Font);
      end if;

      C.Item_Context.Font :=
        Get_Gdkfont (Get_Pref (Value_Font), Get_Pref (Value_Font_Size));

      if C.Item_Context.Type_Font /= null then
         Unref (C.Item_Context.Type_Font);
      end if;

      C.Item_Context.Type_Font :=
        Get_Gdkfont (Get_Pref (Type_Font), Get_Pref (Type_Font_Size));

      if C.Item_Context.Command_Font /= null then
         Unref (C.Item_Context.Command_Font);
      end if;

      C.Item_Context.Command_Font := Get_Gdkfont
        (Get_Pref (Command_Font), Get_Pref (Value_Font_Size));

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

      if C.Box_Context.Title_Font /= null then
         Unref (C.Box_Context.Title_Font);
      end if;

      C.Box_Context.Title_Font := Get_Gdkfont
        (Get_Pref (Title_Font), Get_Pref (Title_Font_Size));
      For_Each_Item (C, Refresh_Item'Unrestricted_Access);
      Refresh_Canvas (C);
   end Preferences_Changed;

   ----------------------
   -- Get_Item_Context --
   ----------------------

   function Get_Item_Context
     (Canvas : access GVD_Canvas_Record'Class) return Items.Drawing_Context is
   begin
      return Canvas.Item_Context;
   end Get_Item_Context;

   ---------------------
   -- Get_Box_Context --
   ---------------------

   function Get_Box_Context
     (Canvas : access GVD_Canvas_Record'Class) return Box_Drawing_Context is
   begin
      return Canvas.Box_Context;
   end Get_Box_Context;

end GVD.Canvas;
