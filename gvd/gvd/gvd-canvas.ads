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

with Gtkada.Canvas;
with Gdk.GC;
with Gdk.Font;
with Gdk.Pixmap;
with Gdk.Bitmap;
with Gtk.Menu;
with Gtk.Widget;
with Gtk.Window;
with Gtk.Accel_Group;
with Display_Items;
with Items;

package GVD.Canvas is

   ----------------
   -- GVD_Canvas --
   ----------------

   type GVD_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record
     with private;
   type GVD_Canvas is access all GVD_Canvas_Record'Class;

   procedure Gtk_New
     (Canvas      : out GVD_Canvas;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group);
   --  Create a new canvas.

   procedure Init_Graphics (Canvas : access GVD_Canvas_Record'Class);
   --  Initializes all the internal graphic contexts needed for the canvas.
   --  The canvas should have been realized before calling this procedure.

   function Get_Detect_Aliases
     (Canvas : access GVD_Canvas_Record'Class) return Boolean;
   --  Return True if aliases detection has been activated.

   procedure Set_Detect_Aliases
     (Canvas   : access GVD_Canvas_Record'Class;
      Activate : Boolean);
   --  Change the status of aliases detection in the canvas

   function Get_Next_Item_Num
     (Canvas : access GVD_Canvas_Record'Class) return Integer;
   --  Return the number that should be used for the next item inserted into
   --  the canvas.
   --  Two successive calls to that function will not return the same value.

   procedure Set_Process
     (Canvas  : access GVD_Canvas_Record;
      Process : access Gtk.Window.Gtk_Window_Record'Class);
   --  Set the process associated with the canvas.

   function Get_Process (Canvas : access GVD_Canvas_Record)
     return Gtk.Window.Gtk_Window;
   --  Return the process tab that contains the canvas.

   procedure Preferences_Changed
     (Canvas : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when the preferences have changed, and the canvas should be
   --  redisplayed with the new setup.

   type Box_Drawing_Context is record
      Grey_GC           : Gdk.GC.Gdk_GC;
      Black_GC          : Gdk.GC.Gdk_GC;
      Refresh_Button_GC : Gdk.GC.Gdk_GC;
      Thaw_Bg_GC        : Gdk.GC.Gdk_GC;
      Freeze_Bg_GC      : Gdk.GC.Gdk_GC;

      Title_Font        : Gdk.Font.Gdk_Font;

      Close_Pixmap        : Gdk.Pixmap.Gdk_Pixmap;
      Close_Mask          : Gdk.Bitmap.Gdk_Bitmap;
      Locked_Pixmap       : Gdk.Pixmap.Gdk_Pixmap;
      Locked_Mask         : Gdk.Bitmap.Gdk_Bitmap;
      Auto_Display_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Auto_Display_Mask   : Gdk.Bitmap.Gdk_Bitmap;
   end record;
   --  Structure that holds all the required information to draw the boxes
   --  around each item in the canvas, including the title. Note that this
   --  doesn't include the necessary information to draw the contents of the
   --  item (see Items.Drawing_Context instead)

   function Get_Item_Context
     (Canvas : access GVD_Canvas_Record'Class) return Items.Drawing_Context;
   --  Return the drawing context associated with the items on the canvas.

   function Get_Tooltip_Context
     (Canvas : access GVD_Canvas_Record'Class) return Items.Drawing_Context;
   --  Return the drawing context associated with the tooltips. Fonts are
   --  not scaled depending on the zoom level.

   function Get_Box_Context
     (Canvas : access GVD_Canvas_Record'Class) return Box_Drawing_Context;
   --  Return the drawing context associated with the box around each
   --  item on the canvas.

   function Contextual_Background_Menu
     (Canvas      : access GVD_Canvas_Record;
      Accel_Group : Gtk.Accel_Group.Gtk_Accel_Group)
     return Gtk.Menu.Gtk_Menu;
   --  Get the contextual background menu associated with canvas.

   function Item_Contextual_Menu
     (Canvas         : access GVD.Canvas.GVD_Canvas_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String) return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when a specific
   --  component in an item is selected.
   --  Note that Component can be null if the user has clicked for instance
   --  on the title bar.

private

   type GVD_Canvas_Record is new Gtkada.Canvas.Interactive_Canvas_Record with
   record
      Detect_Aliases : Boolean;
      Item_Num       : Integer := 0;
      Process        : Gtk.Window.Gtk_Window;
      --  The process tab that contains the canvas

      --  The graphic contexts used to draw the canvas and its items
      Item_Context    : Items.Drawing_Context;
      Box_Context     : Box_Drawing_Context;
      Tooltip_Context : Items.Drawing_Context;

      Contextual_Background_Menu : Gtk.Menu.Gtk_Menu;
      Item_Contextual_Menu : Gtk.Menu.Gtk_Menu;
   end record;

end GVD.Canvas;
