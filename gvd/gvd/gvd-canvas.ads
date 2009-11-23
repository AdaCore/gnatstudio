-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2008, AdaCore             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Display_Items;
with Gdk.GC;
with Gdk.Bitmap;
with Gdk.Pixmap;
with Gtkada.Canvas;
with GPS.Kernel;
with GVD.Process;
with Items;

package GVD.Canvas is

   procedure Attach_To_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach debugger to a data window.
   --  If an unattached data window exists in the desktop, it is reused.
   --  If none exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a data window.
   --
   --  The debugger console should be created already. When it is closed (ie
   --  the debugger exits), the data window will be destroyed

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the data windows

   procedure Refresh_Data_Window
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class);
   --  Refresh the contents of the data window (if any) associated with
   --  Debugger

   procedure Process_Graph_Cmd
     (Process : access GVD.Process.Visual_Debugger_Record'Class;
      Cmd     : String);
   --  Parse and process a "graph print" or "graph display" command

   -------------------
   -- Items support --
   -------------------
   --  The following types and subprograms are used when drawing items in the
   --  canvas.

   type Box_Drawing_Context is record
      Grey_GC           : Gdk.GC.Gdk_GC;
      Black_GC          : Gdk.GC.Gdk_GC;
      Refresh_Button_GC : Gdk.GC.Gdk_GC;
      Thaw_Bg_GC        : Gdk.GC.Gdk_GC;
      Freeze_Bg_GC      : Gdk.GC.Gdk_GC;

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
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class)
      return Items.Drawing_Context;
   --  Return the drawing context associated with the items on the canvas.

   function Get_Box_Context
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class)
      return Box_Drawing_Context;
   --  Return the drawing context associated with the box around each
   --  item on the canvas.

   function Get_Detect_Aliases
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Boolean;
   --  Return True if aliases detection has been activated.

   function Get_Canvas
     (Process : access GVD.Process.Visual_Debugger_Record'Class)
      return Gtkada.Canvas.Interactive_Canvas;
   --  Return the canvas on which the drawing is done

   procedure Select_Item
     (Process   : access GVD.Process.Visual_Debugger_Record'Class;
      Item      : access Display_Items.Display_Item_Record'Class;
      Component : Items.Generic_Type_Access);
   --  Select a specific Component in Item, after unselecting the current
   --  selection.
   --  If Component is null, no new selection is made, but the current one is
   --  released.

   procedure Unselect
     (Process : access GVD.Process.Visual_Debugger_Record'Class;
      Item    : access Display_Items.Display_Item_Record'Class);
   --  Unselect Item if it is currently selected

end GVD.Canvas;
