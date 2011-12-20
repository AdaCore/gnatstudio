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

with Gdk.Color;
with Gdk.Pixbuf;

with Gtkada.Canvas;

with GPS.Kernel;
with GVD.Process;

with Display_Items;
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
      Black_Color          : Gdk.Color.Gdk_Color;
      Refresh_Button_Color : Gdk.Color.Gdk_Color;
      Grey_Color           : Gdk.Color.Gdk_Color;
      Thaw_Bg_Color        : Gdk.Color.Gdk_Color;
      Freeze_Bg_Color      : Gdk.Color.Gdk_Color;

      Close_Pixmap        : Gdk.Pixbuf.Gdk_Pixbuf;
      Locked_Pixmap       : Gdk.Pixbuf.Gdk_Pixbuf;
      Auto_Display_Pixmap : Gdk.Pixbuf.Gdk_Pixbuf;
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
