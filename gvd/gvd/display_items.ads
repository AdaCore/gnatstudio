-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Glib;
with Gtk.Widget;
with Gtkada.Canvas;
with Gdk.Window;
with Generic_Values;
with Odd.Process;
with Gdk.Event;
with Odd.Types;

package Display_Items is

   type Display_Item_Record is new
     Gtkada.Canvas.Canvas_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Gtk_New
     (Item          : out Display_Item;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Debugger      : Odd.Process.Debugger_Process_Tab;
      Auto_Refresh  : Boolean := True);
   --  Create a new item to display the value of Variable_Name.
   --  Auto_Refresh should be set to True if the value of Variable should
   --  be parsed again whenever the debugger stops. This is the default
   --  behavior, that can be changed by the user.

   procedure Free (Item : access Display_Item_Record);
   --  Remove the item from the canvas and free the memory occupied by it,
   --  including its type description.

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Event  : Gdk.Event.Gdk_Event_Button);

   procedure Set_Auto_Refresh
     (Item          : access Display_Item_Record;
      Win           : Gdk.Window.Gdk_Window;
      Auto_Refresh  : Boolean);
   --  Change the auto refresh status of the item, and update its pixmap.

   procedure On_Background_Click
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Event  : Gdk.Event.Gdk_Event);
   --  Called for clicks in the background of the canvas.

   procedure On_Canvas_Process_Stopped
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when the process associated with the Debugger_Process_Tab Object
   --  stops to update the display items.

   function Update_On_Auto_Refresh
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Item   : access Gtkada.Canvas.Canvas_Item_Record'Class) return Boolean;
   --  Update the value of a specific item in the canvas. The new value is
   --  read from the debugger, parsed, and redisplayed.
   --  Do nothing if the auto-refresh status of Item is set to false.
   --  The general prototype for this function must be compatible with
   --  Gtkada.Canvas.Item_Processor.

   procedure Update
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Item   : access Display_Item_Record'Class);
   --  Unconditionally update the value of Item after parsing the new value.

private
   type Display_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with
      record
         Name         : Odd.Types.String_Access := null;
         Entity       : Generic_Values.Generic_Type_Access := null;
         Auto_Refresh : Boolean := True;
         Debugger     : Odd.Process.Debugger_Process_Tab;

         Title_Height : Glib.Gint;

         Id           : Odd.Types.String_Access := null;
         --  Uniq ID used for the variable.
         --  This Id is returned by the debugger, and can be the address of a
         --  variable (in Ada or C), or simply the name of the variable (in
         --  Java) when no overloading exists and addresses don't have any
         --  meaning.  This is used to detect aliases.
      end record;
end Display_Items;
