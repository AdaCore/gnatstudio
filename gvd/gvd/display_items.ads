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

with Glib;
with Gtk.Widget;
with Gdk.Window;
with Items;
with GVD.Process;
with Gdk.Event;
with GVD.Types;
with Gtkada.Canvas;
with Language;
with Gdk.Pixmap;

package Display_Items is

   type Display_Item_Record is new
     Gtkada.Canvas.Canvas_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   type GVD_Link_Record is new Gtkada.Canvas.Canvas_Link_Record with private;
   type GVD_Link is access all GVD_Link_Record'Class;

   procedure Gtk_New
     (Item           : out Display_Item;
      Win            : Gdk.Window.Gdk_Window;
      Variable_Name  : String;
      Debugger       : access GVD.Process.Debugger_Process_Tab_Record'CLass;
      Auto_Refresh   : Boolean := True;
      Default_Entity : Items.Generic_Type_Access := null);
   --  Create a new item to display the value of Variable_Name.
   --  Auto_Refresh should be set to True if the value of Variable should
   --  be parsed again whenever the debugger stops. This is the default
   --  behavior, that can be changed by the user.
   --
   --  If Variable_Name is "", then no parsing is done to get the type and
   --  or value of the variable.
   --  Default_Entity can be used to initialize the entity associated with the
   --  item. This will be used instead of Variable_Name if not null.

   procedure Gtk_New_And_Put
     (Item           : out Display_Item;
      Win            : Gdk.Window.Gdk_Window;
      Variable_Name  : String;
      Debugger       : access GVD.Process.Debugger_Process_Tab_Record'Class;
      Auto_Refresh   : Boolean := True;
      Link_From      : access Display_Item_Record'Class;
      Link_Name      : String := "";
      Default_Entity : Items.Generic_Type_Access := null);
   --  Same as above, but also create a link from Link_From to the new item.
   --  Link_Name is the label used for the link between the two items.

   procedure Free
     (Item : access Display_Item_Record;
      Remove_Aliases : Boolean := True);
   --  Remove the item from the canvas and free the memory occupied by it,
   --  including its type description.
   --  If Remove_Aliases is True, then all the items on the canvas that are
   --  aliases of Item are also removed.

   function Find_Item
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Num    : Integer) return Display_Item;
   --  Return the item whose identifier is Num, or null if there is none

   procedure On_Button_Click
     (Item   : access Display_Item_Record;
      Event  : Gdk.Event.Gdk_Event_Button);
   --  React to button clicks on an item.

   procedure Set_Auto_Refresh
     (Item          : access Display_Item_Record;
      Win           : Gdk.Window.Gdk_Window;
      Auto_Refresh  : Boolean;
      Update_Value  : Boolean := False);
   --  Change the auto refresh status of the item, and update its pixmap.
   --  If Update_Value is True, then the value of the item is recomputed
   --  if necessary.

   function Get_Auto_Refresh
     (Item : access Display_Item_Record) return Boolean;
   --  Return Item.Auto_Refresh;

   procedure On_Background_Click
     (The_Canvas : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event      : Gdk.Event.Gdk_Event);
   --  Called for clicks in the background of the canvas.

   procedure On_Canvas_Process_Stopped
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when the process associated with the Debugger_Process_Tab Object
   --  stops to update the display items.

   procedure Recompute_All_Aliases
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Recompute_Values : Boolean := True);
   --  Recompute all the aliases, and reparse the values for all the
   --  displayed items if Recompute_Values is True

   function Update_On_Auto_Refresh
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Item   : access Gtkada.Canvas.Canvas_Item_Record'Class) return Boolean;
   --  Update the value of a specific item in the canvas. The new value is
   --  read from the debugger, parsed, and redisplayed.
   --  Do nothing if the auto-refresh status of Item is set to false.
   --  The general prototype for this function must be compatible with
   --  Gtkada.Canvas.Item_Processor.
   --  This does not redraw the canvas or the item on the canvas.

   procedure Update
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Item   : access Display_Item_Record'Class;
      Redisplay_Canvas : Boolean := False);
   --  Unconditionally update the value of Item after parsing the new value.
   --  This does not redraw the canvas or the item on the canvas, unless
   --  Redisplay_Canvas is True

   procedure Reset_Recursive (Item : access Display_Item_Record'Class);
   --  Calls Reset_Recursive for the entity represented by the item.

   function Is_Alias_Of
     (Item : access Display_Item_Record) return Display_Item;
   --  Return the item for which Item is an alias, or null if there is none.

   function Get_Name (Item : access Display_Item_Record) return String;
   --  Return the name of Item, or "" if there is no such name.

   function Get_Num (Item : access Display_Item_Record) return Integer;
   --  Return the number of Item.

   function Get_Debugger
     (Item : access Display_Item_Record'Class)
      return GVD.Process.Debugger_Process_Tab;
   --  Return the process tab to which item belongs

   function Is_A_Variable
     (Item : access Display_Item_Record'Class) return Boolean;
   --  Return True if the item is a variable.

   procedure Set_Display_Mode
     (Item : access Display_Item_Record'Class;
      Mode : Items.Display_Mode);
   --  Set the display mode for the item and all its children

   function Get_Display_Mode
     (Item : access Display_Item_Record) return Items.Display_Mode;
   --  Get the display mode for item

   procedure Update_Resize_Display
     (Item             : access Display_Item_Record'Class;
      Was_Visible      : Boolean := False;
      Hide_Big         : Boolean := False;
      Redisplay_Canvas : Boolean := True);
   --  Recompute the size and update the contents of item.
   --  Was_Visible indicates whether the item was initially visible
   --  It also warns the canvas that the item has changed.
   --  If Hide_Big_Items, then components higher than a specific limit are
   --  forced to hidden state.

   function Create_Drawing_Context
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mode   : Items.Display_Mode := Items.Value;
      Lang   : Language.Language_Access := null) return Items.Drawing_Context;
   --  Return a graphic context that can be used to display an item.
   --  The item will be drawn on Pixmap. Mode indicates which information
   --  should be displayed, and Lang is provided to get type information
   --  when needed (it can null if only the Value is displayed).

private

   type Display_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with record
      Num          : Integer;
      Name         : GVD.Types.String_Access := null;
      Entity       : Items.Generic_Type_Access := null;
      Auto_Refresh : Boolean := True;
      Debugger     : GVD.Process.Debugger_Process_Tab;

      Is_A_Variable : Boolean := True;
      --  Set to False if the item is not related to a variable

      Title_Height : Glib.Gint;

      Id           : GVD.Types.String_Access := null;
      --  Uniq ID used for the variable.
      --  This Id is returned by the debugger, and can be the address of a
      --  variable (in Ada or C), or simply the name of the variable (in
      --  Java) when no overloading exists and addresses don't have any
      --  meaning. This is used to detect aliases.

      Is_Alias_Of  : Display_Item := null;
      --  Item for which we are an alias.

      Is_Dereference : Boolean := False;
      --  True if the item was created as a result of a derefence of an
      --  access type. Such items can be hidden as a result of aliases
      --  detection, whereas items explicitly displayed by the user are
      --  never hidden.

      Was_Alias      : Boolean := False;
      --  Memorize whether the item was an alias in the previous display, so
      --  that we can compute a new position for it.

      Mode           : Items.Display_Mode := Items.Value;
      --  Whether we should display the mode itself.
   end record;

   type GVD_Link_Record is new Gtkada.Canvas.Canvas_Link_Record with record
      Alias_Link : Boolean := False;
      --  True if this Link was created as a result of an aliasing operation.
      --  Such links are always deleted before each update, and recreated
      --  whenever an aliasing is detected.

      Source_Component : Items.Generic_Type_Access := null;
      --  Component of the source item to which the link is attached (generally
      --  the dereferenced component itself. This is used when the links are
      --  attached to the components themselves instead of the item).
      --  If left to null, the link will be attached to the center of the
      --  source item.
   end record;

end Display_Items;
