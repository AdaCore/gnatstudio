-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2008, AdaCore              --
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

with GNAT.Strings;
with Browsers.Canvas;
with Glib;
with Items;
with GVD.Process;
with Gdk.Event;
with Gtkada.Canvas;
with Debugger;
with Language;

package Display_Items is

   type Display_Item_Record is new
     Browsers.Canvas.Browser_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   type GVD_Link_Record is new Browsers.Canvas.Browser_Link_Record
      with private;
   type GVD_Link is access all GVD_Link_Record'Class;

   Item_Name_In_Link : constant String := "@";
   --  Shortcut used to represent the dereferenced item when generating a new
   --  item.

   procedure Gtk_New
     (Item           : out Display_Item;
      Browser        : access Browsers.Canvas.General_Browser_Record'Class;
      Graph_Cmd      : String;
      Variable_Name  : String;
      Num            : Integer;
      Debugger       : access GVD.Process.Visual_Debugger_Record'Class;
      Auto_Refresh   : Boolean := True;
      Default_Entity : Items.Generic_Type_Access := null;
      Link_From      : Display_Item := null;
      Link_Name      : String := "");
   --  Create a new item to display the value of Variable_Name.
   --  Auto_Refresh should be set to True if the value of Variable should
   --  be parsed again whenever the debugger stops. This is the default
   --  behavior, that can be changed by the user.
   --
   --  Graph_Cmd is the "graph" command that was used to create the item. This
   --  is the command that is saved across GVD sessions so that we can restore
   --  the displayed variables the next time the debugger is started.
   --
   --  If Variable_Name is "", then no parsing is done to get the type and
   --  or value of the variable.
   --  Default_Entity can be used to initialize the entity associated with the
   --  item. This will be used instead of Variable_Name if not null.
   --
   --  If Link_From is not null, the new item is directly inserted in the
   --  canvas, and a link is created between the new item and Link_From.
   --  Link_Name is the label used for the link between the two items.
   --
   --  Debugger can be null. In this case, the item will never be computed
   --
   --  Num must be specified, and is the number of the item
   --
   --  If the item already existed in the canvas, Item is set to null

   procedure Free
     (Item : access Display_Item_Record;
      Remove_Aliases : Boolean := True);
   --  Remove the item from the canvas and free the memory occupied by it,
   --  including its type description.
   --  If Remove_Aliases is True, then all the items on the canvas that are
   --  aliases of Item are also removed.

   function Get_Graph_Cmd (Item : access Display_Item_record) return String;
   --  Return the "graph display..." command used to create the item

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
      Auto_Refresh  : Boolean;
      Update_Value  : Boolean := False);
   --  Change the auto refresh status of the item, and update its pixmap.
   --  If Update_Value is True, then the value of the item is recomputed
   --  if necessary.

   function Get_Auto_Refresh
     (Item : access Display_Item_Record) return Boolean;
   --  Return Item.Auto_Refresh;

   procedure Recompute_All_Aliases
     (Process          : access GVD.Process.Visual_Debugger_Record'Class;
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
     (Item    : access Display_Item_Record'Class;
      Redisplay_Canvas : Boolean := False);
   --  Unconditionally update the value of Item after parsing the new value.
   --  This does not redraw the canvas or the item on the canvas, unless
   --  Redisplay_Canvas is True

   function Get_Component
     (Item      : access Display_Item_Record;
      X, Y      : Glib.Gint;
      Component : access Items.Generic_Type_Access) return String;
   --  Get the component selected when clicking in (X, Y) in the item. This
   --  also returns the name of the component.

   procedure Update_Component
     (Item      : access Display_Item_Record'Class;
      Component : Items.Generic_Type_Access := null);
   --  Update a specific component of a complex item.
   --  The item must have been displayed at least once before the last time
   --  its visibility state changed.
   --  If Component is null, the whole item is redraw, otherwise only the
   --  specific Component is updated.

   procedure Dereference_Item
     (Item            : access Display_Item_Record;
      Deref_Component : Items.Generic_Type_Access;
      Component_Name  : String;
      Link_Name       : String);
   --  Dereference a component of Item ("graph display" on it with a link from
   --  the item). Link_Name is dereferenced per language rule (ie in Ada .all
   --  is added).

   procedure Reset_Recursive (Item : access Display_Item_Record'Class);
   --  Calls Reset_Recursive for the entity represented by the item.

   function Is_Alias_Of
     (Item : access Display_Item_Record) return Display_Item;
   --  Return the item for which Item is an alias, or null if there is none.

   function Get_Name (Item : access Display_Item_Record) return String;
   --  Return the name of Item, or "" if there is no such name.

   function Get_Language
     (Item : access Display_Item_Record) return Language.Language_Access;
   --  Return the language for Item

   function Get_Num (Item : access Display_Item_Record) return Integer;
   --  Return the number of Item.

   function Get_Entity
     (Item : access Display_Item_Record'Class)
      return Items.Generic_Type_Access;
   --  Return the entity represented by the item. If the name hasn't been
   --  specified, this is what will always be used to represent the item.

   function Get_Debugger
     (Item : access Display_Item_Record'Class)
      return GVD.Process.Visual_Debugger;
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

   procedure Set_Format
     (Item   : access Display_Item_Record'Class;
      Format : Debugger.Value_Format);
   --  Set the display format for the item and all its children

   function Get_Format
     (Item : access Display_Item_Record) return Debugger.Value_Format;
   --  Get the display format for item

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

   overriding function Output_SVG_Item_Content
     (Item : access Display_Item_Record) return String;
   --  See inherited documentation

private

   type Display_Item_Record is new Browsers.Canvas.Browser_Item_Record with
   record
      Num          : Integer;
      Graph_Cmd    : GNAT.Strings.String_Access := null;
      Name         : GNAT.Strings.String_Access := null;
      Entity       : Items.Generic_Type_Access := null;
      Auto_Refresh : Boolean := True;
      Debugger     : GVD.Process.Visual_Debugger;

      Is_A_Variable : Boolean := True;
      --  Set to False if the item is not related to a variable

      Title_Height : Glib.Gint;

      Id           : GNAT.Strings.String_Access := null;
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

      Format         : Standard.Debugger.Value_Format :=
        Standard.Debugger.Default_Format;
   end record;

   type GVD_Link_Record is new Browsers.Canvas.Browser_Link_Record with record
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
