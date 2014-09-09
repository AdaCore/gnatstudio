------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Browsers.Canvas;       use Browsers, Browsers.Canvas;
with Debugger;
with Glib;
with GNAT.Strings;
with Gtkada.Canvas_View;    use Gtkada.Canvas_View;
with GVD.Process;           use GVD.Process;
with Items;                 use Items;
with Language;

package Display_Items is

   type Display_Item_Record is new GPS_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   type GVD_Link_Record is new GPS_Link_Record with private;
   type GVD_Link is access all GVD_Link_Record'Class;

   Item_Name_In_Link : constant String := "@";
   --  Shortcut used to represent the dereferenced item when generating a new
   --  item.

   procedure Gtk_New
     (Item           : out Display_Item;
      Browser        : not null access Debugger_Data_View_Record'Class;
      Graph_Cmd      : String;
      Variable_Name  : String;
      Num            : Integer;
      Debugger       : access GVD.Process.Visual_Debugger_Record'Class;
      Auto_Refresh   : Boolean := True;
      Is_Dereference : Boolean := False;
      Default_Entity : Items.Generic_Type_Access := null);
   --  Create a new item to display the value of Variable_Name (or return an
   --  existing item if one matches).
   --
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
   --  Debugger can be null. In this case, the item will never be computed
   --
   --  Num must be specified, and is the number of the item

   procedure Update_Display (Item : not null access Display_Item_Record'Class);
   --  Recompute the GUI rendering for the item.
   --  This does not refresh the value read from the debugger, only its
   --  display.

   procedure Create_Link
     (Browser    : not null access Debugger_Data_View_Record'Class;
      From, To   : access Display_Item_Record'Class;
      Name       : String;
      Alias_Link : Boolean := False);
   --  Add a new link between two items.
   --  The link is not created if there is already a similar one.

   procedure Remove_With_Aliases
     (Item : access Display_Item_Record;
      Remove_Aliases : Boolean);
   --  Remove the item from the canvas.
   --  If Remove_Aliases is True, then all the items on the canvas that are
   --  aliases of Item are also removed.

   function Get_Graph_Cmd (Item : access Display_Item_Record) return String;
   --  Return the "graph display..." command used to create the item

   function Find_Item
     (Canvas : not null access Debugger_Data_View_Record'Class;
      Num    : Integer) return Display_Item;
   --  Return the item whose identifier is Num, or null if there is none

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

   procedure Update (Item : not null access Display_Item_Record'Class);
   --  Unconditionally update the value of Item after parsing the new value.
   --  This does not redraw the canvas or the item on the canvas, unless
   --  Redisplay_Canvas is True

   procedure Dereference_Item
     (Item      : access Display_Item_Record;
      Component : not null access Component_Item_Record'Class);
   --  Dereference a component of Item ("graph display" on it with a link from
   --  the item).

   procedure Reset_Recursive (Item : access Display_Item_Record'Class);
   --  Mark the corresponding entity as up-to-date (i.e. no longer display
   --  in red).

   --  Calls Reset_Recursive for the entity represented by the item.

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

   overriding procedure Destroy
     (Self     : not null access Display_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class);

private

   type Display_Item_Record is new GPS_Item_Record with record
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

   type GVD_Link_Record is new GPS_Link_Record with record
      Alias_Link : Boolean := False;
      --  True if this Link was created as a result of an aliasing operation.
      --  Such links are always deleted before each update, and recreated
      --  whenever an aliasing is detected.

      Name : Unbounded_String;

      Source_Component : Items.Generic_Type_Access := null;
      --  Component of the source item to which the link is attached (generally
      --  the dereferenced component itself. This is used when the links are
      --  attached to the components themselves instead of the item).
      --  If left to null, the link will be attached to the center of the
      --  source item.
   end record;

end Display_Items;
