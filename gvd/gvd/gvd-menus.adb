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

with Gtk.Object;          use Gtk.Object;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtkada.Types;        use Gtkada.Types;
with Odd.Canvas;          use Odd.Canvas;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Widget;          use Gtk.Widget;

with Odd_Intl;            use Odd_Intl;
with Display_Items;       use Display_Items;
with Items;               use Items;
with Odd.Process;         use Odd.Process;

with Ada.Text_IO;         use Ada.Text_IO;

package body Odd.Menus is

   ---------------------
   -- local constants --
   ---------------------

   Contextual_Background_Menu_Name : constant String := "odd_bg_context";
   --  String used to store the background contextual menu as a user data in
   --  the canvas.

   Item_Contextual_Menu_Name : constant String := "odd_item_context";
   --  String used to store the item contextual menu  as a user data in the
   --  canvas.

   Debugger_Contextual_Menu_Name : constant String := "odd_debugger_context";
   --  String used to store the debugger command window contextual menu.

   -----------------
   -- local types --
   -----------------

   type Item_Record (Name_Length : Natural) is record
      Canvas         : Odd_Canvas;
      Item           : Display_Item;
      Component      : Items.Generic_Type_Access;
      Component_Name : String (1 .. Name_Length);
      Mode           : Display_Mode;
   end record;

   --------------------
   -- local packages --
   --------------------

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, Odd_Canvas);
   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- local procedures --
   ----------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas);
   --  Callback for the "align on grid" contextual menu item.

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas);
   --  Callback for the "detect aliases" contextual menu item.

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item.

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Clone the item or its selected component.

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Change the mode of a specific item to indicate whether the value of the
   --  item should be displayed

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Show all the subcomponents of the selected item.

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Hide all the subcomponents of the selected item.

   --------------------------
   -- Change_Align_On_Grid --
   --------------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas) is
   begin
      Align_On_Grid (Canvas, Get_Active (Item));
   end Change_Align_On_Grid;

   ---------------------------
   -- Change_Detect_Aliases --
   ---------------------------

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas)
   is
   begin
      Set_Detect_Aliases (Canvas, not Get_Detect_Aliases (Canvas));

      --  Recompute all the aliases
      Recompute_All_Aliases (Canvas);
   end Change_Detect_Aliases;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record)
   is
      pragma Warnings (Off, Widget);
   begin
      Display_Items.Update (Item.Canvas, Item.Item);
   end Update_Variable;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
   begin
      Set_Visibility (Item.Component.all, True, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Show_All;

   --------------
   -- Hide_All --
   --------------

   procedure Hide_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record)
   is
   begin
      Set_Visibility (Item.Component.all, False, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Hide_All;

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
      Item    : Item_Record)
   is
      pragma Warnings (Off, Widget);
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
     (Canvas : access Odd_Canvas_Record'Class)
     return Gtk.Menu.Gtk_Menu
   is
      Check : Gtk_Check_Menu_Item;
      Menu  : Gtk_Menu;
   begin
      Menu := Menu_User_Data.Get (Canvas, Contextual_Background_Menu_Name);
      return Menu;

   exception
      when Gtkada.Types.Data_Error =>
         Gtk_New (Menu);

         Gtk_New (Check, Label => -"Align On Grid");
         Set_Always_Show_Toggle (Check, True);
         Set_Active (Check, Get_Align_On_Grid (Canvas));
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, "activate",
            Check_Canvas_Handler.To_Marshaller (Change_Align_On_Grid'Access),
            Odd_Canvas (Canvas));

         Gtk_New (Check, Label => -"Detect Aliases");
         Set_Always_Show_Toggle (Check, True);
         Set_Active (Check, Get_Detect_Aliases (Canvas));
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, "activate",
            Check_Canvas_Handler.To_Marshaller (Change_Detect_Aliases'Access),
            Odd_Canvas (Canvas));

         Show_All (Menu);
         Menu_User_Data.Set (Canvas, Menu, Contextual_Background_Menu_Name);
         return Menu;
   end Contextual_Background_Menu;

   --------------------------
   -- Item_Contextual_Menu --
   --------------------------

   function Item_Contextual_Menu
     (Canvas         : access Odd_Canvas_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
      Radio : Gtk_Radio_Menu_Item;

   begin

      --  Delete the previous contextual menu if needed.
      begin
         Menu := Menu_User_Data.Get (Canvas, Item_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error => null;
      end;

      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Hide all " & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Hide_All'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show all " & Component_Name);
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Show_All'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Mitem);

      --  Display a separator

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      if Is_A_Variable (Item) then
         Gtk_New (Mitem, Label => -"Clone" & " " & Component_Name);
      else
         Gtk_New (Mitem, Label => -"Clone");
      end if;
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Clone_Component'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Update Value");
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Update_Variable'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Mitem);

      --  Display a separator
      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Radio, Widget_SList.Null_List, -"Show Value");
      Set_Active (Radio, Get_Display_Mode (Item) = Value);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Type");
      Set_Active (Radio, Get_Display_Mode (Item) = Type_Only);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Type_Only));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Gtk_New (Radio, Group (Radio), -"Show Value + Type");
      Set_Active (Radio, Get_Display_Mode (Item) = Type_Value);
      Item_Handler.Connect
        (Radio, "activate",
         Item_Handler.To_Marshaller (Change_Display_Mode'Access),
         Item_Record'(Name_Length    => Component_Name'Length,
                      Canvas         => Odd_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Type_Value));
      Append (Menu, Radio);
      Set_Always_Show_Toggle (Radio, True);

      Show_All (Menu);
      Menu_User_Data.Set (Canvas, Menu, Item_Contextual_Menu_Name);
      return Menu;
   end Item_Contextual_Menu;

   ------------------------------
   -- Debugger_Contextual_Menu --
   ------------------------------

   function Debugger_Contextual_Menu
     (Process  : access Odd.Process.Debugger_Process_Tab_Record'Class)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
   begin
      Menu := Menu_User_Data.Get
        (Process.Debugger_Text, Debugger_Contextual_Menu_Name);
      return Menu;

   exception
      when Gtkada.Types.Data_Error =>
         Gtk_New (Menu);

         Gtk_New (Mitem, Label => -"Info");
         Set_State (Mitem, State_Insensitive);
         Append (Menu, Mitem);

         Show_All (Menu);
         Menu_User_Data.Set
           (Process.Debugger_Text, Menu, Debugger_Contextual_Menu_Name);
         return Menu;
   end Debugger_Contextual_Menu;

end Odd.Menus;

