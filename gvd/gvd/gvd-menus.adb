-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

with Gtk.Object;          use Gtk.Object;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Radio_Menu_Item; use Gtk.Radio_Menu_Item;
with Gtkada.Types;        use Gtkada.Types;
with GVD.Canvas;          use GVD.Canvas;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Widget;          use Gtk.Widget;

with Odd_Intl;            use Odd_Intl;
with Display_Items;       use Display_Items;
with Items;               use Items;
with GVD.Process;         use GVD.Process;
with Debugger;            use Debugger;
with GVD.Dialogs;         use GVD.Dialogs;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;

with Ada.Text_IO;         use Ada.Text_IO;
with Process_Proxies;     use Process_Proxies;
with Debugger;            use Debugger;

package body GVD.Menus is

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

   Call_Stack_Contextual_Menu_Name : constant String := "odd_call_stack";
   --  String used to store the debugger command window contextual menu.

   -----------------
   -- local types --
   -----------------

   type Item_Record (Name_Length : Natural) is record
      Canvas         : GVD_Canvas;
      Item           : Display_Item;
      Component      : Items.Generic_Type_Access;
      Component_Name : String (1 .. Name_Length);
      Mode           : Display_Mode;
   end record;

   package Item_Register is new Register_Generic
     (Item_Record, Gtk_Widget_Record);

   type Call_Stack_Record is record
      Process : Debugger_Process_Tab;
      Mask    : Stack_List_Mask;
   end record;

   --------------------
   -- local packages --
   --------------------

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, GVD_Canvas);
   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);
   package Item_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, GVD_Canvas);
   package Call_Stack_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, Call_Stack_Record);

   ----------------------
   -- local procedures --
   ----------------------

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "align on grid" contextual menu item.

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Callback for the "detect aliases" contextual menu item.

   procedure Display_Expression
     (Item   : access Gtk_Menu_Item_Record'Class;
      Canvas : GVD_Canvas);
   --  Popup a dialog to display any expression in the canvas

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item.

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Clone the item or its selected component.

   procedure Set_Value
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record);
   --  Set the value for a specific component

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

   procedure Change_Mask
     (Widget : access Gtk_Menu_Item_Record'Class;
      Mask   : Call_Stack_Record);
   --  Toggle the display of a specific column in the Stack_List window.

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

   procedure Display_Expression
     (Item   : access Gtk_Menu_Item_Record'Class;
      Canvas : GVD_Canvas)
   is
      Process : Debugger_Process_Tab :=
        Debugger_Process_Tab (Get_Process (Canvas));
   begin
      On_Display_Expression1_Activate (Process.Window);
   end Display_Expression;

   ---------------------
   -- Update_Variable --
   ---------------------

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record) is
   begin
      if not Item_Register.Register_Post_Cmd_If_Needed
        (Get_Process (Get_Debugger (Item.Item).Debugger),
         Widget, Update_Variable'Access, Item)
      then
         Display_Items.Update
           (Item.Canvas, Item.Item, Redisplay_Canvas => True);
      end if;
   end Update_Variable;

   --------------
   -- Show_All --
   --------------

   procedure Show_All
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      Set_Visibility (Item.Component, True, Recursive => True);
      Update_Resize_Display (Item.Item, True);
   end Show_All;

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

   -------------------------
   -- Change_Display_Mode --
   -------------------------

   procedure Change_Display_Mode
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      if not Item_Register.Register_Post_Cmd_If_Needed
        (Get_Process (Get_Debugger (Item.Item).Debugger),
         Widget, Change_Display_Mode'Access, Item)
      then
         if Get_Active (Gtk_Radio_Menu_Item (Widget))
           and then Get_Display_Mode (Item.Item) /= Item.Mode
         then
            Set_Display_Mode (Item.Item, Item.Mode);
         end if;
      end if;
   end Change_Display_Mode;

   ---------------------
   -- Clone_Component --
   ---------------------

   procedure Clone_Component
     (Widget  : access Gtk_Widget_Record'Class;
      Item    : Item_Record) is
   begin
      if not Item_Register.Register_Post_Cmd_If_Needed
        (Get_Process (Get_Debugger (Item.Item).Debugger),
         Widget, Clone_Component'Access, Item)
      then
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
      end if;
   end Clone_Component;

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
         Title    => "Setting value of " & Item.Component_Name,
         Message  => "Setting value of " & Item.Component_Name & ':',
         Position => Win_Pos_Mouse,
         Key      => "odd_set_value_dialog");
   begin
      if not Item_Register.Register_Post_Cmd_If_Needed
        (Get_Process (Get_Debugger (Item.Item).Debugger),
         Widget, Set_Value'Access, Item)
      then
         if S /= "" and then S (S'First) /= ASCII.Nul then
            Set_Variable
              (Get_Debugger (Item.Item).Debugger, Item.Component_Name, S);
            Update_Variable (Widget, Item);
         end if;
      end if;
   end Set_Value;

   -----------------
   -- Change_Mask --
   -----------------

   procedure Change_Mask
     (Widget : access Gtk_Menu_Item_Record'Class;
      Mask   : Call_Stack_Record)
   is
   begin
      Mask.Process.Backtrace_Mask :=
        Mask.Process.Backtrace_Mask xor Mask.Mask;
      Show_Call_Stack_Columns (Mask.Process);
   end Change_Mask;

   --------------------------------
   -- Contextual_Background_Menu --
   --------------------------------

   function Contextual_Background_Menu
     (Canvas : access GVD_Canvas_Record'Class) return Gtk.Menu.Gtk_Menu
   is
      Check : Gtk_Check_Menu_Item;
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu;

   begin
      Menu := Menu_User_Data.Get (Canvas, Contextual_Background_Menu_Name);
      return Menu;

   exception
      when Gtkada.Types.Data_Error =>
         --  The menu has not been created yet.

         Gtk_New (Menu);

         Gtk_New (Mitem, Label => -"Display Expression...");
         Append (Menu, Mitem);
         Item_Canvas_Handler.Connect
           (Mitem, "activate",
            Item_Canvas_Handler.To_Marshaller (Display_Expression'Access),
            GVD_Canvas (Canvas));

         Gtk_New (Mitem);
         Append (Menu, Mitem);

         Gtk_New (Check, Label => -"Align On Grid");
         Set_Always_Show_Toggle (Check, True);
         Set_Active (Check, Get_Align_On_Grid (Canvas));
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, "activate",
            Check_Canvas_Handler.To_Marshaller (Change_Align_On_Grid'Access),
            GVD_Canvas (Canvas));

         Gtk_New (Check, Label => -"Detect Aliases");
         Set_Always_Show_Toggle (Check, True);
         Set_Active (Check, Get_Detect_Aliases (Canvas));
         Append (Menu, Check);
         Check_Canvas_Handler.Connect
           (Check, "activate",
            Check_Canvas_Handler.To_Marshaller (Change_Detect_Aliases'Access),
            GVD_Canvas (Canvas));

         Show_All (Menu);
         Menu_User_Data.Set (Canvas, Menu, Contextual_Background_Menu_Name);
         return Menu;
   end Contextual_Background_Menu;

   --------------------------
   -- Item_Contextual_Menu --
   --------------------------

   function Item_Contextual_Menu
     (Canvas         : access GVD_Canvas_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String) return Gtk.Menu.Gtk_Menu
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
                      Canvas         => GVD_Canvas (Canvas),
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
                      Canvas         => GVD_Canvas (Canvas),
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
                      Canvas         => GVD_Canvas (Canvas),
                      Item           => Display_Item (Item),
                      Component      => Component,
                      Component_Name => Component_Name,
                      Mode           => Value));
      Append (Menu, Mitem);

      if Is_A_Variable (Item) then
         Gtk_New (Mitem, Label => -"Set Value of " & " " & Component_Name);
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
                      Mode           => Value));
      Append (Menu, Mitem);
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
                      Canvas         => GVD_Canvas (Canvas),
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
                      Canvas         => GVD_Canvas (Canvas),
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
                      Canvas         => GVD_Canvas (Canvas),
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
     (Process  : access GVD.Process.Debugger_Process_Tab_Record'Class)
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
         --  The menu has not been created yet

         Gtk_New (Menu);
         Gtk_New (Mitem, Label => -"Info");
         Set_State (Mitem, State_Insensitive);
         Append (Menu, Mitem);

         Show_All (Menu);
         Menu_User_Data.Set
           (Process.Debugger_Text, Menu, Debugger_Contextual_Menu_Name);
         return Menu;
   end Debugger_Contextual_Menu;

   --------------------------------
   -- Call_Stack_Contextual_Menu --
   --------------------------------

   function Call_Stack_Contextual_Menu
     (Process : access GVD.Process.Debugger_Process_Tab_Record'Class)
      return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
   begin
      Menu := Menu_User_Data.Get
        (Process.Debugger_Text, Call_Stack_Contextual_Menu_Name);
      return Menu;

   exception
      when Gtkada.Types.Data_Error =>
         --  The menu has not been created yet

         Gtk_New (Menu);
         Gtk_New (Mitem, Label => -"Toggle Frame Num");
         Append (Menu, Mitem);
         Call_Stack_Cb.Connect
           (Mitem, "activate",
            Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
            (Debugger_Process_Tab (Process), Frame_Num));

         Gtk_New (Mitem, Label => -"Toggle Subprogram Name");
         Append (Menu, Mitem);
         Call_Stack_Cb.Connect
           (Mitem, "activate",
            Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
            (Debugger_Process_Tab (Process), Subprog_Name));

         Gtk_New (Mitem, Label => -"Toggle Parameters");
         Append (Menu, Mitem);
         Call_Stack_Cb.Connect
           (Mitem, "activate",
            Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
            (Debugger_Process_Tab (Process), Params));

         Gtk_New (Mitem, Label => -"Toggle File Location");
         Append (Menu, Mitem);
         Call_Stack_Cb.Connect
           (Mitem, "activate",
            Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
            (Debugger_Process_Tab (Process), File_Location));

         Gtk_New (Mitem, Label => -"Toggle Program Counter");
         Append (Menu, Mitem);
         Call_Stack_Cb.Connect
           (Mitem, "activate",
            Call_Stack_Cb.To_Marshaller (Change_Mask'Access),
            (Debugger_Process_Tab (Process), Program_Counter));

         Show_All (Menu);
         Menu_User_Data.Set
           (Process.Debugger_Text, Menu, Call_Stack_Contextual_Menu_Name);
         return Menu;
   end Call_Stack_Contextual_Menu;

end GVD.Menus;

