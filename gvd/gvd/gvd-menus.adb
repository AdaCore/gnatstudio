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
with Gtkada.Types;        use Gtkada.Types;
with Odd.Canvas;          use Odd.Canvas;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Widget;          use Gtk.Widget;

with Odd_Intl;            use Odd_Intl;
with Display_Items;       use Display_Items;
with Items;               use Items;
with Odd.Code_Editors;    use Odd.Code_Editors;
with Debugger;            use Debugger;
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

   Editor_Contextual_Menu_Name : constant String := "odd_editor_context";
   --  String used to store the editor contextual menu as a user data

   Debugger_Contextual_Menu_Name : constant String := "odd_debugger_context";
   --  String used to store the debugger command window contextual menu.

   -----------------
   -- local types --
   -----------------

   type Breakpoint_Record (File_Length : Natural) is record
      Process : Debugger_Process_Tab;
      File    : String (1 .. File_Length);
      Line    : Integer;
   end record;

   type Variable_Record (Name_Length : Natural) is record
      Process      : Debugger_Process_Tab;
      Name         : String (1 .. Name_Length);
      Auto_Refresh : Boolean;
   end record;

   type Item_Record is record
      Canvas : Odd_Canvas;
      Item   : Display_Item;
   end record;

   --------------------
   -- local packages --
   --------------------

   package Menu_User_Data is new Gtk.Object.User_Data (Gtk_Menu);

   package Check_Canvas_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, Odd_Canvas);
   package Check_Editor_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, Odd.Code_Editors.Code_Editor);
   package Widget_Breakpoint_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Breakpoint_Record);
   package Widget_Variable_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Variable_Record);
   package Item_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Item_Record);

   ----------------------
   -- local procedures --
   ----------------------

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a breakpoint on a specific line.

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a temporary breakpoint on a line, and continue execution.

   procedure Change_Align_On_Grid
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas);
   --  Callback for the "align on grid" contextual menu item.

   procedure Change_Detect_Aliases
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Canvas : Odd_Canvas);
   --  Callback for the "detect aliases" contextual menu item.

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor);
   --  Callback for the "show line numbers" contextual menu item.

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record);
   --  Callback for the "print variable" or "display variable" contextual menu
   --  items.

   procedure Update_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Item   : Item_Record);
   --  Callback for the "update value" contextual menu item.

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor);
   --  Callback for the "show lines with code" contextual menu item.

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
   end Change_Detect_Aliases;

   ----------------------
   -- Change_Line_Nums --
   ----------------------

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor) is
   begin
      Set_Show_Line_Nums (Editor, Get_Active (Item));
   end Change_Line_Nums;

   ----------------------------
   -- Change_Lines_With_Code --
   ----------------------------

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor) is
   begin
      Set_Show_Lines_With_Code (Editor, Get_Active (Item));
   end Change_Lines_With_Code;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line);
   end Set_Breakpoint;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line, Temporary => True);
      Continue (Br.Process.Debugger, Display => True);
   end Till_Breakpoint;

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

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record)
   is
      pragma Warnings (Off, Widget);
   begin
      if Var.Auto_Refresh then
         Text_Output_Handler
           (Var.Process, "graph display " & Var.Name & ASCII.LF,
            Is_Command => True);
         Process_User_Command (Var.Process, "graph display " & Var.Name);
      else
         Text_Output_Handler
           (Var.Process, "graph print " & Var.Name & ASCII.LF,
            Is_Command => True);
         Process_User_Command (Var.Process, "graph print " & Var.Name);
      end if;
   end Print_Variable;

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
     (Canvas    : access Odd_Canvas_Record'Class;
      Item      : access Display_Items.Display_Item_Record'Class;
      Component : Items.Generic_Type_Access)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
   begin

      --  Delete the previous contextual menu if needed.
      begin
         Menu := Menu_User_Data.Get (Canvas, Item_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error => null;
      end;

      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Hide all");
      Set_State (Mitem, State_Insensitive);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show all");
      Set_State (Mitem, State_Insensitive);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Update Value");
      Item_Handler.Connect
        (Mitem, "activate",
         Item_Handler.To_Marshaller (Update_Variable'Access),
         Item_Record'(Canvas => Odd_Canvas (Canvas),
                      Item   => Display_Item (Item)));
      Append (Menu, Mitem);

      Show_All (Menu);
      Menu_User_Data.Set (Canvas, Menu, Item_Contextual_Menu_Name);
      return Menu;
   end Item_Contextual_Menu;

   ----------------------------
   -- Editor_Contextual_Menu --
   ----------------------------

   function Editor_Contextual_Menu
     (Editor   : access Odd.Code_Editors.Code_Editor_Record'Class;
      Line     : Glib.Gint;
      Entity   : String)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;
   begin
      --  Destroy the previous menu (which we couldn't do earlier because
      --  of the call to popup. We will change every item anyway.

      begin
         Menu := Menu_User_Data.Get (Editor, Editor_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error => null;
      end;

      --  Create a new menu

      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Print " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'(Name_Length  => Entity'Length,
                          Name         => Entity,
                          Auto_Refresh => False,
                          Process      => Convert (Editor)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      Gtk_New (Mitem, Label => -"Display " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'(Name_Length  => Entity'Length,
                          Name         => Entity,
                          Auto_Refresh => True,
                          Process      => Convert (Editor)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      --  Display a separator

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Line specific items

      Gtk_New (Mitem, Label => -"Set Breakpoint on Line" & Gint'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Set_Breakpoint'Access),
         Breakpoint_Record'(File_Length  => Get_Current_File (Editor)'Length,
                            Process      => Convert (Editor),
                            File         => Get_Current_File (Editor),
                            Line         => Integer (Line)));

      Gtk_New (Mitem, Label => -"Continue Until Line" & Gint'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Till_Breakpoint'Access),
         Breakpoint_Record'(File_Length  => Get_Current_File (Editor)'Length,
                            Process      => Convert (Editor),
                            File         => Get_Current_File (Editor),
                            Line         => Integer (Line)));

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Editor specific items

      Gtk_New (Check, Label => -"Display Line Numbers");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Line_Nums (Editor));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Line_Nums'Access),
         Code_Editor (Editor));

      Gtk_New (Check, Label => -"Show lines with code");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Lines_With_Code (Editor));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Lines_With_Code'Access),
         Code_Editor (Editor));

      Show_All (Menu);
      Menu_User_Data.Set (Editor, Menu, Editor_Contextual_Menu_Name);
      return Menu;
   end Editor_Contextual_Menu;

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

