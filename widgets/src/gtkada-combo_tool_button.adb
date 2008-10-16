-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008, AdaCore                    --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with System;                   use System;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gdk.Event;                use Gdk.Event;
with Gdk.Rectangle;            use Gdk.Rectangle;
with Gdk.Screen;               use Gdk.Screen;
with Gdk.Window;               use Gdk.Window;
with Gtk.Arrow;                use Gtk.Arrow;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Button;               use Gtk.Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Fixed;                use Gtk.Fixed;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Image;                use Gtk.Image;
with Gtk.Menu_Shell;           use Gtk.Menu_Shell;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tool_Item;            use Gtk.Tool_Item;
with Gtk.Widget;               use Gtk.Widget;

with Traces;

package body Gtkada.Combo_Tool_Button is

   type Gtkada_Icon_Widget_Record is new
     Gtk.Fixed.Gtk_Fixed_Record with record
      Menu_Button : Gtk_Toggle_Button;
      Icon_Button : Gtk_Button;
      Menu        : Gtk_Menu;
      Stock_Id    : Unbounded_String;
   end record;
   type Gtkada_Icon_Widget is access all Gtkada_Icon_Widget_Record'Class;

   procedure Gtk_New
     (Widget   : out Gtkada_Icon_Widget;
      Stock_Id : String);

   procedure Set_Icon_Size
     (Widget : access Gtkada_Icon_Widget_Record'Class;
      Size   : Gtk_Icon_Size);

   procedure Set_Menu
     (Widget : access Gtkada_Icon_Widget_Record'Class;
      Menu   : Gtk_Menu);

   procedure On_State
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Icon_Widget);

   procedure On_Menu_Deactivate
     (Menu   : access Gtk_Menu_Record'Class;
      Widget : Gtkada_Icon_Widget);

   function On_Button_Press
     (Button : access Gtk_Toggle_Button_Record'Class;
      Event  : Gdk_Event;
      Widget : Gtkada_Icon_Widget) return Boolean;

   procedure On_Toggle
     (Button : access Gtk_Toggle_Button_Record'Class;
      Widget : Gtkada_Icon_Widget);

   procedure Menu_Detacher
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu          : access Gtk_Menu_Record'Class);
   pragma Convention (C, Menu_Detacher);

   package Button_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Gtkada_Icon_Widget);

   package Toggle_Button_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Toggle_Button_Record, Gtkada_Icon_Widget);

   package Toggle_Button_Return_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Gtk_Toggle_Button_Record, Boolean, Gtkada_Icon_Widget);

   package Menu_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Record, Gtkada_Icon_Widget);

   package Menu_Popup is new User_Menu_Popup
     (Gtkada_Icon_Widget_Record);

   procedure Menu_Position
     (Menu   : access Gtk_Menu_Record'Class;
      X      : out Gint;
      Y      : out Gint;
      Widget : access Gtkada_Icon_Widget_Record);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget   : out Gtkada_Icon_Widget;
      Stock_Id : String)
   is
      Hbox   : Gtk_Hbox;
      Arrow  : Gtk_Arrow;
      Menu   : Gtk_Menu;
      Item   : Gtk_Menu_Item;

   begin
      Widget := new Gtkada_Icon_Widget_Record;
      Gtk.Fixed.Initialize (Widget);

      Widget.Stock_Id := To_Unbounded_String (Stock_Id);

      Gtk_New (Widget.Menu_Button);
      Widget.Menu_Button.Set_Relief (Relief_None);
      Unset_Flags
        (Widget.Menu_Button, Can_Focus + Can_Default + Receives_Default);
      Widget.Put (Widget.Menu_Button, 0, 0);
      Toggle_Button_Callback.Connect
        (Widget.Menu_Button, Signal_Toggled,
         On_Toggle'Access, Widget);
      Toggle_Button_Return_Callback.Connect
        (Widget.Menu_Button, Signal_Button_Press_Event,
         Toggle_Button_Return_Callback.To_Marshaller (On_Button_Press'Access),
         Widget);

      Gtk_New_Hbox (Hbox);
      Add (Widget.Menu_Button, Hbox);

      Gtk.Arrow.Gtk_New (Arrow, Arrow_Down, Shadow_None);
      Hbox.Pack_End (Arrow, False, False, 0);
      Gtk_New (Widget.Icon_Button);
      Widget.Icon_Button.Set_Relief (Relief_None);
      Unset_Flags
        (Widget.Icon_Button, Can_Focus + Can_Default + Receives_Default);
      Widget.Put (Widget.Icon_Button, 0, 0);

      --  Create a default menu widget.
      Gtk_New (Menu);
      Gtk_New (Item, "Item 1");
      Add (Menu, Item);
      Gtk_New (Item, "Item 2");
      Add (Menu, Item);
      Show_All (Menu);
      Set_Menu (Widget, Menu);

      Button_Callback.Connect
        (Widget.Icon_Button, Signal_State_Changed,
         On_State'Access, Widget);
   end Gtk_New;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
     (Widget : access Gtkada_Icon_Widget_Record'Class;
      Size   : Gtk_Icon_Size)
   is
      Icon : Gtk_Image;
      Req  : Gtk_Requisition;
   begin
      Gtk_New (Icon, To_String (Widget.Stock_Id), Size);
      Set_Image (Widget.Icon_Button, Icon);
      Size_Request (Widget.Icon_Button, Req);
      Set_Size_Request
        (Widget.Menu_Button,
         Req.Width + 15,  --  ??? constant in gtkarrow.c
         Req.Height);
   end Set_Icon_Size;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu
     (Widget : access Gtkada_Icon_Widget_Record'Class;
      Menu   : Gtk_Menu)
   is
   begin
      if Widget.Menu /= Menu then
         if Widget.Menu /= null and then Visible_Is_Set (Widget.Menu) then
            Deactivate (Widget.Menu);
         end if;

         if Widget.Menu /= null then
            Detach (Widget.Menu);
         end if;

         Widget.Menu := Menu;

         if Widget.Menu /= null then
            Attach_To_Widget
              (Widget.Menu, Widget, Menu_Detacher'Access);
            Widget.Menu_Button.Set_Sensitive (True);
            Menu_Callback.Connect
              (Menu, Signal_Deactivate, On_Menu_Deactivate'Access,
               Gtkada_Icon_Widget (Widget));

         else
            Widget.Menu_Button.Set_Sensitive (False);
         end if;
      end if;
   end Set_Menu;

   --------------
   -- On_State --
   --------------

   procedure On_State
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Icon_Widget)
   is
      State : constant Gtk_State_Type := Get_State (Button);
   begin
      Set_State (Widget.Menu_Button, State);

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_State;

   ------------------------
   -- On_Menu_Deactivate --
   ------------------------

   procedure On_Menu_Deactivate
     (Menu   : access Gtk_Menu_Record'Class;
      Widget : Gtkada_Icon_Widget)
   is
      pragma Unreferenced (Menu);
   begin
      Widget.Icon_Button.Clicked;
      Widget.Menu_Button.Set_Active (False);

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_Menu_Deactivate;

   ---------------
   -- On_Toggle --
   ---------------

   function On_Button_Press
     (Button : access Gtk_Toggle_Button_Record'Class;
      Event  : Gdk_Event;
      Widget : Gtkada_Icon_Widget) return Boolean
   is
      pragma Unreferenced (Button);
   begin
      if Get_Button (Event) = 1 then
         Menu_Popup.Popup
           (Widget.Menu, Gtkada_Icon_Widget_Access (Widget), null, null,
            Menu_Position'Access,
            Get_Button (Event), Get_Time (Event));
         Select_First (Widget.Menu, False);
         Set_Active (Widget.Menu_Button, True);

         return True;
      end if;

      return False;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
         return False;
   end On_Button_Press;

   ---------------
   -- On_Toggle --
   ---------------

   procedure On_Toggle
     (Button : access Gtk_Toggle_Button_Record'Class;
      Widget : Gtkada_Icon_Widget) is
   begin
      if Widget.Menu = null then
         return;
      end if;

      if Button.Get_Active
        and then not Visible_Is_Set (Widget.Menu)
      then
         Menu_Popup.Popup
           (Widget.Menu, Gtkada_Icon_Widget_Access (Widget), null, null,
            Menu_Position'Access,
            0, 0);
         Select_First (Widget.Menu, False);
      end if;
   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_Toggle;

   -------------------
   -- Menu_Detacher --
   -------------------

   procedure Menu_Detacher
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu          : access Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Menu);
   begin
      Gtkada_Icon_Widget (Attach_Widget).Menu := null;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end Menu_Detacher;

   -------------------
   -- Menu_Position --
   -------------------

   procedure Menu_Position
     (Menu   : access Gtk_Menu_Record'Class;
      X      : out Gint;
      Y      : out Gint;
      Widget : access Gtkada_Icon_Widget_Record)
   is
      pragma Unreferenced (Menu);
      Menu_Req    : Gtk_Requisition;
      Monitor     : Gdk_Rectangle;
      Monitor_Num : Gint;
      Screen      : Gdk_Screen;
      Success     : Boolean;

      function Get_Screen
        (Widg : access Gtk_Widget_Record'Class) return Gdk_Screen;

      ----------------
      -- Get_Screen --
      ----------------

      function Get_Screen
        (Widg : access Gtk_Widget_Record'Class) return Gdk_Screen
      is
         function Internal (Widg : System.Address) return System.Address;
         pragma Import (C, Internal, "gtk_widget_get_screen");
         Stub : Gdk_Screen_Record;
      begin
         return Gdk_Screen
           (Get_User_Data (Internal (Get_Object (Widg)), Stub));
      end Get_Screen;

   begin
      Size_Request (Widget.Menu, Menu_Req);
      Screen := Get_Screen (Widget.Menu);
      Monitor_Num := Get_Monitor_At_Window (Screen, Get_Window (Widget));

      if Monitor_Num < 0 then
         Monitor_Num := 0;
      end if;

      Get_Monitor_Geometry (Screen, Monitor_Num, Monitor);

      Get_Origin (Get_Window (Widget), X, Y, Success);
      X := X + Widget.Get_Allocation_X;
      Y := Y + Widget.Get_Allocation_Y + Widget.Get_Allocation_Height;

      if Widget.Get_Allocation_Width > Menu_Req.Width then
         X := X + Widget.Get_Allocation_Width - Menu_Req.Width;
      end if;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end Menu_Position;

   --------------------------------------------------------------------------

   use Strings_Vector;

   package Tool_Button_Callback is new Gtk.Handlers.Callback
     (Gtkada_Combo_Tool_Button_Record);

   package Icon_Widget_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Gtkada_Combo_Tool_Button);

   procedure Update_Icon
     (Button : access Gtkada_Combo_Tool_Button_Record'Class);

   procedure On_Icon_Widget_Clicked
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   ----------------------------
   -- On_Icon_Widget_Clicked --
   ----------------------------

   procedure On_Icon_Widget_Clicked
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button)
   is
      pragma Unreferenced (Button);
   begin
      Tool_Button_Callback.Emit_By_Name (Widget, Signal_Clicked);

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_Icon_Widget_Clicked;

   -----------------
   -- Update_Icon --
   -----------------

   procedure Update_Icon
     (Button : access Gtkada_Combo_Tool_Button_Record'Class)
   is
   begin
      Set_Icon_Size (Button.Icon_Widget, Button.Get_Icon_Size);

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end Update_Icon;

   Class_Record : GObject_Class := Uninitialized_Class;
   Signals : constant chars_ptr_array :=
               (1 => New_String (String (Signal_Clicked)));
   Signal_Parameters : constant Signal_Parameter_Types :=
                         (1 => (1 => GType_None));

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button   : out Gtkada_Combo_Tool_Button;
      Stock_Id : String)
   is
   begin
      Button := new Gtkada_Combo_Tool_Button_Record;
      Initialize (Button, Stock_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Button   : access Gtkada_Combo_Tool_Button_Record'Class;
      Stock_Id : String)
   is
   begin
      Gtk.Tool_Item.Initialize (Button);
      Initialize_Class_Record
        (Object       => Button,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "GtkadaComboToolButton",
         Parameters   => Signal_Parameters);

      Unset_Flags (Button, Can_Focus + Can_Default + Receives_Default);
      Set_Homogeneous (Button, False);
      Button.Items    := Empty_Vector;
      Button.Selected := No_Index;

      Gtk_New (Gtkada_Icon_Widget (Button.Icon_Widget), Stock_Id);
      Add (Button, Button.Icon_Widget);

      Tool_Button_Callback.Connect
        (Button, Signal_Toolbar_Reconfigured, Update_Icon'Access);
      Icon_Widget_Callback.Connect
        (Button.Icon_Widget.Icon_Button, Signal_Clicked,
         On_Icon_Widget_Clicked'Access, Gtkada_Combo_Tool_Button (Button));
      Show_All (Button);
   end Initialize;

   --------------
   -- Set_Menu --
   --------------

   procedure Set_Menu
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Menu   : Gtk_Menu)
   is
   begin
      Set_Menu (Widget.Icon_Widget, Menu);
   end Set_Menu;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Item   : String)
   is
      First : constant Boolean := Widget.Items.Is_Empty;
   begin
      Widget.Items.Append (To_Unbounded_String (Item));

      if First then
         Widget.Selected := Widget.Items.First_Index;
      end if;
   end Add_Item;

   -------------------
   -- Add_Menu_Item --
   -------------------

   procedure Add_Menu_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Item   : Gtk_Menu_Item)
   is
   begin
      Widget.Icon_Widget.Menu.Add (Item);
   end Add_Menu_Item;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Item   : String)
   is
      Elem   : constant Unbounded_String := To_Unbounded_String (Item);
   begin
      for J in Widget.Items.First_Index .. Widget.Items.Last_Index loop
         if Widget.Items.Element (J) = Elem then
            Widget.Selected := J;
            return;
         end if;
      end loop;
      --  ??? raise something ?
   end Select_Item;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items
     (Widget : access Gtkada_Combo_Tool_Button_Record) is
   begin
      Widget.Items.Clear;
      Widget.Selected := No_Index;
   end Clear_Items;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record) return String is
   begin
      if Widget.Selected >= Widget.Items.First_Index
        and then Widget.Selected <= Widget.Items.Last_Index
      then
         return To_String (Widget.Items.Element (Widget.Selected));
      else
         return "";
      end if;
   end Get_Selected_Item;

end Gtkada.Combo_Tool_Button;
