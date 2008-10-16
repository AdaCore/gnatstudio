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

with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Gdk.Event;                use Gdk.Event;
with Gdk.Window;               use Gdk.Window;
with Gtk.Arrow;                use Gtk.Arrow;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Fixed;                use Gtk.Fixed;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Image;                use Gtk.Image;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Menu_Shell;           use Gtk.Menu_Shell;
with Gtk.Tool_Item;            use Gtk.Tool_Item;
with Gtk.Widget;               use Gtk.Widget;

with Traces;

package body Gtkada.Combo_Tool_Button is

   use Strings_Vector;

   ----------------------
   -- Class definition --
   ----------------------

   Class_Record : GObject_Class := Uninitialized_Class;
   Signals : constant chars_ptr_array :=
               (1 => New_String (String (Signal_Clicked)));
   Signal_Parameters : constant Signal_Parameter_Types :=
                         (1 => (1 => GType_None));

   --------------
   -- Handlers --
   --------------

   package Tool_Button_Callback is new Gtk.Handlers.Callback
     (Gtkada_Combo_Tool_Button_Record);

   package Button_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Gtkada_Combo_Tool_Button);

   package Items_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Item_Record, Gtkada_Combo_Tool_Button);

   package Menu_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Menu_Record, Gtkada_Combo_Tool_Button);

   package Menu_Popup is new User_Menu_Popup
     (Gtkada_Combo_Tool_Button_Record);

   package Toggle_Button_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Toggle_Button_Record, Gtkada_Combo_Tool_Button);

   package Toggle_Button_Return_Callback is
     new Gtk.Handlers.User_Return_Callback
       (Gtk_Toggle_Button_Record, Boolean, Gtkada_Combo_Tool_Button);

   ---------------------------
   -- Callback declarations --
   ---------------------------

   procedure On_State
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   procedure On_Menu_Deactivate
     (Menu   : access Gtk_Menu_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   function On_Button_Press
     (Button : access Gtk_Toggle_Button_Record'Class;
      Event  : Gdk_Event;
      Widget : Gtkada_Combo_Tool_Button) return Boolean;

   procedure On_Toggle
     (Button : access Gtk_Toggle_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   procedure Menu_Detacher
     (Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu          : access Gtk_Menu_Record'Class);
   pragma Convention (C, Menu_Detacher);

   procedure Menu_Position
     (Menu   : access Gtk_Menu_Record'Class;
      X      : out Gint;
      Y      : out Gint;
      Widget : access Gtkada_Combo_Tool_Button_Record);

   procedure On_Toolbar_Reconfigured
     (Button : access Gtkada_Combo_Tool_Button_Record'Class);

   procedure On_Icon_Widget_Clicked
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   procedure On_Menu_Item_Activated
     (Item   : access Gtk_Menu_Item_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   --------------
   -- On_State --
   --------------

   procedure On_State
     (Button : access Gtk_Button_Record'Class;
      Widget : Gtkada_Combo_Tool_Button)
   is
      State : constant Gtk_State_Type := Get_State (Button);
   begin
      if State = State_Active then
         Set_State (Widget.Menu_Button, State_Prelight);
      else
         Set_State (Widget.Menu_Button, State);
      end if;

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_State;

   ------------------------
   -- On_Menu_Deactivate --
   ------------------------

   procedure On_Menu_Deactivate
     (Menu   : access Gtk_Menu_Record'Class;
      Widget : Gtkada_Combo_Tool_Button)
   is
      pragma Unreferenced (Menu);
   begin
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
      Widget : Gtkada_Combo_Tool_Button) return Boolean
   is
      pragma Unreferenced (Button);
   begin
      if Get_Button (Event) = 1 then
         Menu_Popup.Popup
           (Widget.Menu, Gtkada_Combo_Tool_Button_Record (Widget.all)'Access,
            null, null, Menu_Position'Access,
            Get_Button (Event), Get_Time (Event));
         Widget.Menu.Select_Item (Widget.Menu.Get_Active);
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
      Widget : Gtkada_Combo_Tool_Button) is
   begin
      if Widget.Menu = null then
         return;
      end if;

      if Button.Get_Active
        and then not Visible_Is_Set (Widget.Menu)
      then
         Menu_Popup.Popup
           (Widget.Menu, Gtkada_Combo_Tool_Button_Record (Widget.all)'Access,
            null, null, Menu_Position'Access,
            0, 0);
         Widget.Menu.Select_Item (Widget.Menu.Get_Active);
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
      Gtkada_Combo_Tool_Button (Attach_Widget).Menu := null;

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
      Widget : access Gtkada_Combo_Tool_Button_Record)
   is
      pragma Unreferenced (Menu);
      Menu_Req    : Gtk_Requisition;
      Success     : Boolean;

   begin
      Size_Request (Widget.Menu, Menu_Req);
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

   ----------------------------
   -- On_Menu_Item_Activated --
   ----------------------------

   procedure On_Menu_Item_Activated
     (Item   : access Gtk_Menu_Item_Record'Class;
      Widget : Gtkada_Combo_Tool_Button)
   is
      Label : constant Gtk_Label := Gtk_Label (Item.Get_Child);
   begin
      Select_Item (Widget, Label.Get_Text);
      Tool_Button_Callback.Emit_By_Name (Widget, Signal_Clicked);
   end On_Menu_Item_Activated;

   -----------------------------
   -- On_Toolbar_Reconfigured --
   -----------------------------

   procedure On_Toolbar_Reconfigured
     (Button : access Gtkada_Combo_Tool_Button_Record'Class)
   is
      Icon : Gtk_Image;
      Req  : Gtk_Requisition;
   begin
      Gtk_New (Icon, To_String (Button.Stock_Id), Button.Get_Icon_Size);
      Set_Image (Button.Icon_Button, Icon);
      Size_Request (Button.Icon_Button, Req);
      Set_Size_Request
        (Button.Menu_Button,
         Req.Width + 15,  --  ??? constant in gtkarrow.c
         Req.Height);

   exception
      when E : others =>
         Traces.Trace (Traces.Exception_Handle, E);
   end On_Toolbar_Reconfigured;

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
      Hbox   : Gtk_Hbox;
      Arrow  : Gtk_Arrow;
      Fixed  : Gtk_Fixed;

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
      Button.Stock_Id := To_Unbounded_String (Stock_Id);

      Gtk_New (Fixed);
      Button.Add (Fixed);

      Gtk_New (Button.Menu_Button);
      Button.Menu_Button.Set_Relief (Relief_None);
      Unset_Flags
        (Button.Menu_Button, Can_Focus + Can_Default + Receives_Default);
      Gtk_New_Hbox (Hbox);
      Add (Button.Menu_Button, Hbox);
      Gtk.Arrow.Gtk_New (Arrow, Arrow_Down, Shadow_None);
      Hbox.Pack_End (Arrow, False, False, 0);
      Fixed.Put (Button.Menu_Button, 0, 0);

      Gtk_New (Button.Icon_Button);
      Button.Icon_Button.Set_Relief (Relief_None);
      Unset_Flags
        (Button.Icon_Button, Can_Focus + Can_Default + Receives_Default);
      Fixed.Put (Button.Icon_Button, 0, 0);

      --  Create a default menu widget.
      Clear_Items (Button);

      Add_Item (Button, "Item 1");
      Add_Item (Button, "Item 2");
      Select_Item (Button, "Item 2");

      --  Update icon size upon toolbar reconfigured
      Tool_Button_Callback.Connect
        (Button, Signal_Toolbar_Reconfigured,
         On_Toolbar_Reconfigured'Access);
      --  Display menu upon toggle button toggled or clicked
      Toggle_Button_Callback.Connect
        (Button.Menu_Button, Signal_Toggled,
         On_Toggle'Access,
         Gtkada_Combo_Tool_Button (Button));
      Toggle_Button_Return_Callback.Connect
        (Button.Menu_Button, Signal_Button_Press_Event,
         Toggle_Button_Return_Callback.To_Marshaller (On_Button_Press'Access),
         Gtkada_Combo_Tool_Button (Button));
      --  Keep appearance of toggle button synchronized with icon button
      Button_Callback.Connect
        (Button.Icon_Button, Signal_State_Changed,
         On_State'Access,
         Gtkada_Combo_Tool_Button (Button));
      --  Handle single click on icon button
      Button_Callback.Connect
        (Button.Icon_Button, Signal_Clicked,
         On_Icon_Widget_Clicked'Access,
         Gtkada_Combo_Tool_Button (Button));

      Show_All (Button);
   end Initialize;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Widget   : access Gtkada_Combo_Tool_Button_Record;
      Item     : String;
      Stock_Id : String := "")
   is
      pragma Unreferenced (Stock_Id);
      First  : constant Boolean := Widget.Items.Is_Empty;
      M_Item : Gtk_Menu_Item;

   begin
      Gtk_New (M_Item, Item);
      Widget.Menu.Add (M_Item);
      Show_All (M_Item);
      Add (Widget.Menu, M_Item);
      Items_Callback.Connect
        (M_Item, Gtk.Menu_Item.Signal_Activate, On_Menu_Item_Activated'Access,
         Gtkada_Combo_Tool_Button (Widget));

      Widget.Items.Append (To_Unbounded_String (Item));
      Widget.Menu_Button.Set_Sensitive (True);

      if First then
         Widget.Selected := Widget.Items.First_Index;
         Widget.Menu.Set_Active (0);
         Widget.Menu.Select_Item
           (Widget.Menu.Get_Active);
      end if;
   end Add_Item;

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
            Widget.Menu.Set_Active (Guint (J));

            return;

         end if;
      end loop;
      --  ??? raise something ?
   end Select_Item;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items (Widget : access Gtkada_Combo_Tool_Button_Record) is
   begin
      Widget.Items.Clear;
      Widget.Selected := No_Index;

      if Widget.Menu /= null then
         if Visible_Is_Set (Widget.Menu) then
            Deactivate (Widget.Menu);
         end if;

         Detach (Widget.Menu);
      end if;

      Gtk_New (Widget.Menu);

      Attach_To_Widget
        (Widget.Menu, Widget, Menu_Detacher'Access);
      Menu_Callback.Connect
        (Widget.Menu, Signal_Deactivate, On_Menu_Deactivate'Access,
         Gtkada_Combo_Tool_Button (Widget));
      Widget.Menu_Button.Set_Sensitive (False);
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
